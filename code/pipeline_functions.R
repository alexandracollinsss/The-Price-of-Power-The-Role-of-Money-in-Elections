# MS&E 226 Project -- Section 6: functions shared by 04_prediction.R and Part 2.
# Part 2 usage:  source("code/pipeline_functions.R")
#                pipe <- readRDS("results/final_pipeline.rds")
#                p <- predict_pipeline(pipe, new_raw_data)      # predicted win probabilities
#                yhat <- as.integer(p > pipe$threshold)         # classification at the saved threshold

library(data.table)
library(glmnet)
library(ranger)

# ---- Fixed definitions (chosen before any Section 6 results were seen) -------------------
CATS  <- c("party", "ico.status", "cand.gender", "state")
MONEY <- c("num.givers", "total.receipts", "total.indiv.contribs", "total.unitemized",
           "total.pac.contribs", "total.contribs.from.candidate")

# Set A: known at filing, before the primaries (Section 1(b)).
# Set B: adds full-cycle DIME fundraising and ideology measures (retrospective).
FEATURE_SETS <- list(
  A = c(CATS, "district.pres.vs", "aligned"),
  B = c(CATS, "district.pres.vs", "aligned", MONEY, "recipient.cfscore", "contributor.cfscore")
)

P_CAP     <- 0.001  # predictions are kept in [0.001, 0.999] so one confident miss cannot dominate log loss
THRESHOLD <- 1/3    # missing an eventual winner costs 2x backing an eventual loser: predict 1 if p > 1/(1+2)

# Random forest tuning grids (12 settings each), chosen relative to the number of features.
RF_GRID <- list(
  A = CJ(mtry = c(1, 2, 3, 4),  min.node.size = c(5, 15, 40)),
  B = CJ(mtry = c(2, 4, 7, 10), min.node.size = c(5, 15, 40))
)
RF_TREES <- 500
INNER_K  <- 10      # inner folds for choosing the lasso penalty

# ---- Deterministic feature construction (no fitted quantities) ---------------------------
build_features <- function(raw) {
  d <- as.data.table(copy(raw))
  # Negative totals are refunds exceeding receipts; set to 0, then log (as in 03_missing_data.R).
  for (v in MONEY) set(d, j = v, value = log1p(pmax(as.numeric(d[[v]]), 0)))
  for (v in CATS) set(d, j = v, value = as.character(d[[v]]))
  # District presidential share for the candidate's own party; 0.5 for other parties.
  d[, aligned := fifelse(party == "100", district.pres.vs,
                         fifelse(party == "200", 1 - district.pres.vs, 0.5))]
  d
}

# ---- Preprocessing fitted on the training portion only ------------------------------------
fit_preprocessing <- function(tr, features) {
  nums <- setdiff(features, CATS)
  cats <- intersect(features, CATS)
  list(features = features, nums = nums, cats = cats,
       medians = sapply(nums, function(v) median(tr[[v]], na.rm = TRUE)),
       levels  = lapply(setNames(cats, cats), function(v) sort(unique(tr[[v]]))),
       mode    = sapply(cats, function(v) names(which.max(table(tr[[v]])))))
}

apply_preprocessing <- function(d, pp) {
  out <- as.data.frame(d)[, pp$features, drop = FALSE]
  for (v in pp$nums) out[[v]][is.na(out[[v]])] <- pp$medians[[v]]   # median imputation
  for (v in pp$cats) {
    x <- out[[v]]
    x[is.na(x) | !x %in% pp$levels[[v]]] <- pp$mode[[v]]               # unseen category -> most common
    out[[v]] <- factor(x, levels = pp$levels[[v]])
  }
  out
}

# One indicator per category (no dropped baseline), so the lasso penalty does not depend on
# which category happens to be the baseline.
design_matrix <- function(df, pp) {
  ca <- lapply(df[pp$cats], contrasts, contrasts = FALSE)
  model.matrix(~ ., data = df, contrasts.arg = ca)[, -1, drop = FALSE]
}

cap <- function(p) pmin(pmax(p, P_CAP), 1 - P_CAP)

# ---- Learning algorithms -------------------------------------------------------------------
# Each returns list(model, tuning). seed controls any internal randomness.
fit_algorithm <- function(alg, df, y, pp, set_name, seed) {
  if (alg == "baseline") return(list(model = mean(y), tuning = "none"))

  if (alg == "incumbency") {
    rates <- tapply(y, df$ico.status, mean)
    return(list(model = list(rates = rates, overall = mean(y)), tuning = "none"))
  }

  if (alg == "logistic") {
    n_warn <- 0
    fit <- withCallingHandlers(
      glm(y ~ ., data = cbind(df, y = y), family = binomial),
      warning = function(w) { n_warn <<- n_warn + 1; invokeRestart("muffleWarning") })
    return(list(model = fit, tuning = "none", glm_warnings = n_warn))
  }

  if (alg == "lasso") {
    X <- design_matrix(df, pp)
    set.seed(seed)
    foldid <- sample(rep(1:INNER_K, length.out = nrow(X)))
    cv <- cv.glmnet(X, y, family = "binomial", alpha = 1, foldid = foldid,
                    type.measure = "deviance")
    nz <- sum(coef(cv, s = "lambda.min")[-1] != 0)
    return(list(model = cv, lambda = cv$lambda.min, nonzero = nz, n_coef = ncol(X),
                tuning = sprintf("lambda=%.4g (%d of %d coefs nonzero)", cv$lambda.min, nz, ncol(X))))
  }

  if (alg == "forest") {
    yf <- factor(y, levels = c(0, 1))
    grid <- RF_GRID[[set_name]]
    oob <- sapply(seq_len(nrow(grid)), function(g) {
      f <- ranger(x = df, y = yf, probability = TRUE, num.trees = RF_TREES,
                  mtry = grid$mtry[g], min.node.size = grid$min.node.size[g],
                  respect.unordered.factors = "order", seed = seed)
      log_loss(y, f$predictions[, "1"])                                  # out-of-bag log loss
    })
    best <- grid[which.min(oob)]
    fit <- ranger(x = df, y = yf, probability = TRUE, num.trees = RF_TREES,
                  mtry = best$mtry, min.node.size = best$min.node.size,
                  respect.unordered.factors = "order", seed = seed)
    return(list(model = fit, mtry = best$mtry, min.node.size = best$min.node.size,
                tuning = sprintf("mtry=%d, min.node.size=%d", best$mtry, best$min.node.size)))
  }
  stop("unknown algorithm: ", alg)
}

predict_algorithm <- function(alg, fit, df, pp) {
  p <- switch(alg,
    baseline   = rep(fit$model, nrow(df)),
    incumbency = { r <- fit$model$rates[as.character(df$ico.status)]
                   ifelse(is.na(r), fit$model$overall, r) },
    logistic   = predict(fit$model, newdata = df, type = "response"),
    lasso      = as.numeric(predict(fit$model, newx = design_matrix(df, pp),
                                    s = "lambda.min", type = "response")),
    forest     = predict(fit$model, data = df)$predictions[, "1"])
  cap(as.numeric(p))
}

# ---- Metrics -------------------------------------------------------------------------------
log_loss <- function(y, p) { p <- cap(p); -mean(y * log(p) + (1 - y) * log(1 - p)) }

auc <- function(y, p) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ---- Saved pipeline ------------------------------------------------------------------------
# raw: rows in the same format as data/dime_transformed.csv (outcome not needed).
predict_pipeline <- function(pipe, raw) {
  d  <- build_features(raw)
  df <- apply_preprocessing(d, pipe$preprocessing)
  predict_algorithm(pipe$algorithm, pipe$fit, df, pipe$preprocessing)
}
