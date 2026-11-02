# MS&E 226 Project Part 1 -- Section 5(e): compare ways of handling missing values.
# Reads the development data only. Every option uses the same folds, and fill values are
# calculated on the training folds only, then applied to the validation fold.
#
# Held fixed for every option, so that only the missing-data handling changes:
#   - dollar and donor-count columns are logged (Lecture 2, slide 31: skewed data);
#   - district share is also expressed for the candidate's own party.
# This is not the final feature set; that is chosen in Section 6.

library(data.table)
library(rpart)

K <- 10
SEEDS <- c(226, 227)

d <- fread("data/dime_transformed.csv", showProgress = FALSE)

CATS <- c("party", "ico.status", "cand.gender", "state")
MONEY <- c("num.givers", "total.receipts", "total.indiv.contribs", "total.unitemized",
           "total.pac.contribs", "total.contribs.from.candidate")

# Negative totals are refunds exceeding receipts, so they are set to 0 before logging.
for (v in MONEY) set(d, j = v, value = log1p(pmax(d[[v]], 0)))
# District share for the candidate's own party; 0.5 for other parties.
d[, aligned := fifelse(party == 100, district.pres.vs,
                       fifelse(party == 200, 1 - district.pres.vs, 0.5))]

# method: "rows" (delete incomplete rows), "columns" (delete columns with missing values),
#         or a fill value: "zero", "mean", "median"
spec <- function(method, flags = FALSE, dwdime = FALSE)
  list(method = method, flags = flags, dwdime = dwdime)

prepare <- function(tr, va, s) {
  tr <- copy(tr); va <- copy(va)
  has_na <- c("district.pres.vs", "aligned", "contributor.cfscore", if (s$dwdime) "dwdime")
  nums <- c(MONEY, "recipient.cfscore", has_na)

  if (s$method == "columns") nums <- setdiff(nums, has_na)
  if (s$method == "rows") tr <- tr[complete.cases(tr[, ..nums])]
  if (s$method %in% c("zero", "mean", "median")) {
    for (v in has_na) {
      fill <- switch(s$method, zero = 0, mean = mean(tr[[v]], na.rm = TRUE),
                     median = median(tr[[v]], na.rm = TRUE))
      # aligned is missing exactly when district.pres.vs is, so one flag covers both.
      if (s$flags && v != "aligned") {
        flag <- paste0("miss.", v)
        set(tr, j = flag, value = as.numeric(is.na(tr[[v]])))
        set(va, j = flag, value = as.numeric(is.na(va[[v]])))
        nums <- c(nums, flag)
      }
      set(tr, which(is.na(tr[[v]])), v, fill)
      set(va, which(is.na(va[[v]])), v, fill)
    }
  }

  # Categories unseen in training are assigned the most common training category.
  for (v in CATS) {
    ref <- names(which.max(table(tr[[v]])))
    lev <- unique(as.character(tr[[v]]))
    val <- as.character(va[[v]])
    val[!val %in% lev] <- ref
    set(tr, j = v, value = factor(as.character(tr[[v]]), levels = lev))
    set(va, j = v, value = factor(val, levels = lev))
  }
  list(tr = tr, va = va, nums = nums)
}

auc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

evaluate <- function(s, model, folds) {
  rbindlist(lapply(1:K, function(k) {
    p <- prepare(d[folds != k], d[folds == k], s)
    f <- as.formula(paste("won ~", paste(c(p$nums, CATS), collapse = " + ")))
    if (model == "logistic") {
      fit <- suppressWarnings(glm(f, data = p$tr, family = binomial))
      pred <- suppressWarnings(predict(fit, p$va, type = "response"))
    } else {
      fit <- rpart(f, data = p$tr, method = "class")
      pred <- predict(fit, p$va, type = "prob")[, "1"]
    }
    # Deleting rows leaves validation candidates with missing values unscorable. A real
    # candidate would still need a score, so they receive the training win rate.
    unscored <- if (s$method == "rows") !complete.cases(p$va[, p$nums, with = FALSE]) else
      rep(FALSE, nrow(p$va))
    pred[unscored | is.na(pred)] <- mean(p$tr$won)
    data.table(fold = k, loss = mean((pred > 0.5) != p$va$won), auc = auc(p$va$won, pred),
               unscored = sum(unscored))
  }))
}

OPTIONS <- list(
  "Delete rows" = spec("rows"),
  "Delete columns" = spec("columns"),
  "Zero" = spec("zero"),
  "Zero + flags" = spec("zero", TRUE),
  "Mean" = spec("mean"),
  "Mean + flags" = spec("mean", TRUE),
  "Median" = spec("median"),
  "Median + flags" = spec("median", TRUE),
  "Keep DW-DIME: delete rows" = spec("rows", dwdime = TRUE),
  "Keep DW-DIME: median" = spec("median", dwdime = TRUE),
  "Keep DW-DIME: median + flags" = spec("median", TRUE, dwdime = TRUE)
)
REF <- "Median + flags"

out <- rbindlist(lapply(SEEDS, function(seed) {
  set.seed(seed)
  folds <- sample(rep(1:K, length.out = nrow(d)))
  rbindlist(lapply(c("logistic", "tree"), function(m) {
    res <- lapply(OPTIONS, evaluate, model = m, folds = folds)
    base <- res[[REF]]
    rbindlist(Map(function(name, r) {
      data.table(seed = seed, model = m, option = name,
                 loss = mean(r$loss), auc = mean(r$auc),
                 d_auc_vs_ref = mean(r$auc - base$auc),
                 se_d_auc = sd(r$auc - base$auc) / sqrt(K),
                 d_loss_vs_ref = mean(r$loss - base$loss),
                 se_d_loss = sd(r$loss - base$loss) / sqrt(K),
                 pct_unscored = 100 * sum(r$unscored) / nrow(d))
    }, names(res), res))
  }))
}))

num <- setdiff(names(out), c("seed", "model", "option"))
out[, (num) := lapply(.SD, round, 4), .SDcols = num]
dir.create("results", showWarnings = FALSE)
fwrite(out, "results/results_missing_data.csv")
options(width = 170); print(out, nrows = 100)
cat("Baseline (predict that everyone loses): 0-1 loss", round(mean(d$won), 4), ", AUC 0.5\n")
