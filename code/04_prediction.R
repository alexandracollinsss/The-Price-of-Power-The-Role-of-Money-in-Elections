# MS&E 226 Project Part 1 -- Section 6: prediction.
# Reads the development data only. Run from the project folder: Rscript code/04_prediction.R
#
# Design (fixed before results were seen):
#   - Outcome: won (1 = won the 2022 general election).
#   - Primary metric: log loss on all candidates; secondary: AUC, both also on non-incumbents,
#     and a confusion matrix at threshold 1/3 (missing a winner costs 2x backing a loser).
#   - 10-fold CV with the same folds as 03_missing_data.R (fold seed 226). Selection uses these.
#   - All preprocessing (medians, category levels) and all tuning happen inside the training
#     portion of each fold; the validation fold is used only to score predictions.
#   - Final pipeline: best Set A (pre-primary) combination by CV log loss.
#     Secondary benchmark: best Set B (full-cycle) combination, saved and labeled retrospective.
# Robustness checks (added after the first run; not used for selection):
#   - random folds with seed 227;
#   - folds grouped by district (all candidates in a district are held out together), since
#     in use the director scores every candidate in a district at once;
#   - calibration of the out-of-fold predicted probabilities for the two saved pipelines.

source("code/pipeline_functions.R")

K <- 10
SELECTION <- "random_226"
ALGS <- c("logistic", "lasso", "forest")
TOP_K <- 50   # stakeholder view: winners among the 50 highest-ranked non-incumbents
CAL_BINS <- c(0, 0.05, 0.10, 0.20, 1/3, 0.50, 0.75, 1)

raw <- fread("data/dime_transformed.csv", showProgress = FALSE)
d <- build_features(raw)
cat("Development rows:", nrow(d), " winners:", sum(d$won), " districts:", uniqueN(d$district), "\n")

# ---- Fold schemes ------------------------------------------------------------------------
random_folds <- function(seed) { set.seed(seed); sample(rep(1:K, length.out = nrow(d))) }
district_folds <- function(seed) {
  u <- sort(unique(d$district))
  set.seed(seed); f <- sample(rep(1:K, length.out = length(u)))
  f[match(d$district, u)]
}
FOLD_SCHEMES <- list(random_226 = random_folds(226),     # identical to 03_missing_data.R
                     random_227 = random_folds(227),
                     district_226 = district_folds(226))
INNER_SEED_BASE <- c(random_226 = 226, random_227 = 227, district_226 = 2260)
stopifnot(all(sapply(split(FOLD_SCHEMES$district_226, d$district), uniqueN) == 1))

# Combinations: two references + 3 algorithms x 2 feature sets.
COMBOS <- rbind(data.table(alg = c("baseline", "incumbency"), set = "-"),
                CJ(alg = ALGS, set = names(FEATURE_SETS), sorted = FALSE))

# ---- Cross validation: out-of-fold predictions ---------------------------------------------
oof <- list(); tuning <- list()
for (scheme in names(FOLD_SCHEMES)) {
  folds <- FOLD_SCHEMES[[scheme]]
  for (k in 1:K) {
    tr <- d[folds != k]; va <- d[folds == k]
    for (i in seq_len(nrow(COMBOS))) {
      alg <- COMBOS$alg[i]; s <- COMBOS$set[i]
      feats <- if (s == "-") "ico.status" else FEATURE_SETS[[s]]
      pp  <- fit_preprocessing(tr, feats)                   # training portion only
      dtr <- apply_preprocessing(tr, pp); dva <- apply_preprocessing(va, pp)
      fit <- fit_algorithm(alg, dtr, tr$won, pp, s, seed = 1000 * INNER_SEED_BASE[[scheme]] + k)
      p   <- predict_algorithm(alg, fit, dva, pp)
      oof[[length(oof) + 1]] <- data.table(folds = scheme, fold = k, alg, set = s,
                                           row = which(folds == k), y = va$won,
                                           incumbent = va$ico.status == "I", p = p)
      tuning[[length(tuning) + 1]] <- data.table(folds = scheme, fold = k, alg, set = s,
        tuning = fit$tuning,
        lambda = if (is.null(fit$lambda)) NA_real_ else fit$lambda,
        nonzero = if (is.null(fit$nonzero)) NA_integer_ else fit$nonzero,
        mtry = if (is.null(fit$mtry)) NA_integer_ else fit$mtry,
        min.node.size = if (is.null(fit$min.node.size)) NA_integer_ else fit$min.node.size,
        glm_warnings = if (is.null(fit$glm_warnings)) NA_integer_ else fit$glm_warnings)
    }
  }
  cat("Finished fold scheme", scheme, "\n")
}
oof <- rbindlist(oof); tuning <- rbindlist(tuning)
# Note: glmnet may warn that very small penalties did not converge; it then returns the path for
# larger penalties. The selected penalty is checked below to lie inside the returned path.

# ---- Fold-level metrics, then means and standard errors --------------------------------------
by_fold <- oof[, .(log_loss = log_loss(y, p), auc = auc(y, p),
                   ni_log_loss = log_loss(y[!incumbent], p[!incumbent]),
                   ni_auc = auc(y[!incumbent], p[!incumbent])),
               by = .(folds, alg, set, fold)]

# Paired differences vs. the incumbency-only reference (same folds, so differences are paired).
ref <- by_fold[alg == "incumbency", .(folds, fold, ref_ll = log_loss, ref_ni_ll = ni_log_loss)]
by_fold <- merge(by_fold, ref, by = c("folds", "fold"))

se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
summ <- by_fold[, .(log_loss = mean(log_loss), se_log_loss = se(log_loss),
                    d_vs_incumbency = mean(log_loss - ref_ll), se_d = se(log_loss - ref_ll),
                    auc = mean(auc), ni_log_loss = mean(ni_log_loss),
                    ni_d_vs_incumbency = mean(ni_log_loss - ref_ni_ll),
                    se_ni_d = se(ni_log_loss - ref_ni_ll),
                    ni_auc = mean(ni_auc, na.rm = TRUE)),
                by = .(folds, alg, set)]

# Expected winners among the K highest predictions, breaking ties at random (the references
# give many candidates identical predictions, so a fixed tie order would be arbitrary).
top_k_winners <- function(y, p, k) {
  cut <- sort(p, decreasing = TRUE)[k]
  above <- p > cut; tied <- p == cut
  sum(y[above]) + (k - sum(above)) * mean(y[tied])
}

# Confusion matrix at the threshold, pooled over folds.
conf <- oof[, .(TP = sum(p > THRESHOLD & y == 1), FP = sum(p > THRESHOLD & y == 0),
                FN = sum(p <= THRESHOLD & y == 1), TN = sum(p <= THRESHOLD & y == 0)),
            by = .(folds, alg, set)]
# Top-K non-incumbents: rank within each fold (one fitted model per fold), take the top K/10
# per fold, and sum, so no ranking mixes predictions from different fitted models.
topk <- oof[, .(w = top_k_winners(y[!incumbent], p[!incumbent], TOP_K / K)),
            by = .(folds, alg, set, fold)][, .(ni_top_winners = sum(w)), by = .(folds, alg, set)]
conf <- merge(conf, topk, by = c("folds", "alg", "set"))
conf[, `:=`(TPR = TP / (TP + FN), FPR = FP / (FP + TN), precision = TP / pmax(TP + FP, 1),
            cost_per_cand = (FP + 2 * FN) / (TP + FP + FN + TN))]
summ <- merge(summ, conf, by = c("folds", "alg", "set"))

# Tuning summary: settings selected across the 10 folds.
tune_summ <- tuning[, .(
  selected = if (alg[1] == "lasso")
      sprintf("lambda %.4g-%.4g (median %.4g); %d-%d nonzero",
              min(lambda), max(lambda), median(lambda), min(nonzero), max(nonzero))
    else if (alg[1] == "forest")
      paste(names(sort(table(paste0("mtry=", mtry, ",node=", min.node.size)), decreasing = TRUE)),
            collapse = "; ")
    else "none",
  glm_warning_folds = sum(glm_warnings > 0, na.rm = TRUE)),
  by = .(folds, alg, set)]
summ <- merge(summ, tune_summ, by = c("folds", "alg", "set"))

summ[, folds := factor(folds, levels = names(FOLD_SCHEMES))]
summ[, alg := factor(alg, levels = c("baseline", "incumbency", ALGS))]
setorder(summ, folds, set, alg)
num <- names(summ)[sapply(summ, is.double)]
summ[, (num) := lapply(.SD, round, 4), .SDcols = num]

dir.create("results", showWarnings = FALSE)
fwrite(summ, "results/results_prediction.csv")
fwrite(by_fold, "results/results_prediction_folds.csv")
fwrite(tuning, "results/tuning_prediction.csv")
options(width = 200)
print(summ[, .(folds, alg, set, log_loss, se_log_loss, d_vs_incumbency, se_d, auc,
               ni_log_loss, ni_auc, ni_top_winners, TPR, FPR, precision, glm_warning_folds)])

# ---- Selection (pre-specified rule, random folds with seed 226) ----------------------------
pick <- function(s) summ[folds == SELECTION & set == s][which.min(log_loss)]
final_row <- pick("A"); bench_row <- pick("B")
cat("\nFinal (pre-primary, Set A):", as.character(final_row$alg), " CV log loss", final_row$log_loss, "\n")
cat("Benchmark (full-cycle, Set B):", as.character(bench_row$alg), " CV log loss", bench_row$log_loss, "\n")

# Paired comparison of the Set A winner against the other Set A algorithms, in every scheme.
for (sc in names(FOLD_SCHEMES)) {
  wd <- dcast(by_fold[folds == sc & set == "A"], fold ~ alg, value.var = "log_loss")
  for (a in setdiff(ALGS, as.character(final_row$alg))) {
    dd <- wd[[a]] - wd[[as.character(final_row$alg)]]
    cat(sprintf("  [%s] %s minus %s: %.4f (SE %.4f)\n", sc, a, final_row$alg, mean(dd), se(dd)))
  }
}
cat("Within-set ranking by CV log loss in each scheme:\n")
print(summ[set != "-", .(ranking = paste(alg[order(log_loss)], collapse = " < ")), by = .(folds, set)])

# ---- Calibration of out-of-fold probabilities (selection folds) -----------------------------
calibration <- function(combo, label) {   # (not "row": oof has a column named row)
  a <- as.character(combo$alg); st <- combo$set
  oof[folds == SELECTION & alg == a & set == st,
      .(n = .N, mean_predicted = mean(p), observed_win_rate = mean(y)),
      by = .(bin = cut(p, CAL_BINS, include.lowest = TRUE))][order(bin)][, model := label][]
}
cal <- rbind(calibration(final_row, "final (Set A)"), calibration(bench_row, "benchmark (Set B)"))
cal[, `:=`(mean_predicted = round(mean_predicted, 3), observed_win_rate = round(observed_win_rate, 3))]
fwrite(cal, "results/calibration_prediction.csv")
cat("\nCalibration (out-of-fold, random_226):\n"); print(cal)

# ---- Refit on all development data and save --------------------------------------------------
build_pipeline <- function(row, label) {
  alg <- as.character(row$alg); s <- row$set
  pp  <- fit_preprocessing(d, FEATURE_SETS[[s]])
  df  <- apply_preprocessing(d, pp)
  fit <- fit_algorithm(alg, df, d$won, pp, s, seed = 226)
  list(label = label, algorithm = alg, feature_set = s, features = FEATURE_SETS[[s]],
       preprocessing = pp, fit = fit, tuning = fit$tuning, threshold = THRESHOLD, cap = P_CAP,
       cv_log_loss = row$log_loss, trained_on = "data/dime_transformed.csv (development rows)",
       r_version = R.version.string,
       packages = sapply(c("data.table", "glmnet", "ranger"), function(p) as.character(packageVersion(p))),
       usage = "source('code/pipeline_functions.R'); p <- predict_pipeline(pipe, raw)")
}
final <- build_pipeline(final_row, "FINAL: pre-primary (Set A)")
bench <- build_pipeline(bench_row, "SECONDARY: full-cycle retrospective benchmark (Set B)")
saveRDS(final, "results/final_pipeline.rds")
saveRDS(bench, "results/full_cycle_benchmark_pipeline.rds")
cat("Final tuning on all development data:", final$tuning, "\n")
cat("Benchmark tuning on all development data:", bench$tuning, "\n")

# Sanity check: reloaded pipelines reproduce the in-memory predictions.
stopifnot(all.equal(predict_pipeline(readRDS("results/final_pipeline.rds"), raw),
                    predict_algorithm(final$algorithm, final$fit,
                                      apply_preprocessing(d, final$preprocessing), final$preprocessing)))

# Selected lasso penalties should not sit at the end of the penalty path.
if (final$algorithm == "lasso") {
  i <- which(final$fit$model$lambda == final$fit$model$lambda.min)
  cat("Final lasso penalty is", i, "of", length(final$fit$model$lambda), "on the path\n")
  b <- coef(final$fit$model, s = "lambda.min"); print(b[b[, 1] != 0, , drop = FALSE])
}

# ---- Figure: CV log loss (mean +/- 1 SE over folds), every combination and fold scheme -------
fig <- summ[, .(label = ifelse(set == "-", as.character(alg), paste0(alg, " (Set ", set, ")")),
                folds = as.character(folds), log_loss, se_log_loss)]
lev <- fig[folds == SELECTION][order(-log_loss)]$label
fig[, label := factor(label, levels = rev(lev))]
style <- data.table(folds = names(FOLD_SCHEMES), pch = c(19, 1, 2), off = c(0.2, 0, -0.2),
                    text = c("random folds, seed 226 (selection)", "random folds, seed 227",
                             "folds grouped by district"))
fig <- merge(fig, style, by = "folds")
png("results/fig_prediction_logloss.png", width = 7, height = 4.4, units = "in", res = 200)
par(mar = c(4.2, 10, 1, 1))
yy <- as.numeric(fig$label) + fig$off
plot(fig$log_loss, yy, xlim = c(0, max(fig$log_loss + 2 * fig$se_log_loss)), yaxt = "n",
     pch = fig$pch, xlab = "10-fold CV log loss (lower is better)", ylab = "")
segments(fig$log_loss - fig$se_log_loss, yy, fig$log_loss + fig$se_log_loss, yy)
axis(2, at = seq_along(levels(fig$label)), labels = levels(fig$label), las = 1, cex.axis = 0.85)
legend("bottomright", style$text, pch = style$pch, bty = "n", cex = 0.75)
invisible(dev.off())

writeLines(capture.output(sessionInfo()), "results/sessionInfo_prediction.txt")
cat("Done.\n")
