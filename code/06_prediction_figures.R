# MS&E 226 Project Part 1 -- Figure 6.1 for the figures page.
# Reads results written by 04_prediction.R (no model fitting). Run from the project folder:
#   Rscript code/06_prediction_figures.R

library(data.table)

summ <- fread("results/results_prediction.csv")
cal  <- fread("results/calibration_prediction.csv")

ALG_NAME <- c(baseline = "Baseline", incumbency = "Incumbency only", logistic = "Logistic",
              lasso = "Lasso", tree = "Tree", forest = "Forest")
summ[, label := ifelse(set == "-", ALG_NAME[alg], paste0(ALG_NAME[alg], ", Set ", set))]
order_lab <- summ[folds == "random_226"][order(-log_loss)]$label
summ[, ypos := match(label, rev(order_lab))]
sty <- data.table(folds = c("random_226", "random_227", "district_226"),
                  pch = c(19, 1, 2), off = c(0.22, 0, -0.22),
                  text = c("Random folds, seed 226 (used for selection)",
                           "Random folds, seed 227", "Folds grouped by district"))
summ <- merge(summ, sty, by = "folds")

png("results/fig_6_1_prediction.png", width = 7.5, height = 3.9, units = "in", res = 250)
layout(matrix(1:2, 1), widths = c(1.35, 1))

# (a) CV log loss, mean +/- 1 SE over folds.
par(mar = c(4.2, 8.6, 2.2, 0.6), cex = 0.75)
y <- summ$ypos + summ$off
plot(summ$log_loss, y, pch = summ$pch, yaxt = "n", ylab = "",
     xlim = c(0.1, max(summ$log_loss + summ$se_log_loss)), ylim = c(0.6, length(order_lab) + 0.4),
     xlab = "10-fold CV log loss (lower is better)")
segments(summ$log_loss - summ$se_log_loss, y, summ$log_loss + summ$se_log_loss, y)
axis(2, at = seq_along(order_lab), labels = rev(order_lab), las = 1)
abline(v = summ[folds == "random_226" & alg == "incumbency", log_loss], lty = 3, col = "grey40")
legend("bottomright", sty$text, pch = sty$pch, bty = "n", cex = 0.85)
mtext("(a) Prediction error by combination", side = 3, line = 0.6, adj = 0, font = 2, cex = 0.85)

# (b) Calibration of out-of-fold predictions, point area proportional to candidates per bin.
par(mar = c(4.2, 4.2, 2.2, 0.8))
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "Mean predicted win probability",
     ylab = "Observed win rate")
abline(0, 1, lty = 3, col = "grey40")
models <- c("final (Set A)", "benchmark (Set B)")
pchs <- c(19, 2); cols <- c("black", "grey45")
for (i in 1:2) {
  m <- cal[model == models[i]]
  points(m$mean_predicted, m$observed_win_rate, pch = pchs[i], col = cols[i],
         cex = 0.6 + 2.4 * sqrt(m$n / max(cal$n)))
}
legend("topleft", c("Final: lasso, Set A", "Benchmark: forest, Set B"), pch = pchs, col = cols,
       bty = "n", cex = 0.85)
mtext("(b) Calibration", side = 3, line = 0.6, adj = 0, font = 2, cex = 0.85)
invisible(dev.off())
cat("Wrote results/fig_6_1_prediction.png\n")
