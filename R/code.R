
# helper code/functions
report_1sample_ttest <- function(x) {
  ttest <- t.test(x)
  cat(sprintf("mean +/- sd = %.2f +/- %.2f, t(%d) = %.2f, p = %.3f, d = %.2f, 95%% CI: %.2f—%.2f\n",
              mean(x, na.rm = TRUE),
              sd(x, na.rm = TRUE),
              round(ttest$parameter),
              ttest$statistic,
              ttest$p.value,
              mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE),
              ttest$conf.int[1],
              ttest$conf.int[2]))
}

report_paired_ttest <- function(x, y) {
  ttest <- t.test(x, y, paired = TRUE)
  diff <- x - y
  cat(sprintf("mean +/- sd = %.2f +/- %.2f, t(%d) = %.2f, p = %.3f, d = %.2f, 95%% CI: %.2f—%.2f\n",
              mean(diff, na.rm = TRUE),
              sd(diff, na.rm = TRUE),
              round(ttest$parameter),
              ttest$statistic,
              ttest$p.value,
              mean(diff, na.rm = TRUE) / sd(diff, na.rm = TRUE),
              ttest$conf.int[1],
              ttest$conf.int[2]))
}

report_correlation <- function(x, y) {
  corr <- cor.test(x, y, use = "pairwise.complete.obs")
  cat(sprintf("r(%d) = %.2f, p = %.3f, 95%% CI: %.2f—%.2f\n",
              corr$parameter,
              corr$estimate,
              corr$p.value,
              corr$conf.int[1],
              corr$conf.int[2]))
}

report_lmer_effect <- function(model, term) {
  # Load necessary libraries
  library(parameters)
  library(effectsize)
  
