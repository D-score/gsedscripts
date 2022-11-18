# Calibrate scales of SF and LF
# This script creates a Tukey Sum-Difference plot (Bland Altman plot)
# for logit from the independent Rasch model for the full SF and LF
# Conclusion: linear transform SF|LF provides the best agreement
# Note: Must study the work of George Luta (On Optimal Correlation-Based
# Prediction, Am Stat 2022) for a better solution.
#
# Prerequisites: Run lf_155_0.R and sf_139_0.R
# R package fuzzyjoin
library(dplyr)
library(ggplot2)

sfm <- readRDS(file.path("~/project/gsed/phase1/sf/139_0", "model.Rds"))
lfm <- readRDS(file.path("~/project/gsed/phase1/lf/155_0", "model.Rds"))

# fuzzy match on gsed_id and agedays
# We allow for a one-day difference between the SF and LF measurement
# Turn subjid into number to make fuzzyjoin work
library(fuzzyjoin)
sf <- sfm$beta_l %>%
  mutate(id = 10L * as.integer(sub("-GSED-", "", subjid))) %>%
  select(id, agedays, study, a, d)
lf <- lfm$beta_l %>%
  mutate(id = 10L * as.integer(sub("-GSED-", "", subjid))) %>%
  select(id, agedays, study, a, d)
joined <- fuzzyjoin::difference_left_join(sf, lf, by = c("id", "agedays"),
                                          max_dist = 1, distance_col = "dist")

# Add sum and difference in the logit
# We stick to the class of *linear transforms* to correct for differences in
# location and spread in the logits
# d.x = SF logit; d.y = LF logit
joined <- joined %>%
  mutate(sum1 = d.x + d.y,
         dif1 = d.x - d.y,
         sum2 = -1.673 + 0.774 * d.x + d.y,
         dif2 = -1.673 + 0.774 * d.x - d.y,
         sum3 = d.x + 1.99 + 1.22 * d.y,
         dif3 = d.x - (1.99 + 1.22 * d.y))
# Calculate LF|SF (2) and SF|LF (3)
# lm(d.y ~ d.x, joined)
# lm(d.x ~ d.y, joined)

# create Tukey's Sum-Difference plot, three variations
theme_set(theme_light())
col_manual <- c("BGD" = "#D93F46", "PAK" = "#489033", "TZA" = "#47A1D8")

plot1 <- ggplot(joined, aes(x = sum1, y = dif1, group = study.x, colour = study.x)) +
  scale_colour_manual(values = col_manual,
                      na.value = "grey") +
  scale_x_continuous("SF + LF (logits)",
                     limits = c(-22, 12),
                     breaks = seq(-20, 10, 5)) +
  scale_y_continuous(
    paste0("SF - LF (logits)"),
    breaks = seq(-4, 4, 2),
    limits = c(-5, +5)
  ) +
  geom_hline(yintercept = c(-2, 0, 2), colour = "grey") +
  geom_point(
    size = 0.5,
    shape = 19
  ) +
  facet_wrap( ~ .data$study.x, ncol = 3) +
  geom_smooth(se = FALSE, colour = "white", size = 1.5) +
  theme(legend.position = "none")

plot2 <- ggplot(joined, aes(x = sum2, y = dif2, group = study.x, colour = study.x)) +
  scale_colour_manual(values = col_manual,
                      na.value = "grey") +
  scale_x_continuous("SF + LF (logits)",
                     limits = c(-22, 12),
                     breaks = seq(-20, 10, 5)) +
  scale_y_continuous(
    paste0("SF - LF (logits)"),
    breaks = seq(-4, 4, 2),
    limits = c(-5, +5)
  ) +
  geom_hline(yintercept = c(-2, 0, 2), colour = "grey") +
  geom_point(
    size = 0.5,
    shape = 19
  ) +
  facet_wrap( ~ .data$study.x, ncol = 3) +
  geom_smooth(se = FALSE, colour = "white", size = 1.5) +
  theme(legend.position = "none")

plot3 <- ggplot(joined, aes(x = sum3, y = dif3, group = study.x, colour = study.x)) +
  scale_colour_manual(values = col_manual,
                      na.value = "grey") +
  scale_x_continuous("SF + LF (logits)",
                     limits = c(-22, 12),
                     breaks = seq(-20, 10, 5)) +
  scale_y_continuous(
    paste0("SF - LF (logits)"),
    breaks = seq(-4, 4, 2),
    limits = c(-5, +5)
  ) +
  geom_hline(yintercept = c(-2, 0, 2), colour = "grey") +
  geom_point(
    size = 0.5,
    shape = 19
  ) +
  facet_wrap( ~ .data$study.x, ncol = 3) +
  geom_smooth(se = FALSE, colour = "white", size = 1.5) +
  theme(legend.position = "none")
plots <- list(plot1, plot2, plot3)

# save as one pdf with all variations
path <- file.path("~/project/gsed/phase1")
device <- "pdf"
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "tukey-sum-dif.pdf")
  pdf(file, onefile = TRUE, width = 12, height = 4)
  lapply(plots, print)
  message("Saved to: ", file)
  dev.off()
}
