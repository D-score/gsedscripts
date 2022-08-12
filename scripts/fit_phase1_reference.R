# Fits PHASE1 growth reference to the fixed LF and SF D-scores
#
# Prerequisites: Run models/293_0.R to estimate the phase1 key
# 20220811 SvB

library(dplyr)
library(ggplot2)

# If needed, install packages from GitHub
if (!requireNamespace("dscore", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dscore needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dscore")
}
if (packageVersion("dscore") < "1.5.8") stop("Needs dscore 1.5.8")
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.63.1") stop("Needs dmetric 0.63.1")

library("dscore")
library("dmetric")

# get all data
suppressWarnings(source("scripts/assemble_data.R"))
source("scripts/edit_data.R")

# select instrument data and pre-process, select fixed administration
adm <- c("cohort", "cohortn", "subjid", "agedays", "ins")
cn <- colnames(work)
items <- cn[starts_with(c("gpa", "gto"), vars = cn)]
items_lf <- cn[starts_with("gto", vars = cn)]
items_sf <- cn[starts_with("gpa", vars = cn)]
data <- work %>%
  filter(adm == "fixed") %>%
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohort = recode(cohort, "11-GSED" = "GSED-BGD", "17-GSED" = "GSED-PAK", "20-GSED" = "GSED-TZA"),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) %>%
  drop_na(agedays) %>%
  dplyr::select(all_of(adm), all_of(items))

# Set custom itembank
model_name <- "293_0"
path <- file.path("~/project/gsed/phase1/remodel", model_name)
model <- readRDS(file.path(path, "model.Rds"))
itembank <- data.frame(key = "custom", model$itembank)

# set prior_mean and prior_sd
use_new <- TRUE
data$pm <- dscore:::count_mu_phase1(data$agedays/365.25)
data$sd <- rep(5, nrow(data))
mean_name <- ifelse(use_new, "pm", ".gcdg")
sd_name   <- ifelse(use_new, "sd", "sd")
population <- ifelse(use_new, "gcdg", "gcdg")

d_lf <- dscore(data = dplyr::filter(data, ins == "lf"),
               items = items_lf,
               key = "custom",
               itembank = itembank,
               xname = "agedays",
               xunit = "days",
               prior_mean = mean_name,
               population = population)
d_sf <- dscore(data = dplyr::filter(data, ins == "sf"),
               items = items_sf,
               key = "custom",
               itembank = itembank,
               xname = "agedays",
               xunit = "days",
               prior_mean = mean_name,
               population = population)
datin <- bind_rows(d_lf, d_sf) %>%
  filter(daz > -6 & daz < 6) %>%
  drop_na()

# adapted from dmetric::fit_reference()
tx <- function(x) log(x + 100)
ty <- function(y) y
datin$y   <- datin$d
datin$t.y <- ty(datin$y)
datin$x   <- round(datin$a * 365.25)
datin$t.x <- tx(datin$x)
dat <- na.omit(datin[, c("x", "t.x", "y", "t.y")])

# fit the model
# LMS: selected df <- c(8, 2, 0)
# BCT: selected df <- c(8, 2, 0, 0)  WINNER!
library(gamlss)
## some grid search code outcommented
# grid <- expand.grid(df1 = 8, df2 = 0:5, df3 = 0, df4 = 0:2)
# grid$deviance <- NA
# for (i in 1:nrow(grid)) {
  # df <- grid[i, 1:4]
  df <- c(8, 2, 0, 0)
  family <- gamlss.dist::BCT()
  n.cyc <- 20
  fit <- gamlss(t.y ~ cs(t.x, df = df[1]),
                sigma.formula = ~ cs(t.x, df = df[2]),
                nu.formula = ~ cs(t.x, df = df[3]),
                tau.formula = ~ cs(t.x, df = df[4]),
                data = dat,
                family = family,
                control = gamlss.control(n.cyc = n.cyc, trace = FALSE))
#  grid[i, "deviance"] <- round(deviance(fit))
#  cat("\n", as.character(grid[i, ]), "\n")
# }

deviance(fit)
plot(fit)
centiles(fit, xvar = dat$x, cent = round(100 * pnorm(c(-2.5, -2:2, 2.5)), 1))
wp(fit, xvar = dat$x, n.inter = 16)

# grid for reference table (all weeks in data range)
rx <- range(dat$x , na.rm = TRUE)
weeks <- (floor(rx[1] / 7)) : (ceiling(rx[2] / 7))
grid_x <- (2:186) * 7
grid <- data.frame(x = grid_x,
                   t.x = tx(grid_x))

# calculate and round reference table
p <- predictAll(fit, newdata = grid)
reference <- data.frame(grid, p)
reference$t.x <- round(reference$t.x, 4)
reference$mu <- round(reference$mu, 2)
reference$sigma <- round(reference$sigma, 4)
reference$nu <- round(reference$nu, 4)
reference$tau <- round(reference$tau, 3)

# Calculate the parts for the prior mean function `dscore:::count_mu_phase1()`

# Results round 1:
# Count model: < 9MND: 21.3449 + 26.4916 t +  7.0251(t + 0.2)
# Count model: > 9MND & < 3.5 YR: 14.69947 - 12.18636 t + 69.11675(t + 0.92)
# Linear model: > 3.5 YRS: 61.40956 + 3.80904 t
#
# Results round 2:
# Count model: < 9MND: 20.5883 + 27.3376 t +  6.4254(t + 0.2)
# Count model: > 9MND & < 3.5 YR: 14.63748 - 12.11774 t + 69.05463(t + 0.92)
# Linear model: > 3.5 YRS: 61.37967 + 3.83513 t
#

# count < 9MND
dd <- data.frame(
  x = reference$x / 365.25,
  y = reference$mu)
dd <- dd[dd$x < 1, ]
mod1 <- lm(y ~ x, data = dd)
plot(dd$x, dd$y, cex = 0.5, ylim = c(10, 50))
points(dd$x, predict(mod1), col = "red", cex = 0.5)
summary(mod1)

# count 9mnd-3.5
dd <- data.frame(
  x = reference$x / 365.25,
  y = reference$mu)
mod2 <- lm(y ~ x + log(x + 0.92), data = dd)
plot(dd$x, dd$y, cex = 0.5)
points(dd$x, predict(mod2), col = "red", cex = 0.5)
summary(mod2)

# linear +3.5
dd <- dd[dd$x > 3.4, ]
mod3 <- lm(y ~ x, data = dd)
plot(dd$x, dd$y, cex = 0.5)
points(dd$x, predict(mod3), col = "red", cex = 0.5)
summary(mod3)

# Conclusion: Happy with round2 estimates for prior (very close to round3)

# process and save PHASE1 reference table, weeks 2-168
reference <- reference %>%
  mutate(day = x,
         week = day / 7,
         month = round(12 * day / 365.25, 3),
         year = round(day / 365.25, 4)) %>%
  dplyr::select(day, week, month, year, mu, sigma, nu, tau)

write.table(reference,
            file = "phase1.txt",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)

# transfer phase1.txt to dscore package
