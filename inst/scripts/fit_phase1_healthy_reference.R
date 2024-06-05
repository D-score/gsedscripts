# Fits "phase1_health" growth reference to the fixed LF and SF D-scores
# on a healthy subsample of GSED.
#
# Estimates the reference table from the Phase I healthy subsample
#
# Changes made relative to 20240523 reference table (dscore 1.8.7):
# - D-score estimation uses the new model 20240601 with correct scale factor
# - Calculates the D-score for SF and LF separately (not combined)
# - Tunes the GAMSLSS model to fit the healthy subsample
#
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private)
#
# 20240523 IE ("phase1_healthy", part of dscore 1.8.7)
# 20240606 SvB

# Load standard packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(gamlss, quietly = TRUE, warn.conflicts = FALSE)

# If needed, install packages from GitHub
if (!requireNamespace("dscore", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dscore needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dscore")
}
if (packageVersion("dscore") < "1.8.8") stop("Needs dscore 1.8.8")

if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.65.0") stop("Needs dmetric 0.65.0")

if (!requireNamespace("gsedscripts", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package gsedscripts needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/gsedscripts")
}
if (packageVersion("gsedscripts") < "0.5.0") stop("Needs gsedscripts 0.5.0")

# Get the LF and SF data
suppressWarnings(source(system.file("scripts/assemble_data.R",
                                    package = "gsedscripts")))
source(system.file("scripts/edit_data.R", package = "gsedscripts"))

# Select LF and SF item responses (fixed administration)
adm <- c("cohort", "cohortn", "subjid", "agedays", "ins")
cn <- colnames(work)
items <- cn[starts_with(c("gpa", "gto"), vars = cn)]
items_lf <- cn[starts_with("gto", vars = cn)]
items_sf <- cn[starts_with("gpa", vars = cn)]
data <- work |>
  filter(adm == "fixed" & ins %in% c("lf", "sf")) |>
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohort = recode(cohort, "11-GSED" = "GSED-BGD", "17-GSED" = "GSED-PAK",
                    "20-GSED" = "GSED-TZA"),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L,
                                   .default = NA_integer_))) |>
  tidyr::drop_na(agedays) |>
  dplyr::select(all_of(adm), all_of(items))

# Read healthy subset ID's from onedrive
onedrive <- Sys.getenv("ONEDRIVE_GSED")
taz_ids <- file.path(onedrive, "Pemba Validation/TZA Healthy SAMPLE IDS  19Nov2021.xlsx")
pak_ids <- file.path(onedrive, "Pakistan Validation/PAKISTAN Healthy SAMPLE IDS 16Nov2021.xlsx")
ban_ids <- file.path(onedrive, "Bangladesh Validation/BANGLADESH Healthy SAMPLE IDS 16Nov2021.xlsx")

healthy_taz <- readxl::read_excel(taz_ids) |>
  rename(GSED_ID = gsed_id) |>
  mutate(COUNTRY = "Tanzania") |>
  select(GSED_ID, HEALTHY)
healthy_pak <- readxl::read_excel(pak_ids, sheet = "List of IDs") |>
  rename(GSED_ID = `GSED ID`) |>
  mutate(COUNTRY = "Pakistan",
         HEALTHY = ifelse(is.na(`Healthy subsample`), 0, 1)) |>
  select(GSED_ID, HEALTHY)
healthy_ban <- readxl::read_excel(ban_ids, sheet = "List OF IDs") |>
  mutate(HEALTHY = ifelse(Sample_Type == "Healthy", 1, 0),
         COUNTRY = "Bangladesh") |>
  select(GSED_ID, HEALTHY)

healthy <-  rbind(healthy_taz, healthy_pak, healthy_ban) |>
  rename(subjido = GSED_ID,
         healthy = HEALTHY) |>
  mutate(healthy = as.integer(healthy),
         cohort = strtrim(subjido, 7),
         cohort = recode(cohort, "11-GSED" = "GSED-BGD",
                         "17-GSED" = "GSED-PAK", "20-GSED" = "GSED-TZA"),
         cohortn = as.integer(strtrim(subjido, 2)) + 100L,
         subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12))) |>
  select(subjid, healthy)

cat("Number of unique IDs in XLS:", length(unique(healthy$subjid)), "\n")
cat("Number of unique IDs in data:", length(unique(data$subjid)), "\n")
cat("Number of common IDs:", length(intersect(unique(healthy$subjid),
                                              unique(data$subjid))), "\n")

# Add variable "healthy" to data
data <- data |>
  full_join(y = healthy, by = "subjid") |>
  select(all_of(c(adm, "healthy", items)))

# How many cases do we have?
# table(data$ins, data$healthy, useNA = "al")
# table(data$ins, data$healthy, data$cohort, useNA = "al")

# Select healthy subsample data
data_hss <- data |>
  filter(healthy == 1 & !is.na(ins))

# How many healthy cases do we have?
table(data_hss$ins, data_hss$cohort)
# GSED-BGD GSED-PAK GSED-TZA
# lf      709      616      891
# sf      777      625      893

# Set prior_mean and prior_sd
use_new <- TRUE
data_hss$pm_old <- dscore:::count_mu_phase1(data_hss$agedays/365.25)
data_hss$pm_new <- dscore:::count_mu_phase1_healthy(data_hss$agedays/365.25)
data_hss$sd <- rep(5, nrow(data_hss))
mean_name <- ifelse(use_new, "pm_new", "pm_old")
sd_name   <- ifelse(use_new, "sd", "sd")

# Set population
population <- ifelse(use_new, "phase1_healthy", "phase1")

# Calculate LF and SF D-scores separately for hss
lf_hss <- data_hss |>
  filter(ins == "lf")
d <- dscore::dscore(
  data = lf_hss,
  items = items_lf,
  xname = "agedays",
  xunit = "days",
  prior_mean = mean_name,
  prior_sd = sd_name,
  population = population)
lf_hss <- lf_hss |>
  bind_cols(d) |>
  select(all_of(adm), names(d))

sf_hss <- data_hss |>
  filter(ins == "sf")
d <- dscore::dscore(
  data = sf_hss,
  items = items_sf,
  xname = "agedays",
  xunit = "days",
  prior_mean = mean_name,
  prior_sd = sd_name,
  population = population)
sf_hss <- sf_hss |>
  bind_cols(d) |>
  select(all_of(adm), names(d))

hss <- bind_rows(lf_hss, sf_hss) |>
  drop_na(d)

# Transform x and y, adapted from dmetric::fit_reference()
tx <- function(x) log(x + 100)
ty <- function(y) y
hss$y   <- hss$d
hss$t.y <- ty(hss$y)
hss$x   <- round(hss$a * 365.25)
hss$t.x <- tx(hss$x)

# Fit the model
fit <- lms(y = t.y, x = x, trans.x = FALSE, families = c("BCT", "BCCG", "NO"),
           mu.df = 5, sigma.df = 4, data = hss)

# chosen model BCT: mu.df = 7, sigma.df = 6, nu.df = 2, tau.df = 2
# Note: EDF in fit are all 2 higher than specified.

# Check the fit
deviance(fit)
plot(fit)
centiles(fit, xvar = hss$x, cent = round(100 * pnorm(c(-2.5, -2:2, 2.5)), 1))
Q.stats(fit, xvar = hss$x, n.inter = 16)
wp(fit, xvar = hss$x, n.inter = 16)

# grid for reference table (all weeks in data range)
rx <- range(hss$x , na.rm = TRUE)
weeks <- (floor(rx[1] / 7)) : (ceiling(rx[2] / 7))
grid_x <- (2:186) * 7
grid <- data.frame(x = grid_x,
                   t.x = tx(grid_x))

# calculate and round reference table
p <- predictAll(fit, newdata = grid)
reference <- data.frame(grid, p)
reference$x <- round(reference$x, 4)
reference$mu <- round(reference$mu, 2)
reference$sigma <- round(reference$sigma, 4)
reference$nu <- round(reference$nu, 4)
reference$tau <- round(reference$tau, 3)

# Calculate the parts for the prior mean function `dscore:::count_mu_phase1()`

# Results round 1:
# Round 1 model
# t[t1] <- suppressWarnings(24.226 + 24.057 * t[t1] + 8.996 * log(t[t1] + 0.2))
# t[t2] <- suppressWarnings(18.012 - 9.561 * t[t2] + 62.214 * log(t[t2] + 0.92))
# t[t3] <- suppressWarnings(63.0822 + 3.9134 * t[t3])
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
mod1 <- lm(y ~ x + log(x + 0.2), data = dd)
plot(dd$x, dd$y, cex = 0.5, ylim = c(10, 50))
points(dd$x, predict(mod1), col = "red", cex = 0.5)
summary(mod1)

# count 9mnd-3.5
dd <- data.frame(
  x = reference$x / 365.25,
  y = reference$mu)
dd <- dd[dd$x > 0.75, ]
mod2 <- lm(y ~ x + log(x + 0.92), data = dd)
plot(dd$x, dd$y, cex = 0.5)
points(dd$x, predict(mod2), col = "red", cex = 0.5)
summary(mod2)

# linear +3.5
dd <- dd[dd$x > 3.25, ]
mod3 <- lm(y ~ x, data = dd)
plot(dd$x, dd$y, cex = 0.5)
points(dd$x, predict(mod3), col = "red", cex = 0.5)
summary(mod3)

# combine 0-3.5 years
dd <- data.frame(
  x = reference$x / 365.25,
  y = reference$mu)
dd$mu <- predict(mod1, newdata = dd)

# Conclusion: Happy with round3 estimates for prior (very close to round4)

# process and save PHASE1 reference table, weeks 2-168
reference <- reference |>
  mutate(day = x,
         week = day / 7,
         month = round(12 * day / 365.25, 3),
         year = round(day / 365.25, 4)) |>
  dplyr::select(day, week, month, year, mu, sigma, nu, tau)

write.table(reference,
            file = "phase1_healthy.txt",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)

# transfer phase1.txt to dscore package

