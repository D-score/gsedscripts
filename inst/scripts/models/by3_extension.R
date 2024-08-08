# This script calculates an extension of model 293_0 for BSID III items via
# a single group design for three instruments (BSID-III and LF or SF),
# with counterbalancing of LF and BSID-III.
#
# The used data are:
# - Phase1 LF, SF and BSID III (three studies)
# - Phase0 BSID III (five studies)
#
# Objective: The script should find the best BSID III and export the tau
# estimates of those items.
#
# The script takes the following actions:
#  A. select LF, SF and BSID items from phase1 studies
#  B. fuzzy join the LF, SF and BSID administration to one record
#  C. append five phase0 studies with BSID-III data
#  D. select items with at least 25 observations in both categories
#  E. fix the tau difficulties of all SF/LF items to the 293_0 model
#  F. estimate tau of BSID III items by a single group design
#  G. test for proper D-score vs logit alignment
#  H. create diagnostic plots
#  I. keep best fitting BSID-items by outfit and infit < 1.0
#  J. refit model using items selected in I by repeating steps F to H
#  K. create and save key extension.txt
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private), gseddata (private)
# Inline R scripts: assemble_data.R
#                   edit_data.R
#
# Aug 8, 2024 SvB

# Should we produce time-consuming PDF plots?
plot_pdf <- TRUE

library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(testthat)

# If needed, install dmetric and gseddata from GitHub
if (!requireNamespace("dscore", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dscore needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dscore")
}
if (packageVersion("dscore") < "1.9.5") stop("Needs dscore 1.9.5")
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.68.1") stop("Needs dmetric 0.68.1 or higher")

if (!requireNamespace("gseddata", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package gseddata needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/gseddata")
}
if (packageVersion("gseddata") < "1.9.0") stop("Needs gseddata 1.9.0 or higher")
if (packageVersion("gsedread") < "0.8.0") stop("Needs gseddata 0.8.0 or higher")

library("dscore")
library("dmetric")
library("gseddata")

# run auxiliary scripts to read and process data from source
if (packageVersion("gsedscripts") < "0.13.0") stop("Needs gsedscripts 0.13.0")
suppressWarnings(source(system.file("scripts/assemble_data.R", package = "gsedscripts")))
source(system.file("scripts/edit_data.R", package = "gsedscripts"))

#
#  A. select LF, SF and BSID items from phase1 studies
#

# select instrument data and pre-process, select fixed administration
adm <- c("ctrycd", "cohort", "cohortn", "subjid", "joinid", "agedays", "ins")
items <- c(get_itemnames(ins = "gpa", order = "indm"),
           get_itemnames(ins = "gto"),
           get_itemnames(ins = "by3"))
long <- work |>
  filter(adm == "fixed") |>
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohort = recode(cohort, "11-GSED" = "GSED-BGD", "17-GSED" = "GSED-PAK", "20-GSED" = "GSED-TZA"),
    ctrycd = recode(cohort, "GSED-BGD" = "BGD", "GSED-PAK" = "PAK", "GSED-TZA" = "TZA"),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    joinid = subjid * 100,
    across(any_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) |>
  drop_na(agedays) |>
  select(all_of(adm), any_of(items))

sf <- long |>
  filter(ins == "sf") |>
  select(all_of(adm), any_of(get_itemnames(ins = "gpa")))
lf <- long |>
  filter(ins == "lf") |>
  select(all_of(adm), any_of(get_itemnames(ins = "gto")))
bsid <- long |>
  filter(ins == "bsid") |>
  select(all_of(adm), any_of(get_itemnames(ins = "by3")))

#
#  B. fuzzy join the LF, SF and BSID administration to one record
#

join_method <- "onematch_10"

sf_first <- sf |>
  group_by(subjid) |>
  slice(1L)
lf_first <- lf |>
  group_by(subjid) |>
  slice(1L)
bsid_first <- bsid |>
  group_by(subjid) |>
  slice(1L)

phase1 <- fuzzyjoin::difference_left_join(sf_first, lf_first,
                                          by = c("joinid", "agedays"),
                                          max_dist = 10, distance_col = "dist") |>
  ungroup() |>
  rename(ctrycd = ctrycd.x, cohort = cohort.x, cohortn = cohortn.x,
         subjid = subjid.x, agedays = agedays.x, joinid = joinid.x) |>
  mutate(age = agedays / 365.25) |>
  select(all_of(c("ctrycd", "cohort", "cohortn", "subjid", "joinid", "agedays", "ins.x", "ins.y")),
         any_of(items))
# phase1: 4374 records, 301 columns

phase1 <- fuzzyjoin::difference_left_join(phase1, bsid_first,
                                          by = c("joinid", "agedays"),
                                          max_dist = 10, distance_col = "dist") |>
  ungroup() |>
  rename(ctrycd = ctrycd.x, cohort = cohort.x, cohortn = cohortn.x,
         subjid = subjid.x, agedays = agedays.x, joinid = joinid.x) |>
  mutate(age = agedays / 365.25) |>
  select(c("ctrycd", "cohort", "cohortn", "subjid", "agedays"), any_of(items))

# phase1: 4374 records (=children), 624 columns (5 + 138 + 155 + 326)
cat("dim(phase1):", dim(phase1), "\n")

#
#  C. append five phase0 studies with BSID-III data
#

# get phase0 data
adm <- c("ctrycd", "cohort", "cohortn", "subjid", "agedays")
phase0_lean <- get_data(
  data = gseddata::gsed_lean, adm =  adm,
  items = get_itemnames(ins = "by3"))
phase0_lean <- clean_data(phase0_lean)
phase0 <- as.data.frame(phase0_lean)

# dim(phase0): 5057 320
cat("dim(phase0):", dim(phase0), "\n")
data <- bind_rows(phase1, phase0)

# dim(data): 9431 624
cat("dim(data):", dim(data), "\n")

#
#  D. select items with at least 25 observations in both categories
#

items_subset <- dmetric::select_on_minimum_count(
  select(data, any_of(items)), min_ncat = 2L, min_cat = 25)
data <- data |>
  select(all_of(adm), all_of(items_subset))
# dim(data): 9431 606
cat("dim(data):", dim(data), "\n")

#
#  E. fix the tau difficulties of all SF/LF items to the 293_0 model
#

fix_key <- "293_0"
fix_items <- intersect(colnames(data), get_itemnames(ins = c("gpa", "gto")))
d_fixed <- get_tau(items = fix_items, key = fix_key)
transform <- builtin_keys |>
  filter(key == fix_key) |>
  select(all_of(c("intercept", "slope"))) |>
  unlist()
b_fixed <- (d_fixed - transform["intercept"]) / transform["slope"]

#
#  F. estimate tau of BSID III items by a single group design
#

model_name <- paste(length(items_subset), "0", sep = "_")
model <- fit_dmodel(varlist = list(adm = adm, items = items_subset),
                    data = data,
                    b_fixed = b_fixed,
                    name = model_name,
                    transform = transform,
                    data_package = "")

path <- file.path("~/Project/gsed/phase1/202408")
if (!dir.exists(path)) dir.create(path)
path <- file.path(path, model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")

#
#  G. test for proper D-score vs logit alignment
#

lf_items <- items[starts_with("gto", vars = items)]
sf_items <- items[starts_with("gpa", vars = items)]
by3_items <- items[starts_with("by3", vars = items)]
ds <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
             key = model$name, itembank = model$itembank)
dl <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
             key = model$name, itembank = model$itembank, metric = "logit")
test_that("D-score vs logit alignment", {
  expect_equal(ds$d, transform["slope"] * dl$d + transform["intercept"], tolerance = 0.001)
  expect_equal(ds$sem, transform["slope"] * dl$sem, tolerance = 0.001)
})

# plot(x = dl$d, y = ds$d, cex = 0.4, col = "blue", pch = 19,
#      xlab = "D-score (logit)", ylab = "D-score (dscore)",
#      main = "D-score (logit) vs D-score (dscore)")
# abline(coef = transform, col = "orange")
# plot(x = dl$sem, y = ds$sem, cex = 0.4, col = "blue", pch = 19,
#      xlab = "SEM (logit)", ylab = "SEM (dscore)",
#      main = "SEM (logit) vs SEM (dscore)")
# abline(coef = c(0, transform[2]), col = "orange")

#
#  H. create diagnostic plots
#

path <- file.path("~/Project/gsed/phase1/202408", model_name)
model <- readRDS(file.path(path, "model.Rds"))
data <- readRDS(file.path(path, "data.Rds"))

# Plot figures
if (plot_pdf) {
  theme_set(theme_light())
  col.manual <- get_palette("study", package = "gseddata")
  r <- plot_dmodel(data = data,
                   model = model,
                   path = path,
                   col.manual = col.manual,
                   ref_name = "preliminary_standards",
                   maxy = 100,
                   xlim = c(0, 100),
                   xbreaks = seq(0, 100, 10))
}

# Compare D-scores estimated from LF, SF and BSID-III
d_lf <- dscore(items = lf_items, data = data, xname = "agedays", xunit = "days")
d_sf <- dscore(items = sf_items, data = data, xname = "agedays", xunit = "days")
d_by3 <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
                key = model_name, itembank = model$itembank)
d_by3_pub <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days")
d_combined <- data.frame(age = data$agedays / 365.25,
                         LF = d_lf$d, SF = d_sf$d,
                         BSID_III = d_by3$d, BSID_old = d_by3_pub$d)
panel_with_abline <- function(x, y, ...) {
  points(x, y, ...)
  abline(0, 1, col = "orange")
}
pairs(d_combined[, 2:4],
      panel = panel_with_abline,
      main = "D-scores LF, SF, BSID-III",
      cex = 0.3, xlim = c(0, 90), ylim = c(0, 90), gap = 0)

# Compare new tau by3 to old tau (from key gsed2406)
tau_old <- get_tau(items = by3_items, key = "gsed2406")
tau_new <- get_tau(items = by3_items, key = model_name, itembank = model$itembank)
idx <- !is.na(tau_old) & !is.na(tau_new)
tau_old <- tau_old[idx]
tau_new <- tau_new[idx]
plot(x = tau_old, y = tau_new, cex = 0.7, pch = 19,
     xlim = c(0, 90), ylim = c(0, 90),
     main = "Compare tau old and new BSID-III",
     xlab = "tau (gsed2406)", ylab = "tau (new)")
abline(0, 1, col = "orange")

# correlations between instruments
cor(d_combined[, 1:4], use = "pairwise.complete.obs")

#
#  I. keep best fitting BSID-items by outfit and infit < 1.0
#

# statistics
fits <- data.frame(
  item = model$item_fit$item,
  instrument = strtrim(model$item_fit$item, 3),
  outfit = model$item_fit$outfit,
  infit = model$item_fit$infit,
  lt1 = model$item_fit$outfit < 1.0 & model$item_fit$infit < 1.0,
  lt1.1 = model$item_fit$outfit < 1.1 & model$item_fit$infit < 1.1,
  lt1.2 = model$item_fit$outfit < 1.2 & model$item_fit$infit < 1.2)
table(fits$instrument, fits$lt1)
table(fits$instrument, fits$lt1.1)
table(fits$instrument, fits$lt1.2)

#
#  J. refit model using items selected in I by repeating steps F to H
#

# select BSID items with outfit and infit < 1.0
items_subset <- fits |>
  filter(lt1 | instrument %in% c("gpa", "gto")) |>
  pull(item)

#
# J.F
#

model_name <- paste(length(items_subset), "0", sep = "_")
model <- fit_dmodel(varlist = list(adm = adm, items = items_subset),
                    data = data,
                    b_fixed = b_fixed,
                    name = model_name,
                    transform = transform,
                    data_package = "")

path <- file.path("~/Project/gsed/phase1/202408")
if (!dir.exists(path)) dir.create(path)
path <- file.path(path, model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")

#
# J.G
#

ds <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
             key = model$name, itembank = model$itembank)
dl <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
             key = model$name, itembank = model$itembank, metric = "logit")
test_that("D-score vs logit alignment", {
  expect_equal(ds$d, transform["slope"] * dl$d + transform["intercept"], tolerance = 0.001)
  expect_equal(ds$sem, transform["slope"] * dl$sem, tolerance = 0.001)
})


#
# J.H
#

path <- file.path("~/Project/gsed/phase1/202408", model_name)
model <- readRDS(file.path(path, "model.Rds"))
data <- readRDS(file.path(path, "data.Rds"))

# Plot figures
if (plot_pdf) {
  theme_set(theme_light())
  col.manual <- get_palette("study", package = "gseddata")
  r <- plot_dmodel(data = data,
                   model = model,
                   path = path,
                   col.manual = col.manual,
                   ref_name = "preliminary_standards",
                   maxy = 100,
                   xlim = c(0, 100),
                   xbreaks = seq(0, 100, 10))
}

# Compare D-scores estimated from LF, SF and BSID-III
d_lf <- dscore(items = lf_items, data = data, xname = "agedays", xunit = "days")
d_sf <- dscore(items = sf_items, data = data, xname = "agedays", xunit = "days")
d_by3 <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
                key = model_name, itembank = model$itembank)
d_by3_pub <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days")
d_combined <- data.frame(age = data$agedays / 365.25,
                         LF = d_lf$d, SF = d_sf$d,
                         BSID_III = d_by3$d, BSID_old = d_by3_pub$d)
panel_with_abline <- function(x, y, ...) {
  points(x, y, ...)
  abline(0, 1, col = "orange")
}

# correlations between instruments
pairs(d_combined[, 2:4],
      panel = panel_with_abline,
      main = "D-scores LF, SF, BSID-III",
      cex = 0.3, xlim = c(0, 90), ylim = c(0, 90), gap = 0)
cor(d_combined[, 1:4], use = "pairwise.complete.obs")

#
#  J.I
#

# statistics
fits <- data.frame(
  item = model$item_fit$item,
  instrument = strtrim(model$item_fit$item, 3),
  outfit = model$item_fit$outfit,
  infit = model$item_fit$infit,
  lt1 = model$item_fit$outfit < 1.0 & model$item_fit$infit < 1.0,
  lt1.1 = model$item_fit$outfit < 1.1 & model$item_fit$infit < 1.1,
  lt1.2 = model$item_fit$outfit < 1.2 & model$item_fit$infit < 1.2)
table(fits$instrument, fits$lt1)
table(fits$instrument, fits$lt1.1)
table(fits$instrument, fits$lt1.2)


#
#  K. create and save key extension.txt
#

# export key for inclusion into dscore package
ib <- model$itembank |>
  mutate(key = "extends 293_0",
         tau = round(tau, 2)) |>
  filter(strtrim(item, 3) == "by3") |>
  select(key, item, tau)
write.table(ib,
            file = file.path(path, "extension.txt"),
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)
