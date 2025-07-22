# This script refits the core model 293_0 for the phase-1&2 data
#
# Dependencies:
# + Environmental variable "GSED_PHASE1" must be set to the local directory
#   containing the models for phase 1 (will be used only for reading)
# + Environmental variable "GSED_PHASE2" must be set to the local directory
#   containing the models for phase 2 (will be used for writing)
#
# TODO
# - Data CHN not yet cleaned (wait for Gareth's OK)
# - Step D contains temporary fixes. This needs to be improved.
# - Step modifies the data to prevent duplicates. This needs a better solution.
# - Repair/remove rogue points in D-score against age scatter plot
#
# Created   20250708 SvB
# Modified  20250713 SvB

if (nchar(Sys.getenv("GSED_PHASE1")) == 0L) {
  stop("Environmental variable GSED_PHASE1 not set.", call. = FALSE)
}
if (nchar(Sys.getenv("GSED_PHASE2")) == 0L) {
  stop("Environmental variable GSED_PHASE2 not set.", call. = FALSE)
}

# --- GLOBAL SCRIPT VARIABLES
# Set defaults if not specified in the caller script

if (!exists("phases")) {
  phases <- c(1, 2)
}
if (!exists("cohorts")) {
  cohorts <- c("GSED-BGD", "GSED-BRA", "GSED-CHN",
               "GSED-CIV", "GSED-NLD", "GSED-PAK", "GSED-TZA")
}
if (!exists("instruments")) {
  instruments <- c("sf", "lf")
}
if (!exists("remove_visits")) {
  remove_visits <- NULL
}
if (!exists("remove_items")) {
  remove_items <- ""
}
# --- END GLOBAL SCRIPT VARIABLES

# Load GitHub packages
pkg <- "dfine"
if (!requireNamespace(pkg, quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package", pkg, "needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dfine")
}
require("dfine", quietly = TRUE, warn.conflicts = FALSE)
if (packageVersion("dfine") < "0.9.0") stop("Needs dfine 0.9.0")

# Load CRAN packages
library("DBI", quietly = TRUE, warn.conflicts = FALSE)
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("dscore")
library("tidyr", quietly = TRUE, warn.conflicts = FALSE)
library("testthat", quietly = TRUE, warn.conflicts = FALSE)
library("htmlwidgets", quietly = TRUE, warn.conflicts = FALSE)

#
#  A.  Read fixed form Phase 1 & 2 responses and visits
#

dbfile <- file.path(Sys.getenv("GSED_PHASE2"), "data/fixed.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = dbfile)
dbListTables(con)
visits <- dbReadTable(con, "visits")
responses <- dbReadTable(con, "responses")
dbDisconnect(con)

#
#  B.  Subset to Phase 1 & 2 LF and SF data.
#      Ignore any inter-rater scores (vist_type 5, phase 1) to prevent
#      duplicate matches.
#

visits <- visits |>
  filter(phase %in% phases &
           cohort %in% cohorts &
           ins %in% instruments &
           vist_type != 5L)
responses <- semi_join(
  responses,
  visits,
  by = c("subjid", "agedays", "vist_type"))

#
# C. Add country and cohort fields to responses
#

responses <- responses |>
  left_join(
    visits |>
      distinct(cohort, subjid, agedays, vist_type) |>
      mutate(country = substr(cohort, 6, 8)),
    by = c("subjid", "agedays", "vist_type")) |>
  select(cohort, country, subjid, agedays, vist_type, item, response)

#
#  D. Identify pairs of LF-SF records occurring within four days
#

# NOTE: BEGIN TEMPORARY FIXES
# - remove duplicates to prevent double pairing
# - remove records with missing agedays
#
# TODO July 8:
# - in each of these we need to find and remove one record
# - restore agedays where possible

responses <- responses |>
  filter(!(subjid %in% c("076-GSED-0528", "076-GSED-0905", "076-GSED-0905",
                         "076-GSED-1322",
                         "384-GSED-1160", "384-GSED-1323",
                         "528-GSED-0581"))) |>
  filter(!is.na(agedays))

# NOTE: END TEMPORARY FIXES

visits <- visits |>
  select(subjid, agedays, ins, vist_type) |>
  arrange(subjid, agedays, ins, vist_type) |>
  group_by(subjid) |>
  mutate(sf_order = if_else(ins == "sf", row_number(), NA_integer_)) |>
  mutate(sf_order = if_else(ins == "sf", cumsum(ins == "sf"), NA_integer_)) |>
  mutate(lf_order = if_else(ins == "lf", row_number(), NA_integer_)) |>
  mutate(lf_order = if_else(ins == "lf", cumsum(ins == "lf"), NA_integer_)) |>
  ungroup()

# Find pair number (0, 1, 2, 3) for each SF record
sf_rows <- visits |>
  filter(ins == "sf") |>
  select(subjid, sf_agedays = agedays, sf_order)
lf_rows <- visits |>
  filter(ins == "lf") |>
  select(subjid, lf_agedays = agedays, lf_order)
pairs <- sf_rows |>
  left_join(lf_rows, by = "subjid", relationship = "many-to-many") |>
  mutate(diff = abs(sf_agedays - lf_agedays)) |>
  group_by(subjid, sf_order) |>
  slice_min(order_by = diff, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(pair = ifelse(diff > 4L | is.na(diff), -sf_order, sf_order))

# Merge pair number with with response
items_sf <- get_itemnames(ins = "gpa", order = "indm")
items_lf <- get_itemnames(ins = "gto")
responses <- responses |>
  filter(item %in% c(items_sf, items_lf))
responses_sf <- responses |>
  filter(item %in% items_sf) |>
  left_join(pairs, by = join_by(subjid, agedays == sf_agedays)) |>
  mutate(ins = "sf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)
responses_lf <- responses |>
  filter(item %in% items_lf) |>
  left_join(pairs |> filter(pair > 0),
            by = join_by(subjid, agedays == lf_agedays)) |>
  mutate(pair = ifelse(is.na(pair), -agedays, pair),
         ins = "lf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)

# Check for zero duplicate matches
# NOTE: The next line should yield TRUE!!
nrow(responses_sf) + nrow(responses_lf) - nrow(responses) == 0

# Recreate with the new pair number
responses <- bind_rows(responses_sf, responses_lf)

# > tail(table(responses$pair, useNA = "al"), 10)
#
#  -25    -19    -17     -6     -2     -1      1      2      3   <NA>
#   18     17     27     21   2018   7583 689313 162964   8615      0
#
# Explanation of pair numbers:
#
# < -5: no match for LF
# -2, -1: no match for SF
# 1, 2, 3: matched SF and LF

# Finally, remove a few duplicates
responses <- responses |>
  distinct(cohort, country, subjid, agedays, pair, ins, item, response)

#
#  E. Select items with at least 10 observation in both categories
#

min_n <- 10
items <- c(get_itemnames(ins = "gpa", order = "indm"),
           get_itemnames(ins = "gto"))
valid_items <- responses |>
  filter(response %in% c(0, 1)) |>
  count(item, response) |>
  pivot_wider(names_from = response, values_from = n,
              names_prefix = "n_", values_fill = 0) |>
  filter(n_0 >= min_n, n_1 >= min_n) |>
  pull(item)
items <- intersect(items, valid_items)
responses <- responses |>
  filter(item %in% items)

### ---- REMOVE ITEMS (uses remove_items)

items <- setdiff(items, remove_items)
responses <- responses |>
  filter(item %in% items)

### --- REMOVE VISITS (uses remove_visits)

# visits <- visits[remove_visits, ]
# visits_to_drop <- visits |>
#   select(subjid, pair, vist_type)
# responses2 <- responses %>%
#   anti_join(visits_to_drop, by = c("subjid", "pair", "vist_type"))

#
#  F. Estimate tau of SF and LF items by a single group design
#

fit <- rasch(data = responses,
             visit_var = c("subjid", "pair"),
             items = items)

#
#  G. Calculate the dmodel object
#

# Define agedays as mean per LF and SF pair
responses <- responses |>
  group_by(subjid, pair) |>
  mutate(agedays = mean(agedays, na.rm = TRUE)) |>
  ungroup()

model_name_add <- ""
if (length(cohorts) == 1) {
  model_name_add <- paste(model_name_add, "cohort", cohorts, sep = "_")
} else {
  model_name_add <- paste("phase", paste0(phases, collapse = "+"), sep = "_")
}

# 20: Lift head 45 degrees
# 40: Moves from lying to sitting
#
# NOTE: transform = "auto"  runs a linear regresion model to find the
# intercept and slope that predict the gsed2406 tau values
model <- calculate_dmodel(data = responses,
                          fit = fit,
                          name = model_name_add,
                          population = "preliminary_standards",
                          # anchors = c(gtogmd001 = 20, gtogmd026 = 40))
                          # anchors = c(gtogmd001 = 17.94, gtogmd026 = 41.08))
                          # transform = c(55.86, 4.1))
                          transform = "auto")

item_fit <- model$item_fit

# Store and (re)load models
path_old <- file.path(Sys.getenv("GSED_PHASE1"), "202408/293_0")
model_old <- readRDS(file.path(path_old, "model.Rds"))
data_old <- readRDS(file.path(path_old, "data.Rds"))

path_new <- file.path(Sys.getenv("GSED_PHASE2"), "202507", model$name)
if (!dir.exists(path_new)) dir.create(path_new)
saveRDS(model, file = file.path(path_new, "model.Rds"), compress = "xz")

#
# Plot tau constrast as plotly widget
#

tau_tau <- dfine::plot_tau_contrast(model, model_old, detrended = FALSE)
dif_tau <- dfine::plot_tau_contrast(model, model_old)
htmlwidgets::saveWidget(tau_tau,
                        file = file.path(path_new, "tau_tau.html"),
                        selfcontained = TRUE)
htmlwidgets::saveWidget(dif_tau,
                        file = file.path(path_new, "dif_tau.html"),
                        selfcontained = TRUE)

# Person (=visit) fit histograms

oldpar <- par(mfrow = c(2, 2))
hist(model$person_fit$outfit, xlim = c(0, 5), breaks = c(seq(0, 5, 0.1), Inf),
     main = "Person outfit", xlab = "", ylim = c(0, 2.6))
hist(model$person_fit$infit, xlim = c(0, 5), breaks = c(seq(0, 5, 0.1), Inf),
     main = "Person infit", xlab = "", ylim = c(0, 2.6))

# Item fit histograms

hist(model$item_fit$outfit, xlim = c(0, 5), breaks = c(seq(0, 5, 0.1), Inf),
     main = "Item outfit", xlab = "", ylim = c(0, 2.6))
hist(model$item_fit$infit, xlim = c(0, 5), breaks = c(seq(0, 5, 0.1), Inf),
     main = "Item infit", xlab = "", ylim = c(0, 2.6))
par(oldpar)


# Potential cut-offs for removing items and persons

table(model$item_fit$outfit < 1.2, model$item_fit$infit < 1.2)
table(model$item_fit$outfit < 1.4, model$item_fit$infit < 1.4)

table(model$person_fit$outfit < 3, model$person_fit$infit < 3)
table(model$person_fit$outfit < 4, model$person_fit$infit < 4)

# TODO
# Create DIF plots for phase 1 vs phase 2
# Create DIF plots by cohort


# TODO: Diagnostic plots not yet working with dmetric/dfine
#
#  H. create diagnostic plots
#
#
# if (plot_pdf) {
#   theme_set(theme_light())
#   col.manual <- col_manual <- dfine::get_palette("cohort")
#   r <- dmetric::plot_dmodel(data = data,
#                             model = model,
#                             path = path,
#                             col.manual = col.manual,
#                             ref_name = "preliminary_standards",
#                             maxy = 100,
#                             xlim = c(0, 100),
#                             xbreaks = seq(0, 100, 10))
# }


# We do not need to check D-score/logit each time, so outcomment for now
# #
# #  Z. Test for proper D-score vs logit alignment
# #
#
# wide <- responses |>
#   select(subjid, agedays, pair, ins, item, response) |>
#   pivot_wider(names_from = c(item), values_from = response,
#               id_cols = c(subjid, pair)) |>
#   arrange(subjid, pair)
# agedays_info <- responses |>
#   distinct(subjid, pair, ins, agedays) |>
#   pivot_wider(names_from = ins, values_from = agedays,
#               names_prefix = "agedays_") |>
#   mutate(agedays = agedays_sf)
# wide <- wide |>
#   left_join(agedays_info, by = c("subjid", "pair")) |>
#   select(subjid, pair, starts_with("agedays"), any_of(items_sf), any_of(items_lf))
#
# transform <- model$transform
# names(transform) <- c("intercept", "slope")
#
# sf_items <- items[starts_with("gpa", vars = items)]
# ds <- dscore(items = sf_items, data = wide, xname = "agedays", xunit = "days",
#              transform = transform, key = model$name, itembank = model$itembank)
# dl <- dscore(items = sf_items, data = wide, xname = "agedays", xunit = "days",
#              transform = transform, key = model$name,itembank = model$itembank,
#              metric = "logit")
# test_that("D-score vs logit alignment (SF)", {
#   expect_equal(ds$d, transform["slope"] * dl$d + transform["intercept"], tolerance = 0.001)
#   expect_equal(ds$sem, transform["slope"] * dl$sem, tolerance = 0.001)
# })
#
# lf_items <- items[starts_with("gto", vars = items)]
# ds <- dscore(items = lf_items, data = wide, xname = "agedays", xunit = "days",
#              transform = transform, key = model$name, itembank = model$itembank)
# dl <- dscore(items = lf_items, data = wide, xname = "agedays", xunit = "days",
#              transform = transform, key = model$name, itembank = model$itembank, metric = "logit")
# test_that("D-score vs logit alignment (LF)", {
#   expect_equal(ds$d, transform["slope"] * dl$d + transform["intercept"], tolerance = 0.001)
#   expect_equal(ds$sem, transform["slope"] * dl$sem, tolerance = 0.001)
# })
#
# plot(x = dl$d, y = ds$d, cex = 0.4, col = "blue", pch = 19,
#      xlab = "D-score (logit)", ylab = "D-score (dscore)",
#      main = "D-score (logit) vs D-score (dscore)")
# abline(coef = transform, col = "orange")
# plot(x = dl$sem, y = ds$sem, cex = 0.4, col = "blue", pch = 19,
#      xlab = "SEM (logit)", ylab = "SEM (dscore)",
#      main = "SEM (logit) vs SEM (dscore)")
# abline(coef = c(0, transform[2]), col = "orange")
#
# # Check anchors
# # ib <- model$itembank
# # test_that("Anchors items are 20 and 40", {
# #   expect_equal(ib[ib$item == "gtogmd001", "tau"], 20, tolerance = 0.001)
# #   expect_equal(ib[ib$item == "gtogmd026", "tau"], 40, tolerance = 0.001)
# # })
#
