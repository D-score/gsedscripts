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
#  A. read phase1 data into object 'work'
#  B. fuzzy join the LF, SF and BSID phase1 fixed mode data
#  C. append five phase0 studies with BSID-III data
#  D. select items with at least 10 observations in both categories
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
#                   join_LF_SF_BSID.R
#
# Created Aug 13, 2024 SvB
# Updated Oct 17, 2025 SvB

if (nchar(Sys.getenv("ONEDRIVE_GSED")) == 0L) {
  stop("Environmental variable ONEDRIVE_GSED not set.", call. = FALSE)
}
if (nchar(Sys.getenv("GSED_PHASE1")) == 0L) {
  stop("Environmental variable MODELS_PHASE1 not set.", call. = FALSE)
}
if (nchar(Sys.getenv("GSED_PHASE2")) == 0L) {
  stop("Environmental variable MODELS_PHASE2 not set.", call. = FALSE)
}
print(Sys.getenv("ONEDRIVE_GSED"))
print(Sys.getenv("GSED_PHASE1"))
print(Sys.getenv("GSED_PHASE2"))

# ---- PURIFICATION SETTINGS BASED ON GSED CORE MODEL

path_start <- file.path(Sys.getenv("GSED_PHASE2"), "202510", "293_0_phase_1+2")
model_start <- readRDS(file.path(path_start, "model.Rds"))

# Remove selected items with infit > 1.2 after review
remove_for_infit <- c(
  "gs1sec063",
  "gl1gmd047",
  "gl1lgd008",
  "gl1fmd007",
  "gl1fmd032",
  "gl1fmd024",
  "gl1fmd039",
  "gl1fmd035"
)
remove_for_dif <- c("gs1lgc004", "gl1lgd001", "gl1lgd005", "gl1lgd014")
remove_items <- unique(c(remove_for_infit, remove_for_dif))
dscore::get_labels(remove_items)

# Remove visits with persons infit/outout value > 2.0
pfit <- model_start$person_fit |>
  dplyr::arrange(subjid, pair)
remove_visits <- pfit$infit > 2.0 | pfit$outfit > 2.0

## ---- END PURIFICATION BASED ON CORE MODEL

# --- GLOBAL SCRIPT VARIABLES
# Set defaults if not specified in the caller script

if (!exists("phases")) {
  phases <- c(1, 2)
}

if (!exists("cohorts")) {
  cohorts <- c(
    "GSED-BGD",
    "GSED-BRA",
    "GSED-CHN",
    "GSED-CIV",
    "GSED-NLD",
    "GSED-PAK",
    "GSED-TZA"
  )
}
if (!exists("instruments")) {
  instruments <- c("sf", "lf", "bsid")
}
if (!exists("remove_visits")) {
  remove_visits <- NULL
}
if (!exists("remove_items")) {
  remove_items <- ""
}

if (!exists("remove_item_country")) {
  remove_item_country <- data.frame(
    item = NULL,
    country = NULL,
    stringsAsFactors = FALSE
  )
}


# --- END GLOBAL SCRIPT VARIABLES

# Load GitHub packages
pkg <- "dfine"
if (!requireNamespace(pkg, quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package", pkg, "needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dfine")
}
require("dfine", quietly = TRUE, warn.conflicts = FALSE)
if (packageVersion("dfine") < "0.13.0") {
  stop("Needs dfine 0.13.0")
}

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
#  B.  Subset to phase 1 & 2 fixed form data
#      Ignore any inter-rater scores (vist_type 5, phase 1) to prevent
#      duplicate matches.
#

visits <- visits |>
  filter(
    phase %in%
      phases &
      cohort %in% cohorts &
      ins %in% instruments &
      (is.na(vist_type) | vist_type != 5L)
  )

responses <- semi_join(
  responses,
  visits,
  by = c("subjid", "agedays", "vist_type")
)


#
# C. Add country and cohort fields to responses
#

responses <- responses |>
  left_join(
    visits |>
      distinct(cohort, subjid, agedays, vist_type) |>
      mutate(country = substr(cohort, 6, 8)),
    by = c("subjid", "agedays", "vist_type")
  ) |>
  select(cohort, country, subjid, agedays, vist_type, item, response)


#
#  D1. Identify pairs of LF-SF records occurring within four days
#

# NOTE: BEGIN TEMPORARY FIXES
# - remove duplicates to prevent double pairing
# - remove records with missing agedays
#
# TODO July 8:
# - in each of these we need to find and remove one record
# - restore agedays where possible

responses <- responses |>
  filter(
    !(subjid %in%
      c(
        "076-GSED-0528",
        "076-GSED-0905",
        "076-GSED-0905",
        "076-GSED-1322",
        "384-GSED-1160",
        "384-GSED-1323",
        "528-GSED-0581"
      ))
  ) |>
  filter(!is.na(agedays))

# NOTE: END TEMPORARY FIXES

visits <- visits |>
  select(cohort, subjid, agedays, ins, vist_type) |>
  arrange(subjid, agedays, ins, vist_type) |>
  group_by(subjid) |>
  mutate(
    sf_order = if_else(ins == "sf", row_number(), NA_integer_),
    sf_order = if_else(ins == "sf", cumsum(ins == "sf"), NA_integer_),
    lf_order = if_else(ins == "lf", row_number(), NA_integer_),
    lf_order = if_else(ins == "lf", cumsum(ins == "lf"), NA_integer_),
    bsid_order = if_else(ins == "bsid", row_number(), NA_integer_),
    bsid_order = if_else(ins == "bsid", cumsum(ins == "bsid"), NA_integer_)
  ) |>
  ungroup()

# Find pair number (0, 1, 2, 3) for each SF record
sf_rows <- visits |>
  filter(ins == "sf") |>
  select(subjid, agedays, sf_order)
lf_rows <- visits |>
  filter(ins == "lf") |>
  select(subjid, lf_agedays = agedays, lf_order)
bsid_rows <- visits |>
  filter(ins == "bsid") |>
  select(subjid, bsid_agedays = agedays, bsid_order)

match_lf <- sf_rows |>
  left_join(lf_rows, by = "subjid", relationship = "many-to-many") |>
  mutate(diff = abs(agedays - lf_agedays)) |>
  group_by(subjid, sf_order) |>
  slice_min(order_by = diff, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(pair = ifelse(diff > 4L | is.na(diff), -sf_order, sf_order))
match_bsid <- sf_rows |>
  left_join(bsid_rows, by = "subjid", relationship = "many-to-many") |>
  mutate(diff = abs(agedays - bsid_agedays)) |>
  group_by(subjid, sf_order) |>
  slice_min(order_by = diff, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(pair = ifelse(diff > 4L | is.na(diff), -sf_order, sf_order))

# process SF, LF, and BSID items
items_sf <- get_itemnames(ins = "gs1", order = "indm")
items_lf <- get_itemnames(ins = "gl1")
items_lf <- items_lf[c(55:155, 1:54)]
items_bsid <- get_itemnames(ins = "by3")
items <- c(items_sf, items_lf, items_bsid)
responses <- responses |>
  filter(item %in% items)

# Merge pair number with with response
responses_sf <- responses |>
  filter(item %in% items_sf) |>
  left_join(match_lf, by = join_by(subjid, agedays)) |>
  mutate(ins = "sf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)
responses_lf <- responses |>
  filter(item %in% items_lf) |>
  left_join(
    match_lf |> filter(pair > 0),
    by = join_by(subjid, agedays == lf_agedays)
  ) |>
  mutate(pair = ifelse(is.na(pair), -agedays, pair), ins = "lf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)
responses_bsid <- responses |>
  filter(item %in% items_bsid) |>
  left_join(
    match_bsid |> filter(pair > 0),
    by = join_by(subjid, agedays == bsid_agedays)
  ) |>
  mutate(pair = ifelse(is.na(pair), -agedays, pair), ins = "bsid") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)


# Check for zero duplicate matches
# NOTE: The next line should yield TRUE!!
nrow(responses_sf) +
  nrow(responses_lf) +
  nrow(responses_bsid) -
  nrow(responses) ==
  0

# Recreate with the new pair number
responses <- bind_rows(responses_sf, responses_lf, responses_bsid)
tail(table(responses$pair, useNA = "al"), 10)

# > tail(table(responses$pair, useNA = "al"), 10)
#
# sf+lf+bsid
#   -20    -19    -17     -6     -2     -1      1      2      3   <NA>
#   326     17     27     21   2018   7583 779795 163999   8613      0
#
# Explanation of pair numbers:
#
# < -5: no match for LF or BSID
# -2, -1: no match for SF
# 1, 2, 3: matched SF and LF

# Finally, remove a few duplicates
responses <- responses |>
  filter(pair >= -5) |>
  distinct(cohort, country, subjid, agedays, pair, ins, item, response)

#
#  E. Select items with at least 10 observation in both categories
#

min_n <- 10
items <- c(items_sf, items_lf, items_bsid)
valid_items <- responses |>
  filter(response %in% c(0, 1)) |>
  count(item, response) |>
  pivot_wider(
    names_from = response,
    values_from = n,
    names_prefix = "n_",
    values_fill = 0
  ) |>
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
if (sum(remove_visits) > 0L) {
  responses_before <- nrow(responses)
  paired_visits <- dplyr::distinct(responses, subjid, pair) |>
    arrange(subjid, pair)
  to_remove <- paired_visits[remove_visits, ]
  responses <- dplyr::anti_join(responses, to_remove, by = c("subjid", "pair"))
  responses_after <- nrow(responses)
  cat(
    "Removed",
    sum(remove_visits),
    "visits and",
    responses_before - responses_after,
    "responses.\n"
  )
} else {
  to_remove <- data.frame(subjid = character(0), pair = integer(0))
}

#
#  F1. fix the tau difficulties of all SF/LF items to the gsed2510 key
#

fix_key <- "gsed2510"
fix_items <- intersect(
  unique(responses$item),
  get_itemnames(ins = c("gs1", "gl1"))
)
d_fixed <- get_tau(items = fix_items, key = fix_key)
d_fixed <- d_fixed[!is.na(d_fixed)] # 281 items

# apply transform to logit for key gsed2510
transform <- builtin_keys |>
  filter(key == fix_key) |>
  select(all_of(c("intercept", "slope"))) |>
  unlist()
b_fixed <- (d_fixed - transform["intercept"]) / transform["slope"]

#
#  F2. estimate taus of BSID III items by a single group design
#

fit <- rasch(
  data = responses,
  visit_var = c("subjid", "pair"),
  items = items,
  b_fixed = b_fixed
)

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
  model_name_add <- "ext_by3"
}

# Calculate the dmodel object

model <- dfine::calculate_dmodel(
  data = responses,
  fit = fit,
  name = model_name_add,
  transform = transform,
  population = "who_descriptive"
)

#
#  H. Compare D-score for the BSID vs LF and SF
#

wide <- responses |>
  select(subjid, agedays, pair, ins, item, response) |>
  pivot_wider(
    names_from = c(item),
    values_from = response,
    id_cols = c(subjid, pair)
  ) |>
  arrange(subjid, pair)
agedays_info <- responses |>
  distinct(subjid, pair, ins, agedays) |>
  pivot_wider(
    names_from = ins,
    values_from = agedays,
    names_prefix = "agedays_"
  ) |>
  mutate(agedays = agedays_sf)
wide <- wide |>
  left_join(agedays_info, by = c("subjid", "pair")) |>
  select(
    subjid,
    pair,
    starts_with("agedays"),
    any_of(items_sf),
    any_of(items_lf),
    any_of(items_bsid)
  )


# Calculate D-score for LF, SF and BSID
d_sf <- wide |>
  dscore::dscore(
    items = items_sf,
    key = "gsed2510",
    population = "who_descriptive",
    xname = "agedays",
    xunit = "days"
  )
d_lf <- wide |>
  dscore::dscore(
    items = items_lf,
    key = "gsed2510",
    population = "who_descriptive",
    xname = "agedays",
    xunit = "days"
  )
d_bsid <- wide |>
  dscore::dscore(
    items = items_bsid,
    itembank = model$itembank,
    key = model$name,
    population = "who_descriptive",
    xname = "agedays",
    xunit = "days"
  )
d_combined <- data.frame(
  age = wide$agedays / 365.25,
  LF = d_lf$d,
  SF = d_sf$d,
  BSID_III = d_bsid$d,
  LF_DAZ = d_lf$daz,
  SF_DAZ = d_sf$daz,
  BSID_III_DAZ = d_bsid$daz
) |>
  mutate(
    LF = ifelse(LF_DAZ < -4, NA, LF),
    SF = ifelse(SF_DAZ < -4, NA, SF),
    BSID_III = ifelse(BSID_III_DAZ < -4, NA, BSID_III),
    LF_DAZ = ifelse(LF_DAZ < -4, NA, LF_DAZ),
    SF_DAZ = ifelse(SF_DAZ < -4, NA, SF_DAZ),
    BSID_III_DAZ = ifelse(BSID_III_DAZ < -4, NA, BSID_III_DAZ)
  )
cor(d_combined[, 2:7], use = "pairwise.complete.obs")

panel_with_abline <- function(x, y, ...) {
  points(x, y, ...)
  abline(0, 1, col = "orange")
}

# Panel that adds points, y = x line, and a 90% ellipse
panel_abline_ellipse <- function(
  x,
  y,
  level = 0.90,
  n = 200,
  col.ell = "red",
  lwd.ell = 1,
  ...
) {
  # 1) points
  points(x, y, ...)

  # 2) reference line
  abline(0, 1, col = "grey80")

  # 3) ellipse (only if we have enough finite data and non-singular covariance)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) > 2) {
    xy <- cbind(x[ok], y[ok])
    S <- stats::cov(xy)
    if (all(is.finite(S)) && det(S) > .Machine$double.eps) {
      mu <- colMeans(xy)
      eig <- eigen(S, symmetric = TRUE)
      r <- sqrt(stats::qchisq(level, df = 2)) # 90% ellipse radius in Mahalanobis units
      tth <- seq(0, 2 * pi, length.out = n)
      circ <- rbind(cos(tth), sin(tth)) # 2 x n unit circle
      # Transform unit circle by sqrt of covariance (via eigen) and shift by mean
      E <- t(eig$vectors %*% (diag(sqrt(pmax(eig$values, 0))) %*% circ)) * r
      E[, 1] <- E[, 1] + mu[1]
      E[, 2] <- E[, 2] + mu[2]
      lines(E, col = col.ell, lwd = lwd.ell)
    }
  }
}

pairs(
  d_combined[, c(2:4)],
  panel = function(x, y, ...) {
    panel_abline_ellipse(
      x,
      y,
      level = 0.90, # change to e.g. 0.95 if you want
      col.ell = "firebrick",
      lwd.ell = 1,
      ...
    )
  },
  cex = 0.3,
  xlim = c(0, 90),
  ylim = c(0, 90),
  gap = 0
)


# Use it in pairs()
pairs(
  d_combined[, 5:7],
  panel = function(x, y, ...) {
    panel_abline_ellipse(
      x,
      y,
      level = 0.90, # change to e.g. 0.95 if you want
      col.ell = "firebrick",
      lwd.ell = 1,
      ...
    )
  },
  cex = 0.3,
  xlim = c(-3, 3),
  ylim = c(-3, 3),
  gap = 0
)

#
#  I. Analyze item fit statistics
#

# # Calculate item selection vectors under various cut-offs
# cut_offs_infit <- c(seq(1.0, 2.0, by = 0.1), Inf)
# cut_offs_outfit <- c(seq(1.0, 2.0, by = 0.1), Inf)
# screen_me <- model$item_fit$item %in% get_itemnames(instrument = "by3")
# n_screened <- sum(screen_me)
# select_me <- array(
#   NA,
#   dim = c(6, length(cut_offs_infit), length(cut_offs_outfit))
# )
# dimnames(select_me) <- list(
#   c("a", "n", "p", "d", "sem", "daz"),
#   paste0("infit_", cut_offs_infit),
#   paste0("outfit_", cut_offs_outfit)
# )
# for (cut_infit in cut_offs_infit) {
#   for (cut_outfit in cut_offs_outfit) {
#     in_model <- model$item_fit |>
#       filter(screen_me) |>
#       mutate(selected = infit <= cut_infit & outfit <= cut_outfit) |>
#       pull(selected)
#     d_bsid <- wide |>
#       dscore::dscore(
#         items = model$item_fit$item[in_model],
#         itembank = model$itembank,
#         key = model$name,
#         population = "who_descriptive",
#         xname = "agedays",
#         xunit = "days"
#       )
#     select_me[,
#       paste0("infit_", cut_infit),
#       paste0("outfit_", cut_outfit)
#     ] <- colMeans(d_bsid, na.rm = TRUE)
#   }
# }
# Takes a long time to run!!
# Plot the contourplot using contourplot_by3.R script

# plot D-score against age for LF and SF
library(ggplot2)
ggplot2::theme_set(theme_light())
col_manual <- dfine::get_palette("instrument")


dbind <- bind_rows(
  bind_cols(ins = "GSED LF", d_lf),
  bind_cols(ins = "GSED SF", d_sf),
  bind_cols(ins = "BSID-III", d_bsid)
) |>
  mutate(agedays = round(365.25 * a), months = a * 12, ta = log(a + 100))


# Store and (re)load models
path_old <- file.path(Sys.getenv("GSED_PHASE1"), "202408/293_0")
model_old <- readRDS(file.path(path_old, "model.Rds"))
data_old <- readRDS(file.path(path_old, "data.Rds"))

path_new <- file.path(Sys.getenv("GSED_PHASE2"), "202510", model$name)
if (!dir.exists(path_new)) {
  dir.create(path_new)
}
saveRDS(model, file = file.path(path_new, "model.Rds"), compress = "xz")

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
  lt1.2 = model$item_fit$outfit < 1.2 & model$item_fit$infit < 1.2
)
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
model <- dfine::fit_dmodel(
  varlist = list(adm = adm, items = items_subset),
  data = data,
  b_fixed = b_fixed,
  name = model_name,
  transform = transform,
  data_package = ""
)

path <- file.path("~/Project/gsed/phase1/202408")
if (!dir.exists(path)) {
  dir.create(path)
}
path <- file.path(path, model_name)
if (!dir.exists(path)) {
  dir.create(path)
}
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")

#
# J.G
#

ds <- dscore(
  items = by3_items,
  data = data,
  xname = "agedays",
  xunit = "days",
  key = model$name,
  itembank = model$itembank
)
dl <- dscore(
  items = by3_items,
  data = data,
  xname = "agedays",
  xunit = "days",
  key = model$name,
  itembank = model$itembank,
  metric = "logit"
)
test_that("D-score vs logit alignment", {
  expect_equal(
    ds$d,
    transform["slope"] * dl$d + transform["intercept"],
    tolerance = 0.001
  )
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
  ggplot2::theme_set(theme_light())
  col.manual <- get_palette("study", package = "gseddata")
  r <- dmetric::plot_dmodel(
    data = data,
    model = model,
    path = path,
    col.manual = col.manual,
    ref_name = "preliminary_standards",
    maxy = 100,
    xlim = c(0, 100),
    xbreaks = seq(0, 100, 10)
  )
}

# Compare D-scores estimated from LF, SF and BSID-III
d_lf <- dscore(items = lf_items, data = data, xname = "agedays", xunit = "days")
d_sf <- dscore(items = sf_items, data = data, xname = "agedays", xunit = "days")
d_by3 <- dscore(
  items = by3_items,
  data = data,
  xname = "agedays",
  xunit = "days",
  key = model_name,
  itembank = model$itembank,
  population = "preliminary_standards",
  verbose = TRUE
)
d_by3_pub <- dscore(
  items = by3_items,
  data = data,
  xname = "agedays",
  xunit = "days"
)
d_combined <- data.frame(
  age = data$agedays / 365.25,
  LF = d_lf$d,
  SF = d_sf$d,
  BSID_III = d_by3$d,
  BSID_old = d_by3_pub$d
)
panel_with_abline <- function(x, y, ...) {
  points(x, y, ...)
  abline(0, 1, col = "orange")
}

# correlations between instruments D
pairs(
  d_combined[, 2:4],
  panel = panel_with_abline,
  main = "D-scores LF, SF, BSID-III",
  cex = 0.3,
  xlim = c(0, 90),
  ylim = c(0, 90),
  gap = 0
)
cor(d_combined[, 1:4], use = "pairwise.complete.obs")

# correlations between instruments DAZ
daz_combined <- data.frame(
  age = data$agedays / 365.25,
  LF = d_lf$daz,
  SF = d_sf$daz,
  BSID_III = d_by3$daz
)
pairs(
  daz_combined[, 1:4],
  panel = panel_with_abline,
  main = "DAZ LF, SF, BSID-III",
  cex = 0.3,
  xlim = c(-3, 3),
  ylim = c(-3, 3),
  gap = 0
)
cor(daz_combined[, 1:4], use = "pairwise.complete.obs")


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
  lt1.2 = model$item_fit$outfit < 1.2 & model$item_fit$infit < 1.2
)
table(fits$instrument, fits$lt1)
table(fits$instrument, fits$lt1.1)
table(fits$instrument, fits$lt1.2)


#
#  K. create and save key extension.txt
#

# export key for inclusion into dscore package
ib <- model$itembank |>
  mutate(key = "extends 293_0", tau = round(tau, 2)) |>
  filter(strtrim(item, 3) == "by3") |>
  select(key, item, tau)
write.table(
  ib,
  file = file.path(path, "extension.txt"),
  quote = FALSE,
  sep = "\t",
  row.names = FALSE
)
