# This script fits the core model 293_0 for the phase-1 & 2 data
#
# Dependencies:
# - Environmental variable "GSED_PHASE1" must be set to the local directory
#   containing the models for phase 1 (will be used only for reading)
# - Environmental variable "GSED_PHASE2" must be set to the local directory
#   containing the models for phase 2 (will be used for writing)
#
# This script can be called from another script by setting the following
# global variables:
# - phases: vector of phases to include (default: c(1, 2))
# - cohorts: vector of cohorts to include (default: c("GSED-BGD", "GSED-BRA",
#   "GSED-CHN", "GSED-CIV", "GSED-NLD", "GSED-PAK", "GSED-TZA"))
# - instruments: vector of instruments to include (default: c("sf", "lf"))
# - remove_visits: vector of visits to remove (default: NULL)
# - remove_items: vector of items to remove (default: "")
#
# The script will create a new model in the GSED_PHASE2 directory with the
# name "202510/xxx_yy_<cohort>/<phase1+phase2>". The model will contain
# the D-score and logit values for the items in the specified cohorts and
# phases. It will also create a plotly widget with the D-score and logit
# values for the items in the specified cohorts and phases.
#
# TODO
# - Step D contains temporary fixes. This needs to be improved.
# - Step modifies the data to prevent duplicates. This needs a better solution.
# - Repair/remove rogue points in D-score against age scatter plot
#
# Created   20250708 SvB
# Modified  20251006 SvB

if (nchar(Sys.getenv("GSED_PHASE1")) == 0L) {
  stop("Environmental variable MODELS_PHASE1 not set.", call. = FALSE)
}
if (nchar(Sys.getenv("GSED_PHASE2")) == 0L) {
  stop("Environmental variable MODELS_PHASE2 not set.", call. = FALSE)
}

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
  instruments <- c("sf", "lf")
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
#  B.  Subset to Phase 1 & 2 LF and SF data.
#      Ignore any inter-rater scores (vist_type 5, phase 1) to prevent
#      duplicate matches.
#

visits <- visits |>
  filter(
    phase %in%
      phases &
      cohort %in% cohorts &
      ins %in% instruments &
      vist_type != 5L
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
items_sf <- get_itemnames(ins = "gs1", order = "indm")
items_lf <- get_itemnames(ins = "gl1")
items_lf <- items_lf[c(55:155, 1:54)]
responses <- responses |>
  filter(item %in% c(items_sf, items_lf))
responses_sf <- responses |>
  filter(item %in% items_sf) |>
  left_join(pairs, by = join_by(subjid, agedays == sf_agedays)) |>
  mutate(ins = "sf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)
responses_lf <- responses |>
  filter(item %in% items_lf) |>
  left_join(
    pairs |> filter(pair > 0),
    by = join_by(subjid, agedays == lf_agedays)
  ) |>
  mutate(pair = ifelse(is.na(pair), -agedays, pair), ins = "lf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)

# Check for zero duplicate matches
# NOTE: The next line should yield TRUE!!
nrow(responses_sf) + nrow(responses_lf) - nrow(responses) == 0

# Recreate with the new pair number
responses <- bind_rows(responses_sf, responses_lf)

# > tail(table(responses$pair, useNA = "al"), 10)
#
#   -25    -19    -17     -6     -2     -1      1      2      3   <NA>
#    18     17     27     21   2018   7583 691786 163347   8613      0
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
items <- c(items_sf, items_lf)
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
}

### --- APPLY EDITS

# responses <- dplyr::anti_join(responses, remove_item_country,
#                             by = c("item", "country"))

#
#  F. Estimate tau of SF and LF items by a single group design
#

fit <- rasch(data = responses, visit_var = c("subjid", "pair"), items = items)

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
model <- calculate_dmodel(
  data = responses,
  fit = fit,
  name = model_name_add,
  # anchors = c(gtogmd001 = 20, gtogmd026 = 40))
  # anchors = c(gtogmd001 = 17.94, gtogmd026 = 41.08))
  # transform = c(55.86, 4.1))
  transform = "auto"
)

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
# Plot tau constrast as plotly widget
#

tau_tau <- dfine::plot_tau_contrast(model, model_old, detrended = FALSE)
dif_tau <- dfine::plot_tau_contrast(model, model_old)
htmlwidgets::saveWidget(
  tau_tau,
  file = file.path(path_new, "tau_tau.html"),
  selfcontained = TRUE
)
htmlwidgets::saveWidget(
  dif_tau,
  file = file.path(path_new, "dif_tau.html"),
  selfcontained = TRUE
)

# Person (=visit) fit histograms

oldpar <- par(mfrow = c(2, 2))
h1 <- hist(
  model$person_fit$infit,
  freq = TRUE,
  xlim = c(0, 3),
  breaks = c(seq(0, 5, 0.1), Inf),
  main = "Person infit",
  xlab = "",
  ylim = c(0, 2200)
)
h2 <- hist(
  model$person_fit$outfit,
  freq = TRUE,
  xlim = c(0, 3),
  breaks = c(seq(0, 5, 0.1), Inf),
  main = "Person outfit",
  xlab = "",
  ylim = c(0, 2200)
)

# Item fit histograms
h3 <- hist(
  model$item_fit$infit,
  freq = TRUE,
  xlim = c(0, 3),
  breaks = c(seq(0, 5, 0.1), Inf),
  main = "Item infit",
  xlab = "",
  ylim = c(0, 80)
)

h4 <- hist(
  model$item_fit$outfit,
  freq = TRUE,
  xlim = c(0, 3),
  breaks = c(seq(0, 5, 0.1), Inf),
  main = "Item outfit",
  xlab = "",
  ylim = c(0, 80)
)
par(oldpar)


# Count items and persons under the chosen cut-offs

table(model$item_fit$outfit < 1.2, model$item_fit$infit < 1.2)
table(model$person_fit$outfit < 2, model$person_fit$infit < 2)

#
# Classify DIF phase 1 vs phase 2 using Jodoin/Gierl criteria
# Classify DIF country using Jodoin/Gierl criteria
#

# DIF <- dfine::calculate_DIF_classification(responses, model)
# write.table(
#   DIF,
#   file = file.path(path_new, "DIF.csv"),
#   quote = FALSE,
#   row.names = FALSE,
#   sep = "\t",
#   dec = "."
# )

#
#  H. create diagnostic plots
#

# Calculate D-score for SF and LF separately, removing visits with high infit/outfit values
# Use dscore 2.0.0 or higher

# LF
dscore_lf <- responses_lf |>
  anti_join(to_remove, by = c("subjid", "pair")) |>
  distinct(
    cohort,
    country,
    subjid,
    agedays,
    pair,
    ins,
    item,
    response,
    .keep_all = TRUE
  ) |>
  pivot_wider(
    id_cols = c(cohort, country, subjid, agedays, pair, ins),
    names_from = item,
    values_from = response,
    values_fill = NA
  ) |>
  dscore::dscore(
    key = "gsed2510",
    xname = "agedays",
    xunit = "days"
  )

# LF
dscore_sf <- responses_sf |>
  anti_join(to_remove, by = c("subjid", "pair")) |>
  distinct(
    cohort,
    country,
    subjid,
    agedays,
    pair,
    ins,
    item,
    response,
    .keep_all = TRUE
  ) |>
  pivot_wider(
    id_cols = c(cohort, country, subjid, agedays, pair, ins),
    names_from = item,
    values_from = response,
    values_fill = NA
  ) |>
  dscore::dscore(
    key = "gsed2510",
    xname = "agedays",
    xunit = "days"
  )

# Remove outliers DAZ < -4 or DAZ > 4
dscore_lf <- dscore_lf |>
  filter(daz > -4 & daz < 4)
dscore_sf <- dscore_sf |>
  filter(daz > -4 & daz < 4)

# plot D-score against age for LF and SF
library(ggplot2)
ggplot2::theme_set(theme_light())
col_manual <- dfine::get_palette("instrument")

dbind <- bind_rows(
  bind_cols(ins = "GSED LF", dscore_lf),
  bind_cols(ins = "GSED SF", dscore_sf)
) |>
  mutate(agedays = round(365.25 * a), months = a * 12, ta = log(a + 100))

twoplots <- ggplot(dbind, aes(x = months, y = d)) +
  geom_point(size = 0.1, shape = 19) +
  geom_smooth(se = FALSE, color = "grey50") +
  scale_x_continuous(
    limits = c(0, 42),
    breaks = seq(0, 42, by = 6)
  ) +
  scale_y_continuous(
    limits = c(0, 90),
    breaks = seq(0, 80, by = 20)
  ) +
  labs(
    x = "Age (months)",
    y = "D-score"
  ) +
  facet_grid(. ~ ins) +
  theme(legend.position = "none")

# save as pdf
ggsave(
  filename = file.path(
    Sys.getenv("GSED_PHASE2"),
    "202510",
    "Dscore_age_LF_SF.pdf"
  ),
  plot = twoplots,
  device = "pdf",
  unit = "cm",
  width = 30,
  height = 15,
)

# Fit reference curves on LF and SF D-scores from dbind
library("gamlss")
library("gamlss.ggplots")
# fit the model
# NO: selected df <- c(8, 4)
# BCT: selected df <- c(8, 4, 4, 0)

## some grid search code outcommented
grid <- expand.grid(df1 = 8, df2 = 4, df3 = 4, df4 = 0)
grid$family <- "BCT"
fits <- vector("list", nrow(grid))
worms <- vector("list", nrow(grid))
grid$deviance <- NA
for (i in 1:nrow(grid)) {
  df <- grid[i, 1:4]
  # df <- c(8, 2, 0)
  family <- get(grid$family[i])
  n.cyc <- 20
  fits[[i]] <- gamlss(
    d ~ cs(ta, df = df[1]),
    sigma.formula = ~ cs(ta, df = df[2]),
    nu.formula = ~ cs(ta, df = df[3]),
    tau.formula = ~ cs(ta, df = df[4]),
    data = dbind,
    family = family,
    control = gamlss.control(n.cyc = n.cyc, trace = FALSE)
  )
  grid[i, "deviance"] <- round(deviance(fits[[i]]))
  cat("\n", as.character(grid[i, ]), "\n")

  worms[[i]] <- resid_wp_wrap(
    fits[[i]],
    xvar = dbind$a,
    n_inter = 16,
    ylim = 0.4,
    title = paste(
      "family =",
      grid$family[i],
      ",",
      "   df =",
      paste(df, collapse = ", "),
      "   dev =",
      grid[i, "deviance"]
    )
  )
}


worms
# lapply(fits, deviance)
# lapply(fits, plot)

i <- 1
centiles(
  fits[[i]],
  xvar = dbind$agedays,
  cent = round(100 * pnorm(c(-2.5, -2:2, 2.5)), 1),
  legend = FALSE,
  main = paste(
    "Model: ",
    paste(grid[i, ], collapse = ", ")
  )
)

# grid for reference table (all weeks in data range)
fit <- fits[[i]]
rx <- range(dbind$agedays, na.rm = TRUE)
rxweeks <- (floor(rx[1] / 7)):(ceiling(rx[2] / 7))
grid_x <- (0:186) * 7
raster <- data.frame(
  agedays = grid_x,
  a = grid_x,
  ta = log(100 + grid_x / 365.25)
)

# calculate and round reference table
p <- predictAll(fit, newdata = raster, type = "response")
reference <- data.frame(raster, p) |>
  transmute(
    day = agedays,
    week = round(day / 7, 2),
    month = round(day / (365.25 / 12), 2),
    year = round(day / 365.25, 4),
    mu = round(mu, 2),
    sigma = round(sigma, 4),
    nu = round(nu, 4),
    tau = round(tau, 3)
  ) |>
  select(day, week, month, year, mu, sigma, nu, tau)
# append age = 5 reference (extrapolated, linearly)
add <- tail(reference, 1)
add$day <- 1826
add$week <- 261
add$month <- 60
add$year <- 5
add$mu <- round(63.12173 + 3.844458 * 5, 2)
reference <- bind_rows(reference, add)

# write reference table
write.table(
  reference,
  file = file.path(path_new, "who_descriptive_gsed2510.txt"),
  quote = FALSE,
  row.names = FALSE,
  sep = "\t",
  dec = "."
)

# Calculate the parts for the prior mean function `dscore:::count_mu_phase2()`
#
# Descriptive references
# ref1 <- reference[reference$age < 0.75, ]
# ref2 <- reference[reference$age >= 0.75 & reference$age < 3.5, ]
# ref3 <- reference[reference$age > 3, ]
#
# Count model: < 9MND:
# summary(mod <- lm(formula = mu ~ age + log(age + 10), data = ref1))
# -2775.83728 - 75.02296 age + 1210.41737 log(age + 10)
#
# Count model: > 9MND & < 3.5 YR
# summary(mod <- lm(formula = mu ~ age + I(log(age + 0.25)), data = ref2))
# 46.69057068 - 6.42038876 age + 39.77773960 log(age + 0.25)
#
# Linear model: > 3.5 YRS: 63.12172890 + 3.84445765 age

# # per cohort
# col_manual <- dfine::get_palette("cohort")
# plotdata <- model$dscore |>
#   mutate(cohort = dfine::calculate_cohort(subjid))
# cohortplot_y <- dfine::plot_d_a_cohort(
#   data = plotdata,
#   show_smooth = FALSE,
#   model_name = model$name,
#   file = NULL,
#   ref_name = "preliminary_standards",
#   xlim = c(0, 42),
#   ylim = c(0, 90),
#   col_manual = col_manual,
#   size = 0.5,
#   shape = 19
# )
# cohortplot_z <- dfine::plot_d_a_cohort(
#   data = plotdata,
#   daz = TRUE,
#   show_smooth = TRUE,
#   model_name = model$name,
#   file = NULL,
#   ref_name = "preliminary_standards",
#   xlim = c(0, 42),
#   ylim = c(-3, 3),
#   ybreaks = c(-3, -2, -1, 0, 1, 2, 3),
#   col_manual = col_manual,
#   size = 1,
#   shape = 19,
#   smooth_line_color = "grey35"
# )

# # Calculate means per cohort
# dscore <- model$dscore |>
#   mutate(cohort = dfine::calculate_cohort(subjid)) |>
#   summarize(
#     d = mean(d, na.rm = TRUE),
#     daz = mean(daz, na.rm = TRUE),
#     sem = mean(sem, na.rm = TRUE),
#     n = n(),
#     .by = c("cohort")
#   )

# Diagnostic plots not yet working with dmetric/dfine
# plot_pdf <- TRUE
# if (plot_pdf) {
#   r <- dmetric::plot_dmodel(
#     data = responses,
#     model = model,
#     path = path,
#     col.manual = col.manual,
#     ref_name = "preliminary_standards",
#     maxy = 100,
#     xlim = c(0, 100),
#     xbreaks = seq(0, 100, 10)
#   )
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
