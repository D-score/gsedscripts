# This script plots the proportion pass by dscore for all items of the
# 1) GSED SF and LF, phase 1 & 2.
#
# The premier technical difficulty compared to plot_pass_age.R is the
# calculation of the D-score. This script makes the following
# assumptions:
# 1) D-scores are calculated for paired LF/SF observations. The pairing
#    used is identical to that used in the core model estimation.
# 2) The key and population correspond to the defaults used in dscore 0.10.0
#    These are key = "gsed2406" and population = "preliminary_standards".
#
# Dependencies:
# + Environmental variable "DUCKPATH_GSED" must be set to the directory
#   containing database "fixed.duckdb" containing fixed administration data
#
# Created   20250630 SvB
# Modified  20250704 SvB

if (nchar(Sys.getenv("LOCAL_DUCKDB")) == 0L) {
  stop("Environmental variable LOCAL_DUCKDB not set.", call. = FALSE)
}

# Install required packages if not already installed
if (!requireNamespace("dfine", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dfine needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dfine")
}

# Load required packages
library("DBI", quietly = TRUE, warn.conflicts = FALSE)
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("tidyr", quietly = TRUE, warn.conflicts = FALSE)
library("ggplot2", quietly = TRUE, warn.conflicts = FALSE)
library("dfine")
library("dscore")

if (packageVersion("dfine") < "0.4.0") stop("Needs dfine 0.4.0")
if (packageVersion("dscore") < "1.10.0") stop("Needs dscore 1.10.0")

#
#  A.  Read fixed form Phase 1&2 data responses and visits
#

dbfile <- file.path(Sys.getenv("LOCAL_DUCKDB"), "fixed.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = dbfile)
dbListTables(con)
visits <- dbReadTable(con, "visits")
responses <- dbReadTable(con, "responses")
dbDisconnect(con)

#
#  B.  Subset to phase 1&2 LF and SF data
#      Ignore any inter-rater scores (vist_type 5) to prevent duplicate matches
#

visits <- visits |>
  filter(ins %in% c("lf", "sf") & vist_type != 5L)
responses <- semi_join(
  responses,
  visits,
  by = c("subjid", "agedays", "vist_type"))

#
# C. Add country and cohort to responses from visits
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

# remove duplicates to prevent double pairing
# remove records with missing agedays
#
# TODO: this is a temporary fix
# - in each of these we need to find and remove one record
# - restore agedays where possible
responses <- responses |>
  filter(!(subjid %in% c("076-GSED-0528", "076-GSED-0905", "076-GSED-0905",
                         "076-GSED-1322",
                         "384-GSED-1160", "384-GSED-1323",
                         "528-GSED-0581"))) |>
  filter(!is.na(agedays))

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
nrow(responses_sf) + nrow(responses_lf) - nrow(responses) == 0

# Recreate with the new pair number
responses <- bind_rows(responses_sf, responses_lf)

# table(responses$pair, useNA = "al")
#
#   -10     -2     -1      1      2      3   <NA>
#   5907   2018   7583 689313 162964   8615      0
# < -5: no match for LF
# -2, -1: no match for SF
# 1, 2, 3: matched SF and LF

responses <- responses |>
  distinct(cohort, country, subjid, agedays, pair, ins, item, response)
#
#  D. Create wide format that puts items from a SF-LF pair on the same row
#

data <- responses |>
  select(subjid, agedays, pair, ins, item, response) |>
  pivot_wider(names_from = c(item), values_from = response,
              id_cols = c(subjid, pair)) |>
  arrange(subjid, pair)

# duplicates
# anomalies <- responses |>
#   dplyr::summarise(n = dplyr::n(), .by = c(subjid, pair, item)) |>
#   dplyr::filter(n > 1L)

agedays_info <- responses |>
  distinct(subjid, pair, ins, agedays) |>
  pivot_wider(names_from = ins, values_from = agedays,
              names_prefix = "agedays_")
data <- data |>
  left_join(agedays_info, by = c("subjid", "pair")) |>
  select(subjid, pair, starts_with("agedays_"), any_of(items_sf), any_of(items_lf))

cat("dim(data):", dim(data), "\n")
# wide: 11461 visits (measurements), 297 columns (2 + 2 + 138 + 155)
# wide: 9280 unique subjid (=children)

#
#  E. Select items with at least 10 observation in both categories
#

min_n <- 10
id_cols <- c("subjid", "pair", "agedays_sf", "agedays_lf")
items <- setdiff(colnames(data), id_cols)
counts <- sapply(data[items], function(x) {
  c(count_0 = sum(x == 0, na.rm = TRUE),
    count_1 = sum(x == 1, na.rm = TRUE))
})
counts_df <- as.data.frame(t(counts))
counts_df$item <- rownames(counts_df)
valid_items <- counts_df |>
  filter(count_0 >= min_n, count_1 >= min_n) |>
  pull(item)
data <- data |>
  select(all_of(id_cols), all_of(valid_items))

#
#  F. Calculate combined agedays for SF and LF
#

data <- data |>
  mutate(agedays = rowMeans(across(c(agedays_sf, agedays_lf)), na.rm = TRUE))

#
#  G. Calculate D-score for all SF + LF items combined
#     using the gsed2406 key and preliminary_standards population
#

ds <- dscore(data = data, items = items,
             xname = "agedays", xunit = "days",
             key = "gsed2406", population = "preliminary_standards",
             metric = "dscore")
data <- bind_cols(data, ds)

#
#  H. Extend responses matrix with the D-score
#

responses <- responses |>
  left_join(data |> select(subjid, pair, d),
            by = c("subjid", "pair")) |>
  mutate(d = ifelse(is.na(d), NA_real_, d))

#
# I. Define data for rug plot
#

data_rug <- responses |>
  select(item, response, d, cohort)

# Permute rows in data_rug plot better plotting
idx <- sample(1:nrow(data_rug))
data_rug <- data_rug[idx, ]

# Calculate summary statistics
pass <- data_rug |>
  mutate(dcut = cut(d, breaks = seq(0, 90, 3))) |>
  group_by(item, cohort, dcut) |>
  summarise(
    p = round(100 * mean(response, na.rm = TRUE)),
    d = mean(d, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  left_join(dscore::get_itemtable(), by = "item")

#
# E.  Plotting
#

theme_set(theme_light())

# SF
plots_sf <- dfine::plot_p_d_item(
  pass = pass,
  data_rug = data_rug,
  items = items_sf,
  x_var = "d",
  model_name = "gsed2406",
  xlim = c(0, 100),
  xbreaks = seq(10, 90, 10),
  label.trunc = 80,
  col.manual = get_palette("cohort"))

# LF
plots_lf <- plot_p_d_item(
  pass = pass,
  data_rug = data_rug,
  items = items_lf,
  x_var = "d",
  model_name = "Key: gsed2406",
  xlim = c(0, 100),
  xbreaks = seq(10, 90, 10),
  label.trunc = 80,
  col.manual = get_palette("cohort"))

# BSID
# plots_bsid <- plot_p_d_item(
#   pass = pass, data_rug = data_rug,
#   items = items_bsid,
#   x_var = "d",
#   model_name = "Key: gsed2406",
#   xlim = c(0, 100),
#   xbreaks = seq(10, 90, 10),
#   label.trunc = 80,
#   col.manual = get_palette("cohort"),
#   min_n = 5)

#
# F.  Save plots as PDF
#

path <- file.path("~/project/gsed/phase2/descriptives")
device <- "pdf"

# SF
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "gpa_items_by_dscore.pdf")
  pdf(file, onefile = TRUE, width = 10, height = 5)
  lapply(plots_sf, print)
  message("Saved to: ", file)
  dev.off()
}

# LF
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "gto_items_by_dscore.pdf")
  pdf(file, onefile = TRUE, width = 10, height = 5)
  lapply(plots_lf, print)
  message("Saved to: ", file)
  dev.off()
}

# # BSID
# if (!is.null(file) & device == "pdf") {
#   file <- file.path(path, "by3_items_by_dscore.pdf")
#   pdf(file, onefile = TRUE, width = 10, height = 5)
#   lapply(plots_bsid, print)
#   message("Saved to: ", file)
#   dev.off()
# }



# Plot D-score against age in months

reduced <- responses |>
  distinct(cohort, ins, subjid, agedays, d) |>
  mutate(agemos = agedays / 30.44) |>
  slice_sample(prop = 1)

dba <- ggplot(reduced, aes(y = d, x = agemos, color = cohort)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.75) +
  labs(y = "D-score", x = "Age (months)") +
  scale_color_manual(values = col_manual) +
  scale_y_continuous(
    breaks = seq(0, 90, 10),
  ) +
  scale_x_continuous(
    breaks = seq(0, max(reduced$agemos, na.rm = TRUE), by = 6),
  ) +
  theme_minimal()

file <- file.path(path, "dscore_by_age.pdf")
pdf(file, onefile = TRUE, width = 10, height = 10)
dba
message("Saved to: ", file)
dev.off()
