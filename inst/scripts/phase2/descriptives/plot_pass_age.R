# This script plots the proportion pass by age for all items of the
# 1) GSED SF and LF, phase 1 & 2.
# 2) BSID, phase 1.
#
# Dependencies:
# + Environmental variable "GSED_PHASE2" must be set to the directory
#   containing database "data/fixed.duckdb" containing fixed administration data
#
# Created   20250630 SvB
# Modified  20250721 SvB

if (nchar(Sys.getenv("GSED_PHASE2")) == 0L) {
  stop("Environmental variable GSED_PHASE2 not set.", call. = FALSE)
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

if (packageVersion("dfine") < "0.10.0") stop("Needs dfine 0.10.0")
if (packageVersion("dscore") < "1.10.4") stop("Needs dscore 1.10.4")

#
#  A.  Read fixed form Phase 1&2 data responses and visits
#

dbfile <- file.path(Sys.getenv("GSED_PHASE2"), "data/fixed.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = dbfile)
dbListTables(con)
visits <- dbReadTable(con, "visits")
responses <- dbReadTable(con, "responses")
dbDisconnect(con)

#
# B. Add country, cohort and ins to responses
#

responses <- responses |>
  left_join(
    visits |>
      distinct(cohort, subjid, agedays, vist_type) |>
      mutate(country = substr(cohort, 6, 8)),
    by = c("subjid", "agedays", "vist_type")) |>
  select(cohort, country, subjid, agedays, vist_type, item, response)

#
#  C1. Select SF/LF items with at least 10 observations in both categories
#

min_n <- 10
items_sf <-get_itemnames(ins = "sf_", order = "indm")
items_lf <- get_itemnames(instrument = c("lfa", "lfb", "lfc"))
valid_items <- responses |>
  filter(response %in% c(0, 1)) |>
  count(item, response) |>
  pivot_wider(names_from = response, values_from = n,
              names_prefix = "n_", values_fill = 0) |>
  filter(n_0 >= min_n, n_1 >= min_n) |>
  pull(item)
items1 <- intersect(c(items_sf, items_lf), valid_items)
responses1 <- responses |>
  filter(item %in% items1)

# For SF & LF Phase 1&2
# table(responses1$vist_type, responses1$country)
#
#       BGD    BRA    CHN    CIV    NLD    PAK    TZA
# 1   56967 116873  68482  50161  16770  63931  60760
# 2   47162     83   7452  46277      0  53342  49240
# 4       0      0      0      0  18600      0      0
# 5    8407   6147   6331   6961    583   8335   8618
# 6    4331   4188   3140   3833   4027   5140   4896
# 7    6915    101   5779   5363   1188  11001   7341
# 8   43695  19074      0      0   1969  39592  41707
# 12    101      0      0      0      0   7696    333

#
#  C2. Select BSID items with at least 3 observations in both categories
#

min_n <- 3
items_bsid <- get_itemnames(ins = "by3")
valid_items <- responses |>
  filter(response %in% c(0, 1)) |>
  count(item, response) |>
  pivot_wider(names_from = response, values_from = n,
              names_prefix = "n_", values_fill = 0) |>
  filter(n_0 >= min_n, n_1 >= min_n) |>
  pull(item)
items2 <- intersect(items_bsid, valid_items)
responses2 <- responses |>
  filter(item %in% items2)

# For BSID Phase 1&2
# table(responses2$vist_type, responses2$country, useNA = "al")
#         BGD   BRA   CIV   NLD   PAK   TZA  <NA>
#  <NA> 12492 42597 11424  1861 10838 13475     0

#
# C3. Combine responses
#

responses <- bind_rows(responses1, responses2)

#
# D. Calculate passing probabilities
#

# Define data for rug plot
data_rug <- responses |>
  mutate(a = agedays / 365.25 * 12) |>
  select(item, response, a, cohort)

# Permute rows in data_rug plot better plotting
idx <- sample(1:nrow(data_rug))
data_rug <- data_rug[idx, ]

# Calculate summary statistics
pass <- data_rug |>
  mutate(agegp = cut(a, breaks = seq(0, 42, 2))) |>
  group_by(item, cohort, agegp) |>
  summarise(
    p = round(100 * mean(response, na.rm = TRUE)),
    a = mean(a, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  left_join(dscore::get_itemtable(), by = "item")

#
# E.  Plotting
#

col_manual <- dfine::get_palette("cohort")
theme_set(theme_light())

# SF
plots_sf <- plot_pass(
  pass = pass,
  data_rug = data_rug,
  items = items_sf,
  x_var = "a",
  xlim = c(0, 48),
  xbreaks = seq(0, 48, 6),
  label_trunc = 80,
  col_manual = col_manual)

# LF
plots_lf <- plot_pass(
  pass = pass,
  data_rug = data_rug,
  items = items_lf,
  xlim = c(0, 48),
  xbreaks = seq(0, 48, 6),
  label_trunc = 80,
  col_manual = col_manual)

# BSID
plots_bsid <- plot_pass(
  pass = pass,
  data_rug = data_rug,
  items = items_bsid,
  xlim = c(0, 48),
  xbreaks = seq(0, 48, 6),
  label_trunc = 80,
  col_manual = col_manual,
  min_n = 3)

#
# F.  Save plots as PDF
#

path <- file.path(Sys.getenv("GSED_PHASE2"), "descriptives")
device <- "pdf"

# SF
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "sf_items_by_age.pdf")
  pdf(file, onefile = TRUE, width = 10, height = 5)
  lapply(plots_sf, print)
  message("Saved to: ", file)
  dev.off()
}

# LF
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "lf_items_by_age.pdf")
  pdf(file, onefile = TRUE, width = 10, height = 5)
  lapply(plots_lf, print)
  message("Saved to: ", file)
  dev.off()
}

# BSID
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "by3_items_by_age.pdf")
  pdf(file, onefile = TRUE, width = 10, height = 5)
  lapply(plots_bsid, print)
  message("Saved to: ", file)
  dev.off()
}

