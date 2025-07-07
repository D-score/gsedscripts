# This script plots the proportion pass by age for all items of the
# 1) GSED SF and LF, phase 1 & 2.
# 2) BSID, phase 1.
#
# Dependencies:
# + Environmental variable "DUCKPATH_GSED" must be set to the directory
#   containing database "fixed.duckdb" containing fixed administration data
#
# Created   20250630 SvB
# Modified  20250707 SvB

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
items_sf <-get_itemnames(ins = "gpa", order = "indm")
items_lf <- get_itemnames(ins = "gto")
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
  mutate(agemos = agedays / 365.25 * 12) |>
  select(item, response, agemos, cohort)

# Permute rows in data_rug plot better plotting
idx <- sample(1:nrow(data_rug))
data_rug <- data_rug[idx, ]

# Calculate summary statistics
pass <- data_rug |>
  mutate(agegp = cut(agemos, breaks = seq(0, 42, 6))) |>
  group_by(item, cohort, agegp) |>
  summarise(
    p = round(100 * mean(response, na.rm = TRUE)),
    a = mean(agemos, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  left_join(dscore::get_itemtable(), by = "item")

#
# E.  Plotting
#

col_manual <- c(
  "GSED-BGD" = "#A6001A",  # deeper red (rich crimson/burgundy)
  "GSED-BRA" = "#002776",  # navy blue (flag globe)
  "GSED-CHN" = "#DE2010",  # bright red (saturated)
  "GSED-CIV" = "#1BAF5F",  # cool green (teal-shifted for distinctiveness)
  "GSED-NLD" = "#F77F33",  # Dutch orange
  "GSED-PAK" = "#166A2F",  # forest green
  "GSED-TZA" = "#47A1D8"   # light blue
)
theme_set(theme_light())

# SF
plots_sf <- plot_p_a_item(
  pass = pass, data_rug = data_rug,
  items = items_sf,
  xlim = c(0, 48),
  xbreaks = seq(0, 48, 6),
  label.trunc = 80,
  col.manual = col_manual)

# LF
plots_lf <- plot_p_a_item(
  pass = pass, data_rug = data_rug,
  items = items_lf,
  xlim = c(0, 48),
  xbreaks = seq(0, 48, 6),
  label.trunc = 80,
  col.manual = col_manual)

# BSID
plots_bsid <- plot_p_a_item(
  pass = pass, data_rug = data_rug,
  items = items_bsid,
  xlim = c(0, 48),
  xbreaks = seq(0, 48, 6),
  label.trunc = 80,
  col.manual = col_manual,
  min_n = 3)

#
# F.  Save plots as PDF
#

path <- file.path("~/project/gsed/phase2/descriptives")
device <- "pdf"

# SF
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "gpa_items_by_age.pdf")
  pdf(file, onefile = TRUE, width = 10, height = 5)
  lapply(plots_sf, print)
  message("Saved to: ", file)
  dev.off()
}

# LF
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "gto_items_by_age.pdf")
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

