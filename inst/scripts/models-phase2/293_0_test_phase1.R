# This script refits the core model 293_0 for the phase-1 data witj
# the following changes:
# + The sample size is slighly lower due to updated duplicate removal
# + The data are read from the DuckDB database
# + Responses from inter-rater scores (vist_type 5) are ignored
#
# Dependencies:
# + Environmental variable "DUCKPATH_GSED" must be set to the directory
#   containing database "phase1.duckdb" containing fixed administration
#   phase1 data
#
# Created 20250618 SvB

plot_pdf <- FALSE

# Load required packages
library("DBI", quietly = TRUE, warn.conflicts = FALSE)
library("dscore")
library("ggplot2")
library("dmetric")
library("gsedread")
library("gsedscripts")
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("tidyr", quietly = TRUE, warn.conflicts = FALSE)
library("testthat", quietly = TRUE, warn.conflicts = FALSE)
library("difR")

# if (packageVersion("gsedread") < "0.10.0") stop("Needs gsedread 0.10.0")

#
#  A.  Read phase1 database into responses and visits objects
#
dbfile <- file.path(Sys.getenv("DUCKPATH_GSED"), "phase1.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = dbfile)
dbListTables(con)
responses <- dbReadTable(con, "responses")
visits <- dbReadTable(con, "visits")
dbDisconnect(con, shutdown = TRUE)

#
#  B. Identify pairs of LF-SF records occurring within four days
#     Ignore any inter-rater scores (vist_type 5) to prevent duplicate matches
#

visits <- visits |>
  filter(vist_type != 5L) |>  # skip inter-rater scores
  select(gsed_id, agedays, ins, vist_type) |>
  arrange(gsed_id, agedays, ins, vist_type) |>
  group_by(gsed_id) |>
  mutate(sf_order = if_else(ins == "sf", row_number(), NA_integer_)) |>
  mutate(sf_order = if_else(ins == "sf", cumsum(ins == "sf"), NA_integer_)) |>
  ungroup()

# Find pair number (0, 1, 2, 3) for each SF record
sf_rows <- visits |>
  filter(ins == "sf") |>
  select(gsed_id, sf_agedays = agedays, sf_order)
lf_rows <- visits |>
  filter(ins == "lf") |>
  select(gsed_id, lf_agedays = agedays)
pairs <- sf_rows |>
  left_join(lf_rows, by = "gsed_id", relationship = "many-to-many") |>
  mutate(diff = abs(sf_agedays - lf_agedays)) |>
  group_by(gsed_id, sf_order) |>
  slice_min(order_by = diff, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(pair = ifelse(diff > 4L | is.na(diff), 0L, sf_order))

# Merge pair number with with response
items_sf <- get_itemnames(ins = "gpa", order = "indm")
items_lf <- get_itemnames(ins = "gto")
responses <- responses |>
  filter(vist_type != 5L & item %in% c(items_sf, items_lf)) # skip inter-rater scores
responses_sf <- responses |>
  filter(item %in% items_sf) |>
  left_join(pairs, by = join_by(gsed_id, agedays == sf_agedays)) |>
  mutate(ins = "sf") |>
  select(gsed_id, agedays, pair, ins, item, response)
responses_lf <- responses |>
  filter(item %in% items_lf) |>
  left_join(pairs |> filter(pair > 0),
            by = join_by(gsed_id, agedays == lf_agedays)) |>
  mutate(pair = ifelse(is.na(pair), -1L, pair),
         ins = "lf") |>
  select(gsed_id, agedays, pair, ins, item, response)

# Check for zero duplicate matches
nrow(responses_sf) + nrow(responses_lf) - nrow(responses) == 0

# Recreate with the new pair number
responses <- bind_rows(responses_sf, responses_lf)

# table(responses$pair, useNA = "al")
#
#     -1      0      1      2      3    <NA>
#   1126   3421 362196 129924   7483      0
#
# -1 : Only LF
#  0 : Only SF
#  1-3: Pair number of SF-LF match

#
#  C. Create wide format that puts items from a SF-LF pair on the same row
#

data <- responses |>
  select(gsed_id, agedays, pair, ins, item, response) |>
  pivot_wider(names_from = c(item), values_from = response,
              id_cols = c(gsed_id, pair)) |>
  arrange(gsed_id, pair)
agedays_info <- responses |>
  distinct(gsed_id, pair, ins, agedays) |>
  pivot_wider(names_from = ins, values_from = agedays,
              names_prefix = "agedays_")
data <- data |>
  left_join(agedays_info, by = c("gsed_id", "pair")) |>
  select(gsed_id, pair, starts_with("agedays_"), any_of(items_sf), any_of(items_lf))

# wide: 5961 visits (measurements), 297 columns (2 + 2 + 138 + 155)
# wide: 4374 unique gsed_id (=children)
cat("dim(data):", dim(data), "\n")

#
#  D. Select items with at least 10 observation in both categories
#

min_n <- 10
id_cols <- c("gsed_id", "pair", "agedays_sf", "agedays_lf")
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
#  E. Specify anchors to define the D-score scale
#

# 20: Lift head 45 degrees
# 40: Moves from lying to sitting
anchor <- c(20, 40)
names(anchor) <- c("gtogmd001", "gtogmd026")

# restrict transform
transform <- builtin_keys |>
  filter(key == "gsed2406") |>
  select(all_of(c("intercept", "slope"))) |>
  unlist()

#
#  F. Estimate tau of SF and LF items by a single group design
#

data <- data |>
  rename(subjid = gsed_id,
         agedays = agedays_sf)
varlist <- list(adm = c("subjid", "agedays"),
                items = valid_items)
model_name <- paste(length(valid_items), "0", sep = "_")
model_name <- paste0(model_name, "_test_phase1")
model <- fit_dmodel(varlist = varlist,
                    data = data,
                    name = model_name,
                    anchors = anchor,
                    #                    transform = transform,
                    data_package = "",
                    verbose = TRUE)

# Store and reload model
path <- path.expand(file.path("~/project/gsed/phase2/test", model_name))
path_old <- "/Users/buurensv/Dropbox/Project/gsed/phase1/202408/293_0"
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")
model_old <- readRDS(file.path(path_old, "model.Rds"))
data_old <- readRDS(file.path(path_old, "data.Rds"))

#
#  G. Test for proper D-score vs logit alignment
#

transform <- model$transform
names(transform) <- c("intercept", "slope")

sf_items <- items[starts_with("gpa", vars = items)]
ds <- dscore(items = sf_items, data = data, xname = "agedays", xunit = "days",
             transform = transform, key = model$name, itembank = model$itembank)
dl <- dscore(items = sf_items, data = data, xname = "agedays", xunit = "days",
             transform = transform, key = model$name,itembank = model$itembank,
             metric = "logit")
test_that("D-score vs logit alignment (SF)", {
  expect_equal(ds$d, transform["slope"] * dl$d + transform["intercept"], tolerance = 0.001)
  expect_equal(ds$sem, transform["slope"] * dl$sem, tolerance = 0.001)
})

lf_items <- items[starts_with("gto", vars = items)]
ds <- dscore(items = lf_items, data = data, xname = "agedays", xunit = "days",
             transform = transform, key = model$name, itembank = model$itembank)
dl <- dscore(items = lf_items, data = data, xname = "agedays", xunit = "days",
             transform = transform, key = model$name, itembank = model$itembank, metric = "logit")
test_that("D-score vs logit alignment (LF)", {
  expect_equal(ds$d, transform["slope"] * dl$d + transform["intercept"], tolerance = 0.001)
  expect_equal(ds$sem, transform["slope"] * dl$sem, tolerance = 0.001)
})

plot(x = dl$d, y = ds$d, cex = 0.4, col = "blue", pch = 19,
     xlab = "D-score (logit)", ylab = "D-score (dscore)",
     main = "D-score (logit) vs D-score (dscore)")
abline(coef = transform, col = "orange")
plot(x = dl$sem, y = ds$sem, cex = 0.4, col = "blue", pch = 19,
     xlab = "SEM (logit)", ylab = "SEM (dscore)",
     main = "SEM (logit) vs SEM (dscore)")
abline(coef = c(0, transform[2]), col = "orange")

# Check anchors
ib <- model$itembank
test_that("Anchors items are 20 and 40", {
  expect_equal(ib[ib$item == "gtogmd001", "tau"], 20, tolerance = 0.001)
  expect_equal(ib[ib$item == "gtogmd026", "tau"], 40, tolerance = 0.001)
})
