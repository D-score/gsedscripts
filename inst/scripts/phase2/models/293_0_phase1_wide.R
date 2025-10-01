# This script refits the core model 293_0 for the phase-1 data with
# the following changes compared to the 2024 analyses:
# + The sample size is slighly lower due to updated duplicate removal
# + The data are read from the DuckDB database
# + Responses from inter-rater scores (vist_type 5) are ignored
#
# Dependencies:
# + Environmental variable "GSED_PHASE1" must be set to the local directory
#   containing the models for phase 1 (will be used only for reading)
# + Environmental variable "GSED_PHASE2" must be set to the local directory
#   containing the models for phase 2 (will be used for writing)
#
# Created   20250618 SvB
# Modified  20250713 SvB

plot_pdf <- FALSE

if (nchar(Sys.getenv("GSED_PHASE1")) == 0L) {
  stop("Environmental variable GSED_PHASE1 not set.", call. = FALSE)
}
if (nchar(Sys.getenv("GSED_PHASE2")) == 0L) {
  stop("Environmental variable GSED_PHASE2 not set.", call. = FALSE)
}


# Load required packages
library("DBI", quietly = TRUE, warn.conflicts = FALSE)
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("dscore")
library("dfine")
library("tidyr", quietly = TRUE, warn.conflicts = FALSE)
library("testthat", quietly = TRUE, warn.conflicts = FALSE)
library("ggplot2", quietly = TRUE, warn.conflicts = FALSE)

if (packageVersion("gsedread") < "0.16.0") stop("Needs gsedread 0.16.0")
if (packageVersion("dfine") < "0.13.0") stop("Needs gsedread 0.13.0")

#
#  A.  Read fixed form Phase 1 data responses and visits
#
dbfile <- file.path(Sys.getenv("GSED_PHASE2"), "data/fixed.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = dbfile)
dbListTables(con)
visits <- dbReadTable(con, "visits")
responses <- dbReadTable(con, "responses")
dbDisconnect(con)

#
#  B.  Subset to phase 1 LF and SF data
#      Ignore any inter-rater scores (vist_type 5) to prevent duplicate matches
#

visits <- visits |>
  filter(phase == 1L & ins %in% c("lf", "sf") & vist_type != 5L)
responses <- semi_join(
  responses,
  visits,
  by = c("subjid", "agedays", "vist_type"))

#
#  C. Identify pairs of LF-SF records occurring within four days
#

visits <- visits |>
  select(subjid, agedays, ins, vist_type) |>
  arrange(subjid, agedays, ins, vist_type) |>
  group_by(subjid) |>
  mutate(sf_order = if_else(ins == "sf", row_number(), NA_integer_)) |>
  mutate(sf_order = if_else(ins == "sf", cumsum(ins == "sf"), NA_integer_)) |>
  ungroup()

# Find pair number (0, 1, 2, 3) for each SF record
sf_rows <- visits |>
  filter(ins == "sf") |>
  select(subjid, sf_agedays = agedays, sf_order)
lf_rows <- visits |>
  filter(ins == "lf") |>
  select(subjid, lf_agedays = agedays)
pairs <- sf_rows |>
  left_join(lf_rows, by = "subjid", relationship = "many-to-many") |>
  mutate(diff = abs(sf_agedays - lf_agedays)) |>
  group_by(subjid, sf_order) |>
  slice_min(order_by = diff, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(pair = ifelse(diff > 4L | is.na(diff), 0L, sf_order))

# Merge pair number with with response
items_sf <- get_itemnames(ins = "gpa", order = "indm")
items_lf <- get_itemnames(ins = "gto")
responses <- responses |>
  filter(item %in% c(items_sf, items_lf))
responses_sf <- responses |>
  filter(item %in% items_sf) |>
  left_join(pairs, by = join_by(subjid, agedays == sf_agedays)) |>
  mutate(ins = "sf") |>
  select(subjid, agedays, pair, ins, item, response)
responses_lf <- responses |>
  filter(item %in% items_lf) |>
  left_join(pairs |> filter(pair > 0),
            by = join_by(subjid, agedays == lf_agedays)) |>
  mutate(pair = ifelse(is.na(pair), -1L, pair),
         ins = "lf") |>
  select(subjid, agedays, pair, ins, item, response)

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
#  D. Create wide format that puts items from a SF-LF pair on the same row
#

data <- responses |>
  select(subjid, agedays, pair, ins, item, response) |>
  pivot_wider(names_from = c(item), values_from = response,
              id_cols = c(subjid, pair)) |>
  arrange(subjid, pair)
agedays_info <- responses |>
  distinct(subjid, pair, ins, agedays) |>
  pivot_wider(names_from = ins, values_from = agedays,
              names_prefix = "agedays_")
data <- data |>
  left_join(agedays_info, by = c("subjid", "pair")) |>
  select(subjid, pair, starts_with("agedays_"), any_of(items_sf), any_of(items_lf))

# wide: 5961 visits (measurements), 297 columns (2 + 2 + 138 + 155)
# wide: 4374 unique subjid (=children)
cat("dim(data):", dim(data), "\n")

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
#  F. Estimate difficulty of SF and LF items by a single group design
#

fit <- rasch(data = data, items = valid_items)

#
#  G. Specify anchors to define the D-score scale
#

# Define agedays
data <- data |>
  mutate(agedays = rowMeans(across(c(agedays_sf, agedays_lf)), na.rm = TRUE))

# 20: Lift head 45 degrees
# 40: Moves from lying to sitting
model <- calculate_dmodel(data = data,
                          fit = fit,
                          name = "phase1_wide",
                          anchors = c(gtogmd001 = 20, gtogmd026 = 40))


# Store and (re)load models
path_old <- file.path(Sys.getenv("GSED_PHASE1"), "202408/293_0")
model_old <- readRDS(file.path(path_old, "model.Rds"))
data_old <- readRDS(file.path(path_old, "data.Rds"))

path_new <- file.path(Sys.getenv("GSED_PHASE2"), "202507", model$name)
if (!dir.exists(path_new)) dir.create(path_new)
saveRDS(model, file = file.path(path_new, "model.Rds"), compress = "xz")


#
#  H. Test for proper D-score vs logit alignment
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

#
#  H. create diagnostic plots
#

# from here on, we still need dmetric

# Add cohort field to data

model$dscore$cohort <- data |>
  mutate(
    isonum = as.integer(substr(subjid, 1, 3)),
    ctry = case_when(
      isonum == 50L  ~ "BGD",
      isonum == 586L ~ "PAK",
      isonum == 834L ~ "TZA",
      isonum == 76L  ~ "BRA",
      isonum == 156L ~ "CHN",
      isonum == 384L ~ "CIV",
      isonum == 528L ~ "NLD",
      TRUE ~ NA_character_),
    cohort = paste0("GSED-", ctry)
  ) |>
  pull(cohort)

if (plot_pdf) {
  theme_set(theme_light())
  col.manual <- dfine::get_palette("cohort")
  r <- dmetric::plot_dmodel(data = data,
                            model = model,
                            path = path,
                            col.manual = col.manual,
                            ref_name = "preliminary_standards",
                            maxy = 100,
                            xlim = c(0, 100),
                            xbreaks = seq(0, 100, 10))
}

#
#  I. evaluate item and person fit
#

## The code below calculates person fit by sirt::personfit.stat()
## Conclusion: Very similar, some divergence in the tails
# my_items <- model$items[starts_with(c("gpa", "gto"), vars = model$items)]
# dat <- data[, my_items]
# lgt <- dscore(items = model$items, data = data, xname = "agedays", xunit = "days",
#               transform = transform, key = model$name, metric = "logit")
# my_tau <- get_tau(my_items, itembank = model$itembank, key = model$name)
# my_tau <-  (my_tau - transform["intercept"]) / transform["slope"]
# pfit <- sirt::personfit.stat(
#   dat = dat,
#   abil = lgt$d,
#   b = my_tau)
# hist(pfit$outfit, xlim = c(0, 5), breaks = c(seq(0, 6, 0.1), Inf))
# hist(pfit$infit, xlim = c(0, 5), breaks = c(seq(0, 6, 0.1), Inf))
# plot(pfit$infit, pfit$outfit, xlim = c(0, 3), ylim = c(0, 3), cex = 0.2)
# abline(v = 1, h = 1, col = "grey", lty = 2)
# plot(pfit$infit, model$person_fit$infit, xlim = c(0, 3), ylim = c(0, 3), cex = 0.2)
# plot(pfit$outfit, model$person_fit$outfit, xlim = c(0, 3), ylim = c(0, 3), cex = 0.2)
# abline(0,1)

# Person fit statistics

oldpar <- par(mfrow = c(2, 2))
hist(model$person_fit$outfit, xlim = c(0, 5), breaks = c(seq(0, 5, 0.1), Inf),
     main = "Person outfit", xlab = "", ylim = c(0, 2.6))
hist(model$person_fit$infit, xlim = c(0, 5), breaks = c(seq(0, 5, 0.1), Inf),
     main = "Person infit", xlab = "", ylim = c(0, 2.6))

# Item fit statistics

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

# Potential item removal cut-off by instrument
fits <- data.frame(
  item = model$item_fit$item,
  instrument = strtrim(model$item_fit$item, 3),
  lt1.2 = model$item_fit$outfit < 1.2 & model$item_fit$infit < 1.2,
  lt1.4 = model$item_fit$outfit < 1.4 & model$item_fit$infit < 1.4)
table(fits$instrument, fits$lt1.2)
table(fits$instrument, fits$lt1.4)

# Potential person removal cut-off by instrument

# ...


#
#  J. test DIF by cohort by instrument
#

## ---- DIF TEST FOR SF

itm <- model$items[starts_with("gpa", vars = model$items)]
# lgt <- dscore(items = itm, data = data, xname = "agedays", xunit = "days",
#               transform = transform, key = model$name, itembank = model$itembank,
#               metric = "logit")
# score <- lgt$d
group <- as.character(data$cohort)
dif_table <- calculate_DIF_table(data = data, items = itm, group = group)
dif_table <- left_join(dif_table, model$item_fit[, c("item", "infit", "outfit")], by = c("item" = "item"))
dif_table <- left_join(dif_table, model$itembank[, c("item", "tau", "label")], by = c("item" = "item"))
dif_table <- dif_table[, c("num", "item", "tau", "infit", "outfit",
                           "MH_stat", "MH_DIF",
                           "LR_stat", "LR_deltaR2" , "LR_zumbo", "LR_jodoin",
                           "label")]
dif_table_sf <- dif_table

## ---- DIF TEST FOR LF

itm <- model$items[starts_with("gto", vars = model$items)]
group <- as.character(data$cohort)
dif_table <- calculate_DIF_table(data = data, items = itm, group = group)
dif_table <- left_join(dif_table, model$item_fit[, c("item", "infit", "outfit")], by = c("item" = "item"))
dif_table <- left_join(dif_table, model$itembank[, c("item", "tau", "label")], by = c("item" = "item"))
dif_table <- dif_table[, c("num", "item", "tau", "infit", "outfit",
                           "MH_stat", "MH_DIF",
                           "LR_stat", "LR_deltaR2" , "LR_zumbo", "LR_jodoin",
                           "label")]
dif_table_lf <- dif_table

# plot main DIF statistics
# FIX NEEDED: Too many zeroes in Delta R2 Statistic, and many NaNs
oldpar <- par(mfrow = c(2, 2))
hist(dif_table_sf$MH_stat, breaks = c(seq(0, 30, 0.5), Inf), xlim = c(0, 30), ylim = c(0, 0.45), main = "SF: Generalized Mantel-Haenszel", xlab = "MH DIF-statistic")
hist(dif_table_sf$LR_deltaR2, breaks = c(seq(0, 0.3, 0.005), Inf), xlim = c(0, 0.3), main = "SF: Generalized logistic regression", xlab = "Delta R2 Statistic")
hist(dif_table_lf$MH_stat, breaks = c(seq(0, 30, 0.5), Inf), xlim = c(0, 30), ylim = c(0, 0.45), main = "LF: Generalized Mantel-Haenszel", xlab = "MH DIF-statistic")
hist(dif_table_lf$LR_deltaR2, breaks = c(seq(0, 0.3, 0.005), Inf), xlim = c(0, 0.3), main = "LF: Generalized logistic regression", xlab = "Delta R2 Statistic")
par <- par(mfrow = c(1, 1))

# Save DIF tables
write.csv(dif_table_sf, file = file.path(path, "dif_table_sf.csv"), row.names = FALSE)
write.csv(dif_table_lf, file = file.path(path, "dif_table_lf.csv"), row.names = FALSE)

#
#  H. evaluate uni-dimensionality
#

itm <- model$items[starts_with("gpa", vars = model$items)]
score <- dscore(items = itm, data = data, xname = "agedays", xunit = "days",
                transform = transform, key = model$name, itembank = model$itembank,
                metric = "logit")$d
taus <- get_tau(itm, itembank = model$itembank, key = model$name)
taus <- (taus - transform["intercept"]) / transform["slope"]
observed <- cor(data[, itm], method = "spearman", use = "pairwise")
q3 <- sirt::Q3(dat = data[, itm], theta = score, b = taus)

p <- length(itm)
q3m <- q3$q3.matrix
diag(q3m) <- NA
# q3m[abs(q3m) < 0.2] <- NA
obs <- observed
obs[abs(obs) < 0.2] <- NA
q3m[abs(q3m) < 0.4] <- NA
dif <- obs - q3m

# oldpar <- par(mfrow = c(1, 2))
# hist(observed[as.numeric(q3$q3.long$N) > 0], breaks = seq(-1, 1, 0.02), main = "Pairwise Spearman correlation", xlab = "Correlation")
hist(q3$q3.long$Q3[as.numeric(q3$q3.long$N) > 200], freq = TRUE, breaks = c(-Inf, seq(-0.5, 0.5, 0.01), Inf), xlim = c(-0.5, 0.5), main = "Q3 correlations", xlab = "Correlation")
# oldpar <- par(mfrow = c(1, 1))

boxplot(q3$q3.long$Q3[as.numeric(q3$q3.long$N) > 200], horizontal = TRUE)
table(q3$q3.long$Q3[as.numeric(q3$q3.long$N) > 200] < 0.2)
table(q3$q3.long$Q3[as.numeric(q3$q3.long$N) > 200] < -0.2)

image( 1:p, 1:p, q3m, col = gray(1-(0:32)/32), xlab = "Item", ylab = "Item")
image( 1:p, 1:p, q3m, col = rainbow(n = 8), xlab = "Item", ylab = "Item", useRaster = TRUE)

filled.contour( 1:p, 1:p, obs, col = colorRampPalette(c("blue", "white", "red"))(17), xlab = "Item", ylab = "Item")
filled.contour( 1:p, 1:p, q3m, col = colorRampPalette(c("blue", "white", "red"))(20), xlab = "Item", ylab = "Item")
filled.contour( 1:p, 1:p, dif, col = colorRampPalette(c("blue", "white", "red"))(20), xlab = "Item", ylab = "Item")

