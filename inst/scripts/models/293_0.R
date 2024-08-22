# This script fits the Rasch model to all items of the LF and SF collected
# in Phase 1 Validation, fixed data only.
#
# SETS TWO ANCHOR ITEMS
# 20: Lift head 45 degrees (Same as before)
# 40: Moves from lying to sitting (Replaces "Sits in stable position")
#
# Effect: Compared to previous anchoring:
# 1) D-score during the first year increases at slower rate
# 2) D-score beyond 3 yrs increases at higher rate
# Effect 1 is beneficial because it fits the GSED data better
# Effect 2 is beneficial because it postpones the developmental asymptote
#
# Data edits (see edit_data.R)
# 1. Removes clench fist item
# 2. Overwrites late response on early LF language items by NA
#
# The actual transform is according to the new anchors is c(54.94, 4.06)
#
# The script takes the following actions:
#  A. read phase1 data into object 'work'
#  B. fuzzy join the LF and SF phase1 fixed mode data
#  D. select items with at least 10 observations in both categories
#  E. specify anchors to define the D-score scale
#  F. estimate tau of SF and LF items by a single group design
#  G. test for proper D-score vs logit alignment
#  H. create diagnostic plots
#  I. evaluate item and person fit
#  J. calculate DIF by cohort by instrument
#  H. evaluate uni-dimensionality
#
#  Future steps
#  - keep best fitting items by outfit and infit < criterion
#  - keep best fitting children by outfit and infit < criterion
#  - study sources of DIF, remove or replace DIF-items
#  - suggest DIF-free subset through DIF-purification
#  - refit model with DIF-free subset
#  - store final core model
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private)
# Inline R scripts: assemble_data.R
#                   edit_data.R
#
# The objective is to estimate difficulty parameters under the assumption
# that the D-score of a child should be the same for both LF and SF.
#
# This model forms the CORE of the D-score definition.
#
# Aug 8, 2022, 2022 SvB
# Edits Aug 10, 2022 SvB
# Update 20221201 SvB: Replaces incorrect gto labels
#                      Changes: Upper anchor (gtogmd026) lying to sitting: 40
# Update 20221202 SvB: Rerun model 293_0 with correct gto order
# Check  20240601 SvB: Check model 293_0 with dscore 1.8.8 version
# Update 20240703 SvB: Using more LF data, 4374 records
# Update 202407: SvB: Just a rename to collect all July 2024 model results
# Update 20240813: Use new functions to replace duplicate fragments

plot_pdf <- FALSE
include_return <- TRUE

gsedscripts::update_required_packages(allow_update = FALSE)

# Load required packages
library("dscore")
library("dmetric")
library("gsedread")
library("gsedscripts")

library("dplyr")
library("testthat")
library("difR")

#
#  A. read phase1 data into object 'work'
#

suppressWarnings(source(system.file("scripts/assemble_data.R", package = "gsedscripts")))
source(system.file("scripts/edit_data.R", package = "gsedscripts"))

#
#  B. fuzzy join the LF and SF phase1 fixed mode data
#

phase1 <- bind_cols(calculate_administrative(work), work) |>
  filter(adm == "fixed") |>
  select(-colnames(work)[c(1:2, 4:26)]) |>
  make_wide(instruments = c("gpa", "gto"),
            include_return = include_return)

# phase1: 4374 records (=children), 300 columns (5 + 2 + 138 + 155)
# phase1: 6228 records (=measurements), 300 columns (5 + 2 + 138 + 155)
cat("dim(phase1):", dim(phase1), "\n")

#
#  D. select items with at least 10 observation in both categories
#

adm <- c("cohort", "cohortn", "subjid", "subjido", "agedays")
items <- c(get_itemnames(ins = "gpa", order = "indm"),
           get_itemnames(ins = "gto"))
items_subset <- dmetric::select_on_minimum_count(
  select(phase1, any_of(items)), min_ncat = 2L, min_cat = 10L)
data <- phase1 |>
  select(all_of(adm), all_of(items_subset))

# dim(data): 9431 298 (5 + 138 + 153)
cat("dim(data):", dim(data), "\n")

#
#  E. specify anchors to define the D-score scale
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
#  F. estimate tau of SF and LF items by a single group design
#

model_name <- paste(length(items_subset), "0", sep = "_")
if (include_return) model_name <- paste0(model_name, "_6228")
model <- fit_dmodel(varlist = list(adm = adm, items = items_subset),
                    data = data,
                    name = model_name,
                    anchors = anchor,
                    #                    transform = transform,
                    data_package = "",
                    verbose = TRUE)

# Store and reload model
path <- file.path("~/project/gsed/phase1/20221201_remodel", model_name)
path_old <- file.path("~/project/gsed/phase1/20240601", model_name)
if (include_return) path_old <- file.path("~/project/gsed/phase1/20240601/293_0")
path <- file.path("~/project/gsed/phase1/20240703", model_name)
path <- file.path("~/project/gsed/phase1/202408", model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")
model_old <- readRDS(file.path(path_old, "model.Rds"))
data_old <- readRDS(file.path(path_old, "data.Rds"))

#
#  G. test for proper D-score vs logit alignment
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

