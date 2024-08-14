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
#  I. keep best fitting BSID-items by outfit and infit < 1.0
#  J. refit model using items selected in I by repeating steps F to H
#  K. create and save key extension.txt
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

plot_pdf <- TRUE
include_return <- TRUE

gsedscripts::update_required_packages(allow_update = FALSE)

# Load required packages
library("dscore")
library("dmetric")
library("gsedread")
library("gsedscripts")

library("dplyr")
library("testthat")

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
#  J. test DIF by cohort
#

my_items <- model$items[starts_with(c("gpa", "gto"), vars = model$items)]
lgt <- dscore(items = model$items, data = data, xname = "agedays", xunit = "days",
              transform = transform, key = model$name, itembank = model$itembank, metric = "logit")
score <- lgt$d
my_items <- model$items

# # -- Using sirt for one focal group, SF/LF as one test
# This is cumbersome because we have multiple groups
# dat <- data.frame(data[, c("cohortn", my_items)], score)
# BGD <- ifelse(dat$cohortn == 111, 1, 0)
# PAK <- ifelse(dat$cohortn == 117, 1, 0)
# TZA <- ifelse(dat$cohortn == 120, 1, 0)
#
# BGD_dif <- sirt::dif.logistic.regression(dat = dat[, my_items],
#                                          group = BGD,
#                                          score = dat$score)
# PAK_dif <- sirt::dif.logistic.regression(dat = dat[, my_items],
#                                          group = PAK,
#                                          score = dat$score)
# TZA_dif <- sirt::dif.logistic.regression(dat = dat[, my_items],
#                                          group = TZA,
#                                          score = dat$score)
# table(BGD_dif$DIF.ETS)
# table(PAK_dif$DIF.ETS)
# table(TZA_dif$DIF.ETS)

# -- Using difR::difGenLogistic() for multiple groups, SF/LF as one test
library(difR)

result_GLR <- difR::difGenLogistic(Data = as.matrix(data[, my_items]),
                                   group = as.character(data$cohort),
                                   focal.names = unique(data$cohort),
                                   match = score,
                                   p.adjust = "holm")
cat("Number of failed GLR DIF tests: ", sum(is.na(result_GLR$deltaR2)), "\n")

# print.genLogistic(dif)
zumbo <- cut(result_GLR$deltaR2, breaks = c(-Inf, 0.13, 0.26, Inf), labels = c(LETTERS[1:3]))
jodoin <- cut(result_GLR$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf), labels = c(LETTERS[1:3]))
table(zumbo, useNA = "always")
table(jodoin, useNA = "always")

# -- Using difR::difGMH() for multiple groups, SF/LF as one test
result_GMH <- difR::difGMH(Data = as.matrix(data[, my_items]),
                           group = as.character(data$cohort),
                           focal.names = unique(data$cohort),
                           match = score)
cat("Number of failed GMH DIF tests: ", sum(is.na(result_GMH$GMH)), "\n")

GHM_result <- cut(result_GMH$GMH, breaks = c(-Inf, result_GMH$thr, Inf), labels = c("", "DIF"))
tmp <- data.frame(item = my_items, GMH = result_GMH$GMH, p.value = result_GMH$p.value, GHM_result)

# plot main DIF statistics
oldpar <- par(mfrow = c(1, 2))
hist(result_GLR$deltaR2, breaks = seq(0, 0.3, 0.005), main = "Generalized logistic regression DIF", xlab = "delta R2")
hist(result_GMH$GMH, breaks = seq(0, 30, 0.5), main = "Generalized Mantel-Haenszel DIF", xlab = "GHM-statistic")
par <- par(mfrow = c(1, 1))

# create item table sorted by DIF

dif_table <- data.frame(num = 1:length(my_items), item = my_items, deltaR2 = result_GLR$deltaR2, zumbo = zumbo, jodoin = jodoin)
dif_table <- left_join(dif_table, tmp, by = c("item" = "item"))
dif_table <- left_join(dif_table, model$item_fit[, c("item", "infit", "outfit")], by = c("item" = "item"))
dif_table <- left_join(dif_table, model$itembank[, c("item", "tau", "label")], by = c("item" = "item"))
dif_table <- dif_table[, c("num", "item", "tau", "infit", "outfit", "deltaR2", "GMH", "zumbo", "jodoin", "GHM_result", "label")]
head(dif_table, 30)

