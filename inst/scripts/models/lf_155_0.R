# This script fits the Rasch model to all items of the GSED LF.
# The script
# 1. select LF items
# 2. fits Rasch model
# 3. produces the diagnostic plots
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private)
# Inline R scripts: assemble_data.R
#
# The objective is to estimate difficulty parameters in the traditional way.
#
# May 29, 2022 SvB
# Update 20221202 SvB: Rerun model lf_155_0 with correct gto order
# Update 20240815: Use new functions to replace duplicate fragments

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
items <- get_itemnames(ins = "gto")
items_subset <- dmetric::select_on_minimum_count(
  select(phase1, any_of(items)), min_ncat = 2L, min_cat = 10L)
data <- phase1 |>
  select(all_of(adm), all_of(items_subset))

# dim(data): 9431 160 (5 + 153)
cat("dim(data):", dim(data), "\n")

#
#  E. specify anchors to define the D-score scale
#

# 20: Lift head 45 degrees
# 40: Moves from lying to sitting
anchor <- c(20, 40)
names(anchor) <- c("gtogmd001", "gtogmd026")

#
#  F. estimate tau of SF and LF items by a single group design
#

model_name <- paste(length(items_subset), "0", sep = "_")
if (include_return) model_name <- paste0(model_name, "_6228")
model <- fit_dmodel(varlist = list(adm = adm, items = items_subset),
                    data = data,
                    name = model_name,
                    anchors = anchor,
                    data_package = "",
                    verbose = TRUE)

# Store and reload model
path <- file.path("~/project/gsed/phase1/20221201_remodel", model_name)
path_old <- file.path("~/project/gsed/phase1/20240601", model_name)
if (include_return) path_old <- file.path("~/project/gsed/phase1/20240601/293_0")
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

