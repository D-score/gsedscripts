# This script extends the model 293_0 with items from the GSED BSID.
#
# Data: GSED BGD/PAK/TZA
#
# SETS TWO ANCHOR ITEMS
# 20: Lift head 45 degrees (Same as before)
# 40: Lying to sitting (Replaces "Sits in stable position")
#
# The script
# 1. select LF, SF and BSID items
# 2. fuzzy joins the LF, SF and BSID administration to one record
# 3. prepares item selection
# 4. fits Rasch model
# 5. produces the diagnostic plots
# 6. compares D-score by age and tau-estimates
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private), gseddata (private)
# Inline R scripts: assemble_data.R
#                   edit_data.R
#
# Run 293_0.R first
#
# Aug 3, 2024 SvB - Goal is to align D-score and logit metric
# Feb 3, 2025 SvB - This is an experimental script showing that due to the
# reduced reliability in the GSED2406 model, many more BSID items now fit
# the D-score model. This script needs to be developed into a
# methodology that can be applied other instruments for which we wish
# to create a D-score key.

plot_pdf <- TRUE

library(dplyr)
library(ggplot2)
library(fuzzyjoin)

# If needed, install dmetric and gseddata from GitHub
if (!requireNamespace("dscore", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dscore needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dscore")
}
if (packageVersion("dscore") < "1.9.3") stop("Needs dscore 1.9.3")
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.67.0") stop("Needs dmetric 0.67.0 or higher")

if (!requireNamespace("gseddata", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package gseddata needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/gseddata")
}
if (packageVersion("gseddata") < "1.9.0") stop("Needs gseddata 1.9.0 or higher")
if (packageVersion("gsedread") < "0.8.0") stop("Needs gseddata 0.8.0 or higher")

library("dscore")
library("dmetric")
library("gseddata")

# run auxiliary scripts to read and process data from source
if (packageVersion("gsedscripts") < "0.13.0") stop("Needs gsedscripts 0.13.0")
suppressWarnings(source(system.file("scripts/assemble_data.R", package = "gsedscripts")))
source(system.file("scripts/edit_data.R", package = "gsedscripts"))

# select instrument data and pre-process, select fixed administration
adm <- c("cohort", "cohortn", "subjid", "joinid", "agedays", "ins")
items <- colnames(work)[starts_with(c("gpa", "gto", "by3"), vars = colnames(work))]
long <- work |>
  filter(adm == "fixed") |>
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohort = recode(cohort, "11-GSED" = "GSED-BGD", "17-GSED" = "GSED-PAK", "20-GSED" = "GSED-TZA"),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    joinid = subjid * 100,
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) |>
  drop_na(agedays) |>
  select(all_of(adm), all_of(items))

sf <- long |>
  filter(ins == "sf") |>
  select(all_of(adm), items[all_of(starts_with("gpa", vars = items))])
lf <- long |>
  filter(ins == "lf") |>
  select(all_of(adm), items[all_of(starts_with("gto", vars = items))])
bsid <- long |>
  filter(ins == "bsid") |>
  select(all_of(adm), items[all_of(starts_with("by3", vars = items))])

join_method <- "onematch_10"

sf_first <- sf |>
  group_by(subjid) |>
  slice(1L)
lf_first <- lf |>
  group_by(subjid) |>
  slice(1L)
bsid_first <- bsid |>
  group_by(subjid) |>
  slice(1L)

data <- fuzzyjoin::difference_left_join(sf_first, lf_first,
                                        by = c("joinid", "agedays"),
                                        max_dist = 10, distance_col = "dist") |>
  ungroup() |>
  rename(cohort = cohort.x, cohortn = cohortn.x, subjid = subjid.x,
         agedays = agedays.x, joinid = joinid.x) |>
  mutate(age = agedays / 365.25) |>
  select(c("cohort", "cohortn", "subjid", "joinid", "agedays", "ins.x", "ins.y"), any_of(items))
# data: 4374 records, 301 columns

data <- fuzzyjoin::difference_left_join(data, bsid_first,
                                        by = c("joinid", "agedays"),
                                        max_dist = 10, distance_col = "dist") |>
  ungroup() |>
  rename(cohort = cohort.x, cohortn = cohortn.x, subjid = subjid.x,
         agedays = agedays.x, joinid = joinid.x) |>
  mutate(age = agedays / 365.25) |>
  select(c("cohort", "cohortn", "subjid", "agedays", "ins.x", "ins.y"), any_of(items))
# data: 4374 records (=children), 626 columns

cat("dim(data", dim(data), "\n")

# number of duplo measurements
has_lf <- apply(!is.na(select(data, items[all_of(starts_with("gto", vars = items))])), 1, any)
has_sf <- apply(!is.na(select(data, items[all_of(starts_with("gpa", vars = items))])), 1, any)
has_bsid <- apply(!is.na(select(data, items[all_of(starts_with("by3", vars = items))])), 1, any)

# renaming into gsed naming schema
# 18 duplicates 18 by3: we keep the first occurrence in the data
# which contain LF or SF version responses
#colnames(data) <- c("cohort", "cohortn", "subjid", "agedays", "ins.x", "ins.y",
#                    rename_vector(items, "gsed2", "gsed"))
colnames(data) <- c("cohort", "cohortn", "subjid", "agedays", "ins.x", "ins.y", items)

# find duplicate items in LF that overlap with BSID
items_lf <- colnames(data)[starts_with(c("gto"), vars = colnames(data))]
items_lf_oldnames <- rename_vector(items_lf, "gsed2", "gsed")
items_bsid <- colnames(data)[starts_with("by3", vars = colnames(data))]
twins <- intersect(items_lf_oldnames, items_bsid)
match <- items_lf_oldnames %in% twins
names(twins) <- items_lf[match]

# add manual twins
# manually fix
man_twins <- c("by3gmd025", "by3gmd035")
names(man_twins) <- c("by3gmd025", "gtogmd025")
# by3gmd025, value: 31.6 (instead of 6.3)
twins <- c(twins, man_twins)

# the LF has 18 BSID twin items
# rather than removing these 18 columns from the data,
# we fix the values of twin items to their difficulties in model 293_0
# double linked data: both links at subject and item level!

# LARGE MODEL, 67 BSID items

# bind rows from gseddata in 807_17 model
# old_model_name <- "807_17_revisit4"
# path <- path.expand("~/Package/dmodellib/dmodellib/gsed1/models")
# old_lean <- readRDS(file = file.path(path, old_model_name, "data.Rds"))
# old_model <- readRDS(file = file.path(path, old_model_name, "model.Rds"))
# old_data <- as.data.frame(old_lean)

# select only by3 items that are in the 807 model
# by3_in_model <- colnames(old_data)[starts_with(c("by3"), vars = colnames(old_data))]
# old_data <- old_data |>
#  select(all_of(all_by3))
# 56846    67
# by3cgd059, mdtgmd021, mdtlgd003, mdtsed005, teplgd031, teplgd034

# SMALL MODEL, all BSID items, only GSED-BGD, GSED-PAK, GSED-TZA

# select items with at least 10 observations in both categories
items_subset <- dmetric::select_on_minimum_count(
  data[, items], min_ncat = 2L, min_cat = 10)
adm <- c("cohort", "cohortn", "subjid", "agedays")
data <- data[, c(adm, items_subset)]
# 4374  540




## Method 1: single group design for three instruments, with counterbalancing
## of LF and BSID-III
## Add subset of 64 key by3 items
## Fix difficulties of 293_0 model items
all_by3_items <- get_itemnames(ins = "by3")
tau_by3 <- get_tau(items = all_by3_items, key = "gsed2406")
key_by3_items <- names(tau_by3[!is.na(tau_by3)])
items_method1 <- c(items[starts_with("gpa", vars = items)],
                   items[starts_with("gto", vars = items)],
                   intersect(items_subset, key_by3_items))
d_fixed <- get_tau(items = items_method1, key = "293_0")[1:293]
transform <- unlist(builtin_keys[builtin_keys$key == "293_0", c("intercept", "slope")])
b_fixed <- (d_fixed - transform["intercept"]) / transform["slope"]

## check b_fixed
# core_path <- path.expand("~/Project/gsed/phase1/20221201_remodel/293_0")
# core_model <- readRDS(file.path(core_path, "model.Rds"))
# b_core <- -core_model$fit$betapar
# all.equal(b_fixed, b_core, tolerance = 0.0015)

data_method1 <- data |>
  select(all_of(adm), all_of(items_method1))
model_name <- paste(length(items_method1), "0", sep = "_")
anchor <- c(20, 40)
names(anchor) <- c("gtogmd001", "gtogmd026")
model <- fit_dmodel(varlist = list(adm = adm, items = items_method1),
                    data = data_method1,
                    b_fixed = b_fixed,
                    name = model_name,
                    anchors = anchor,
                    data_package = "")


# Store
path <- file.path("~/Project/gsed/phase1/202408")
if (!dir.exists(path)) dir.create(path)
path <- file.path(path, model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data_method1, file = file.path(path, "data.Rds"), compress = "xz")

# Reload
model <- readRDS(file.path(path, "model.Rds"))
# data <- readRDS(file.path(path, "data.Rds"))

# Plot figures
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

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))

# Compare tau lf/sf in model to tau published in model 293_0
oldpar <- par(mfrow = c(1, 2))
tau_published <- get_tau(items = items_method1[1:293], key = "293_0")
tau_model <- get_tau(items = items_method1[1:293], key = model_name,
                     itembank = model$itembank)
plot(tau_published, tau_model, cex = 0.5,
     xlim = c(0, 90), ylim = c(0, 90),
     main = "Estimated tau vs core model (LF and SF)",
     xlab = "Core model tau (published)",
     ylab = "Estimated tau")
abline(0, 1, col = "orange")
all.equal(tau_published, tau_model, tolerance = 0.00001)

# Compare new tau by3 to published tau of twins in model 293_0
by3_items <- items[starts_with("by3", vars = items)]
tau_lf_twins <- get_tau(items = names(twins), key = "gsed2406")
tau_lf <- get_tau(items = by3_items, key = "gsed2406")
tau_by3_twins <- get_tau(items = twins, key = model_name, itembank = model$itembank)
tau_by3 <- get_tau(items = by3_items, key = model_name, itembank = model$itembank)
plot(tau_lf, tau_by3, cex = 0.5,
     xlim = c(0, 90), ylim = c(0, 90),
     main = "Item twins LF vs BSID-III", xlab = "LF twin (published)",
     ylab = "BSID-III (estimated)")
abline(0, 1, col = "orange")
points(tau_lf_twins, tau_by3_twins, cex = 0.5, pch = 19)
par(oldpar)

# Compare D-scores estimated from LF, SF and BSID-III
lf_items <- items[starts_with("gto", vars = items)]
sf_items <- items[starts_with("gpa", vars = items)]
by3_items <- items[starts_with("by3", vars = items)]
d_lf <- dscore(items = lf_items, data = data, xname = "agedays", xunit = "days")
d_sf <- dscore(items = sf_items, data = data, xname = "agedays", xunit = "days")
d_by3 <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
                key = model_name, itembank = model$itembank)
d_by3_pub <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days")

d_combined <- data.frame(age = data$agedays / 365.25,
                         LF = d_lf$d, SF = d_sf$d,
                         BSID_III = d_by3$d, BSID_old = d_by3_pub$d)
panel_with_abline <- function(x, y, ...) {
  points(x, y, ...)
  abline(0, 1, col = "orange")
}
pairs(d_combined[, 2:4],
      panel = panel_with_abline,
      main = "Method 1: D-scores LF, SF, BSID-III",
      cex = 0.3, xlim = c(0, 90), ylim = c(0, 90), gap = 0)
cor(d_combined[, 1:4], use = "pairwise.complete.obs")

## Conclusions Method 1:
## The model 293_0 is extended with 64 BSID-III items, 17 twins
## Method 1 needs both the data of the core model and the new instrument
## measured on the same group of children
## Relations are linear, correlations very high



## Method 2: Use the common-item nonequivalent groups design
## - Fit restricted Rasch model to new data
## - Restrict tau estimation of 17 by3-lf twins to the tau from model 293_0
## - Do not use LF and SF data for estimation
## - As before, take only by3 items with at least 10 observations in both categories
## - As before, take only 67 items present in model "818_17"
all_by3_items <- get_itemnames(ins = "by3")
tau_by3 <- get_tau(items = all_by3_items, key = "gsed2406")
key_by3_items <- names(tau_by3[!is.na(tau_by3)])
items_method2 <- c(intersect(items_subset, key_by3_items))

# fix difficulties of the twin items
d_fixed <- get_tau(items = names(twins), key = "gsed2406")
names(d_fixed) <- twins
b_fixed <- (d_fixed - transform["intercept"]) / transform["slope"]

data_method2 <- data |>
  select(all_of(adm), all_of(items_method2))

model_name <- paste(length(items_method2), length(intersect(twins, items_method2)), sep = "_")
model <- fit_dmodel(varlist = list(adm = adm, items = items_method2),
                    data = data_method2,
                    b_fixed = b_fixed,
                    name = model_name,
                    transform = transform,
                    data_package = "")

# Store
path <- file.path("~/Project/gsed/phase1/202408")
if (!dir.exists(path)) dir.create(path)
path <- file.path(path, model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data_method2, file = file.path(path, "data.Rds"), compress = "xz")

# Reload
model <- readRDS(file.path(path, "model.Rds"))
# data <- readRDS(file.path(path, "data.Rds"))

# Plot figures
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

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))

# Compare tau lf/sf in model to tau published in model 293_0
oldpar <- par(mfrow = c(1, 2))
tau_published <- get_tau(items = items_method1[1:293], key = "293_0")
tau_model <- tau_published
plot(tau_published, tau_model, cex = 0.5,
     xlim = c(0, 90), ylim = c(0, 90),
     main = "Estimated tau vs core model (LF and SF)",
     xlab = "Core model tau (published)",
     ylab = "Estimated tau")
abline(0, 1, col = "orange")
all.equal(tau_published, tau_model, tolerance = 0.00001)

# Compare new tau by3 to published tau of twins in model 293_0
by3_items <- items[starts_with("by3", vars = items)]
tau_lf_twins <- get_tau(items = names(twins), key = "gsed2406")
tau_lf <- get_tau(items = by3_items, key = "gsed2406")
tau_by3_twins <- get_tau(items = twins, key = model_name, itembank = model$itembank)
tau_by3 <- get_tau(items = by3_items, key = model_name, itembank = model$itembank)
plot(tau_lf, tau_by3, cex = 0.5,
     xlim = c(0, 90), ylim = c(0, 90),
     main = "Item twins LF vs BSID-III", xlab = "LF twin (published)",
     ylab = "BSID-III (estimated)")
abline(0, 1, col = "orange")
points(tau_lf_twins, tau_by3_twins, cex = 0.5, pch = 19)
par(oldpar)


# Compare D-scores estimated from LF, SF and BSID-III
lf_items <- items[starts_with("gto", vars = items)]
sf_items <- items[starts_with("gpa", vars = items)]
by3_items <- items[starts_with("by3", vars = items)]
d_lf <- dscore(items = lf_items, data = data, xname = "agedays", xunit = "days")
d_sf <- dscore(items = sf_items, data = data, xname = "agedays", xunit = "days")
d_by3 <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
                key = model_name, itembank = model$itembank)
d_by3_pub <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days")

d_combined <- data.frame(age = data$agedays / 365.25,
                         LF = d_lf$d, SF = d_sf$d,
                         BSID_III = d_by3$d, BSID_old = d_by3_pub$d)
panel_with_abline <- function(x, y, ...) {
  points(x, y, ...)
  abline(0, 1, col = "orange")
}
pairs(d_combined[, 2:4],
      panel = panel_with_abline,
      main = "Method 2: D-scores LF, SF, BSID-III",
      cex = 0.3, xlim = c(0, 90), ylim = c(0, 90), gap = 0)
cor(d_combined[, 1:4], use = "pairwise.complete.obs")

## Conclusions Method 2:
## We do not need the data of the core model
## There are 15 restricted twin items
## (we removed 3 twins without at least 10 observations in the least observed category)
## Restricted tau of 15 twin items line up perfectly with the core model
## Misfit of twin items wags the lower tail of the distribution, not well calibrated
## D-score below 60 are too low

path <- file.path("~/Project/gsed/phase1/202408")
if (!dir.exists(path)) dir.create(path)
path <- file.path(path, model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data_method1, file = file.path(path, "data.Rds"), compress = "xz")

# Reload model1 and model2
model1 <- readRDS(path.expand("~/Project/gsed/phase1/202408/357_0/model.Rds"))
model2 <- readRDS(file.path("~/Project/gsed/phase1/202408", model$name, "model.Rds"))

# Compare tau
items <- intersect(model2$itembank$item, model1$itembank$item)
ib1 <- model1$itembank |>
  filter(item %in% items)
ib2 <- model2$itembank |>
  filter(item %in% items)
plot(ib1$tau, ib2$tau, xlim = c(0, 90), ylim = c(0, 90), pch = 19,
     xlab = "Item Difficulty BSID (Method 1)", ylab = "Item Difficulty BSID (Method 2)")
abline(0, 1, col = "orange")

# Compare D-scores
plot(model1$dscore$d, model2$dscore$d, xlim = c(0, 90), ylim = c(0, 90),
     pch = 19, xlab = "D-score BSID (Method 1)", ylab = "D-score BSID (Method 2)")
abline(0, 1, col = "orange")

# Compare D-scores versus age
d_by3_model1 <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
                       key = model1$name, itembank = model1$itembank)
d_by3_model2 <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
                       key = model2$name, itembank = model2$itembank)

plot(d_by3_model1$a, d_by3_model1$d, ylim = c(0, 90), xlim = c(0, 3.5),
     xlab = "Age (Method 1)", ylab = "D (Method 1)", col = "blue")
points(d_by3_model2$a, d_by3_model2$d, col = "red")

# make dual itembank
dib <- data.frame(ib1[, 1:5], key2 = ib2[, 5], tau1 = ib1[, 6], tau2 = ib2[, 6], diff = ib1[, 6] - ib2[, 6])

# Check D-score - logit relation - Method 1/2
model <- model2
ds <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
             key = model$name, itembank = model$itembank)
dl <- dscore(items = by3_items, data = data, xname = "agedays", xunit = "days",
             key = model$name, itembank = model$itembank, metric = "logit")
plot(x = dl$d, y = ds$d, cex = 0.4, col = "blue", pch = 19,
     xlab = "D-score (logit)", ylab = "D-score (dscore)",
     main = "D-score (logit) vs D-score (dscore)")
abline(coef = transform, col = "orange")
plot(x = dl$sem, y = ds$sem, cex = 0.4, col = "blue", pch = 19,
     xlab = "SEM (logit)", ylab = "SEM (dscore)",
     main = "SEM (logit) vs SEM (dscore)")
abline(coef = c(0, transform[2]), col = "orange")

# CONCLUSION: Both methods 1 and 2 produce D-scores with a perfect linear
# relation to the logit metric

ds <- dscore(data = data, xname = "agedays", xunit = "days", key = "gsed2406")
dl <- dscore(data = data, xname = "agedays", xunit = "days", key = "gsed2406", metric = "logit")
