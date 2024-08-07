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
# Setting the anchor removes the arbitrary transformation c(55, 4)
# The actual transform is according to the new anchors is c(54.94, 4.06)
#
# The script
# 1. select LF and SF items
# 2. fuzzy joins the LF and SF administration to one record
# 3. fits Rasch model
# 4. produces the diagnostic plots
# 5. compares D-score by age and tau-estimates to gsed2206
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

library(dplyr)
library(ggplot2)
library(fuzzyjoin)

# If needed, install dmetric from GitHub
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.67.0") stop("Needs dmetric 0.67.0")
library("dmetric")

# run auxiliary scripts to read and process data from source
if (packageVersion("gsedscripts") < "0.13.0") stop("Needs gsedscripts 0.13.0")
suppressWarnings(source(system.file("scripts/assemble_data.R", package = "gsedscripts")))
source(system.file("scripts/edit_data.R", package = "gsedscripts"))

# select instrument data and pre-process, select fixed administration
adm <- c("cohort", "cohortn", "subjid", "joinid", "agedays", "ins")
items <- colnames(work)[starts_with(c("gpa", "gto"), vars = colnames(work))]
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

# Fuzzy match on gsed_id and agedays
# We allow for a 4-day difference between the SF and LF measurement
# Double fuzzy match, lf, sf keep all records
sf <- long |>
  filter(ins == "sf") |>
  select(all_of(adm), items[all_of(starts_with("gpa", vars = items))])
lf <- long |>
  filter(ins == "lf") |>
  select(all_of(adm), items[all_of(starts_with("gto", vars = items))])

## SvB 20240626: Changed to slice(1L) to keep only the first record
## Conservative method with only one record per child
# Fuzzy join, allowing for one row per child, < 4 days
sf_first <- sf |>
  group_by(subjid) |>
  slice(1L)
lf_first <- lf |>
  group_by(subjid) |>
  slice(1L)
data <- fuzzyjoin::difference_left_join(sf_first, lf_first,
                                        by = c("joinid", "agedays"),
                                        max_dist = 10, distance_col = "dist") |>
  ungroup() |>
  # filter(!is.na(ins.y)) |>
  rename(cohort = cohort.x, cohortn = cohortn.x, subjid = subjid.x,
         agedays = agedays.x) |>
  mutate(age = agedays / 365.25) |>
  select(cohort, cohortn, subjid, agedays, age, ins.x, ins.y, any_of(items))
# Result: 4374 records, 300 columns

# 20: Lift head 45 degrees
# 40: Moves from lying to sitting
anchor <- c(20, 40)
names(anchor) <- c("gtogmd001", "gtogmd026")
varlist = list(adm = c("cohort", "cohortn", "subjid", "age", "agedays"),
              items = items)

# Fit the Rasch model
model_name <- paste(length(items), "0", sep = "_")
model <- fit_dmodel(varlist = varlist,
                    data = data,
                    name = model_name,
                    anchors = anchor,
                    data_package = "", verbose = TRUE)

# Store and reload model
path <- file.path("~/project/gsed/phase1/20221201_remodel", model_name)
path <- file.path("~/project/gsed/phase1/20240601", model_name)
path <- file.path("~/project/gsed/phase1/20240703", model_name)
path <- file.path("~/project/gsed/phase1/202407", model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")
model_old <- readRDS(file.path(path, "model.Rds"))

# Plot figures
theme_set(theme_light())
col.manual <- c("GSED-BGD" = "#D93F46", "GSED-PAK" = "#489033", "GSED-TZA" = "#47A1D8")
r <- plot_dmodel(data = data,
                 model = model,
                 path = path,
                 col.manual = col.manual,
                 ref_name = "preliminary_standards",
                 maxy = 100,
                 xlim = c(0, 85),
                 xbreaks = seq(0, 100, 10))

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))
