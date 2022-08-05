# This script fits the Rasch model to all items of the LF, SF and BSID collected
# in Phase 1 Validation, fixed data only.
#
# The script
# 1. select LF, SF and BSID items
# 2. fuzzy joins the LF, SF and BSID administration to one record
# 3. fits Rasch model
# 4. produces the diagnostic plots
# 5. compares D-score by age and tau-estimates to gsed2206
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private)
# Inline R scripts: assemble_data.R
#
# The objective is to estimate difficulty parameters under the assumption
# that the D-score of a child should be the same for both LF and SF.
#
# Aug 3, 2022, 2022 SvB
#
# R package fuzzyjoin
library(dplyr)
library(ggplot2)
library(fuzzyjoin)

# If needed, install dmetric from GitHub
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.62.0") stop("Needs dmetric 0.62.0")

library("dmetric")

# get all data
suppressWarnings(source("scripts/assemble_data.R"))

# select instrument data and pre-process, select fixed administration
adm <- c("cohort", "cohortn", "study", "subjid", "joinid", "agedays", "ins")
items <- colnames(work)[starts_with(c("gpa", "gto", "by3"), vars = colnames(work))]
long <- work %>%
  filter(adm == "fixed") %>%
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    joinid = subjid * 10,
    country = strtrim(file, 3),
    study = recode(country, "ban" = "GSED-BGD", "pak" = "GSED-PAK", "tza" = "GSED-TZA"),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) %>%
  drop_na(agedays) %>%
  select(all_of(adm), all_of(items))

# Fuzzy match on gsed_id and agedays
# We allow for a 4-day difference between the SF, LF and BSID measurement
# Double fuzzy match, lf, sf, bsid, keep all records
sf <- long %>%
  filter(ins == "sf") %>%
  select(all_of(adm), items[all_of(starts_with("gpa", vars = items))])
lf <- long %>%
  filter(ins == "lf") %>%
  select(all_of(adm), items[all_of(starts_with("gto", vars = items))])
bsid <- long %>%
  filter(ins == "bsid") %>%
  select(all_of(adm), items[all_of(starts_with("by3", vars = items))])
joined <- fuzzyjoin::difference_full_join(sf, lf, by = c("joinid", "agedays"),
                                         max_dist = 4, distance_col = "dist") %>%
  mutate(
    cohort = ifelse(is.na(cohort.x), cohort.y, cohort.x),
    cohortn = ifelse(is.na(cohortn.x), cohortn.y, cohortn.x),
    study = ifelse(is.na(study.x), study.y, study.x),
    subjid = ifelse(is.na(subjid.x), subjid.y, subjid.x),
    joinid = ifelse(is.na(joinid.x), joinid.y, joinid.x),
    agedays = ifelse(is.na(agedays.x), agedays.y, agedays.x),
    ins = ifelse(is.na(ins.x), ins.y, ins.x),
  ) %>%
  select(all_of(adm), any_of(items))
data <- fuzzyjoin::difference_full_join(joined, bsid, by = c("joinid", "agedays"),
                                         max_dist = 4, distance_col = "dist") %>%
  mutate(
    cohort = ifelse(is.na(cohort.x), cohort.y, cohort.x),
    cohortn = ifelse(is.na(cohortn.x), cohortn.y, cohortn.x),
    study = ifelse(is.na(study.x), study.y, study.x),
    subjid = ifelse(is.na(subjid.x), subjid.y, subjid.x),
    joinid = ifelse(is.na(joinid.x), joinid.y, joinid.x),
    agedays = ifelse(is.na(agedays.x), agedays.y, agedays.x),
    ins = ifelse(is.na(ins.x), ins.y, ins.x),
  ) %>%
  select(all_of(adm), any_of(items))
# Result: 6838 records, 627 columns

# Remove items with fewer than 2 categories or fewer than 10 scores in either category
min_cat <- 10
min_ncat <- 2
tabs <- lapply(data[, items], table)
t1 <- sapply(tabs, length) >= min_ncat
n2 <- suppressWarnings(sapply(tabs, min))
n2[is.infinite(n2)] <- 0L
t2 <- n2 >= min_cat
items <- items[t1 & t2]
data <- data %>%
  select(all_of(adm), all_of(items))

# Fit the Rasch model
model_name <- paste(length(items), "0", sep = "_")
model <- fit_dmodel(varlist = list(adm = adm, items = items),
                    data = data,
                    name = model_name,
                    transform = c(55, 4),
                    data_package = "")

# Store and reload model
path <- file.path("~/project/gsed/phase1/lfsfbsid", model_name)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
model <- readRDS(file.path(path, "model.Rds"))

# Plot figures
theme_set(theme_light())
col.manual <- c("GSED-BGD" = "#D93F46", "GSED-PAK" = "#489033", "GSED-TZA" = "#47A1D8")
r <- plot_dmodel(data = data,
                 model = model,
                 path = path,
                 col.manual = col.manual,
                 ref_name = "gcdg",
                 maxy = 85,
                 xlim = c(0, 85),
                 xbreaks = seq(0, 80, 10))

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))

