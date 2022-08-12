# This script fits the Rasch model to GCDG + CREDI + IYCD + GSED data
# Final model 818_6 using GSED-BGD, GSED-PAK and GSED-TZA as model core.
# 818 items, 6 equates
#
# This solution becomes the gsed2208 key in dscore.
#
# The tau values of models 293_0 and 818_6 are equivalent
# except for minor deviations (< 0.1) in by1ppd028, iyomoc039, croclc024 and
# mdtfmd033.
#
# SETS TWO ANCHOR ITEMS
# 20: Lift head 45 degrees (Same as before)
# 40: Pulls to stand (Replaces "Sits in stable position")
#
# The script
# 1. select LF and SF items
# 2. fuzzy joins the LF and SF administration to one record
# 3. combines with the data used to form the 807_17 model
# 4. fits Rasch model
# 5. produces the diagnostic plots
# 6. compares D-score by age and tau-estimates to gsed2206
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private), gseddata (private)
# Inline R scripts: assemble_data.R
#                   edit_data.R
#
# Run 293_0.R first
#
# Aug 12, 2022, 2022 SvB
#
library(dplyr)
library(ggplot2)
library(fuzzyjoin)

# If needed, install dmetric and gseddata from GitHub
if (!requireNamespace("dscore", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dscore needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dscore")
}
if (packageVersion("dscore") < "1.5.10") stop("Needs dscore 1.5.10")
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.64.0") stop("Needs dmetric 0.64.0 or higher")

if (!requireNamespace("gseddata", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package gseddata needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/gseddata")
}
if (packageVersion("gseddata") < "1.9.0") stop("Needs gseddata 1.9.0 or higher")

library("dmetric")
library("gseddata")

# get all data
suppressWarnings(source("scripts/assemble_data.R"))
source("scripts/edit_data.R")

# select instrument data and pre-process, select fixed administration
adm <- c("cohort", "cohortn", "subjid", "joinid", "agedays", "ins")
items <- colnames(work)[starts_with(c("gpa", "gto", "by3"), vars = colnames(work))]
long <- work %>%
  filter(adm == "fixed") %>%
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohort = recode(cohort, "11-GSED" = "GSED-BGD", "17-GSED" = "GSED-PAK", "20-GSED" = "GSED-TZA"),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    joinid = subjid * 10,
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
    subjid = ifelse(is.na(subjid.x), subjid.y, subjid.x),
    joinid = ifelse(is.na(joinid.x), joinid.y, joinid.x),
    agedays = ifelse(is.na(agedays.x), agedays.y, agedays.x),
    ins = ifelse(is.na(ins.x), ins.y, ins.x),
  ) %>%
  select(all_of(adm), any_of(items))
# Result: 6838 records, 625 columns

# renaming into gsed naming schema
# 18 duplicates 18 by3: we keep the first occurrence in the data
# which contain LF or SF version responses
colnames(data) <- c(adm, rename_vector(items, "gsed2", "gsed"))
new_data <- data[, !duplicated(colnames(data))]

# bind rows from gseddata in 807_17 model
old_model_name <- "807_17_revisit4"
path <- path.expand("~/Package/dmodellib/dmodellib/gsed1/models")
old_lean <- readRDS(file = file.path(path, old_model_name, "data.Rds"))
old_model <- readRDS(file = file.path(path, old_model_name, "model.Rds"))
old_data <- as.data.frame(old_lean)

# select only by3 items that are in the 807 model
data <- bind_rows(new_data, old_data)
all_by3 <- colnames(data)[starts_with(c("by3"), vars = colnames(data))]
keep_by3 <- old_model$items[starts_with("by3", vars = old_model$items)]
remove_by3 <- setdiff(all_by3, keep_by3)
keepvars <- setdiff(colnames(data), remove_by3)
data <- data[, keepvars]
# 63684, 826

# last data preparations
data <- data %>% select(-c("ins", "keep", "joinid"))
adm <- c("cohort", "cohortn", "subjid", "subjido", "agedays")
items <- setdiff(colnames(data), adm)
varlist <- list(adm = adm, items = items)

# 20: Lift head 45 degrees
# 40: Pulls to stand
anchor <- c(20, 40)
names(anchor) <- gsedread::rename_vector(c("gtogmd001", "gtogmd026"), lexin = "gsed2", lexout = "gsed")

# Connect bar, by2 and gri
equatelist <- list(
  GM25 = c("by1pdd028", "barxxx005"),
  GM42 = c("cromoc021", "barxxx012", "by2pdd062"),
  GM60 = c("iyomoc039", "barxxx019"),
  COG55 = c("by1mdd118", "by2mdd098"),
  EXP26 = c("croclc024", "by2mdd114", "grihsd217"),
  FM61 = c("mdtfmd033", "griehd306")
)

# obtain difficulty estimates from model 293_0
model0 <- readRDS("~/project/gsed/phase1/remodel/293_0/model.Rds")
b_fixed <- dmetric::get_diff(model0$fit)
names(b_fixed) <- gsedread::rename_vector(names(b_fixed), lexin = "gsed2", lexout = "gsed")

# set prior_mean and prior_sd
use_new <- TRUE
data$pm <- dscore:::count_mu_phase1(data$agedays/365.25)
data$sd <- rep(5, nrow(data))
mean_name <- ifelse(use_new, ".phase1", ".gcdg")
sd_name   <- ifelse(use_new, "sd", "sd")
population <- ifelse(use_new, "phase1", "gcdg")

# Fit the Rasch model
model_name <- paste(length(items), "6", sep = "_")
model <- fit_dmodel(varlist = list(adm = adm, items = items),
                    data = data,
                    b_fixed = b_fixed,
                    equate = equatelist,
                    name = model_name,
                    anchors = anchor,
                    data_package = "")

# Store and reload model
path <- file.path("~/project/gsed/phase1/remodel", model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")
model <- readRDS(file.path(path, "model.Rds"))

# Plot figures
theme_set(theme_light())
col.manual <- get_palette("study", package = "gseddata")
r <- plot_dmodel(data = data,
                 model = model,
                 path = path,
                 col.manual = col.manual,
                 ref_name = "phase1",
                 maxy = 100,
                 xlim = c(0, 100),
                 xbreaks = seq(0, 100, 10))

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))

# check equivalence of tau with model 293_0
ib0 <- cbind(key = "293_0", model0$itembank)
ib1 <- cbind(key = "custom", model$itembank)
items2 <- ib0$item
items <- rename_vector(items2, lexin = "gsed2", lexout = "gsed")
tau_293_0 <- dscore::get_tau(items2, key = "293_0", itembank = ib0)
tau_model <- dscore::get_tau(items, key = "custom", itembank = ib1)
plot(tau_293_0, tau_model); abline(0, 1)
plot(x = (tau_293_0 + tau_model)/2, y = tau_293_0 - tau_model, type = "n")
text(x = (tau_293_0 + tau_model)/2, y = tau_293_0 - tau_model, label = names(tau_model))

