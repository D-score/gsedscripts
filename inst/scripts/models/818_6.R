# This script fits the Rasch model to GCDG + CREDI + IYCD + GSED data
# Final model 818_6 using GSED-BGD, GSED-PAK and GSED-TZA as model core.
# 818 items, 6 equates
#
# This solution becomes the gsed2407 key in dscore.
#
# The tau values of models 293_0 and 818_6 are equivalent
# except for minor deviations (< 0.1) in by1ppd028, iyomoc039, croclc024 and
# mdtfmd033.
#
# SETS TWO ANCHOR ITEMS
# 20: Lift head 45 degrees (Same as before)
# 40: Lying to sitting (Replaces "Sits in stable position")
#
# The script
# 1. select LF and SF items
# 2. fuzzy joins the LF and SF administration to one record
# 3. combines with the data used to form the 807_17 model
# 4. fits Rasch model
# 5. produces the diagnostic plots
# 6. compares D-score by age and tau-estimates to gsed2212
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
# Update 20221201 SvB: Replaces incorrect gto labels
#                      Changes: Upper anchor (gtogmd026) lying to sitting: 40
# Update 20221202 SvB: Rerun model 818_6 with correct gto order
# Update 20221205 SvB: Remove bad matches: by3cgd059, mdtgmd021, mdtlgd003, mdtsed005, teplgd031, teplgd034
# Update 20240708 SvB: Update with dscore 1.9.3

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
colnames(data) <- c("cohort", "cohortn", "subjid", "agedays", "ins.x", "ins.y",
                    rename_vector(items, "gsed2", "gsed"))
new_data <- data[, !duplicated(colnames(data))]

# bind rows from gseddata in 807_17 model
old_model_name <- "807_17_revisit4"
path <- path.expand("~/Package/dmodellib/dmodellib/gsed1/models")
old_lean <- readRDS(file = file.path(path, old_model_name, "data.Rds"))
old_model <- readRDS(file = file.path(path, old_model_name, "model.Rds"))
old_data <- as.data.frame(old_lean)

# select only by3 items that are in the 807 model
data <- bind_rows(new_data, old_data)
# 61220, 1085
all_by3 <- colnames(data)[starts_with(c("by3"), vars = colnames(data))]
keep_by3 <- old_model$items[starts_with("by3", vars = old_model$items)]
remove_by3 <- setdiff(all_by3, keep_by3)
keepvars <- setdiff(colnames(data), remove_by3)
data <- data[, keepvars]
# 61220, 826

# by3cgd059, mdtgmd021, mdtlgd003, mdtsed005, teplgd031, teplgd034

# last data preparations
data <- data |> select(-c("ins.x", "ins.y", "keep"))
adm <- c("cohort", "cohortn", "subjid", "subjido", "agedays")
items <- setdiff(colnames(data), adm)
varlist <- list(adm = adm, items = items)

# 20: Lift head 45 degrees (gsed: ddigmd057, gsed2: gtogmd001)
# 40: Lying to sitting (gsed: kdigmd031, gsed2: gtogmd026)
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
model0 <- readRDS("~/Project/gsed/phase1/20221201_remodel/293_0/model.Rds")
b_fixed <- dmetric::get_diff(model0$fit)
names(b_fixed) <- gsedread::rename_vector(names(b_fixed), lexin = "gsed2", lexout = "gsed")

# Added 20221205
# remove constraint on by3cgd059, mdtgmd021, mdtlgd003, mdtsed005, teplgd031, teplgd034
items_takeout <- c("by3cgd059", "mdtgmd021", "mdtlgd003", "mdtsed005", "teplgd031", "teplgd034")
b_fixed <- b_fixed[!names(b_fixed) %in% items_takeout]
data[data$cohort %in% c("GSED-BGD", "GSED-PAK", "GSED-TZA"), items_takeout] <- NA

# set prior_mean and prior_sd
use_new <- TRUE
data$pm <- dscore:::count_mu_preliminary_standards(data$agedays/365.25)
data$sd <- rep(5, nrow(data))
# mean_name <- ifelse(use_new, ".phase1", ".gcdg")
# sd_name   <- ifelse(use_new, "sd", "sd")
# population <- ifelse(use_new, "phase1", "gcdg")

# Fit the Rasch model
model_name <- paste(length(items), "6", sep = "_")
model <- fit_dmodel(varlist = list(adm = adm, items = items),
                    data = data,
                    b_fixed = b_fixed,
                    equate = equatelist,
                    name = model_name,
                    anchors = anchor,
                    data_package = "")

# Store
# path <- file.path("~/Project/gsed/phase1/20221201_remodel", model_name)
# model_name <- "818_6"
path <- file.path("~/Project/gsed/phase1/202407", model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
saveRDS(data, file = file.path(path, "data.Rds"), compress = "xz")

# Reload
model <- readRDS(file.path(path, "model.Rds"))
data <- readRDS(file.path(path, "data.Rds"))

# Plot figures
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

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))

# compare with previous 818_6 model
# old_path <- file.path("~/Project/gsed/phase1/remodel", model_name)
old_path <- file.path("~/Project/gsed/phase1/20221201_remodel", model_name)
old_model <- readRDS(file.path(old_path, "model.Rds"))
with(old_model$item_fit, table(outfit<1.2 & infit<1.2))

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

# export key to dscore package
ib <- model$itembank |>
  mutate(key = "gsed2407",
         tau = round(tau, 2)) |>
  select(key, item, tau)
write.table(ib,
            file = file.path(path, "itembank.txt"),
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)

# compare gsed2407 to gsed2212
items <- ib$item
tau2407 <- dscore::get_tau(items, key = "gsed2407", itembank = ib)
tau2212 <- dscore::get_tau(items, key = "gsed2212")
plot(tau2212, tau2407)
abline(0,1)
diff <- tau2407 - tau2212
idx <- abs(diff) > 5
text(x = tau2212[idx], y = tau2407[idx], label = names(tau2407)[idx])
