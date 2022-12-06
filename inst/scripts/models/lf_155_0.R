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

# If needed, install dmetric from GitHub
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.64.2") stop("Needs dmetric 0.64.2")
library("dmetric")

# run auxiliary scripts to read and process data from source
if (packageVersion("gsedscripts") < "0.5.0") stop("Needs gsedscripts 0.5.0")
suppressWarnings(source(system.file("scripts/assemble_data.R", package = "gsedscripts")))
# source(system.file("scripts/edit_data.R", package = "gsedscripts"))

# select instrument data and pre-process
items <- colnames(work)[starts_with("gto", vars = colnames(work))]
adm <- c("cohort", "subjid", "agedays", "country", "study")
vars <- c(adm, items)
data <- work %>%
  filter(ins == "lf") %>%
  rename(
    subjid = gsed_id,
    agedays = age) %>%
  mutate(
    cohort = strtrim(subjid, 7),
    country = strtrim(file, 3),
    study = recode(country, "ban" = "BGD", "pak" = "PAK", "tza" = "TZA"),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) %>%
  select(all_of(vars))

# The following anchors do not work
# 20 - Lifts head to 45d in prone position - gtogmd001 (ddigmd057)
# 40 - Sits in stable position, without support - gtogmd022 (ddigmd063)
#

# Fit the Rasch model
model_name <- paste(length(items), "0", sep = "_")
model <- fit_dmodel(varlist = list(adm = adm, items = items),
                    data = data,
                    name = model_name,
                    transform = c(55.46, 4.07),
                    data_package = "")

# Store and reload model
path <- file.path("~/project/gsed/phase1/20221201_remodel", model_name)
if (!dir.exists(path)) dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
model <- readRDS(file.path(path, "model.Rds"))

# Plot figures
theme_set(theme_light())
col_manual <- c("BGD" = "#D93F46", "PAK" = "#489033", "TZA" = "#47A1D8")
r <- plot_dmodel(data = data,
                 model = model,
                 path = path,
                 ref_name = "gcdg",
                 col.manual = col_manual,
                 maxy = 85,
                 xlim = c(0, 85),
                 xbreaks = seq(0, 80, 10))

# # Alternative anchoring: Obtain regression coefficients relative to gsed key for DDI
# items_gsed <- gsedread::rename_vector(items, lexin = "gsed2", lexout = "gsed")
# ddi <- grep("ddi", items_gsed)
# beta_gsed <- dscore::get_tau(items_gsed[ddi], key = "gsed", itembank = dscore::builtin_itembank)
# beta_l <- get_diff(model$fit)[ddi]
# cal <- lm(beta_gsed ~ beta_l)
# # plot(y = beta_gsed, x = beta_l, col = "orange", pch = 20)
# # abline(cal, col = "red")
# transform <- coef(cal)

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))

