# This script fits the Rasch model to all items of the GSED SF.
# The script
# 1. select SF items
# 2. fits Rasch model
# 3. produces the diagnostic plots
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private)
# Inline R scripts: assemble_data.R
#
# The objective is to estimate difficulty parameters in the traditional way.
# 20220531 SvB

# If needed, install dmetric from GitHub
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.62.0") stop("Needs dmetric 0.62.0")

library("dmetric")

# get all data
suppressWarnings(source("scripts/assemble_data.R"))

# select instrument data and pre-process
items <- colnames(work)[starts_with("gpa", vars = colnames(work))]
adm <- c("cohort", "subjid", "agedays", "country", "study")
vars <- c(adm, items)
data <- work %>%
  filter(ins == "sf") %>%
  rename(
    subjid = gsed_id,
    agedays = age) %>%
  mutate(
    cohort = strtrim(subjid, 7),
    country = strtrim(file, 3),
    study = recode(country, "ban" = "BGD", "pak" = "PAK", "tza" = "TZA"),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_)),
    gpamoc008 = recode(gpamoc008, "1" = 0L, "0" = 1L, .default = NA_integer_)) %>%
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
                    transform = c(53.98, 3),
                    data_package = "")

# Store and reload model
path <- file.path("~/project/gsed/phase1/sf", model_name)
dir.create(path, showWarnings = FALSE)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
model <- readRDS(file.path(path, "model.Rds"))

# Plot figures
theme_set(theme_light())
col_manual = c("BGD" = "#D93F46", "PAK" = "#489033", "TZA" = "#47A1D8")
r <- plot_dmodel(data = data,
                 items = items,
                 model = model,
                 path = path,
                 ref_name = "gcdg",
                 col.manual = col_manual,
                 maxy = 85,
                 xlim = c(0, 85),
                 xbreaks = seq(0, 80, 10))

# # # Alternative anchoring: Obtain regression coefficients relative to gsed key
# items_gsed <- gsedread::rename_vector(items, lexin = "gsed2", lexout = "gsed")
# idx <- 5:length(items_gsed)
# beta_gsed <- dscore::get_tau(items_gsed[idx], key = "gsed", itembank = dscore::builtin_itembank)
# beta_l <- get_diff(model$fit)[idx]
# cal <- lm(beta_gsed ~ beta_l)
# plot(y = beta_gsed, x = beta_l, col = "orange", pch = 20)
# abline(cal, col = "red")
# transform <- coef(cal)

# statistics
with(model$item_fit, table(outfit<1.2 & infit<1.2))
