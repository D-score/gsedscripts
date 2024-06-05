# This script fits the Rasch model to all items of the GSED BSID.
# The script
# 1. select GSID items
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
if (packageVersion("gsedscripts") < "0.5.0") stop("Needs gsedscripts 0.5.0")
suppressWarnings(source(system.file("scripts/assemble_data.R", package = "gsedscripts")))

# select instrument data and pre-process
items <- colnames(work)[starts_with("by3", vars = colnames(work))]
adm <- c("cohort", "subjid", "agedays", "country", "study")
vars <- c(adm, items)
data <- work |>
  filter(ins == "bsid") |>
  rename(
    subjid = gsed_id,
    agedays = age) |>
  mutate(
    cohort = strtrim(subjid, 7),
    country = strtrim(file, 3),
    study = recode(country, "ban" = "BGD", "pak" = "PAK", "tza" = "TZA"),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) |>
  select(all_of(vars))

# remove orphans
orphans <- c("by3cgd001", "by3cgd086", "by3cgd087", "by3cgd088", "by3cgd089",
             "by3cgd090", "by3cgd091", "by3fmd063", "by3fmd064", "by3fmd065",
             "by3fmd066", "by3gmd002")
data <- data |>
  select(-all_of(orphans))
items <- colnames(data)[starts_with("by3", vars = colnames(data))]

# The following anchors do not work
# 20 - Lifts head to 45d in prone position - gtogmd001 (ddigmd057)
# 40 - Sits in stable position, without support - gtogmd022 (ddigmd063)
#

# Fit the Rasch model
# STOP HERE --- Error: Inf estimates, due to sparse data ---
#
model <- fit_dmodel(varlist = list(adm = adm, items = items),
                    data = data,
                    name = paste(length(items), "0", sep = "_"),
                    transform = c(53.98, 3),
                    data_package = "", progress = TRUE)

# Store and reload model
path <- file.path("~/project/gsed/phase1/bsid", model$name)
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
