# This script plots the proportion pass by age for all items of the GSED LF.
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private)
# Inline R scripts: assemble_data.R
# 20220530SvB

# If needed, install dmetric from GitHub
if (!requireNamespace("dmetric", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dmetric")
}
if (packageVersion("dmetric") < "0.62.0") stop("Needs dmetric 0.62.0")

if (!requireNamespace("dscore", quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package dscore needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dscore")
}
if (packageVersion("dscore") < "1.4.4") stop("Needs dscore 1.4.4")

library("dmetric")
library("tidyr")

# get all data
suppressWarnings(source("scripts/assemble_data.R"))

# select instrument data and pre-process
items <- colnames(work)[starts_with("gto", vars = colnames(work))]
adm <- c("cohort", "subjid", "agedays", "country")
vars <- c(adm, items)
data <- work |>
  filter(ins == "lf") |>
  rename(
    subjid = gsed_id,
    agedays = age) |>
  mutate(
    cohort = strtrim(subjid, 7),
    country = strtrim(file, 3),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) |>
  select(all_of(vars))

# define data for rug plot
data_rug <- data |>
  pivot_longer(cols = starts_with("gto"), names_to = "item", values_to = "value",
               values_drop_na = TRUE) |>
  mutate(study = recode(country, "ban" = "BGD", "pak" = "PAK", "tza" = "TZA")) |>
  mutate(agemos = agedays / 365.25 * 12) |>
  select(item, value, agemos, study)

# permute rows in data_rug
idx <- sample(1:nrow(data_rug))
data_rug <- data_rug[idx, ]

# calculate summary statistics
pass <- data_rug |>
  mutate(agegp = cut(agemos, breaks = seq(0, 42, 1))) |>
  group_by(item, study, agegp) |>
  summarise(p = round(100 * mean(value, na.rm = TRUE)),
            a = mean(agemos, na.rm = TRUE),
            n = n()) |>
  ungroup() |>
  left_join(dscore::get_itemtable(), by = "item")

col_manual = c("BGD" = "#D93F46", "PAK" = "#489033", "TZA" = "#47A1D8")

# do it
theme_set(theme_light())
plots <- plot_p_a_item(pass = pass, data_rug = data_rug,
                       model_name = "none",
                       x.limits = c(0, 42),
                       x.breaks = seq(0, 42, 6),
                       col.manual = col_manual)

# save as pdf per instrument
path <- file.path("~/project/gsed/phase1/lf")
device <- "pdf"
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "gto_items_by_age.pdf")
  pdf(file, onefile = TRUE, width = 10, height = 5)
  lapply(plots, print)
  message("Saved to: ", file)
  dev.off()
}
