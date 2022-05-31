# This script plots the proportion pass by age for all items of the GSED SF.
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Non-standard packages: dmetric (private)
# Inline R scripts: assemble_data.R
# 20220531SvB

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
items <- colnames(work)[starts_with("by3", vars = colnames(work))]
adm <- c("cohort", "subjid", "agedays", "country", "study")
vars <- c(adm, items)
data <- work %>%
  filter(ins == "bsid") %>%
  rename(
    subjid = gsed_id,
    agedays = age) %>%
  mutate(
    cohort = strtrim(subjid, 7),
    country = strtrim(file, 3),
    study = recode(country, "ban" = "BGD", "pak" = "PAK", "tza" = "TZA"),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) %>%
  select(all_of(vars))

col_manual = c("BGD" = "#D93F46", "PAK" = "#489033", "TZA" = "#47A1D8")

ds <- dscore::dscore(data = data, items = items,
                     xname = "agedays", xunit = "days", key = "gsed")
ds <- bind_cols(data[, adm], ds)

reference <- dscore::get_reference(population = "gcdg") %>%
  mutate(month = .data$age * 12) %>%
  select(.data$month, .data$SDM2:.data$SDP2) %>%
  filter(.data$month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -.data$month)

theme_set(theme_light())
plot <-
  ggplot(reference, aes(x = .data$month, y = .data$d, group = .data$centile)) +
  scale_colour_manual(values = col_manual,
                      na.value = "grey") +
  scale_x_continuous("Age (in months)",
                     limits = c(0, 42),
                     breaks = seq(0, 42, 7)) +
  scale_y_continuous(
    paste0("D-score (807_17)"),
    breaks = seq(0, 80, 20),
    limits = c(0, 80)
  ) +
  geom_line(colour = "lightblue") +
  geom_point(
    mapping = aes(
      x = .data$a *12,
      y = .data$d,
      group = .data$study,
      colour = .data$study
    ),
    data = ds,
    size = 0.5,
    shape = 1
  ) +
  facet_wrap( ~ .data$study, ncol = 4) +
  theme(legend.position = "none")


# save as pdf per instrument
path <- file.path("~/project/gsed/phase1/bsid")
device <- "pdf"
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "d_a_study.pdf")
  pdf(file, onefile = TRUE, width = 12, height = 4)
  print(plot)
  message("Saved to: ", file)
  dev.off()
}
