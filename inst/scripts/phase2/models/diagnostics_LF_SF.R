# This script produces diagnostic plots and tables for LF versus SF
# for the current model (gsed2406) and a newly fitted model.
#
# Dependencies:
# + Environmental variable "GSED_PHASE2" must be set to the local directory
#   containing the models for phase 2
#
# Prerequisites: Run the model first, or use a published key
# Uses gsed2 lexicon
#
# Created   20250722 SvB
# Modified  20250724 SvB

if (nchar(Sys.getenv("GSED_PHASE2")) == 0L) {
  stop("Environmental variable MODELS_PHASE2 not set.", call. = FALSE)
}

# --- GLOBAL SCRIPT VARIABLES
# Set defaults if not specified in the caller script

if (!exists("phases")) {
  phases <- c(1, 2)
}
if (!exists("cohorts")) {
  cohorts <- c("GSED-BGD", "GSED-BRA", "GSED-CHN",
               "GSED-CIV", "GSED-NLD", "GSED-PAK", "GSED-TZA")
}
if (!exists("instruments")) {
  instruments <- c("sf", "lf", "bsid")
}
if (!exists("remove_visits")) {
  remove_visits <- NULL
}
if (!exists("remove_items")) {
  remove_items <- ""
}

if (!exists("remove_item_country")) {
  remove_item_country <- data.frame(item = NULL, country = NULL,
                                    stringsAsFactors = FALSE)
}

# --- END GLOBAL SCRIPT VARIABLES

# Set the itembank
model_name <- "281_0_phase_1+2"
model_name <- "285_0_phase_1+2"  # includes item (8) and person removal (2.0)
# model_name <- "412_0_phase_1+2"
path_new <- file.path(Sys.getenv("GSED_PHASE2"), "202507", model_name)
model <- readRDS(file = file.path(path_new, "model.Rds"))

itembank <- model$itembank
key <- model$name

# Load GitHub packages
pkg <- "dfine"
if (!requireNamespace(pkg, quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package", pkg, "needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/dfine")
}
require("dfine", quietly = TRUE, warn.conflicts = FALSE)
if (packageVersion("dfine") < "0.11.0") stop("Needs dfine 0.11.0")

# Load CRAN packages
library("DBI", quietly = TRUE, warn.conflicts = FALSE)
library("duckdb", quietly = TRUE, warn.conflicts = FALSE)
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("dscore")
library("tidyr", quietly = TRUE, warn.conflicts = FALSE)
library("ggplot2", quietly = TRUE, warn.conflicts = FALSE)

#
#  A.  Read fixed form Phase 1 & 2 responses and visits
#

dbfile <- file.path(Sys.getenv("GSED_PHASE2"), "data/fixed.duckdb")
con <- dbConnect(duckdb(), dbdir = dbfile)
dbListTables(con)
visits <- dbReadTable(con, "visits")
responses <- dbReadTable(con, "responses")
dbDisconnect(con)

#
#  B.  Subset to phase 1&2 LF and SF data
#      Ignore any inter-rater scores (vist_type 5) to prevent duplicate matches
#

visits <- visits |>
  filter(ins %in% instruments & (is.na(vist_type) | vist_type != 5L))
responses <- semi_join(
  responses,
  visits,
  by = c("subjid", "agedays", "vist_type"))

#
# C. Add country and cohort fields to responses
#

responses <- responses |>
  left_join(
    visits |>
      distinct(cohort, subjid, agedays, vist_type) |>
      mutate(country = substr(cohort, 6, 8)),
    by = c("subjid", "agedays", "vist_type")) |>
  select(cohort, country, subjid, agedays, vist_type, item, response)

#
#  D. Identify pairs of LF-SF records occurring within four days
#

# remove duplicates to prevent double pairing
# remove records with missing agedays
#
# TODO: this is a temporary fix
# - in each of these we need to find and remove one record
# - restore agedays where possible
responses <- responses |>
  filter(!(subjid %in% c("076-GSED-0528", "076-GSED-0905", "076-GSED-0905",
                         "076-GSED-1322",
                         "384-GSED-1160", "384-GSED-1323",
                         "528-GSED-0581"))) |>
  filter(!is.na(agedays))

visits <- visits |>
  select(subjid, agedays, ins, vist_type) |>
  arrange(subjid, agedays, ins, vist_type) |>
  group_by(subjid) |>
  mutate(sf_order = if_else(ins == "sf", row_number(), NA_integer_)) |>
  mutate(sf_order = if_else(ins == "sf", cumsum(ins == "sf"), NA_integer_)) |>
  mutate(lf_order = if_else(ins == "lf", row_number(), NA_integer_)) |>
  mutate(lf_order = if_else(ins == "lf", cumsum(ins == "lf"), NA_integer_)) |>
  ungroup()

# Find pair number (0, 1, 2, 3) for each SF record
sf_rows <- visits |>
  filter(ins == "sf") |>
  select(subjid, sf_agedays = agedays, sf_order)
lf_rows <- visits |>
  filter(ins == "lf") |>
  select(subjid, lf_agedays = agedays, lf_order)
pairs <- sf_rows |>
  left_join(lf_rows, by = "subjid", relationship = "many-to-many") |>
  mutate(diff = abs(sf_agedays - lf_agedays)) |>
  group_by(subjid, sf_order) |>
  slice_min(order_by = diff, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(pair = ifelse(diff > 4L | is.na(diff), -sf_order, sf_order))

# Merge pair number with with response
items_sf <- get_itemnames(ins = "sf_", order = "imnd")
items_lf <- get_itemnames(ins = c("lfa", "lfb", "lfc"))
items_bsid <- get_itemnames(ins = "by3")

responses <- responses |>
  filter(item %in% c(items_sf, items_lf, items_bsid))
responses_sf <- responses |>
  filter(item %in% items_sf) |>
  left_join(pairs, by = join_by(subjid, agedays == sf_agedays)) |>
  mutate(ins = "sf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)
responses_lf <- responses |>
  filter(item %in% items_lf) |>
  left_join(pairs |> filter(pair > 0),
            by = join_by(subjid, agedays == lf_agedays)) |>
  mutate(pair = ifelse(is.na(pair), -agedays, pair),
         ins = "lf") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)
responses_bsid <- responses |>
  filter(item %in% items_bsid) |>
  mutate(pair = NA_integer_,
         ins = "bsid") |>
  select(cohort, country, subjid, agedays, pair, ins, item, response)

# Check for zero duplicate matches
nrow(responses_sf) + nrow(responses_lf) + nrow(responses_bsid) - nrow(responses) == 0

# Recreate with the new pair number
responses <- bind_rows(responses_sf, responses_lf, responses_bsid)

# > tail(table(responses$pair, useNA = "al"), 8)
#
# -17     -6     -2     -1      1      2      3   <NA>
#  27     21   2018   7583 691786 163347   8613  96203
#
# < -5: no match for LF
# -2, -1: no match for SF
# 1, 2, 3: matched SF and LF
# <NA>: BSID items

responses <- responses |>
  distinct(cohort, country, subjid, agedays, pair, ins, item, response)


#
#  F. Create wide format that puts items from a SF-LF pair on the same row
#

data <- responses |>
  select(subjid, pair, item, response) |>
  pivot_wider(names_from = c(item), values_from = response,
              id_cols = c(subjid, pair)) |>
  arrange(subjid, pair)

# duplicates
# anomalies <- responses |>
#   dplyr::summarise(n = dplyr::n(), .by = c(subjid, pair, item)) |>
#   dplyr::filter(n > 1L)

agedays_info <- responses |>
  distinct(subjid, pair, ins, agedays) |>
  pivot_wider(names_from = ins, values_from = agedays,
              names_prefix = "agedays_")
data <- data |>
  left_join(agedays_info, by = c("subjid", "pair")) |>
  mutate(agedays = rowMeans(across(c(agedays_sf, agedays_lf)), na.rm = TRUE)) |>
  select(subjid, pair, agedays, any_of(items_sf), any_of(items_lf), any_of(items_bsid))

cat("dim(data):", dim(data), "\n")
# wide: 12252 visits (measurements), 622 columns (2 + 1 + 138 + 155 + 326)
# wide: 9283 unique subjid (=children)

#
#  G1. Calculate D-score for all SF + LF items combined
#      using the gsed2406 key and preliminary_standards population
#

colnames(data) <- dscore::rename_vector(colnames(data),
                                        lexin = "gsed4", lexout = "gsed3")
itembank$item <- dscore::rename_vector(itembank$item,
                                       lexin = "gsed4", lexout = "gsed3")
items_sf <- rename_vector(items_sf, lexin = "gsed4", lexout = "gsed3")
items_lf <- rename_vector(items_lf, lexin = "gsed4", lexout = "gsed3")

d_sf <- dscore(data = data, items = items_sf,
               xname = "agedays", xunit = "days",
               key = "gsed2406", population = "preliminary_standards",
               metric = "dscore")
d_lf <- dscore(data = data, items = items_lf,
               xname = "agedays", xunit = "days",
               key = "gsed2406", population = "preliminary_standards",
               metric = "dscore")

joined <- bind_cols(
  select(data, subjid, pair, agedays),
  select(d_sf, a, n, p, d, sem, daz) |>
    rename(n_sf = n, p_sf = p, d_sf = d, sem_sf = sem, daz_sf = daz),
  select(d_lf, n, p, d, sem, daz) |>
    rename(n_lf = n, p_lf = p, d_lf = d, sem_f = sem, daz_lf = daz)) |>
  mutate(
    cohort = calculate_cohort(subjid),
    d_mean = (d_lf + d_sf) / 2,
    d_diff = (d_lf - d_sf),
    daz_mean = (daz_lf + daz_sf) / 2,
    daz_diff = (daz_lf - daz_sf)
  )

# summary over subjid-pair units
summary_gsed2406 <- joined |>
  group_by(cohort) |>
  summarise(
    n = n(),
    agedays = mean(agedays, na.rm = TRUE),
    d_lf = mean(d_lf, na.rm = TRUE),
    d_sf = mean(d_sf, na.rm = TRUE),
    d_diff = mean(d_diff, na.rm = TRUE),
    daz_lf = mean(daz_lf, na.rm = TRUE),
    daz_sf = mean(daz_sf, na.rm = TRUE),
    daz_diff = mean(daz_diff, na.rm = TRUE)) |>
  ungroup()

plot(joined$d_sf, joined$d_lf, cex = 0.5, main = "gsed2406", xlim = c(0, 90), ylim = c(0, 90))
abline(0, 1, col = "red", lwd = 2)
cor(joined$d_sf, joined$d_lf, use = "pair")
cor(joined$daz_sf, joined$daz_lf, use = "pair")

d_sf <- dscore(data = data, items = items_sf,
               itembank = itembank,
               key = key,
               xname = "agedays", xunit = "days",
               population = "preliminary_standards",
               metric = "dscore")
d_lf <- dscore(data = data, items = items_lf,
               itembank = itembank,
               key = key,
               xname = "agedays", xunit = "days",
               population = "preliminary_standards",
               metric = "dscore")

joined <- bind_cols(
  select(data, subjid, pair, agedays),
  select(d_sf, a, n, p, d, sem, daz) |>
    rename(n_sf = n, p_sf = p, d_sf = d, sem_sf = sem, daz_sf = daz),
  select(d_lf, n, p, d, sem, daz) |>
    rename(n_lf = n, p_lf = p, d_lf = d, sem_f = sem, daz_lf = daz)) |>
  mutate(
    cohort = calculate_cohort(subjid),
    d_mean = (d_lf + d_sf) / 2,
    d_diff = (d_lf - d_sf),
    daz_mean = (daz_lf + daz_sf) / 2,
    daz_diff = (daz_lf - daz_sf)
  )

# summary over subjid-pair units
summary_new_0 <- joined |>
  group_by(cohort) |>
  summarise(
    n = n(),
    agedays = mean(agedays, na.rm = TRUE),
    d_lf = mean(d_lf, na.rm = TRUE),
    d_sf = mean(d_sf, na.rm = TRUE),
    d_diff = mean(d_diff, na.rm = TRUE),
    daz_lf = mean(daz_lf, na.rm = TRUE),
    daz_sf = mean(daz_sf, na.rm = TRUE),
    daz_diff = mean(daz_diff, na.rm = TRUE)) |>
  ungroup()

summary_gsed2406
summary_new_0

plot(joined$d_sf, joined$d_lf, cex = 0.5, main = key, xlim = c(0, 90), ylim = c(0, 90))
abline(0, 1, col = "red", lwd = 2)
cor(joined$d_sf, joined$d_lf, use = "pair")
cor(joined$daz_sf, joined$daz_lf, use = "pair")

# create Bland-Altman D
col_manual <- dfine::get_palette("cohort")
theme_set(theme_light())

plot1 <- ggplot(joined, aes(x = d_lf, y = d_sf, group = cohort, colour = cohort)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("LF (D-score)", limits = c(10, 80)) +
  scale_y_continuous("SF (D-score)", limits = c(10, 80)) +
  coord_fixed() +
  geom_abline(slope = 1, colour = "grey", linewidth = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$cohort, ncol = 3) +
  theme(legend.position = "none")
plot1

plot2 <- ggplot(joined, aes(x = d_mean, y = d_diff, group = cohort, colour = cohort)) +
  scale_colour_manual(values = col_manual,
                      na.value = "grey") +
  scale_x_continuous("(LF + SF) / 2",
                     limits = c(10, 80)) +
  scale_y_continuous(
    paste0("LF - SF"),
    breaks = seq(-10, 10, 2),
    limits = c(-10, +10)
  ) +
  geom_hline(yintercept = c(0), colour = "grey", linewidth = 1.5) +
  geom_smooth(se = TRUE, colour = "grey60", linewidth = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$cohort, ncol = 3) +
  theme(legend.position = "none")
plot2

plot3 <- ggplot(joined, aes(x = a, y = daz_lf, group = cohort, colour = cohort)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("Age (years)", limits = c(0, 3.5)) +
  scale_y_continuous("LF (DAZ preliminary_standards)", limits = c(-5, 5)) +
  geom_hline(yintercept = c(-2, 0, 2), colour = "grey", linewidth = 1.5) +
  geom_smooth(se = TRUE, linewidth = 1) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$cohort, ncol = 3) +
  theme(legend.position = "none")
plot3

plot4 <- ggplot(joined, aes(x = a, y = daz_sf, group = cohort, colour = cohort)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("Age (years)", limits = c(0, 3.5)) +
  scale_y_continuous("SF (DAZ preliminary_standards)", limits = c(-5, 5)) +
  geom_hline(yintercept = c(-2, 0, 2), colour = "grey", linewidth = 1.5) +
  geom_smooth(se = TRUE, linewidth = 1) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$cohort, ncol = 3) +
  theme(legend.position = "none")
plot4


plot5 <- ggplot(joined, aes(x = daz_lf, y = daz_sf, group = cohort, colour = cohort)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("LF (DAZ preliminary_standards)", limits = c(-5, 5)) +
  scale_y_continuous("SF (DAZ preliminary_standards)", limits = c(-5, 5)) +
  coord_fixed() +
  geom_abline(slope = 1, colour = "grey", linewidth = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$cohort, ncol = 3) +
  theme(legend.position = "none")
plot5

plot6 <- ggplot(joined, aes(x = daz_mean, y = daz_diff, group = cohort, colour = cohort)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("(LF + SF) / 2 (DAZ preliminary_standards)", limits = c(-4, 4)) +
  scale_y_continuous("LF - SF (DAZ preliminary_standards)", limits = c(-4, +4)) +
  coord_fixed() +
  geom_hline(yintercept = c(0), colour = "grey", linewidth = 1.5) +
  geom_smooth(se = TRUE, colour = "grey60", linewidth = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$cohort, ncol = 3) +
  theme(legend.position = "none")
plot6

dec <- decompose_itemnames(model$item_fit$item)
dec$ins <- ifelse(dec$instrument == "sf_", "SF", "LF")
itemfit <- bind_cols(model$item_fit, dec)
plot7 <- ggplot(itemfit, aes(x = ins, y = infit, colour = domain)) +
  scale_y_continuous("Infit", limits = c(0, 2)) +
  scale_x_discrete("Instrument") +
  geom_hline(yintercept = 1, colour = "grey", linewidth = 1) +
  geom_boxplot()
plot7

plot8 <- ggplot(itemfit, aes(x = ins, y = outfit, colour = domain)) +
  scale_y_continuous("Outfit", limits = c(0, 2)) +
  scale_x_discrete("Instrument") +
  geom_hline(yintercept = 1, colour = "grey", linewidth = 1) +
  geom_boxplot()
plot8


plots <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)

# save as one pdf with all variations
device <- "pdf"
if (!is.null(file) & device == "pdf") {
  file <- file.path(path_new, paste0("diagnostics.pdf"))
  pdf(file, width = 20, height = 12.5)
  lapply(plots, print)
  message("Saved to: ", file)
  dev.off()
}

# save diagnostic plots as separate png files
device <- "png"
if (!is.null(file) & device == "png") {
  dir.create(path_new, showWarnings = FALSE, recursive = TRUE)
  for (i in seq_along(plots)) {
    file <- file.path(path_new, paste0("diagnostics_", i, ".png"))
    png(file, width = 5000, height = 5000/1.6, res = 300)
    print(plots[[i]])
    dev.off()
    message("Saved to: ", file)
  }
}
