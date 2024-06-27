# This script calculates basic data counts
# Phase 1 Validation, fixed data: GSED LF, GSED SF, BSID
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Inline R scripts: gsedscripts package
#
# Nov 18 2022, SvB

require(fuzzyjoin, quietly = TRUE, warn.conflicts = FALSE)
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
fn <- system.file("scripts/assemble_data.R", package = "gsedscripts")
suppressWarnings(source(fn))

# tabulate fixed/adaptive for bsid/lf/sf
# with(work, table(ins, adm))

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

# Fuzzy match on gsed_id and agedays
# Double fuzzy match, lf, sf, bsid, keep all records
sf <- long |>
  filter(ins == "sf") |>
  select(all_of(adm), items[all_of(starts_with("gpa", vars = items))])
lf <- long |>
  filter(ins == "lf") |>
  select(all_of(adm), items[all_of(starts_with("gto", vars = items))])
bsid <- long |>
  filter(ins == "bsid") |>
  select(all_of(adm), items[all_of(starts_with("by3", vars = items))])

join_method <- "greedy_4"
join_method <- "onematch_10"

if (join_method == "greedy_4") {
  joined <- fuzzyjoin::difference_full_join(sf, lf, by = c("joinid", "agedays"),
                                            max_dist = 4, distance_col = "dist") |>
    mutate(
      cohort = ifelse(is.na(cohort.x), cohort.y, cohort.x),
      cohortn = ifelse(is.na(cohortn.x), cohortn.y, cohortn.x),
      subjid = ifelse(is.na(subjid.x), subjid.y, subjid.x),
      joinid = ifelse(is.na(joinid.x), joinid.y, joinid.x),
      agedays = ifelse(is.na(agedays.x), agedays.y, agedays.x),
      ins = ifelse(is.na(ins.x), ins.y, ins.x),
    ) |>
    select(all_of(adm), any_of(items))
  data <- fuzzyjoin::difference_full_join(joined, bsid, by = c("joinid", "agedays"),
                                          max_dist = 4, distance_col = "dist") |>
    mutate(
      cohort = ifelse(is.na(cohort.x), cohort.y, cohort.x),
      cohortn = ifelse(is.na(cohortn.x), cohortn.y, cohortn.x),
      subjid = ifelse(is.na(subjid.x), subjid.y, subjid.x),
      joinid = ifelse(is.na(joinid.x), joinid.y, joinid.x),
      agedays = ifelse(is.na(agedays.x), agedays.y, agedays.x),
      ins = ifelse(is.na(ins.x), ins.y, ins.x),
    ) |>
    select(all_of(adm), any_of(items))
  # Result: 6838 records, 626 columns
}

if (join_method == "onematch_10") {
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
}

# number of duplo measurements
has_lf <- apply(!is.na(select(data, items[all_of(starts_with("gpa", vars = items))])), 1, any)
has_sf <- apply(!is.na(select(data, items[all_of(starts_with("gto", vars = items))])), 1, any)
has_bsid <- apply(!is.na(select(data, items[all_of(starts_with("by3", vars = items))])), 1, any)

lf_items <- dscore::get_itemnames(data, instrument = "gto")
sf_items <- dscore::get_itemnames(data, instrument = "gpa")
bsid_items <- dscore::get_itemnames(data, instrument = "by3")

n_scores <- sum(!is.na(data[, lf_items]))
cat("Average number of items LF:      ", n_scores/sum(has_lf), "\n")
n_scores <- sum(!is.na(data[, sf_items]))
cat("Average number of items SF:      ", n_scores/sum(has_sf), "\n")
n_scores <- sum(!is.na(data[, bsid_items]))
cat("Average number of items BSID:    ", n_scores/sum(has_bsid), "\n")

n_children <- length(unique(data$subjid))
n_dup <- sum(duplicated(long[, c("subjid", "agedays")]))
n_visits1 <- nrow(long) - n_dup
n_visits5 <- nrow(data)
n_scores <- sum(!is.na(data[, items]))
n_items <- length(items)
instruments <- unique(dscore::decompose_itemnames(items)$instrument)
n_instruments <- length(instruments)
n_mean_items <- n_scores/n_visits1
n_studies <- length(unique(data$cohort))
ctrycd <- substr(data$cohort, 6, 8)
n_countries <- length(unique(ctrycd))
n_duplo <- sum(has_lf & has_sf)

cat("Number of children:     ", n_children, "\n")
cat("Number of items:        ", n_items, "\n")
cat("Number of visits:       ", n_visits1, "\n")
cat("Number of visits (> 4d):", n_visits5, "\n")
cat("Number of responses:    ", n_scores, "\n")
cat("Number of instruments:  ", n_instruments, "\n")
cat("Items per visit (mean): ", n_mean_items, "\n")
cat("Number of studies:      ", n_studies, "\n")
cat("Number of countries:    ", n_countries, "\n")
cat("Number of duplo tests:  ", n_duplo, "\n")

cat("Instruments: \n")
cat(instruments, "\n")

cat("Studies: \n")
cat(unique(data$cohort), "\n")

cat("Countries: \n")
cat(unique(ctrycd), "\n")

