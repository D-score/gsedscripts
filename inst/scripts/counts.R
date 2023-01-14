# This script calculates basic data counts
# Phase 1 Validation, fixed data: GSED LF, GSED SF, BSID
#
# Dependencies
# Assumed environmental variable: ONEDRIVE_GSED
# Inline R scripts: gsedscripts package
#
# Nov 18 2022, SvB

require(fuzzyjoin)
require(dplyr)
fn <- system.file("scripts/assemble_data.R", package = "gsedscripts")
suppressWarnings(source(fn))

# tabulate fixed/adaptive for bsid/lf/sf
# with(work, table(ins, adm))

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
# Result: 6838 records, 626 columns

# number of duplo measurements
has_lf <- apply(!is.na(select(data, items[all_of(starts_with("gpa", vars = items))])), 1, any)
has_sf <- apply(!is.na(select(data, items[all_of(starts_with("gto", vars = items))])), 1, any)
has_bsid <- apply(!is.na(select(data, items[all_of(starts_with("by3", vars = items))])), 1, any)

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
