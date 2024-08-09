# Merge LF, SF and BSID data so that each record corresponds to one child
#
# Input:  'work' (phase1 data, long form)
# Output: 'phase1'

#
#  Select LF, SF and BSID items from phase1 studies
#

adm <- c("ctrycd", "cohort", "cohortn", "subjid", "joinid", "agedays", "ins")
items <- c(get_itemnames(ins = "gpa", order = "indm"),
           get_itemnames(ins = "gto"),
           get_itemnames(ins = "by3"))
long <- work |>
  filter(adm == "fixed") |>
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohort = recode(cohort, "11-GSED" = "GSED-BGD", "17-GSED" = "GSED-PAK", "20-GSED" = "GSED-TZA"),
    ctrycd = recode(cohort, "GSED-BGD" = "BGD", "GSED-PAK" = "PAK", "GSED-TZA" = "TZA"),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    joinid = subjid * 100,
    across(any_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) |>
  drop_na(agedays) |>
  select(all_of(adm), any_of(items))

sf <- long |>
  filter(ins == "sf") |>
  select(all_of(adm), any_of(get_itemnames(ins = "gpa")))
lf <- long |>
  filter(ins == "lf") |>
  select(all_of(adm), any_of(get_itemnames(ins = "gto")))
bsid <- long |>
  filter(ins == "bsid") |>
  select(all_of(adm), any_of(get_itemnames(ins = "by3")))

#
#  Fuzzy join the LF, SF and BSID administration to one record
#

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

phase1 <- fuzzyjoin::difference_left_join(sf_first, lf_first,
                                          by = c("joinid", "agedays"),
                                          max_dist = 10, distance_col = "dist") |>
  ungroup() |>
  rename(ctrycd = ctrycd.x, cohort = cohort.x, cohortn = cohortn.x,
         subjid = subjid.x, agedays = agedays.x, joinid = joinid.x) |>
  mutate(age = agedays / 365.25) |>
  select(all_of(c("ctrycd", "cohort", "cohortn", "subjid", "joinid", "agedays", "ins.x", "ins.y")),
         any_of(items))
# phase1: 4374 records, 301 columns

phase1 <- fuzzyjoin::difference_left_join(phase1, bsid_first,
                                          by = c("joinid", "agedays"),
                                          max_dist = 10, distance_col = "dist") |>
  ungroup() |>
  rename(ctrycd = ctrycd.x, cohort = cohort.x, cohortn = cohortn.x,
         subjid = subjid.x, agedays = agedays.x, joinid = joinid.x) |>
  mutate(age = agedays / 365.25) |>
  select(c("ctrycd", "cohort", "cohortn", "subjid", "agedays"), any_of(items))


# Clean up
rm(adm, items, long, sf, lf, bsid, sf_first, lf_first, bsid_first, join_method)

