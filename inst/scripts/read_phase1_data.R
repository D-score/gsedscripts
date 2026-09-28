library(dplyr)
library(tidyr)
library(gsedscripts)

# read Phase 1 data
Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
fn <- system.file("scripts/assemble_data.R", package = "gsedscripts")
suppressWarnings(source(fn))

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
sf <- long %>%
  filter(ins == "sf") %>%
  select(all_of(adm), items[all_of(starts_with("gpa", vars = items))])
lf <- long %>%
  filter(ins == "lf") %>%
  select(all_of(adm), items[all_of(starts_with("gto", vars = items))])
bsid <- long %>%
  filter(ins == "bsid") %>%
  select(all_of(adm), items[all_of(starts_with("by3", vars = items))])
