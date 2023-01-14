# This script reads all SF, LF and BSID data from source and combines
# these into one dataset. Items conform to the gsed2 naming schema, which
# defines new 9-position names for SF and LF. Each row in the resulting
# dataset is a unique combination of four factors:
#   gsed_id - the child
#   age - age in days
#   ins - instrument (lf, sf, bsid)
#   worker_code - code of data collector
#
# Dependencies
#
# Assumed installed packages: remotes, dplyr, tidyr, tibble
# Assumed environmental variable: ONEDRIVE_GSED
#
# TODO:
# 1. Develop more subtle duplicate removal
# 2. Repair missing parent_id, worker_code, date, and so on
# 3. Check whether assumed vist_type is OK
# 4. Use tidyverse grammar more consistently
# 5. Expand to other input data (anthro, and so on)
# 6. Include Rapid Short Form data
# 7. Replace cat() by nicer testing statements, including a silencer
# 8. When everything is polished, turn into function
# None of these issues is critical for modeling, so let's use this as
# base data.
#
# May 29, 2022 SvB

# If needed, install gsedread from GitHub
pkg <- "gsedread"
if (!requireNamespace(pkg, quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package", pkg, "needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/gsedread")
}
if (packageVersion("gsedread") < "0.8.0") stop("Needs gsedread 0.8.0")

require(gsedread, quietly = TRUE, warn.conflicts = FALSE)
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(tidyr, quietly = TRUE, warn.conflicts = FALSE)
require(tibble, quietly = TRUE, warn.conflicts = FALSE)

# read data
sf_f <- read_sf("fixed") %>% mutate(adm = "fixed")
lf_f <- read_lf("fixed")
bsid <- read_bsid()
sf_a <- read_sf("adaptive")
lf_a <- read_lf("adaptive")

# rename items into gsed2 lexicon
colnames(sf_f) <- rename_vector(colnames(sf_f), trim = "Ma_SF_")
colnames(lf_f) <- rename_vector(colnames(lf_f), trim = "Ma_LF_")
colnames(sf_a) <- rename_vector(colnames(sf_a), trim = "Ma_SF_")
colnames(lf_a) <- rename_vector(colnames(lf_a), trim = "Ma_LF_")
sf_a$item <- rename_vector(sf_a$item, "gsed", "gsed2", contains = "Ma_SF_")
lf_a$item <- rename_vector(lf_a$item, "gsed", "gsed2", contains = "Ma_LF_")
colnames(bsid) <- rename_vector(colnames(bsid), contains = "bsid_")

# -- transform adaptive SF into wide format

# Rough duplicate removal
# Most duplicate administrations occur in different files, so then it is
# a matter of simply deleting one of the administration.
# Here, we do a more rough approach that removes all gsed_id's with two
# administrations.
sf_a_wide <- sf_a %>%
  pivot_wider(id_cols = gsed_id, names_from = item, values_from = scores)
# cat("Total number of unique gsed_id's: ", length(unique(sf_a_wide$gsed_id)), "\n")

# count administrations with not-duplicate gsed_id
nodup <- sf_a %>%
  dplyr::group_by(gsed_id, item) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n == 1L)
# cat("Total number of unique gsed_id's with one administration: ", length(unique(nodup$gsed_id)), "\n")

gsed_id_nodup <- unique(nodup$gsed_id)
gsed_id_nodup <- setdiff(gsed_id_nodup, c("20-GSED-0816", "17-GSED-1402"))
idx <- sf_a$gsed_id %in% gsed_id_nodup
sf_a_wide <- sf_a[idx, ] %>%
  mutate(age = as.double(dov - dob)) %>%
  pivot_wider(id_cols = c(file, gsed_id, location, age),
              names_from = item, values_from = scores) %>%
  mutate(vist_type = 8L,
         ins = "sf",
         adm = "adaptive")

# -- bind SF fixed and SF adaptive wide, remove duplicates
sf <- bind_rows(sf_f, sf_a_wide) %>%
  distinct(gsed_id, age, vist_type, .keep_all = TRUE) %>%
  mutate(ins = "sf") %>%
  arrange(gsed_id, age, vist_type) %>%
  select(gsed_id, age, vist_type, ins, adm, file, parent_id:location,
         date, caregiver, ah01:m03, gpalac001:gpaclc139) %>%
  as_tibble()
dim(sf)

# -- transform adaptive LF into wide format
lf_a_wide <- lf_a %>%
  pivot_wider(id_cols = gsed_id, names_from = item, values_from = scores)
# cat("Total number of unique gsed_id's: ", length(unique(lf_a_wide$gsed_id)), "\n")

# count administrations with not-duplicate gsed_id
nodup <- lf_a %>%
  dplyr::group_by(gsed_id, item) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n == 1L)
# cat("Total number of unique gsed_id's with one administration: ", length(unique(nodup$gsed_id)), "\n")

gsed_id_nodup <- unique(nodup$gsed_id)
# gsed_id_nodup <- setdiff(gsed_id_nodup, c("20-GSED-0816", "17-GSED-1402"))
idx <- lf_a$gsed_id %in% gsed_id_nodup
lf_a_wide <- lf_a[idx, ] %>%
  mutate(age = as.double(dov - dob)) %>%
  pivot_wider(id_cols = c(file, gsed_id, location, age, adm),
              names_from = item, values_from = scores) %>%
  mutate(vist_type = 8L,
         ins = "lf")

# -- bind LF fixed and LF adaptive wide, remove duplicates
lf <- bind_rows(lf_f, lf_a_wide) %>%
  distinct(gsed_id, age, vist_type, .keep_all = TRUE) %>%
  mutate(ins = "lf") %>%
  arrange(gsed_id, age, vist_type) %>%
  dplyr::select(gsed_id, age, vist_type, ins, adm, file, parent_id:location,
                date, caregiver, ah01:m03, gtogmd001:gtofmd054) %>%
  as_tibble()
dim(lf)

# Add administrative variable to bsid data
bsid <- bsid %>%
  mutate(
    ins = "bsid",
    vist_type = 1L
  )

# bind LF, SF and BSID, add number of responses per instrument
work <- bind_rows(sf, lf, bsid) %>%
  arrange(gsed_id, age, ins, worker_code) %>%
  mutate(n_sf = rowSums(!is.na(across(starts_with("gpa")))),
         n_lf = rowSums(!is.na(across(starts_with("gto")))),
         n_bsid = rowSums(!is.na(across(starts_with("by3"))))) %>%
  dplyr::select(gsed_id, age, ins, worker_code, adm, vist_type, n_sf:n_bsid,
                file:caregiver, age_adj_premature, ah01:m03,
                gpalac001:by3gmd072)

# check whether combination gsed_id, age, ins, worker_code is unique
uni <- work %>%
  group_by(gsed_id, age, ins, worker_code) %>%
  summarise(n_gp = dplyr::n(), .groups = "drop") %>%
  filter(n_gp == 1L)
cat("Number of rows in work    ", nrow(work), "\n")
cat("Number of unique records  ", nrow(uni), "\n")

# clean up
rm(list = setdiff(ls(), "work"))
cat("The combined data are in object `work`.\n")
