# Assemble relevant GSED validation data
# May/June 2022 SvB

if (packageVersion("gsedread") < "0.7.1") stop("Needs gsedread 0.7.1")
library(gsedread)
library(dplyr)
library(tidyr)
library(tibble)

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
cat("Total number of unique gsed_id's: ", length(unique(sf_a_wide$gsed_id)), "\n")

# count administrations with not-duplicate gsed_id
nodup <- sf_a %>%
  dplyr::group_by(gsed_id, item) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n == 1L)
cat("Total number of unique gsed_id's with one administration: ", length(unique(nodup$gsed_id)), "\n")

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
cat("Total number of unique gsed_id's: ", length(unique(lf_a_wide$gsed_id)), "\n")

# count administrations with not-duplicate gsed_id
nodup <- lf_a %>%
  dplyr::group_by(gsed_id, item) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n == 1L)
cat("Total number of unique gsed_id's with one administration: ", length(unique(nodup$gsed_id)), "\n")

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
  select(gsed_id, age, vist_type, ins, adm, file, parent_id:location,
         date, caregiver, ah01:m03, gtogmd001:gtofmd054) %>%
  as_tibble()
dim(lf)

# Add administrative variable to bsid data
bsid <- bsid %>%
  mutate(
    ins = "bsid",
    vist_type = 1L
  )


# bind LF, SF and BSID
data <- bind_rows(sf, lf, bsid) %>%
  arrange(gsed_id, age, vist_type)

dim(data)
with(data, table(ins, adm))
