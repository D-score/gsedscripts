# Builds database with item responses for GSED Phase 1 and 2 validation data
# Fixed administration only
#
# Assumed environmental variables, specified in .Renviron:
# - ONEDRIVE_GSED
#   Must point to 'CAVALLERA, Vanessa - GSED Validation 2021_phase I'
# - GSED_PHASE2
#   Must point to a local directory where the PHASE2 analysis results will
#   be written. This script will create a file called 'data/fixed.duckdb'
#   in this directory.
#
# Created: Stef van Buuren, June 20, 2025
# Last modified: Oct 1, 2025
#
# TODO:
# - Add covariate data (e.g. child factors, antropometry)

if (nchar(Sys.getenv("ONEDRIVE_GSED")) == 0L) {
  stop("Environmental variable ONEDRIVE_GSED not set.", call. = FALSE)
}
if (nchar(Sys.getenv("GSED_PHASE2")) == 0L) {
  stop("Environmental variable GSED_PHASE2 not set.", call. = FALSE)
}

# Load required CRAN packages
require(duckdb, quietly = TRUE, warn.conflicts = FALSE)
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(tidyr, quietly = TRUE, warn.conflicts = FALSE)
require(stringr, quietly = TRUE, warn.conflicts = FALSE)

# Load dedicated package for reading GSED data
pkg <- "gsedread"
if (!requireNamespace(pkg, quietly = TRUE) && interactive()) {
  answer <- askYesNo(paste("Package", pkg, "needed. Install from GitHub?"))
  if (answer) remotes::install_github("d-score/gsedread")
}
require(gsedread, quietly = TRUE, warn.conflicts = FALSE)
if (packageVersion("gsedread") < "0.23.0") stop("Needs gsedread 0.23.0")

# Set paths and filenames
onedrive <- Sys.getenv("ONEDRIVE_GSED")
path_phase1 <- file.path("GSED Phase 1 Final Analysis",
                         "GSED Final Collated Phase 1 Data Files 18_05_22")
path_phase2 <- "GSED Final Collated Phase 2 Files 02_06_25/Temp Clean LF_ SF_ to add once China data is cleaned" # temporary
if (!dir.exists(file.path(Sys.getenv("GSED_PHASE2"), "data"))) {
  dir.create(file.path(Sys.getenv("GSED_PHASE2"), "data"), recursive = TRUE)
}
output_fixed <- file.path(Sys.getenv("GSED_PHASE2"), "data/fixed.duckdb")

# Read Phase 1 and Phase 2 data
phase1 <- read_gsed_fixed(onedrive = onedrive, path = path_phase1, phase = 1, hard_edits = TRUE)
phase2 <- read_gsed_fixed(onedrive = onedrive, path = path_phase2, phase = 2, hard_edits = TRUE)

# Combine Phase 1 and Phase 2 data
responses <- bind_rows(phase1$responses, phase2$responses)
visits <- bind_rows(
  phase1$visits |> mutate(phase = 1L),
  phase2$visits |> mutate(phase = 2L)) |>
  select(subjid, agedays, vist_type, phase, date, ins, adm,
         file, parent_id, worker_code, location, caregiver,
         agedays_adj_premature, everything())

# Repair visits and responses
visits <- repair_visits(visits)
responses <- repair_responses(responses)

#--- Write to DuckDB ---
message("Writing to DuckDB...")
if (file.exists(output_fixed)) {
  message("Removing existing database: ", output_fixed)
  file.remove(output_fixed)
}
con <- dbConnect(duckdb(), dbdir = output_fixed, read_only = FALSE)

dbWriteTable(con, "responses", responses, overwrite = TRUE)
dbWriteTable(con, "visits", visits,  overwrite = TRUE)

dbDisconnect(con)
message("Database written to: ", output_fixed)
