# Purify
# Creates model 202507/281_0_phase_1+2

path_start <- file.path(Sys.getenv("GSED_PHASE2"), "202507", "293_0_phase_1+2")
model_start <- readRDS(file.path(path_start, "model.Rds"))

# itemfit <- model_start$item_fit
# itemfit <- itemfit |>
#   filter(.data$infit > 1.2)
# remove_items <- itemfit$item

# Remove selected items with infit > 1.2 after review
# remove_for_infit <- c("gtofmd005",
#                       "gtolgd009",
#                       "gtogmd035",
#                       "gtofmd031",
#                       "gtofmd016",
#                       "gpasec082",
#                       "gtofmd041",
#                       "gtofmd029")
remove_for_infit <- c("sf_sec063",
                      "lfagmd047",
                      "lfblgd008",
                      "lfcfmd007",
                      "lfcfmd032",
                      "lfcfmd024",
                      "lfcfmd039",
                      "lfcfmd035")
# remove_for_dif <- c("gtofmd009",
#                     "gtolgd011",
#                     "gtolgd003",
#                     "gtolgd004")
remove_for_dif <- c("lfblgd001",
                    "lfblgd005",
                    "lfblgd014",
                    "lfcfmd010")
remove_items <- unique(c(remove_for_infit, remove_for_dif))
dscore::get_labels(remove_items)

# Remove visits with persons infit/outout value > 2.0
pfit <- model_start$person_fit |>
  dplyr::arrange(subjid, pair)
remove_visits <- pfit$infit > 2.0 | pfit$outfit > 2.0

# Edits
# remove_item_country <- data.frame(
#   item = c("gtogmd002", "gtogmd003", "gtolgd051", "gtolgd052"),
#   country = c("PAK", "PAK", "BRA", "BRA"))

source(system.file("scripts/phase2/models/fit_core_model.R", package = "gsedscripts"))
