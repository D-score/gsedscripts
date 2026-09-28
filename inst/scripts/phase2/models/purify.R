# Purify
# Creates model 202510/281_0_phase_1+2
#
# Modified: 20251008 SvB

path_start <- file.path(Sys.getenv("GSED_PHASE2"), "202510", "293_0_phase_1+2")
model_start <- readRDS(file.path(path_start, "model.Rds"))

# Remove selected items with infit > 1.2 after review
remove_for_infit <- c(
  "gs1sec063",
  "gl1gmd047",
  "gl1lgd008",
  "gl1fmd007",
  "gl1fmd032",
  "gl1fmd024",
  "gl1fmd039",
  "gl1fmd035"
)
remove_for_dif <- c("gs1lgc004", "gl1lgd001", "gl1lgd005", "gl1lgd014")
remove_items <- unique(c(remove_for_infit, remove_for_dif))
dscore::get_labels(remove_items)

# Remove visits with persons infit/outout value > 2.0
pfit <- model_start$person_fit |>
  dplyr::arrange(subjid, pair)
remove_visits <- pfit$infit > 2.0 | pfit$outfit > 2.0

source(system.file(
  "scripts/phase2/models/fit_core_model.R",
  package = "gsedscripts"
))

# Extract the new key gsed2510
items_sf <- get_itemnames(ins = "gs1", order = "indm")
items_lf <- get_itemnames(ins = "gl1")
items_lf <- items_lf[c(55:155, 1:54)]
tau <- c(
  get_tau(items_lf, key = "", itembank = itembank),
  get_tau(items_sf, key = "", itembank = itembank)
)


key2510 <- data.frame(
  key = "gsed2510",
  item = names(tau),
  tau = round(unname(tau), 2)
)

# Write new key
write.table(
  key2510,
  file = "key2510.txt",
  quote = FALSE,
  sep = "\t",
  row.names = FALSE
)
