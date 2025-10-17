# remove visits with infit or outfit > 3 based on model 293_0_phase_1+2
path_start <- path.expand(file.path(
  "~/project/gsed/phase2/models/293_0_phase_1+2"
))
model_start <- readRDS(file.path(path_start, "model.Rds"))
remove_visits <- model_start$person_fit$outfit > 3 |
  model_start$person_fit$infit > 3

# remove items
remove_items <- ""
# remove_items <- c("gpaclc088", "gtogmd038", "gtolgd009", "gtofmd005", "gtolgd012")
