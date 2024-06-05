# Recreate model 807_17
# JUNE 2022
#
# Starting from 881_17, removes items with infit/oufit > 1
# but keeps items from CREDI and IYCD with more relaxed
# criteria

library("dmetric")
library("gseddata")
library("dplyr")
library("dscore")
packageVersion("dmetric")
packageVersion("gseddata")
packageVersion("dscore")
theme_set(theme_light())

old_model <- load_model("881_17_eqs")
old_items <- old_model$item_fit$item
equatelist <- old_model$fit$equate

# keep items that have infit <= 1 and outfit <= 1
infit <- old_model$item_fit$infit
outfit <- old_model$item_fit$outfit
items <- old_items[infit <= 1 & outfit <= 1]

# the next items are added because these have more relaxed
# inclusion criteria. See 619_17.R
items_extra <- c("croclc006", "croclc045", "croclc048", "cromoc002",
                 "cromoc005", "cromoc007", "cromoc031", "cromoc033",
                 "cromoc035", "cromoc039", "crosec003", "crosec049",
                 "ddicmm030", "ddicmm031", "ddifmd002", "ddifmm004",
                 "denfmd024", "denlgd033", "denlgd036",
                 "iyolgc030", "iyomoc037")
items <- sort_itemnames(union(items, items_extra))

# put back any items in active equates
add_these <- setdiff(unlist(equatelist), items)
items <- sort_itemnames(unique(c(items, add_these)))

# get data
varlist <- list(adm = c("subjid", "agedays", "cohort", "cohortn", "subjido"),
                items = items)
data <- get_data(items = varlist$items, adm = varlist$adm)
data <- clean_data(data)

# keep persons with infit <= 3 and outfit <= 3
# pre-selected by old_model
old_model2 <- load_model("598_17_64951_fixed")
keep_person <- old_model2$person_fit |>
  mutate(keep = TRUE) |>
  select(subjid, agedays, keep)
data$visit <- left_join(data$visit, keep_person, by = c("subjid", "agedays")) |>
  mutate(keep = ifelse(cohortn %in% c(60, 61, 67), TRUE, keep))
data <- as.lean(subset(as.data.frame(data), keep == TRUE))

# 23JUL2021
# lean data
# visits: 56846   subjects: 45663   cohorts: 46
# scores: 1306784   items: 807   instruments: 20   domains: 20

# set paths for new model
model_name <- "807_17_revisit4"
path <- path.expand("~/Package/dmodellib/dmodellib/gsed1/models")
dir.create(file.path(path, model_name))

# obtain difficulty estimates from model 619_17
model0 <- readRDS(file.path(path, "619_17", "model.Rds"))
b_fixed <- get_diff(model0$fit)

# refit
model <- fit_dmodel(varlist = varlist,
                    data, equate = equatelist, b_fixed = b_fixed,
                    name = model_name, age_unit = "years")

saveRDS(data, file = file.path(path, model_name, "data.Rds"), compress = "xz")
saveRDS(model, file = file.path(path, model_name, "model.Rds"), compress = "xz")
model <- readRDS(file.path(path, model_name, "model.Rds"))

r <- plot_dmodel(data, model, file.path(path, model_name), ref_name = "gcdg")

write.table(model$itembank,
            file = file.path(path, model_name, "itembank.txt"), sep = "\t",
            na = "", row.names = FALSE, quote = FALSE)

# Additional plots
plot_p_d_item(data = data, model = model, items = items_extra,
              file = file.path(path, model_name, "p_d_item_diff.pdf"))
