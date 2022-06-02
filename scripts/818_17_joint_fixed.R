# Combines the 807_17 data with the new SF/LF/BSID data
# Conclusion:
# - The D-score is calculated from combined SF,LF,BSID. The D-score by age
#  distributions for BGD, PAK and TZA look convincing.
# - There is better agreement between gsed22 key (this solution) and isolated
# instrument key than the existing gsed key. See compare_itembank.R.
# - This solution improves upon the existing gsed key.
# - There are still problems that need to be resolved for tau < 30 (SF) and for
# the nonlinear nature of tau (LF). Future work to resolve this.
# - We suggest to replace gsed by the better gsed22 to calculate D-score,
# at least until the data of Phase 2 are available.
#
# 20220601 SvB

# get the phase 1 validation data
suppressWarnings(source("scripts/assemble_data.R"))

library(gseddata)
library(dmetric)

adm <- c("cohort", "cohortn", "subjid", "subjido", "agedays", "ins")
items <- colnames(work)[starts_with(c("gpa", "gto", "by3"), vars = colnames(work))]
new <- work %>%
  mutate(
    subjido = gsed_id,
    agedays = age + (ins == "lf") * 0.1 + (ins == "bsid") * 0.2,
    cohort = strtrim(subjido, 7),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) %>%
  drop_na(agedays) %>%
  select(all_of(adm), all_of(items))
colnames(new) <- c(adm, rename_vector(items, "gsed2", "gsed"))
dup <- duplicated(colnames(new))  # remove some by3 columns because of duplication
new_data <- new[, !dup]
# for checking
# lean <- as.lean(new, remove_duplicates = TRUE)

# 807_17 data
old_model_name <- "807_17_revisit4"
path <- path.expand("~/Package/dmodellib/dmodellib/gsed1/models")
old_lean <- readRDS(file = file.path(path, old_model_name, "data.Rds"))
old_model <- readRDS(file = file.path(path, old_model_name, "model.Rds"))
old_data <- as.data.frame(old_lean)

# select only by3 items that are in the 807 model
data <- bind_rows(old_data, new_data)
all_by3 <- colnames(work)[starts_with(c("by3"), vars = colnames(work))]
keep_by3 <- old_model$items[starts_with("by3", vars = old_model$items)]
remove_by3 <- setdiff(all_by3, keep_by3)
keepvars <- setdiff(colnames(data), remove_by3)
data <- data[, keepvars]

# make lean
# lean <- as.lean(data, remove_duplicates = TRUE)

items <- setdiff(colnames(data), c("cohort", "cohortn", "subjid", "subjido", "agedays", "ins", "keep"))
varlist <- list(adm = c("cohort", "cohortn", "subjid", "subjido", "agedays"),
                items = items)
equatelist <- list(
  COG36 = c("cromoc022", "by1mdd090", "by3cgd036", "denfmd014", "ddifmd011"),
  COG55 = c("by1mdd118", "by2mdd098", "by3cgd055"),
  EXP26 = c("croclc024", "iyolgc018", "by1mdd136", "by2mdd114", "denlgd019", "grihsd217", "ddicmm041", "vinxxc016"),
  FM26 = c("dmcfmd006", "cromoc019", "iyomoc017", "by3fmd026", "barxxx008", "ddifmd010"),
  FM31 = c("iyomoc030", "aqifmc025", "by1mdd111", "by2mdd097", "by3fmd031", "denfmd017", "griehd207", "ddifmd013"),
  FM38 = c("by1mdd143", "by2mdd123", "by3fmd038", "denfmd019", "griehd223", "ddifmd017"),
  FM52 = c("by3fmd052", "teplgd002", "ddifmd023"),
  GM35 = c("dmcgmd007", "mdtgmd012", "cromoc015", "by3gmd035", "barxxx009", "dengmd012", "ddigmm065"),
  GM42 = c("mdsgmd006", "kdigmd006", "dmcgmd011", "mdtgmd016", "iyomoc024", "cromoc021", "by1pdd046", "by2pdd062", "by3gmd042", "barxxx012", "grigmd203", "ddigmd068"),
  REC6 = c("mdtlgd004", "iyolgc001", "by1mdd047", "denlgd006", "ddicmd116"),
  REC40 = c("by3red040", "teplgd018"),
  GM60 = c("kdigmd009", "cromoc036", "iyomoc039", "barxxx019", "macgmd041", "tepmod442"),
  SA1 = c("vinxxc048", "vinxxc030"),
  FM72 = c("cromoc029", "by1mdd119", "ddifmd015"),
  FM61 = c("by3fmd061", "denfmd025", "griehd306", "tepcod010"),
  FM43 = c("mdtfmd032", "iyomoc038", "aqifmc033", "by3fmd043", "barxxx018", "denfmd023", "tepmod009", "ddifmd026"),
  GM25 = c("mdtgmd008", "cromoc010", "by1pdd028", "by3gmd025", "barxxx005", "dengmd008", "grigmd013", "ddigmm060"))

# obtain difficulty estimates from model 619_17
model0 <- readRDS(file.path(path, "619_17", "model.Rds"))
b_fixed <- dmetric::get_diff(model0$fit)

# fit
model_name <- paste(length(items), length(equatelist), "joint", "fixed", sep = "_")
model <- fit_dmodel(varlist = varlist,
                    data, equate = equatelist, b_fixed = b_fixed,
                    name = model_name, age_unit = "years")

# Store and reload model
path <- file.path("~/project/gsed/phase1/joint", model_name)
dir.create(path)
saveRDS(model, file = file.path(path, "model.Rds"), compress = "xz")
model <- readRDS(file.path("~/project/gsed/phase1/joint", model_name, "model.Rds"))

# Plot figures
theme_set(theme_light())
col_manual <- c("BGD" = "#D93F46", "PAK" = "#489033", "TZA" = "#47A1D8")
r <- plot_dmodel(data = data,
                 model = model,
                 path = path,
                 ref_name = "gcdg",
                 col.manual = col_manual,
                 maxy = 85,
                 xlim = c(0, 85),
                 xbreaks = seq(0, 80, 10))
