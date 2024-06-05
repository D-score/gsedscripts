# Calculated dscore and DAZ for Phase 1 data
# 20220805 SvB

# get the phase 1 validation data
suppressWarnings(source("scripts/assemble_data.R"))
source("scripts/edit_data.R")

library(gseddata)
library(dmetric)
library(dscore)
library(ggplot2)

stopifnot(packageVersion("dscore") >= "1.5.4")

key <- "293_0"   # experimental key for LF and SF
# key <- "294_0"   # experimental key for LF and SF
# key <- "gsed2206"
# key <- "gsed1912"
# key <- "lf2206"
# key <- "sf2206"

adm <- c("cohort", "cohortn", "country", "subjid", "subjido", "agedays", "ins")
items <- colnames(work)[starts_with(c("gpa", "gto"), vars = colnames(work))]
if (key == "lf2206") {
  items <- colnames(work)[starts_with(c("gto"), vars = colnames(work))]
}
if (key == "sf2206") {
  items <- colnames(work)[starts_with(c("gpa"), vars = colnames(work))]
}
if (key == "gsed1912") {
  items2 <- rename_vector(items, "gsed2", "gsed")
  colnames(work)[colnames(work) %in% items] <- items2
  items <- items2
}

dup <- duplicated(colnames(work))  # remove some by3 columns because of duplication
work <- work[, !dup]
dup2 <- duplicated(items)
items <- items[!dup2]

data <- work |>
  mutate(
    subjido = gsed_id,
    agedays = age,
    cohort = strtrim(subjido, 7),
    cohortn = as.integer(strtrim(subjido, 2)) + 100L,
    country = recode(cohort, "11-GSED" = "GSED-BGD", "17-GSED" = "GSED-PAK", "20-GSED" = "GSED-TZA"),
    subjid = cohortn * 100000L + as.integer(substr(subjido, 9, 12)),
    across(all_of(items), ~ recode(.x, "1" = 1L, "0" = 0L, .default = NA_integer_))) |>
  drop_na(agedays) |>
  filter(ins %in% c("lf", "sf")) |>
  select(all_of(adm), all_of(items))

# # We do not need custom itembank anymore since dscore 1.5.4, but just keep these statement to go back if needed
path <- file.path("~/project/gsed/phase1/remodel", key)
model <- readRDS(file.path(path, "model.Rds"))
# itembank <- data.frame(key = "custom", model$itembank)
# # # Use as key = "custom" and itembank = itembank in dscore(...)
# ds <- dscore(data = data,
#              items = items,
#              key = "custom",
#              itembank = itembank,
#              xname = "agedays",
#              xunit = "days",
#              population = "phase1")

ds <- dscore(data = data,
             items = items,
             key = "gsed",
             xname = "agedays",
             xunit = "days")
md <- cbind(data, ds)

r <- builtin_references |>
  filter(pop == "phase1" & age < 3.5) |>
  select(age, SDM2, SD0, SDP2)

col_manual = c("GSED-BGD" = "#D93F46", "GSED-PAK" = "#489033", "GSED-TZA" = "#47A1D8")

g <- ggplot(md, aes(x = a, y = d, group = subjid, colour = country)) +
  theme_light() +
  theme(legend.position = c(.85, .15)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank()) +
  scale_colour_manual(values = col_manual) +
  annotate("polygon", x = c(r$age, rev(r$age)),
           y = c(r$SDM2, rev(r$SDP2)), alpha = 0.0, fill = "transparent") +
  annotate("line", x = r$age, y = r$SDM2, lwd = 0.7, color = "#C5EDDE") +
  annotate("line", x = r$age, y = r$SDP2, lwd = 0.7, color = "#C5EDDE") +
  annotate("line", x = r$age, y = r$SD0, lwd = 1.3, color = "#C5EDDE") +
  coord_cartesian(xlim = c(0, 3.5)) +
  ylab(expression(paste(italic(D), "-score", sep = ""))) +
  xlab("Age (in years)") +
  geom_point(size = 0.6) +
  facet_wrap(vars(ins, country)) +
  theme(legend.position = "none")

g

ggsave(
  file = paste0("dscore_by_age_", key, ".pdf"),
  plot = g,
  device = "pdf",
  width = 16,
  height = 10
)

