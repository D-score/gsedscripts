# Calibrate scales of SF and LF for model 294_0
# This script creates a Tukey Sum-Difference plot (Bland Altman plot)
# for D-score calcuate for SF and LF
#
# Prerequisites: Run sf_lf_294_0.R
# R package fuzzyjoin
library(dplyr)
library(ggplot2)
library(dscore)

# Get the joined data
model_name <- "294_0"
path <- file.path("~/project/gsed/phase1/lfsfbsid", model_name)
data <- readRDS(file.path(path, "data.Rds"))
model <- readRDS(file.path(path, "model.Rds"))

# LF/SF items groups in data
items_lf <- colnames(data)[starts_with("gto", vars = colnames(data))]
items_sf <- colnames(data)[starts_with("gpa", vars = colnames(data))]

# read custom itembank
itembank <- data.frame(key = "custom", model$itembank)

# go LF
d_lf <- dscore(data = data,
               items = items_lf,
               key = "custom",
               itembank = itembank,
               xname = "agedays",
               xunit = "days",
               population = "gcdg",
               relevance = c(-20, 5)) %>%
  rename(d_lf = d, daz_lf = daz) %>%
  select(all_of(c("a", "d_lf", "daz_lf")))

# go SF
d_sf <- dscore(data = data,
               items = items_sf,
               key = "custom",
               itembank = itembank,
               xname = "agedays",
               xunit = "days",
               population = "gcdg",
               relevance = c(-20, 5)) %>%
  rename(d_sf = d, daz_sf = daz) %>%
  select(all_of(c("d_sf", "daz_sf")))

joined <- cbind(study = data$study, d_lf, d_sf) %>%
  mutate(
    d_mean = (d_lf + d_sf) / 2,
    d_diff = (d_lf - d_sf),
    daz_mean = (daz_lf + daz_sf) / 2,
    daz_diff = (daz_lf - daz_sf)
  )

# create Bland-Altman D
theme_set(theme_light())
col_manual <- c("GSED-BGD" = "#D93F46", "GSED-PAK" = "#489033", "GSED-TZA" = "#47A1D8")

plot1 <- ggplot(joined, aes(x = d_lf, y = d_sf, group = study, colour = study)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("LF (D-score)", limits = c(10, 80)) +
  scale_y_continuous("SF (D-score)", limits = c(10, 80)) +
  coord_fixed() +
  geom_abline(slope = 1, colour = "grey", size = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$study, ncol = 3) +
  theme(legend.position = "none")
plot1

plot2 <- ggplot(joined, aes(x = d_mean, y = d_diff, group = study, colour = study)) +
  scale_colour_manual(values = col_manual,
                      na.value = "grey") +
  scale_x_continuous("(LF + SF) / 2",
                     limits = c(10, 80)) +
  scale_y_continuous(
    paste0("LF - SF"),
    breaks = seq(-10, 10, 2),
    limits = c(-10, +10)
  ) +
  geom_hline(yintercept = c(0), colour = "grey", size = 1.5) +
  geom_smooth(se = TRUE, colour = "grey60", size = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$study, ncol = 3) +
  theme(legend.position = "none")
plot2

plot3 <- ggplot(joined, aes(x = a, y = daz_lf, group = study, colour = study)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("Age (years)", limits = c(0, 3.5)) +
  scale_y_continuous("LF (DAZ GCDG)", limits = c(-5, 5)) +
  geom_hline(yintercept = c(-2, 0, 2), colour = "grey", size = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$study, ncol = 3) +
  theme(legend.position = "none")
plot3

plot4 <- ggplot(joined, aes(x = a, y = daz_sf, group = study, colour = study)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("Age (years)", limits = c(0, 3.5)) +
  scale_y_continuous("SF (DAZ GCDG)", limits = c(-5, 5)) +
  geom_hline(yintercept = c(-2, 0, 2), colour = "grey", size = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$study, ncol = 3) +
  theme(legend.position = "none")
plot4


plot5 <- ggplot(joined, aes(x = daz_lf, y = daz_sf, group = study, colour = study)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("LF (DAZ GCDG)", limits = c(-5, 5)) +
  scale_y_continuous("SF (DAZ GCDG)", limits = c(-5, 5)) +
  coord_fixed() +
  geom_abline(slope = 1, colour = "grey", size = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$study, ncol = 3) +
  theme(legend.position = "none")
plot5

plot6 <- ggplot(joined, aes(x = daz_mean, y = daz_diff, group = study, colour = study)) +
  scale_colour_manual(values = col_manual, na.value = "grey") +
  scale_x_continuous("(LF + SF) / 2 (DAZ GCDG)", limits = c(-4, 4)) +
  scale_y_continuous("LF - SF (DAZ GCDG)", limits = c(-4, +4)) +
  coord_fixed() +
  geom_hline(yintercept = c(0), colour = "grey", size = 1.5) +
  geom_smooth(se = TRUE, colour = "grey60", size = 1.5) +
  geom_point(
    size = 0.7,
    shape = 19
  ) +
  facet_wrap( ~ .data$study, ncol = 3) +
  theme(legend.position = "none")
plot6

dec <- decompose_itemnames(model$item_fit$item)
dec$ins <- ifelse(dec$instrument == "gpa", "SF", "LF")
itemfit <- bind_cols(model$item_fit, dec)
plot7 <- ggplot(itemfit, aes(x = ins, y = infit, colour = domain)) +
  scale_y_continuous("Infit", limits = c(0, 3)) +
  scale_x_discrete("Instrument") +
  geom_hline(yintercept = 1, colour = "grey", size = 1) +
  geom_boxplot()
plot7

plot8 <- ggplot(itemfit, aes(x = ins, y = outfit, colour = domain)) +
  scale_y_continuous("Outfit", limits = c(0, 7)) +
  scale_x_discrete("Instrument") +
  geom_hline(yintercept = 1, colour = "grey", size = 1) +
  geom_boxplot()
plot8


plots <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)

# save as one pdf with all variations
path <- file.path("~/project/gsed/phase1")
device <- "pdf"
if (!is.null(file) & device == "pdf") {
  file <- file.path(path, "calibration_plot_294_0.pdf")
  pdf(file, onefile = TRUE, width = 24, height = 8)
  lapply(plots, print)
  message("Saved to: ", file)
  dev.off()
}

# compare difficulty estimates with key gsed2206
tau_gsed2206 <- get_tau(items = c(items_lf, items_sf), key = "gsed2206")
tau_294_0 <- get_tau(items = c(items_lf, items_sf), key = "294_0",
                     itembank = cbind(key = "294_0", model$itembank))
df <- data.frame(item = c(items_lf, items_sf),
                 instrument = c(rep("LF", length(items_lf)), rep("SF", length(items_sf))),
                 tau_gsed2206 = tau_gsed2206,
                 tau_294_0 = tau_294_0)
g <- ggplot(df, aes(x = ins, y = outfit, colour = domain)) +
  scale_y_continuous("Outfit", limits = c(0, 7)) +
  scale_x_discrete("Instrument") +
  geom_hline(yintercept = 1, colour = "grey", size = 1) +
  geom_boxplot()
plot8

# plotting with basic ggparcoord from GGally
library(GGally)
g1 <- ggparcoord(df, columns = 3:4, groupColumn = 2, scale = 'globalminmax',?
                showPoints = TRUE, alphaLines = 0.7) +
  xlab("Key") +
  ylab("Item difficulty")

g <- g1 +
  geom_text(aes(y = tau_gsed2206, x = 1, label = item), data = df[df$instrument == "LF", ],
            nudge_x = -0.2, inherit.aes = FALSE, cex = 3, check_overlap = TRUE,
            family = "Courier") +
  geom_text(aes(y = tau_294_0, x = 2, label = item), data = df[df$instrument == "SF", ],
          nudge_x = 0.2, inherit.aes = FALSE, cex = 3, check_overlap = TRUE,
          family = "Courier")
g

# save as one pdf with all variations
path <- file.path("~/project/gsed/phase1")
device <- "pdf"
file <- file.path(path, "key_match.pdf")
pdf(file, width = 6, height = 30)
print(g)
message("Saved to: ", file)
dev.off()

# create xls type of file for manual inspection

d <- df %>%
  rename(gsed2 = item) %>%
  select(-instrument)
dataset <- data.frame(
  gsed = gsedread::rename_vector(d$gsed2, lexin = "gsed2", lexout = "gsed"),
  d,
  decompose_itemnames(d$gsed2)
)
labs <- data.frame(
  label = get_labels(items = gsed, trim = 50),
  gsed = names(get_labels(items = gsed, trim = 30))
)
dataset <- left_join(dataset, labs, by = "gsed")


require(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, sheet = "keys", gridLines = TRUE)
writeData(wb, sheet = "keys", x = dataset, rowNames = FALSE, withFilter = TRUE)
# saveWorkbook(wb, file = file.path(path, "key_match.xlsx"), overwrite = TRUE)
