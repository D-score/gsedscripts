# Informal inspection of uni-dimensionality of 293_0 model
#
# This is not a formal test for dimensionality, but a visual inspection of the
# difference between the
# 1. tau-estimates for LF gross motor, language and fine motor for separate models
# 2. tau-estimates of the joint model
#
# For a truly unidimensional model, the tau-estimates should be the same.
# Here we find high correlations: 0.981 (gross motor), 0.987 (language),
# 0.999 (fine motor).
#
# Conclusion: For all practical purposes, the joint LF model is unidimensional.
#
# Created for phase1 manuscript, included as figure.
# Created 20240704 SvB

library(dplyr)
library(dscore)
library(ggplot2)
library(gsedread)
library(dmetric)

# Get data and model
model_name <- "293_0"
path <- file.path("~/project/gsed/phase1/20240703", model_name)
data <- readRDS(file.path(path, "data.Rds"))
model <- readRDS(file.path(path, "model.Rds"))

# LF/SF items groups in data
items_lf <- colnames(data)[starts_with("gto", vars = colnames(data))]
items_sf <- colnames(data)[starts_with("gpa", vars = colnames(data))]

# LF/SF item-domains
items_lf_gm <- colnames(data)[starts_with("gtogm", vars = colnames(data))]
items_lf_lg <- colnames(data)[starts_with("gtolg", vars = colnames(data))]
items_lf_fm <- colnames(data)[starts_with("gtofm", vars = colnames(data))]

items <- items_lf_gm
anchor <- c(20, 40)
names(anchor) <- c("gtogmd001", "gtogmd026")
varlist = list(adm = c("cohort", "cohortn", "subjid", "age", "agedays"),
               items = c(items))
model_name <- paste("lf_gm", length(items), "0", sep = "_")
model_lf_gm <- fit_dmodel(varlist = varlist,
                    data = data,
                    name = model_name,
                    anchors = anchor,
                    data_package = "", verbose = TRUE)

plot(model_lf_gm$itembank$tau, get_tau(items), main = "LF Gross motor")
abline(0,1)

items <- items_lf_lg
# get_tau(items)
anchor <- c(25.43, 81.73)
names(anchor) <- c("gtolgd005", "gtolgd043")
varlist = list(adm = c("cohort", "cohortn", "subjid", "age", "agedays"),
               items = c(items))
model_name <- paste("lf_lg", length(items), "0", sep = "_")
model_lf_lg <- fit_dmodel(varlist = varlist,
                          data = data,
                          name = model_name,
                          anchors = anchor,
                          data_package = "", verbose = TRUE)

plot(model_lf_lg$itembank$tau, get_tau(items), main = "LF Language")
abline(0,1)


items <- items_lf_fm
# get_tau(items)
anchor <- c(25.02, 45.32)
names(anchor) <- c("gtofmd003", "gtofmd014")
varlist = list(adm = c("cohort", "cohortn", "subjid", "age", "agedays"),
               items = c(items))
model_name <- paste("lf_lg", length(items), "0", sep = "_")
model_lf_fm <- fit_dmodel(varlist = varlist,
                          data = data,
                          name = model_name,
                          anchors = anchor,
                          data_package = "", verbose = TRUE)

plot(model_lf_fm$itembank$tau, get_tau(items), pch = 19,
     ylab = "Fine motor items", xlab = "D-score",
     main = "Fine motor", xlim = c(0, 90), ylim = c(0, 90),
     pty = "s")
abline(0,1, col = "grey")


# -------

pdf(file.path(path, "lf_submodels.pdf"), width = 16, height = 6)

par(mfrow = c(1, 3))
plot(y = model_lf_gm$itembank$tau, x = get_tau(items_lf_gm), pch = 19,
     ylab = "Gross motor model", xlab = "D-score",
     main = "LF-A: Gross motor", xlim = c(0, 90), ylim = c(0, 90),
     pty = "s", type = "n")
abline(0,1, col = "grey")
points(y = model_lf_gm$itembank$tau, x = get_tau(items_lf_gm), pch = 19)
text(x = 15, y = 70, paste("R =", round(cor(model_lf_gm$itembank$tau, get_tau(items_lf_gm)), 3)), cex = 1.5)

plot(y = model_lf_lg$itembank$tau, x = get_tau(items_lf_lg), pch = 19,
     ylab = "Language model", xlab = "D-score",
     main = "LF-B: Language", xlim = c(0, 90), ylim = c(0, 90),
     pty = "s")
abline(0,1, col = "grey")
points(y = model_lf_lg$itembank$tau, x = get_tau(items_lf_lg), pch = 19)
text(x = 15, y = 70, paste("R =", round(cor(model_lf_lg$itembank$tau, get_tau(items_lf_lg)), 3)), cex = 1.5)

plot(y = model_lf_fm$itembank$tau, x = get_tau(items_lf_fm), pch = 19,
     ylab = "Fine motor model", xlab = "D-score",
     main = "LF-C: Fine motor", xlim = c(0, 90), ylim = c(0, 90),
     pty = "s", type = "n")
abline(0,1, col = "grey")
points(y = model_lf_fm$itembank$tau, x = get_tau(items_lf_fm), pch = 19)
text(x = 15, y = 70, paste("R =", round(cor(model_lf_fm$itembank$tau, get_tau(items_lf_fm)), 3)), cex = 1.5)


par(mfrow = c(1, 1))

dev.off()
