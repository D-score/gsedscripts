# This script demonstrates how to use the difR package to detect DIF
# in the Dutch SMOCC data.
#
# Stef van Buuren, Aug 16, 2024

# install.packages(c("childdevdata", "difR"))
library(childdevdata)
library(difR)

data <- childdevdata::gcdg_nld_smocc
group <- cut(data$gagebrth, c(0, 223, 257, 314), labels = c("VPT", "PT", "AT"))

# Remove missing values from group
data <- data[!is.na(group), ]
group <- as.character(group[!is.na(group)])

# MH-test: 1 DIF, uniform
MH <- genDichoDif(Data = as.matrix(data[, -(1:7)]),
                  group = group,
                  focal.names = unique(group),
                  p.adjust.method = "holm",
                  method = c("GMH"))

# LR-test: This call fails
LR <- genDichoDif(Data = as.matrix(data[, -(1:7)]),
                  group = group,
                  focal.names = unique(group),
                  p.adjust.method = "holm",
                  method = c("genLogistic"))
