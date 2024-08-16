# This script demonstrates how to use the difR package to detect DIF
# in the Dutch SMOCC data.
#
# Stef van Buuren, Aug 16, 2024

# install.packages(c("childdevdata", "difR"))
library(childdevdata)
library(difR)

data <- childdevdata::gcdg_nld_smocc
group <- ifelse(data$sex == "Male", 1, 0)

# MH-test: No uniform DIF
MH <- dichoDif(Data = as.matrix(data[, -(1:7)]),
               group = group,
               focal.name = 1,
               p.adjust.method = "holm",
               method = c("MH"))

# LR-test: 12 DIF unif/nunif, all negligible
LR <- dichoDif(Data = as.matrix(data[, -(1:7)]),
               group = group,
               focal.name = 1,
               p.adjust.method = "holm",
               method = c("Logistic"))

# Join results
both <- dichoDif(Data = as.matrix(data[, -(1:7)]),
                 group = group,
                 focal.name = 1,
                 p.adjust.method = "holm",
                 method = c("MH", "Logistic"))
both
