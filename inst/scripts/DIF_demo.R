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


# difr has a function genDichoDif() function to test for DIF
# in multiple groups. This is useful when we have more than
# two groups.

# Let us first try using genDichoDif() for two groups:
# Here: focal.names should have length 1!

# Method 1: Use a numeric group variable
group <- ifelse(data$sex == "Male", 1, 0)
focal.names <- 1

# Method 2: Use a character group variable
group <- as.character(data$sex)
focal.names <- "Male"

# Method 3: Use a factor
group <- factor(data$sex, levels = c("Male", "Female"))
focal.names <- levels(group)[1]


# MH-test
MH <- genDichoDif(Data = as.matrix(data[, -(1:7)]),
               group = group,
               focal.names = focal.names,
               p.adjust.method = "holm",
               method = c("GMH"))
# LR-test
LR <- genDichoDif(Data = as.matrix(data[, -(1:7)]),
               group = group,
               focal.names = focal.names,
               p.adjust.method = "holm",
               method = c("genLogistic"))
# combined
both <- genDichoDif(Data = as.matrix(data[, -(1:7)]),
                  group = group,
                  focal.names = focal.names,
                  p.adjust.method = "holm",
                  method = c("GMH", "genLogistic"))


# NOW try genDichoDif() for 3 groups:
# Here: focal.names should have length 3!

group <- cut(data$gagebrth, c(0, 223, 257, 314), labels = c("VPT", "PT", "AT"))
data <- data[!is.na(group), ]
group <- group[!is.na(group)]

# Method 1: Use a numeric group variable
gp <- as.integer(group) - 1
focal.names <- unique(gp)

# Method 2: Use a character group variable
gp <- as.character(group)
focal.names <- unique(gp)

# Method 3: Use a factor
gp <- group
focal.names <- levels(gp)


# MH-test: RUNS OK for methods 1-3
MH <- genDichoDif(Data = as.matrix(data[, -(1:7)]),
                  group = gp,
                  focal.names = focal.names,
                  p.adjust.method = "holm",
                  method = c("GMH"))

# LR-test: FAILS:  number of items to replace is not a multiple of replacement length
LR <- genDichoDif(Data = as.matrix(data[, -(1:7)]),
                  group = gp,
                  focal.names = focal.names,
                  p.adjust.method = "holm",
                  method = c("genLogistic"))


