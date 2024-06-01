# Compares two dscore algorithms fitted to the same data
# 20240601 SvB: Created for comparing dscore current (1.8.8) vs dscore 1.8.7

library(dplyr)
library(dscore)
library(ggplot2)

algorithm1 <- "1.8.7"
algorithm2 <- "current"

# Set and get data and models
model_name <- "293_0"
path1 <- file.path("~/project/gsed/phase1", version1, model_name)
data <- readRDS(file.path(path1, "data.Rds"))
model1 <- readRDS(file.path(path1, "model.Rds"))
model2 <- readRDS(file.path(path2, "model.Rds"))

# LF/SF items groups in data
items_lf <- colnames(data)[starts_with("gto", vars = colnames(data))]
items_sf <- colnames(data)[starts_with("gpa", vars = colnames(data))]

# read custom itembanks
itembank1 <- data.frame(key = "custom", model1$itembank)
itembank2 <- data.frame(key = "custom", model2$itembank)

plot(itembank1$tau, itembank2$tau)

