# Compares two model fitted to the same data
# Prerequisites: path names to model1 and model2
# Depends on gsed2 lexicon
# 20240601 SvB: Created for comparing 20240601 vs 20221201_remodel
#+ fig.width=7, fig.height=7

library(dplyr)
library(dscore)
library(ggplot2)

# Set and get data and models
model_name <- "293_0"
version1 <- "20221201_remodel"
version2 <- "20240601"
path1 <- file.path("~/project/gsed/phase1", version1, model_name)
path2 <- file.path("~/project/gsed/phase1", version2, model_name)
data <- readRDS(file.path(path1, "data.Rds"))
model1 <- readRDS(file.path(path1, "model.Rds"))
model2 <- readRDS(file.path(path2, "model.Rds"))

# Which package versions were used?
model1$version
model2$version

# SF + LF combined, 293 items

# Are tau's the same? Yes!
plot(model1$itembank$tau, model2$itembank$tau)

# How are D-scores related? Very similar
plot(model1$dscore$d, model2$dscore$d, cex = 0.4, main = "D-score scale"); abline(0, 1)
plot(model1$beta_l$d, model2$beta_l$d, cex = 0.4, main = "Logit scale"); abline(0, 1)

# How are SEM scores related? As expected, all SEM's are higher
plot(model1$dscore$sem, model2$dscore$sem, cex = 0.4, main = "D-score scale"); abline(0, 1)
plot(model1$beta_l$sem, model2$beta_l$sem, cex = 0.4, main = "Logit scale"); abline(0, 1)

# How is SEM related to number of items? As expected, higher and - as a bonus - better behaved.
plot(model1$dscore$n, model1$dscore$sem, cex = 0.4, main = "Model 1", ylim = c(0, 2.5)); abline(0, 1)
plot(model2$dscore$n, model2$dscore$sem, cex = 0.4, main = "Model 2", ylim = c(0, 2.5)); abline(0, 1)

# How are DAZ related? Quite similar
plot(model1$dscore$daz, model2$dscore$daz, cex = 0.4, main = "DAZ scale"); abline(0, 1)

# How are infits related? Similar, better fit for about 15 items
plot(model1$item_fit$infit, model2$item_fit$infit, cex = 0.4, main = "Item infit"); abline(0, 1)

# How are outfits related? Similar, better fit handful of items
plot(model1$item_fit$outfit, model2$item_fit$outfit, cex = 0.4, main = "Item outfit"); abline(0, 1)

# How are person fits related? Similar, better fit 10 persons
plot(model1$person_fit$infit, model2$person_fit$infit, cex = 0.4, main = "Person infit"); abline(0, 1)

# How are outfits related? Similar, better fit 10 person
plot(model1$person_fit$outfit, model2$person_fit$outfit, cex = 0.4, main = "Person outfit"); abline(0, 1)

# How model residuals related? Same, but improved fit for small subset
smp <- sample(1:nrow(model1$residuals), 100000)
plot(model1$residuals$z[smp], model2$residuals$z[smp], cex = 0.2, main = "Residuals"); abline(0, 1)

# How is the DAZ distribution against age? A little more homogeneous spread across age
plot(model1$dscore$a, model1$dscore$daz, cex = 0.4, main = "Model 1"); abline(h = c(-2, 0, 2))
plot(model2$dscore$a, model2$dscore$daz, cex = 0.4, main = "Model 2"); abline(h = c(-2, 0, 2))
