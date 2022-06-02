# Compare itembank of gsed and gsed22
# This script assumes that models 818_17_joint_fixed, 155_0 and 139_0 have
# been fitted.
# Compares the difficulty estimates produced by the various models.
# Conclusions:
# - The gsed22 key (update) and existing gsed key correlate 0.997, and are
# well placed along the diagonal. Still, some separate taus may differ up to
# 8 D-score points.
# - Agreement between the isolated gsed_lf key and gsed22 is higher than
# agreement between isolated gsed_lf and existing gsed. Both forms of
# disagreement is non-linear in tau.
# - Agreement between isolated gsed_sf key and gsed22 is excellent above tau 40,
# but systematically biased below that. Agreement with gsed22 is better than with gsed.
#
# 20220601 SvB

library(dscore)
library(dplyr)

gsed <- builtin_itembank %>%
  filter(key == "gsed")
dim(gsed)

model <- readRDS(file.path("~/project/gsed/phase1/joint/818_17_joint_fixed/model.Rds"))
gsed22 <- model$itembank

comb <- gsed22 %>%
  left_join(gsed, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)

## compare isolated LF - GSED
lf_model <- readRDS(file.path("~/project/gsed/phase1/lf/155_0/model.Rds"))
lf <- lf_model$itembank
z <- rename_vector(gsed$item, "gsed", "gsed2", contains = "Ma_LF_")
gsed$item <- z
comb <- left_join(lf, gsed, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)

## compare isolated LF - GSED22
z <- rename_vector(gsed22$item, "gsed", "gsed2", contains = "Ma_LF_")
gsed22$item <- z
comb <- left_join(lf, gsed22, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)

## compare isolate SF - GSED
sf_model <- readRDS(file.path("~/project/gsed/phase1/sf/139_0/model.Rds"))
sf <- sf_model$itembank
z <- rename_vector(gsed$item, "gsed", "gsed2", contains = "Ma_SF_")
gsed$item <- z
comb <- left_join(sf, gsed, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)

## compare unconnected SF - GSED22
z <- rename_vector(gsed22$item, "gsed", "gsed2", contains = "Ma_SF_")
gsed22$item <- z
comb <- left_join(sf, gsed22, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)
