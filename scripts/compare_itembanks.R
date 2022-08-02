# Compare itembanks of gsed1912, gsed2206, sf2206, lf2206
# This script assumes that models 818_17_joint_fixed, 155_0 and 139_0 have
# been fitted.
# Compares the difficulty estimates produced by the various models.
# Conclusions:
# - The gsed2206 key (update) and gsed key gsed1912 correlate 0.997, and are
# well placed along the diagonal. Still, some separate taus may differ up to
# 8 D-score points.
# - Agreement between the isolated lf2206 key and gsed2206 is higher than
# agreement between isolated lf2206 and gsed1912. Both forms of
# disagreement is non-linear in tau.
# - Agreement between isolated sf2206 key and gsed2206 is excellent above tau 40,
# but systematically biased below that. Agreement with gsed2206 is better than with gsed1912.
#
# 20220601 SvB
# Updated 22020802 SvB

library(dscore)
library(dplyr)
library(gsedread)

gsed <- builtin_itembank %>%
  filter(key == "gsed1912")
dim(gsed)

model <- readRDS(file.path("~/project/gsed/phase1/joint/818_17_joint_fixed/model.Rds"))
gsed22 <- model$itembank

## compare gsed2206 - gsed1912
comb <- gsed22 %>%
  left_join(gsed, by = "item") %>%
  rename(gsed2206 = tau.x, gsed1912 = tau.y) %>%
  mutate(tau_mean = (gsed2206 + gsed1912) / 2,
         tau_diff = gsed2206 - gsed1912)
with(comb, plot(gsed2206, gsed1912)); abline(0,1)
with(comb, cor(gsed2206, gsed1912, use = "pair"))
with(comb, plot(tau_mean, tau_diff)); abline(h=0)

# show item labels for subset of large deviations
bound <- 3
csub <- comb %>%
  filter(abs(tau_diff) > bound)
with(csub, plot(tau_mean, tau_diff, type = "n"));
abline(h = c(-bound, bound), lty = 3);
with(csub, text(tau_mean, tau_diff, item, cex = 0.8))

## compare isolated LF - GSED1912
lf_model <- readRDS(file.path("~/project/gsed/phase1/lf/155_0/model.Rds"))
lf <- lf_model$itembank
z <- rename_vector(gsed$item, "gsed", "gsed2", contains = "Ma_LF_")
gsed$item <- z
comb <- left_join(lf, gsed, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)

## compare isolated LF - GSED2206
z <- rename_vector(gsed22$item, "gsed", "gsed2", contains = "Ma_LF_")
gsed22$item <- z
comb <- left_join(lf, gsed22, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)

## compare isolate SF - GSED1912
sf_model <- readRDS(file.path("~/project/gsed/phase1/sf/139_0/model.Rds"))
sf <- sf_model$itembank
z <- rename_vector(gsed$item, "gsed", "gsed2", contains = "Ma_SF_")
gsed$item <- z
comb <- left_join(sf, gsed, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)

## compare unconnected SF - GSED2206
z <- rename_vector(gsed22$item, "gsed", "gsed2", contains = "Ma_SF_")
gsed22$item <- z
comb <- left_join(sf, gsed22, by = "item")
plot(comb$tau.x, comb$tau.y); abline(0,1)
cor(comb$tau.x, comb$tau.y, use = "pair")
with(comb, plot((tau.x+tau.y)/2, tau.x-tau.y)); abline(h=0)
