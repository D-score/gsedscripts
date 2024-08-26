#DIF_demo residuals#
library(dplyr)
library(tidyr)
library(dscore)
library(childdevdata)


data <- childdevdata::gcdg_nld_smocc %>%
  bind_cols(dscore(data = ., key = "dutch", population = "dutch", xname = "agedays", xunit = "days") )

ddata <- dscore(data = data, key = "dutch", population = "dutch", xname = "agedays", xunit = "days")

#extract relevant inputs
transform <- dscore::builtin_keys[1,5:6] %>% unlist %>% as.vector


datl <-
  data %>%
  pivot_longer(starts_with("ddi"), names_to = "item", values_to = "value") %>%
  drop_na(value) %>%
  left_join({builtin_itembank %>% filter(key == "dutch")}, by = "item")

residuals <-
  datl %>% drop_na(d) %>% mutate(
                    taut = (.data$tau -
                              transform[1L]) /
                      transform[2],
                    dt = (.data$d - transform[1L]) / transform[2],
                    p = plogis(.data$dt, location = .data$taut),
                    psi = exp(.data$dt - .data$taut),
                    pi = .data$psi / (1 + .data$psi)
                  )

residuals <- residuals %>% mutate(
  w = pmax(.data$p ^ 2 * (1 -
                            .data$p) + (1 - .data$p) ^
             2 * .data$p, 0.01), #variance of residual var  sum(var)/ N^2 per item. total over items = total var RdifR
  c = pmax(.data$p ^ 4 *
             (1 - .data$p) + (1 - .data$p) ^
             4 * .data$p, 0.01),
  y = .data$value - .data$p, #score residual  RDIFR: mean(y-grp1) - mean(y-grp2) >> expected = 0
  z = .data$y / .data$w ^ 0.5,
  z2 = .data$z ^ 2,
  y2 = .data$w * .data$z2, #score residual squared: mean(y2-grp1) - mean(y2-grp2)
  w2 = .data$w ^ 2,
  varRi = .data$p * (1 - .data$p),
  varSi = .data$p * (1 - .data$p) * (1 - 2 * .data$p)^2,
  cdivw2 = .data$c / .data$w2,
  cminw2 = .data$c - .data$w2
)

dif_items <-
  residuals %>%
  group_by(item, sex) %>%
  summarise(
    n = n(),
    RdifRi = sum(.data$y)/n,
    varRdifRi = sum(.data$varRi)/ n^2,
    RdifSi = sum(.data$y2)/n,
    meanRdifSi = sum(.data$w)/n,
    varRdifSi = sum(.data$varSi)/n^2
  )

dif_items %>% group_by(item) %>%
  summarise(RdifR = diff(RdifRi),
            varRdifR = sum(varRdifRi),
            RdifS = diff(RdifSi),
            varRdifS = sum(varRdifSi),
            expS = diff(meanRdifSi)
  ) %>%
  mutate(
    zR = RdifR/sqrt(varRdifR),
    pR = 2 * (1- pnorm(abs(zR))),
    zS = (RdifS - expS) /sqrt(varRdifS),
    pS = 2 * (1- pnorm(abs(zS)))
  ) %>%
  select(RdifR, varRdifR, zR, pR, RdifS, expS, varRdifS, zS, pS)

