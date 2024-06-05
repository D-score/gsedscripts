## add Dutch reference based on GSED key
# IE 11-04-2024
library(tidyr)
library(gseddata)
NLD <- data.frame(gseddata::get_data(cohorts = c(53,54)))

library(dscore)
NLdscores <- dscore(data = NLD,
                    key = "gsed2212",
                    xname = "agedays",
                    xunit = "days")

datin <- NLdscores %>%
  filter(daz > -6 & daz < 6) %>%
  drop_na()

# adapted from dmetric::fit_reference()
tx <- function(x) log(x + 100)
ty <- function(y) y
datin$y   <- datin$d
datin$t.y <- ty(datin$y)
datin$x   <- round(datin$a * 365.25)
datin$t.x <- tx(datin$x)
dat <- na.omit(datin[, c("x", "t.x", "y", "t.y")])

# fit the model
# LMS: selected df <- c(8, 2, 0)
# BCT: selected df <- c(8, 2, 0, 0)  WINNER!
library(gamlss)
## some grid search code outcommented
#grid <- expand.grid(df1 = 8, df2 = 0:5, df3 = 0, df4 = 0:2)
# grid$deviance <- NA
# for (i in 1:nrow(grid)) {
# df <- grid[i, 1:4]
df <- c(4, 2, 0, 0)
family <- gamlss.dist::BCT()
n.cyc <- 20
fit <- gamlss(t.y ~ cs(t.x, df = df[1]),
              sigma.formula = ~ cs(t.x, df = df[2]),
              nu.formula = ~ cs(t.x, df = df[3]),
              tau.formula = ~ cs(t.x, df = df[4]),
              data = dat,
              family = family,
              control = gamlss.control(n.cyc = n.cyc, trace = FALSE))
#  grid[i, "deviance"] <- round(deviance(fit))
#  cat("\n", as.character(grid[i, ]), "\n")
#  }

deviance(fit)
plot(fit)
centiles(fit, xvar = dat$x, cent = round(100 * pnorm(c(-2.5, -2:2, 2.5)), 1))
#wp(fit, xvar = dat$x, n.inter = 16)

# grid for reference table (all weeks in data range)
rx <- range(dat$x , na.rm = TRUE)
weeks <- (floor(rx[1] / 7)) : (ceiling(rx[2] / 7))
grid_x <- (2:186) * 7
grid <- data.frame(x = grid_x,
                   t.x = tx(grid_x))

# calculate and round reference table
p <- predictAll(fit, newdata = grid)
reference <- data.frame(grid, p)
reference$t.x <- round(reference$t.x, 4)
reference$mu <- round(reference$mu, 2)
reference$sigma <- round(reference$sigma, 4)
reference$nu <- round(reference$nu, 4)
reference$tau <- round(reference$tau, 3)

# process and save PHASE1 reference table, weeks 2-168
reference <- reference %>%
  mutate(day = x,
         week = day / 7,
         month = round(12 * day / 365.25, 3),
         year = round(day / 365.25, 4)) %>%
  dplyr::select(day, week, month, year, mu, sigma, nu, tau)

refdutch <-
  reference %>%
  mutate(pop = "dutch",
         P3 = dscore:::qBCT(0.03, mu, sigma, nu, tau),
         P10 = dscore:::qBCT(0.10, mu, sigma, nu, tau),
         P25 = dscore:::qBCT(0.25, mu, sigma, nu, tau),
         P50 = dscore:::qBCT(0.50, mu, sigma, nu, tau),
         P75 = dscore:::qBCT(0.75, mu, sigma, nu, tau),
         P90 = dscore:::qBCT(0.90, mu, sigma, nu, tau),
         P97 = dscore:::qBCT(0.97, mu, sigma, nu, tau),
         SDM2 = dscore:::qBCT(pnorm(-2), mu, sigma, nu, tau),
         SDM1 = dscore:::qBCT(pnorm(-1), mu, sigma, nu, tau),
         SD0 = dscore:::qBCT(pnorm(-0), mu, sigma, nu, tau),
         SDP1 = dscore:::qBCT(pnorm(+1), mu, sigma, nu, tau),
         SDP2 = dscore:::qBCT(pnorm(+2), mu, sigma, nu, tau),
         age = year
  )

refphase1 <- get_reference() %>% mutate(pop = "phase1")

refs <- bind_rows(refdutch, refphase1) %>%
  pivot_longer(P3:P97, names_to = "percentile", values_to = "D")

ggplot()+
  geom_line(data=refs %>% filter(pop == "phase1"), aes(x = age, y = D, group = percentile), color = "#377EB8", size = 1)+
  theme_light()+
  geom_line(data=refs %>% filter(pop == "dutch"), aes(x = age, y = D, group = percentile), color = "#E41A1C", size = 1)




write.table(reference,
            file = "Dutch_gsed2212.txt",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)

# transfer phase1.txt to dscore package


## Results grid search Dutch data.

# 8 0 0 0 106763
#
# 8 1 0 0 106485
#
# 8 2 0 0 106241
#
# 8 3 0 0 105872
#
# 8 4 0 0 105288
#
# 8 5 0 0 104755
#
# 8 0 0 1 106691
#
# 8 1 0 1 106437
#
# 8 2 0 1 106222
#
# 8 3 0 1 105857
#
# 8 4 0 1 105257
#
# 8 5 0 1 104735
#
# 8 0 0 2 106682
#
# 8 1 0 2 106429
#
# 8 2 0 2 106215
#
# 8 3 0 2 105847
#
# 8 4 0 2 105236
#
# 8 5 0 2 104723
