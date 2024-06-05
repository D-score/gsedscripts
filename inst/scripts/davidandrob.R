library(dscore)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

theme_set(ggplot2::theme_minimal())

#example from the adp function in ggplot:
qp <- -10:80
adp_dutch <- function(t, qp = -10:80) {
  mu <- dscore:::count_mu_dutch(t)
  dnorm(qp, mean = mu, sd = 5)
}

p1m <- data.frame(qp = qp, month = rep(1, length(qp)), p = adp_dutch(1/12))
p15m <- data.frame(qp = qp, month = rep(15, length(qp)), p = adp_dutch(15/12))
p24m <- data.frame(qp = qp, month = rep(24, length(qp)), p = adp_dutch(24/12))

dataprior <- rbind(p1m, p15m, p24m) |>
  mutate(month = as.factor(month)) |>
  dplyr::filter(p > 0.00001)

p <- ggplot(dataprior, aes(qp, p, group = month)) +
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1)) +
  geom_path(aes(colour = month)) +
  coord_cartesian(xlim = c(-10, 80)) +
  scale_y_continuous(name = "Density", limits = c(0, 0.3)) +
  scale_x_continuous(name = "D-score") +
  labs(colour = "Age (in months)") +
  scale_colour_manual(values = brewer.pal(12, "Paired")[c(1, 3, 5)])
p

items <- c("ddifmd011", "ddifmm012", "gtolgd010", "gtogmd046", "by3fmd061")
# items <- items[c(2, 3, 1, 4, 5)]
taus <- get_tau(items, key = "gsed2206")

dr <- data.frame(item = names(taus),
                 tau = taus,
                 David = c(1, 1, 1, 1, NA),
                 Rob = c(0, 0, 0, 0, 0))

qp <- -10:80

# David
david <- matrix(NA, nrow = length(qp), ncol = 10,
                dimnames = list(NULL, c(paste0("prior", 1:5), paste0("post", 1:5))))
data_david <- data.frame(
  age = rep(2/12, 6),
  ddifmd011 = c(NA,  1,  1,  1,  1,  1),
  ddifmm012 = c(NA, NA,  1,  1,  1,  1),
  ddicmm037 = c(NA, NA, NA,  1,  1, 1),
  ddigmm066 = c(NA, NA, NA, NA,  1,  1),
  ddigmm067 = c(NA, NA, NA, NA, NA, 1)
)

post <- dscore::dscore_posterior(data = data_david, qp = -10:80, prior_mean = ".dutch")

david[, "prior1"] <- post[1, ]
david[, c("post1", "prior2")] <- post[2, ]
david[, c("post2", "prior3")] <- post[3, ]
david[, c("post3", "prior4")] <- post[4, ]
david[, c("post4", "prior5")] <- post[5, ]
david[, "post5"] <- post[6, ]

# Rob
rob <- matrix(NA, nrow = length(qp), ncol = 10,
              dimnames = list(NULL, c(paste0("prior", 1:5), paste0("post", 1:5))))
data_rob <- data.frame(
  age = rep(2/12, 6),
  ddifmd011 = c(NA,  0,  0,  0,  0, 0),
  ddifmm012 = c(NA, NA,  0,  0,  0, 0),
  ddicmm037 = c(NA, NA, NA,  0,  0, 0),
  ddigmm066 = c(NA, NA, NA, NA,  0, 0),
  ddigmm067 = c(NA, NA, NA, NA, NA, 0)
)


post <- dscore::dscore_posterior(data = data_rob, qp = -10:80, prior_mean = ".dutch")

rob[, "prior1"] <- post[1, ]
rob[, c("post1", "prior2")] <- post[2, ]
rob[, c("post2", "prior3")] <- post[3, ]
rob[, c("post3", "prior4")] <- post[4, ]
rob[, c("post4", "prior5")] <- post[5, ]
rob[, "post5"] <- post[6, ]

# create plotting data
plotdata <- expand.grid(x = qp, item = items,
                        type = c("Prior", "Posterior"),
                        person = c("David", "Rob"))
plotdata <- cbind(plotdata, y = c(as.vector(david), as.vector(rob)))
plotdata <- plotdata |>
  dplyr::filter(y > 0.00001)
# |> filter(person == "Rob")

p <- ggplot(plotdata, aes(x = x, y = y)) +
  theme(legend.position = "bottom") +
  geom_path(aes(colour = type)) +
  facet_grid(item ~ person) +
  coord_cartesian(xlim = c(0, 60)) +
  scale_y_continuous(name = "Density", limits = c(0, 0.32)) +
  scale_x_continuous(name = "D-score") +
  labs(colour = "Type of distribution") +
  scale_colour_manual(values = brewer.pal(12, "Paired")[c(3, 4)])
p

