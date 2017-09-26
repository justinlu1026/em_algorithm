
#### model data faithful

library(tidyverse)

p <- ggplot(faithful, aes(x = waiting)) + geom_density()
p

head(faithful)

p + 
  geom_vline(xintercept = 53, col = "red", size = 2) +
  geom_vline(xintercept = 80, col = "blue", size = 2)

library(mixtools)

#' Plot a Mixture Component
#'
#' @param x Input data
#' @param mu Mean of component
#' @param sigma Standard deviation of component
#' @param lam Mixture weight of component
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}


set.seed(1)
wait <- faithful$waiting
mixmdl <- normalmixEM(wait, k = 2)

## number of iterations = 29

data.frame(x = mixmdl$x) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, color = "black", fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                color = "red", lwd = 1.5
                ) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                color = "blue", lwd = 1.5) +
  ylab("Density")


mixmdl$sigma

mixmdl$lambda

post.df <- as.data.frame(cbind(x = mixmdl$x, mixmdl$posterior))

head(post.df, 10)

post.df %>%
  filter(x > 66, x < 68)

## no "label" have been assigned.
## Unlike k-means which assigns each data point to a cluster ("hard-label")
## mixture models provide "soft labels" - the end-user decides on what "threshold" to use

post.df %>%
  mutate(label = ifelse(comp.1 > .3, 1, 2)) %>%
  ggplot(aes(x = factor(label))) +
  geom_bar() +
  xlab("Component") +
  ylab("Number of Data Points")


devtools::session_info()

post.df %>%
  mutate(label = ifelse(comp.1 > .8, 1, 2)) %>%
  ggplot(aes(x = factor(label))) +
  geom_bar() + 
  xlab("component") +
  ylab("Number of Data Points")


#### Dymistify normalmixEM

set.seed(1)

comp1.vals <- tibble(
  comp = "A",
  vals = rnorm(50, mean = 1, sd = .5)
)

comp2.vals <- tibble(
  comp = "B",
  vals = rnorm(50, mean = 1.5, sd = .5)
)

vals.df <- bind_rows(comp1.vals, comp2.vals)
vals.df

vals.df %>%
  ggplot(aes(x = vals, y = "A", color = factor(comp))) +
  geom_point(alpha = .4) +
  scale_color_discrete(name = "Source of Data") +
  xlab("Values") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "top"
  )

vals.df %>%
  group_by(comp) %>%
  summarise(
    mean_vals = mean(vals),
    sd_vals = sd(vals)
  )

#### Parameter Estimation in the "Incomplete Data" scenario

vals.df %>%
  ggplot(aes(x = vals, y = 0)) +
  geom_point(alpha = .4) +
  xlab("Values") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

## 1, Set some initial parameter estimates on your gaussians
## 2. Assign (label) the data to one of the gaussians based on
## which one most likely generated the data
## 3. Treat the labels as being correct and then use MLE to 