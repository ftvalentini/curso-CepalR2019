library(ggplot2)
library(dplyr)

n = 100
dat = tibble(
  x = runif(n, 0, 1)
  ,y = rnorm(n, 6*x, 1)
  ,z = rgamma(n, shape=2, rate=2)
)

# boxplot
(
  g_box = ggplot(dat, aes(y=z)) +
    geom_boxplot(fill = "forestgreen", alpha=0.5) +
    theme(axis.text.x=element_blank())
)
ggsave("plots_slides/img/02/boxplot.png", g_box, width=3, height=5)


# scatter plot
(
  g_scatter = ggplot(dat, aes(x,y)) + 
    geom_point() +
    geom_smooth(method="lm", se=F)
)
ggsave("plots_slides/img/02/scatter.png", g_scatter, width=5, height=4)

# histograma
(
  g_hist = ggplot(dat, aes(x=y)) + 
    geom_histogram(aes(y=..density..), bins=12
                   ,fill="forestgreen", alpha=0.5, color="black") +
    geom_density(adjust=1, size=1, color="black") +
    NULL
)
ggsave("plots_slides/img/02/hist.png", g_hist, width=7, height=4)
