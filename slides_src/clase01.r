library(ggplot2)
library(dplyr)
library(purrr)

set.seed(10)


# plots dgp ---------------------------------------------------------------


# plot function
fplot = function(data) { 
  ggplot(data, aes(x,y)) + 
    geom_point(color="navy") + 
    geom_line(aes(x,mu), color="orange", size=1) + 
    labs(x=NULL,y=NULL)
}

# plot 1
dat1 = tibble(
  x = runif(100)
  ,mu = 1*x
  ,y = rnorm(100, mu, 0.05)
)

# plot 2
dat2 = tibble(
  x = runif(100)
  ,mu = 1*x
  ,y = rnorm(100, mu, 0.15)
)

# plot 3
dat3 = tibble(
  x = runif(100,-3,3)
  ,mu = x + 0.5*x**2 + 0.5*x**3
  ,y = rnorm(100, mu, 1.5)
)

# plot 4
dat4 = tibble(
  x = runif(100,-3,3)
  ,mu = x + 0.5*x**2 + 0.5*x**3
  ,y = rnorm(100, mu, 10)
)

plist = list(dat1,dat2,dat3,dat4) %>% map(fplot)
for (i in 1:4) {
  ggsave(paste0("plots_slides/img/",i,".png"), plot=plist[[i]], width=5, height=5)
}





# plots estimacion --------------------------------------------------------

# plot function
kplot = function(data, k, plot_mu=F) { 
  mod = kknn::kknn(y ~ x, data, data, k=k)
  data$fitted = mod$fitted.values
  g = ggplot(data, aes(x,y)) + 
    geom_point(color="navy") + 
    geom_line(aes(x,fitted), color="green", size=1) + 
    labs(x=NULL,y=NULL)
  if (!plot_mu) {
    return(g)
  } else {
    return(g + geom_line(aes(x,mu), color="orange", size=1)) 
  }
}

dat = tibble(
  x = runif(300,-5,5)
  ,mu = x + 0.5*x**3
  ,y = rnorm(300, mu, 15)
)

klist = list(
  kplot(dat,1)
  ,kplot(dat,5)
  ,kplot(dat,50, plot_mu=T)
  ,kplot(dat,250)
)
ggsave("plots_slides/fit_1.png", plot=klist[[1]], width=5, height=5)
ggsave("plots_slides/fit_5.png", plot=klist[[2]], width=5, height=5)
ggsave("plots_slides/fit_50.png", plot=klist[[3]], width=5, height=5)
ggsave("plots_slides/fit_250.png", plot=klist[[4]], width=5, height=5)




plist = list(dat1,dat2,dat3,dat4) %>% map(fplot)
for (i in 1:4) {
  ggsave(paste0("plots_slides/",i,".png"), plot=plist[[i]], width=5, height=5)
}

