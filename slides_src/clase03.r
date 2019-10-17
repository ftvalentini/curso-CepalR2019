library(ggplot2)
library(dplyr)

set.seed(88)

n = 150
dat = tibble(
  x = runif(n)
  ,z = runif(n)
  ,mu = 1*x
  ,cat = sample(letters[1:3],n,rep=T)
  ,y = rnorm(n, mu, 0.15)
) %>% 
  mutate(y_fitted = lm(y ~ x, data=.)$fitted.values)

# DGP
(
  g_dgp = ggplot(dat, aes(x,y)) + 
    geom_line(aes(x,mu), color="orange", size=1) + 
    geom_point(color="navy") + 
    labs(x=NULL,y=NULL)
)
ggsave("plots_slides/img/03/dgp_lineal.png", plot=g_dgp, width=5, height=4)

# MCO
(
  g_mco = ggplot(dat, aes(x,y)) + 
    geom_smooth(color="forestgreen", method="lm", se=F, size=1) + 
    geom_point(color="navy") + 
    geom_segment(aes(x=x, xend=x, y=y, yend=y_fitted), color="forestgreen") +
    labs(x=NULL,y=NULL) +
    NULL
)
ggsave("plots_slides/img/03/mco_lineal.png", plot=g_mco, width=5, height=4)

# regresion
mod = lm(y ~ x+z, dat)
summary(mod)

# sct
(
  g_sct = ggplot(dat, aes(x,y)) + 
    geom_hline(aes(yintercept=mean(y)), color="red", size=1, linetype="dashed") +
    geom_segment(aes(x=x, xend=x, y=y, yend=mean(y)), color="red", size=0.1) +
    geom_point(color="navy") + 
    labs(x=NULL,y=NULL) +
    NULL
)
ggsave("plots_slides/img/03/mco_sct.png", plot=g_sct, width=5, height=4)

# categoricas
dat_cat = dat %>% 
  mutate(mu = case_when(
    cat=="a" ~  0 + 1*x
    ,cat=="b" ~ 0.05 + 1*x
    ,cat=="c" ~ 0.5 + 1*x
  )) %>% 
  mutate(y = rnorm(n, mu, 0.15))
(
  g_cat = ggplot(dat_cat, aes(x, y, color=cat)) + 
    geom_line(aes(y=mu), size=0.7) +
    geom_point() + 
    labs(x=NULL,y=NULL) +
    NULL
  
)
ggsave("plots_slides/img/03/mco_dummies.png", plot=g_cat, width=6, height=4)
mod = lm(y ~ x + cat, dat_cat)
summary(mod)

# interacciones
dat_int = dat %>% 
  mutate(mu = case_when(
    cat=="a" ~  0 + 2*x
    ,cat=="b" ~ -0.5 + 1*x
  )) %>% 
  mutate(y = rnorm(n, mu, 0.15)) %>% 
  dplyr::filter(!cat=="c")
(
  g_int = ggplot(dat_int, aes(x, y, color=cat)) + 
    geom_line(aes(y=mu), size=0.7) +
    geom_point() + 
    labs(x=NULL,y=NULL) +
    NULL
)
ggsave("plots_slides/img/03/mco_interacciones.png", plot=g_int, width=6, height=4)
mod = lm(y ~ x + cat + x:cat, dat_int)
summary(mod)

# cuadratica
dat_cuad = dat %>% 
  mutate(
    mu =  1*x + 3*(x**2)
    ,y = rnorm(n, mu, 0.20)
  )
(
  g_cuad = ggplot(dat_cuad, aes(x, y)) + 
    geom_line(aes(y=mu), size=1, color="orange") +
    geom_smooth(se=F, method="lm", color="forestgreen") +
    geom_point() + 
    labs(x=NULL,y=NULL) +
    NULL
  
)
ggsave("plots_slides/img/03/mco_polinomica.png", plot=g_cuad, width=6, height=4)
mod = lm(y ~ poly(x,2), dat_cuad)
summary(mod)

# logaritmos
dat_log = dat %>% 
  mutate(
    x =  runif(n,5,10)
    ,mu =  1*log(x)
    ,y = log(rnorm(n, mu, 0.15))
  )
(
  g_log_bad = ggplot(dat_log, aes(exp(x), exp(y))) + 
    geom_point() + 
    geom_smooth(se=F, method="lm", color="forestgreen") +
    labs(x="x",y="y") +
    NULL
  
)
(
  g_log_good = ggplot(dat_log, aes(x, y)) + 
    geom_point() + 
    geom_smooth(se=F, method="lm", color="forestgreen") +
    labs(x="log(x)",y="log(y)") +
    NULL
)
ggsave("plots_slides/img/03/mco_log_mal.png", plot=g_log_bad, width=5, height=4)
ggsave("plots_slides/img/03/mco_log_bien.png", plot=g_log_good, width=5, height=4)

# residual plots
dat_log_sq = dat %>% 
  mutate(
    y = exp(rnorm(n, 2*x + 1*x**2, 0.25))
    ,fitted = lm(y ~ x, dat_log_sq) %>% fitted()
    ,residuals = lm(y ~ x, dat_log_sq) %>% residuals()
    ,fitted_ok = lm(log(y) ~ x + I(x**2), dat_log_sq) %>% fitted()
    ,residuals_ok = lm(log(y) ~ x + I(x**2), dat_log_sq) %>% residuals()
  )
(
  g_resid_bad = ggplot(dat_log_sq, aes(fitted, residuals)) + 
    geom_point() + 
    geom_smooth(se=F, method="lm", linetype="dashed", color="black") +
    geom_smooth(se=F) +
    NULL
)
(
  g_resid_good = ggplot(dat_log_sq, aes(fitted_ok, residuals_ok)) + 
    geom_point() + 
    geom_smooth(se=F, method="lm", linetype="dashed", color="black") +
    geom_smooth(se=F) +
    NULL
)
ggsave("plots_slides/img/03/resid_mal.png", plot=g_resid_bad, width=4, height=4)
ggsave("plots_slides/img/03/resid_bien.png", plot=g_resid_good, width=4, height=4)



