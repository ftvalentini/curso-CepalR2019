library(dplyr)
library(ggplot2)

set.seed(666)
# muestra total
n = 1200
# proporcion train
p_train = 0.8
# variables que se agregan a cada una de las original
newx = 400
# correlacion de las nuevas vars con las originales (mas chico es mas correlacion)
new_var = 0.05

# Y se explica con x1 y x2
dat = tibble(
  x1 = runif(n,0,1)
  ,x2 = runif(n,0,1)
  ,y = rnorm(n, 1+2*x1+2*x2, 0.25)
)
# se agregan variables correlacionadas con x1 y x2
x_cor1 = replicate(newx, rnorm(n, 1 + 2*dat$x1, new_var), simplify=T) %>% as_tibble() %>% 
  setNames(paste0("xc1_",1:ncol(.)))
x_cor2 = replicate(newx, rnorm(n, 1 + 2*dat$x2, new_var), simplify=T) %>% as_tibble() %>% 
  setNames(paste0("xc2_",1:ncol(.)))
dat_exp = list(dat, x_cor1, x_cor2) %>% bind_cols()

# se parte en training y test
i_train = sample(nrow(dat_exp), p_train*nrow(dat_exp))
d_train = dat_exp[i_train,]
d_test = dat_exp[-i_train,]

# modelo simple
mod_sim = lm(y ~ x1 + x2, d_train)
pred_sim = predict(mod_sim, newdata=d_test)

# modelo con multicolinealidad
mod_col = lm(y ~ ., d_train)
pred_col = predict(mod_col, newdata=d_test)

# ridge
X = d_train %>% select(-y) %>% as.matrix()
Y = d_train$y
l_seq = exp(seq(-6,4,length.out=50))
cv_ridge = glmnet::cv.glmnet(x=X, y=Y, alpha=0, nfolds=5, lambda=l_seq)
l_opt = cv_ridge$lambda.1se
mod_ridge = glmnet::glmnet(x=X, y=Y, alpha=0, lambda=l_opt)
pred_ridge = predict(mod_ridge, newx=d_test %>% select(-y) %>% as.matrix())

# plots -------------------------------------------------------------------

gdat = tibble(
  y = d_test$y
  ,"p = 2" = pred_sim
  ,"p = 802" = pred_col
  ,ridge = as.vector(pred_ridge)
) %>% 
  tidyr::pivot_longer(-y, names_to="modelo", values_to="fitted")

# plot 1
(
  g_multi_cor = ggplot(gdat %>% dplyr::filter(!modelo %in% "ridge")
       , aes(x=fitted, y=y, color=modelo)) + 
  geom_point() +
  geom_abline() +
  scale_color_brewer(type="qual") +
  NULL
)
ggsave("plots_slides/img/03/multicol_concorr.png", plot=g_multi_cor
       ,width=6, height=4)
broom::tidy(mod_col) %>% arrange(-abs(estimate))

# plot con ridge
(
  g_multi_cor_ridge = ggplot(gdat, aes(x=fitted, y=y, color=modelo)) + 
    geom_point() +
    geom_abline() +
    scale_color_brewer(type="qual") +
    NULL
)
ggsave("plots_slides/img/03/multicol_concorr_ridge.png", plot=g_multi_cor_ridge
       ,width=6, height=4)
broom::tidy(mod_ridge) %>% arrange(-abs(estimate))

# cv lambda
png("plots_slides/img/03/lambda_cv_ridge.png", width=700, height=300)
plot(cv_ridge)
dev.off()




# broom::tidy(mod_ridge) %>% arrange(-abs(estimate))
# broom::tidy(mod_lasso) %>% arrange(-abs(estimate))


mod_sim %>% broom::tidy() %>% arrange(p.value)
mod_col %>% broom::tidy() %>% arrange(p.value)

cor(pred_sim, d_test$y)
cor(pred_col, d_test$y)

cor(dat$x1, dat$x2)



sd(pred_sim)
sd(pred_col)

cor(dat$x2, x_cor2$xc2_1)

names(x_cor1)

x_cor1 = MASS::mvrnorm(n, 0, 0.9, empirical=T)
x_cor2 = MASS::mvrnorm(n, 0, 0.9, empirical=T)



mod = lm(y ~ ., dat_exp)
plot(mod$fitted.values, mod$model$y)


MASS::mvrnorm()