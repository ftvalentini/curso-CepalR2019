library(magrittr)
library(purrr)
library(dplyr)
library(ggplot2)

semilla = 800

# base descargada de
# https://www.kaggle.com/aljarah/xAPI-Edu-Data


# notas para la clase -----------------------------------------------------

# leer documentacion de como parte arbol y RF las numericas y las categoricas
# leer ISLR de como parte arbol y RF las variables
# leer sobre variable importance
# ver que es OOB prediction error en ranger
# armar ejemplo de como funciona map_* y map2
# armar ejemplo de como funcionan tibbles con listas
# ver como obtener facilmente signo de la importancia de feature RF cra la clase

# read --------------------------------------------------------------------

f = "data/raw/lms/xAPI-Edu-Data.csv"
base = read.csv(f, stringsAsFactors=F) %>% janitor::clean_names()

dict = read.delim("resources/variables_lms.txt", sep="|", stringsAsFactors=F)

# clean -------------------------------------------------------------------

dat = base %>% 
  mutate(
    target = factor(ifelse(class %in% "L", 1, 0), levels=1:0)
  ) %>% 
  select(-class) %>% 
  select(-c(placeof_birth, national_i_ty))


# exploratorio ------------------------------------------------------------

skimr::skim(dat)

num_vars = dat %>% select_if(is.numeric) %>% names()
cat_vars = dat %>% select_if(function(x) !is.numeric(x)) %>% names()

dat_num = dat %>% select(num_vars, target)
dat_cat = dat %>% select(cat_vars)

GGally::ggpairs(dat_num, aes(color=target))

# ejercicio: hallar cuantos niveles tiene cada variable categorica
# cat_levels = dat_cat %>% map_dbl(function(x) length(unique(x)))

plot_bar = function(var) {
  ggplot(dat_cat) + 
    facet_wrap(as.formula(paste("~", var)), scales="free") + 
    geom_bar(aes(x=target, fill=target)) +
    labs(title=var) +
    NULL
}

plots = list()
for (v in names(dat_cat)) plots[[v]] = plot_bar(v)
  

# arboles de decision -----------------------------------------------------

library(rpart)
library(rpart.plot)

# fit function
fit_tree = function(data, cp=0.01, maxdepth=30, minsplit=20) {
  rpart(target ~ ., data=data, method="class", model=T
        , cp=cp, maxdepth=maxdepth, minsplit=minsplit)
}

mod = fit_tree(dat)
mod2 = fit_tree(dat, cp=0, minsplit=1)

# plot
rpart.plot(mod, cex=0.5)
rpart.plot(mod, extra=104)
rpart.plot(mod, extra=2)
rpart.plot(mod2)
# reglas
rpart.rules(mod, cover=T) %>% View()

# predict
predict_tree = function(model, newdata) {
  predict(model, newdata=newdata, type="prob")[,1]
} 

pred_3 = predict_tree(mod3, newdata=dat)
table(pred_3, dat$target)


# performance -------------------------------------------------------------

# performance
library(yardstick)
metrica_auc = function(target, prob_pred) {
  tab = data.frame(y=factor(target), prob=prob_pred)
  roc_auc(tab, truth=y, prob)$.estimate
} 
metrica_auc(dat$target, pred_3)


# data split --------------------------------------------------------------

# idealmente: train - test y CV dentro de train
# hacemos solo CV por pocos datos

# library(rsample)
# set.seed(semilla)
# tt_split = dat %>% initial_split(prop=0.8)
# dat_train = tt_split %>% training()
# dat_test = tt_split %>% testing()

library(rsample)
set.seed(semilla)
cv_split = vfold_cv(dat, v=5)
# (analisis y assessment sets)


# random forest -----------------------------------------------------------

library(recipes)
receta_rf = function(dataset) {
  recipe(target ~ ., data = dataset) %>%
    step_other(all_nominal(), -all_outcomes(), threshold=0.05)
}

library(ranger)
# fit
fit_rf = function(data, mtry=4, minsize=1, trees=500) {
  ranger(target ~ ., data=data, mtry=mtry, min.node.size=minsize, num.trees=trees
         , probability=T, importance="permutation")
}
set.seed(semilla)
mod_rf = fit_rf(data=dat, mtry=4, minsize=50)

# predict
predict_rf = function(model, newdata) {
  predict(model, data=newdata)$predictions[,1]
}
pred_rf = predict_rf(mod_rf, dat)

# train y predict para un corte de CV
train_apply_rf = function(fold_split, receta, mtry=4, minsize=1, trees=500) {
  
  # get analysis data
  dat_an = fold_split %>% analysis()
  # train receta sobre analysis data
  receta_trained = dat_an %>% receta %>% prep(retain=T)
  # get analysis preprocesado
  dat_an_prep = juice(receta_trained)
  # get assessment data
  dat_as = fold_split %>% assessment()
  # dat_as_baked = dat_as %>% receta() %>% prep() %>% bake(newdata=dat_as)
  dat_as_baked = receta_trained %>% bake(new_data=dat_as)
  # entrena modelo 
  mod = fit_rf(dat_an_prep, mtry, minsize, trees)
  # predict  
  out = tibble(
    "id" = fold_split$id$id
    ,"obs" = dat_as$target
    ,"pred" = predict_rf(mod, newdata=dat_as_baked)
  )
  return(out)  
}
train_apply_rf(cv_split$splits$`1`, receta=receta_rf)

kcv_rf = function(cv_splits, receta, mtry=4, minsize=1, trees=500) {
  map_df(
    cv_split$splits,
    function(s)
      train_apply_rf(fold_split=s, receta, mtry=mtry, minsize=minsize, trees=trees)
  )
}

library(yardstick)
kcv_auc_rf = function(tab_cv_pred) {
  tab_cv_pred %>%
    group_by(id) %>%
    roc_auc(obs, pred) %>% 
    select(id, .estimate) %>% 
    rename(auc = .estimate)
}

tab_cv_rf = kcv_rf(cv_splits, receta_rf)
kcv_auc_rf(tab_cv_rf)

# hiperparametrizacion
set.seed(semilla)
grilla = expand.grid(
  mtry = 2:(ncol(dat)-1)
  ,minsize = seq(10,100,10)
) %>% 
  slice(sample(nrow(.),50)) %>% 
  as_tibble()

kcv = grilla %>% 
  mutate(
    pred_cv = map2(
      .x=mtry, .y=minsize
      ,function(x,y) kcv_rf(cv_splits=cv_split, receta=receta_rf
                            ,mtry=x, minsize=y, trees=100)
    )
    ,auc_cv = map(pred_cv, kcv_auc_rf)
  )

resultados = kcv %>% 
    tidyr::unnest(auc_cv)

# performance por fold
resultados %>% 
  group_by(id) %>% 
  summarise(auc_media = mean(auc))

# performance por hiperparametros
resultados_sum = resultados %>% 
  group_by(mtry, minsize) %>% 
  summarise(
    m_auc = mean(auc)
    ,se = sd(auc)
    ,se1 = m_auc-se
    ) %>% 
  ungroup() %>% 
  arrange(-m_auc)

auc_min_se1 = resultados_sum %>% 
  filter(m_auc == max(m_auc)) %>% pull(se1)

resultados_sum %>% 
  filter(m_auc >= auc_min_se1) %>% 
  filter(minsize == max(minsize))

mtry_opt = 2
minsize_opt = 100

# modelo final
# se ajusta modelo con toda la data y parametros optimos
dat_prep = dat %>% receta_rf() %>% prep(retain=T) %>% juice()
set.seed(semilla)
mod_rf = fit_rf(dat_prep, mtry=2, minsize=100, trees=100)

treeInfo(mod_rf, tree = 1)

# feature importance
varimp = tibble(
  variable = names(mod_rf$variable.importance)
  ,importance = mod_rf$variable.importance
) %>% arrange(-importance)

# feature importance plot
# ggplot(varimp, aes(x=reorder(variable,importance), y=importance, fill=importance))+
#   geom_bar(stat="identity", position="dodge") +
#   coord_flip() +
#   guides(fill=F)



# ejercicio:
  # optimizar hiperparametros de rpart con CV

# ejercicio:
  # incluir el "step_other" en la hiperparametrizacion

# ejercicio:
  # obtener hiperparametros que optimizacion una funcion de ganancia ficticia
  # (incluyendo el punto de corte de la prob predicha como hiperparametro!)


# PARA TENER EN CUENTA:
# la clasificacion es la misma si la hace directo ranger
  # que si sacamos las clasficaciones de todos los arboles y clasificamos
    # segun la clase mayoritaria predicha para cada obs
# la probabilidad devuleta por ranger no es la misma que la proporcion de la clase
  # mayoritaria devuelta por ranger (pero es cercana)

# library(ranger)
# fit_rf = function(data, mtry=4, minsize=1, trees=500) {
#   ranger(target ~ ., data=data, mtry=mtry, min.node.size=minsize, num.trees=trees
#          , probability=T)
# }
# fit_rf2 = function(data, mtry=4, minsize=1, trees=500) {
#   ranger(target ~ ., data=data, mtry=mtry, min.node.size=minsize, num.trees=trees
#          , probability=F)
# }
# set.seed(semilla)
# gg = fit_rf(data=dat, mtry=4, minsize=50)
# set.seed(semilla)
# hh = fit_rf2(data=dat, mtry=4, minsize=50)
# aa = predict(gg, data=dat)
# bb = predict(hh, data=dat, predict.all=F)
# cc = predict(hh, data=dat, predict.all=T)
# aaa = aa$predictions[,1]
# bbb = bb$predictions
# rr = ifelse(cc$predictions == 2, 0, 1)
# ccc = apply(rr, 1, mean) %>% {ifelse(.>0.5, 1, 0)}
# table(bbb, ccc)
