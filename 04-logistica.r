library(magrittr)
library(purrr)
library(dplyr)
library(ggplot2)

source("helpers_eph.r")

# descargar las bases de 
# https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos
# y unzip
# from cmd:
  # cd data\raw\ 
  # unzip \*.zip

semilla = 150

# read --------------------------------------------------------------------

# nombres de archivos eph
files = c(
   "data/raw/eph/usu_individual_t119.txt"
  ,"data/raw/eph/usu_individual_t418.txt"
  ,"data/raw/eph/usu_individual_T318.txt"
  ,"data/raw/eph/usu_individual_t218.txt"
  ,"data/raw/eph/usu_individual_t118.txt"
  ,"data/raw/eph/usu_individual_T417.txt"
  ,"data/raw/eph/usu_individual_t317.txt"
  ,"data/raw/eph/usu_individual_t217.txt"
  ,"data/raw/eph/usu_individual_t117.txt"
)

# lee cada archivo iterando
raws = files %>% map(read_eph)
# nombre de cada elemento
names(raws) = files

# read emae
emae_ia = readRDS("data/working/emae_ia.rds")

# clean -------------------------------------------------------------------

# append de las tablas
indiv = raws %>% bind_rows()

# clean
indiv = clean_eph(indiv)

# crea periodo_ord (periodo ordinal)
indiv = indiv %>% 
  mutate(
    periodo_ord = match(periodo, sort(unique(periodo)))
    # periodo_ord = as.numeric(factor(id_temp))
  ) 

# ids-periodo con un trimestre de diferencia
obs = indiv %>% select(id_indiv, periodo_ord, estado) 
comp = indiv %>% select(id_indiv, periodo_ord, estado)
temp = full_join(obs, comp, by="id_indiv", suffix=c("_obs","_comp")) %>%
  arrange(id_indiv, periodo_ord_obs, periodo_ord_comp) %>% 
  mutate(dif = periodo_ord_comp - periodo_ord_obs) %>% 
  select(id_indiv, periodo_ord_obs, periodo_ord_comp, everything()) %>% 
  dplyr::filter(dif==1)
# ids-periodo ocupados en primer periodo
temp_f = temp %>% filter(estado_obs==1)
# crea target (salto al desempleo-inactividad en 1 trimestre)
temp_f = temp_f %>%
  mutate(target = ifelse(estado_comp %in% c(2,3), 1, 0))

# conservamos IDs unicos (eliminamos periodos al azar por ID)
set.seed(semilla)
dat_target = temp_f %>% 
  group_by(id_indiv) %>% 
  slice(sample(n(), 1)) %>% 
  ungroup() %>% 
  select(id_indiv, periodo_ord_obs, target) %>% 
  rename(periodo_ord = periodo_ord_obs)

# join de la muestra final con atributos
# elimina atributos inutiles
dat = dat_target %>% 
  left_join(indiv, by=c("id_indiv","periodo_ord")) %>% 
  left_join(emae_ia, by="periodo") %>% 
  select(-c(
    id_indiv
    ,anio, trimestre, periodo, periodo_ord
    , estado
  ))

# remove objetos pesados inutiles
rm(raws, indiv)

# exploratorio ------------------------------------------------------------

# table(dat$periodo_ord)
# table(indiv$periodo_ord)
# table(dat$target) %>% prop.table()
# table(dat$target, dat$periodo) %>% prop.table(margin=2)

# ggpairs segun target
# skimr


# lineal vs logistic ---------------------------------------------------------

# creo que mejor sacarlo!

# mod_lineal = lm(target ~ . , dat)

# gdat_lineal = broom::augment(mod_lineal)
# ggplot(gdat_lineal) +
#   geom_point(aes(x=p21, y=target, color=factor(target))) +
#   geom_point(aes(x=p21, y=.fitted)) +
#   # geom_smooth(aes(x=.fitted, y=target), method="lm", se=F) +
#   NULL

# train - test split ------------------------------------------------------------

library(rsample)
set.seed(semilla)
tt_split = dat %>% initial_split(prop=0.8)
dat_train = tt_split %>% training()
dat_test = tt_split %>% testing()

# modelo simple --------------------------------------------------------

# step_ZV porque quedan constantes en las dummies de factores que estan en indiv 
  # pero no en dat

# receta
library(recipes)
receta_simple = function(dataset) {
  recipe(target ~ ., data=dataset) %>%
    step_medianimpute(p21) %>% 
    step_log(pp03d, pp3e_tot, t_vi, p21, signed=T) %>% 
    step_other(all_nominal(), threshold=0.05, other="otros") %>%
    step_dummy(all_nominal(), one_hot=F) %>% 
    step_zv(all_predictors()) %>% 
    step_interact(terms = ~ emae:.)
}

# fit 
fit_logistic = function(data, weighted=F, w=NULL) {
  if (weighted) {
    pesos = ifelse(data$target==0, w, 1)
    glm(target ~ ., data=data, family="binomial", weights=pesos)  
  } else {
    glm(target ~ ., data=data, family="binomial")  
  }
}

# predict
pred_logistic = function(model, newdata) {
  newdata_prep = newdata %>% receta_simple() %>% prep() %>% bake(new_data=newdata)
  # newdata_prep = receta_trained %>% bake(new_data=dat_test)
  predict(model, newdata=newdata_prep, type="response") 
}

# performance
library(yardstick)
metrica_auc = function(target, prob) {
  tab = data.frame(y=factor(target), prob=prob)
  roc_auc(tab, truth=y, prob)$.estimate
} 

# plot
plot_roc = function(dat_obs_pred) {
  dat = dat_obs_pred %>% 
    pivot_longer(-obs, names_to="modelo", values_to="prob")
  dat %>% 
    group_by(modelo) %>% 
    roc_curve(obs, prob) %>% 
    autoplot()
}
plot_violin = function(dat_obs_pred) {
  dat = dat_obs_pred %>% 
    pivot_longer(-obs, names_to="modelo", values_to="prob")
  ggplot(dat, aes(x=obs, y=prob, group=obs, fill=obs)) +
    facet_wrap(~modelo) +
    geom_violin() +
    theme_bw() +
    guides(fill=FALSE) +
    NULL
}

# cutoff metrics
metricas_cutoff = function(cutoff, obs, pred) {
  tab = data.frame(obs=factor(obs, levels=1:0)
                   ,pred=factor(ifelse(pred>cutoff, 1, 0), levels=1:0))
  cm = conf_mat(tab, obs, pred)
  metricas = summary(cm) %>% 
    dplyr::filter(
      .metric %in% c("accuracy","sens","spec","precision","recall")
    ) %>% 
    select(.metric, .estimate) %>% 
    rename(metrica=.metric, valor=.estimate) %>% 
    mutate(cutoff = cutoff)
  return(metricas)
}
# plot metricas cutoff
metricas_plot = function(obs, pred) {
  gdat = seq(0.01,0.98,0.01) %>% map_dfr(function(c) metricas_cutoff(c, obs, pred))
  ggplot(gdat, aes(x=cutoff, y=valor, color=metrica)) +
    geom_line(size=1) +
    theme_bw() +
    NULL
}
# get optimal cutoff por metrica
opt_cutoff = function(obs, pred) {
  dat = seq(0.01,0.98,0.01) %>% map_dfr(function(c) metricas_cutoff(c, obs, pred))
  dat %>% 
    dplyr::filter(metrica %in% c("accuracy","precision")) %>% 
    group_by(metrica) %>% 
    dplyr::filter(valor==max(valor, na.rm=T)) %>% 
    ungroup()
}

# REVISAR COMO Y CUANDO SE USAN LOS PESOS

# PROCESO
dat_train_prep = receta_simple(dat_train) %>% prep(retain=T) %>% juice()
mod_l = fit_logistic(dat_train_prep, weighted=F)
pred = pred_logistic(mod_l, dat_test)
auc = metrica_auc(dat_test$target, pred)

dat_test_prep = dat_test %>% receta_simple() %>% prep() %>% bake(new_data=dat_test)
tab = tibble(
  obs = as.factor(dat_test_prep$target)
  ,logistic = pred
  ,logistic_reg = pred_reg
)
plot_roc(tab)
plot_violin(tab)
metricas_plot(dat_test_prep$target, pred)
opt_cutoff(dat_test_prep$target, pred)
tab2 = data.frame(obs=factor(dat_test_prep$target, levels=1:0)
                 ,pred=factor(ifelse(pred>0.82, 1, 0), levels=1:0))
conf_mat(tab2, obs, pred)





dat_test %>% 
  mutate(pp = pred) %>% 
  group_by(aglomerado) %>% 
  summarise(pm = mean(pp)) %>% 
  arrange(-pm)

dat_test %>% 
  group_by(aglomerado) %>% 
  summarise(pm = mean(target)) %>% 
  arrange(-pm)

# BAJAR LA EPH MAS RECIENTE PARA APLICAR EL MODELO!


cv = glmnet::cv.glmnet(x = dat_train_prep %>% select(-target) %>% as.matrix()
          ,y = dat_train_prep$target, family="binomial"
          ,lambda=exp(seq(-3,-20,length.out=50))
          ,alpha=0
          ,nfolds=5)
plot(cv)

receta_trained = receta_simple(dat_train) %>% prep()
dat_test_prep = receta_trained %>% bake(new_data=dat_test)
pred_reg = predict(cv$glmnet.fit
                   ,newx = dat_test_prep %>% select(-target) %>% as.matrix()
                   ,s = cv$lambda.1se, type="response") %>% as.vector() 
auc = auc(dat_test_prep$target, pred_reg)
tab = tibble(
  obs = as.factor(dat_test$target)
  ,logistic = pred
  ,logistic2 = runif(length(pred), 0, 1)
)
plot_roc(tab)
plot_violin(tab)
  


mm = lm(target ~ ., dat_train_prep)
broom::tidy(mm) %>% arrange(p.value) %>% print(n=100)
broom::tidy(mm) %>% filter(term=="emae")


broom::tidy(mod_l) %>% tail(100) %>% print(n=100)
broom::glance(mod_l)


receta_compleja = function(dataset) {
  recipe(target ~ ., data=dataset) %>%
    step_knnimpute(p21, neighbors=10) %>% 
    step_log(pp03d, pp3e_tot, t_vi, p21, signed=T) %>% 
    step_other(all_nominal(), threshold=0.01, other="otros") %>%
    step_dummy(all_nominal(), one_hot=F)  
}

siglog = function(x) ifelse(x<1, 0, log(x))
receta_reg = function(dataset) {
  recipe(target ~ ., data=dataset) %>%
    step_knnimpute(p21, neighbors=10) %>% 
    step_mutate_at(all_numeric(),-all_outcomes()
                   ,fn = list(log=siglog, sqrt=sqrt, sq=function(x) x**2)) %>%
    step_dummy(all_nominal(), one_hot=F)  
}


receta_compleja = function(dataset) {
  recipe(target ~ ., data=dataset) %>%
    step_corr(all_numeric(), -all_outcomes(), threshold=0.9) %>%
    step_nzv(all_predictors()) %>%
    step_other(all_nominal(), threshold=0.01, other="otros") %>%
    step_dummy(all_nominal(), one_hot=F)  
}


receta_atributos = function(dataset) {
  recipe(target ~ ., data=dataset) %>%
    step_mutate_at(all_numeric(),-all_outcomes()
                   ,fn = list(log=siglog, sqrt=sqrt, sq=function(x) x**2)) %>% 
    step_dummy(all_nominal(), one_hot=F)  
}


dat_train_prep = receta_simple(dat_train) %>% prep()
dat_train_prep = receta_simple(dat_train) %>% prep(retain=T)


%>% juice()


dat %>% map_dbl(function(x) mean(is.na(x)))


dat_train_prep$p21
dat_train$p21



dat_bin = recipe(target ~ ., data=dat) %>% 
  step_dummy(all_nominal()) %>% 
  prep(retain=T) %>% juice() %>% 
  select(-p21)
lc = caret::findLinearCombos(dat_bin)

for (i in seq_along(lc$linearCombos)) {
  print(dat_bin[lc$linearCombos[[i]]] %>% names())
}

dat_bin[lc$remove] %>% names()

indiv$ch07 %>% table()
indiv$cat_ocup %>% table()
dat$ch16 %>% table()
indiv$ch16 %>% table()


mod = lm(target ~ ., data=dat_bin)
lc2 = alias(mod)


# las transiciones al desempleo depende mucho del tiempo/variacion de actividad!!
  # dependen poc de caracs de los indivs
  # o depende de como activ interactua con carac de los indivs
# el problema es que hay que incluir evolucion (previa?) del nivel de activ
# y es dificil incluir atributos historicos:
  # (depende de cuando fue la ultima vez que se observo cada indiv, y
    # es muy variable)
  

# se puede incluir como regresion la variacion interanual del emae en el futuro?
  # (por ej en el proximo trimestre)
  # y armar escenarios de actividad cuando hay que predecir
# como incluir atributos historicos 
# incluir interacciones de nivel de actividad por las variables?


wide = indiv_f %>% 
  arrange(t) %>% 
  tidyr::pivot_wider(id_cols=id_indiv, names_from=t, names_prefix="t_"
                     , values_from=
                       c(ch07
                         ,ch08
                         ,pp03d
                         ,pp3e_tot
                         ,intensi
                         ,pp03i
                         ,cat_ocup
                         ,ayuda
                         ,t_vi
                         ,p21
                       ))
wide

indiv_f %>% 
  group_by(id_indiv) %>% 
  summarise()

indiv_f$t %>% table()



# atributos con variabilidad posible en el tiempo
ch07,situacion conyugal
ch08,tipo de cobertura medica
pp03d,cantidad de ocupaciones
pp3e_tot,horas trabajadas
intensi,intensidad del trabajo
pp03i,busqueda de mayor cantidad de horas
cat_ocup,categoria ocupacional
t_vi,monto total de ingreso no laboral
p21,ingreso ocupacion principal
ayuda



# COMO SE ENTIENDEN CASOS CON periodo=2 (sin periodo=1) Y CON periodo=5 ???
# tal vez no necesariamente se cumple el 2-2-2
# estado_long %>% 
#   group_by(id_indiv) %>% 
#   dplyr::filter(any(t %in% 3) & any(periodo %in% 2)) %>% 
#   # dplyr::filter(n()>2) %>% 
#   ungroup()



# EXPLORATORIO ANTES DE MODELAR:
  # con GGally::ggcorr()
  # con skimr


# ejercicio 1: modificar la lectura y limpieza de los archivos para que
# no se lea variables_eph.csv cada vez que se importa




# OLD ---------------------------------------------------------------------

# UNA FORMA DE EVITARLO: DFRAME INTERMEDIO:
# temp = dat_temp %>% 
#   select(id_indiv, starts_with("temp_"), -temp_0) %>% 
#   filter_at(vars(starts_with("temp_")), any_vars(.==2)) %>% 
#   mutate(clase = 1)
# dat = dat_temp %>% 
#   left_join(temp, by="id_indiv") %>% 
#   dplyr::filter(temp_0 %in% 1) %>%
#   mutate(clase = ifelse(is.na(clase), 0, clase))


# crea t (punto de obs de cada individuo (0,1,2,...,n))
# %>% 
# group_by(id_indiv) %>% 
# mutate(t = periodo_ord - min(periodo_ord)) %>%
# ungroup()  

# indiv$periodo %>% table()
# indiv$periodo_ord %>% table()
# table(indiv$periodo_ord, indiv$periodo)

# ENTENDER ESTO:
# table(indiv$t, indiv$periodo)

# filter: ocupados en t=0
# filter: individuos que son observados en el trimestre siguiente
# id_keep = indiv %>% filter(t==0 & estado==1) %>% pull(id_indiv)
# indiv_f = indiv %>% 
#   filter(id_indiv %in% id_keep) %>% 
#   group_by(id_indiv) %>% 
#   dplyr::filter(1 %in% t) %>% 
#   ungroup()
# 
# 
# pp = indiv %>% select(id_indiv, periodo_ord, estado) 
# dd = indiv %>% select(id_indiv, periodo_ord, estado)
# 
# xx = full_join(pp, dd, by="id_indiv") %>%
#   arrange(id_indiv, periodo_ord.x, periodo_ord.y) %>% 
#   mutate(dif = periodo_ord.y - periodo_ord.x)
# 
# filter(xx, dif==1)
# 
# View(head(xx,100))
# 
# estado_wide = indiv %>% 
#   arrange(t, id_indiv, periodo) %>% 
#   tidyr::pivot_wider(id_cols=c(id_indiv,periodo), names_from=t
#                      ,names_prefix="estado_"
#                      ,values_from=estado) 
# 
# %>% 
#   mutate(target = ifelse(estado_1 %in% c(2,3), 1, 0))
# 
# 
# # indiv_f %>%
# #   select(id_indiv, periodo, t, estado) %>%
# #   arrange(id_indiv, periodo, t) %>% print(n=100)
# 
# # tienen que quedar balanceados los periodos?  
# # table(indiv_f$periodo)
# # table(indiv_f$t)
# # table(indiv_f$t, indiv_f$periodo)
# 
# # estado en cada periodo de cada id (formato wide)
# # creamos target (caida en desempleo/inactividad en el trimestre siguiente)
# # MARCAMOS CAIDA EN INACTIVIDAD?
# estado_wide = indiv_f %>% 
#   arrange(t, id_indiv) %>% 
#   tidyr::pivot_wider(id_cols=id_indiv, names_from=t, names_prefix="estado_"
#                      , values_from=estado) %>% 
#   mutate(target = ifelse(estado_1 %in% c(2,3), 1, 0))
# 
# # join con primer periodo de cada id de la base
# dat_temp = indiv_f %>% 
#   group_by(id_indiv) %>% 
#   dplyr::filter(periodo == min(periodo)) %>% 
#   ungroup() %>% 
#   left_join(estado_wide %>% select(id_indiv, target), by="id_indiv")

# # filter: individuos ocupados en temp_0
# # genera clase (VER COMO HACERLO SIN TENER QUE ESCRIBIR TODOS LOS PERIODOS!)
# dat = dat_temp %>% 
#   dplyr::filter(temp_0 %in% 1) %>% 
#   mutate(clase = ifelse(
#     temp_1 %in% 2 |
#       # temp_2 %in% 2 |
#       temp_3 %in% 2 |
#       temp_4 %in% 2 |
#       temp_5 %in% 2 
#     , 1, 0))