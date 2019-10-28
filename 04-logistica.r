library(magrittr)
library(purrr)
library(dplyr)
library(ggplot2)

semilla = 150

# bases descargadas de
# https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

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

# diferencia de entre distintos puntos de observacion para cada id
obs = indiv %>% select(id_indiv, periodo_ord, estado) 
comp = indiv %>% select(id_indiv, periodo_ord, estado)
temp = full_join(obs, comp, by="id_indiv", suffix=c("_obs","_comp")) %>%
  arrange(id_indiv, periodo_ord_obs, periodo_ord_comp) %>% 
  mutate(dif = periodo_ord_comp - periodo_ord_obs) %>% 
  select(id_indiv, periodo_ord_obs, periodo_ord_comp, everything()) %>% 
  dplyr::filter(dif==1)
# ids-periodo ocupados en primer periodo
temp_ocup = temp %>% filter(estado_obs==1)
# crea target (salto al desempleo-inactividad en 1 trimestre)
temp_ocup = temp_ocup %>%
  mutate(target = ifelse(estado_comp %in% c(2,3), 1, 0))

# conservamos IDs unicos (eliminamos periodos al azar por ID)
set.seed(semilla)
dat_target = temp_ocup %>% 
  group_by(id_indiv) %>% 
  slice(sample(n(), 1)) %>% 
  ungroup() %>% 
  select(id_indiv, periodo_ord_obs, target) %>% 
  rename(periodo_ord = periodo_ord_obs)

# join de la muestra final con atributos y con emae
# elimina atributos inutiles
dat = dat_target %>% 
  left_join(indiv, by=c("id_indiv","periodo_ord")) %>% 
  left_join(emae_ia, by="periodo") %>% 
  select(-c(
    id_indiv
    ,anio, trimestre, periodo, periodo_ord
    ,estado
  ))

# exploratorio ------------------------------------------------------------

# ejercicio:
  # tablas de frecuencia de:
  # (a) periodos en la muestra final
  # (b) casos positivos en la muestra final
  # (c) casos positivos por periodo en la muestra final

# bonus track:
# temp = dat %>% select(target, pp03i, ch06, ch04) %>% 
#   mutate(target = as.factor(target))
# GGally::ggpairs(temp, aes(color=target))

# bonus track:
# expl = skimr::skim(dat)

# train - test split ------------------------------------------------------------

library(rsample)
set.seed(semilla)
tt_split = dat %>% initial_split(prop=0.8)
dat_train = tt_split %>% training()
dat_test = tt_split %>% testing()


# preproc -----------------------------------------------------------------

# receta
library(recipes)
receta_simple = function(dataset) {
  recipe(target ~ ., data=dataset) %>%
    step_medianimpute(all_numeric(), -all_outcomes()) %>%
    step_log(pp03d, pp3e_tot, t_vi, p21, signed=T) %>% 
    step_other(all_nominal(), threshold=0.025, other="otros") %>%
    step_dummy(all_nominal(), one_hot=F) %>% 
    step_interact(terms = ~ emae:all_predictors()) %>%
    step_zv(all_predictors())
}

receta_trained = receta_simple(dat_train) %>% prep(retain=T)
# receta_trained
# tidy(receta_trained)
# tidy(receta_trained, number=1)
# tidy(receta_trained, number=3) %>% arrange(terms)
# tidy(receta_trained, number=6)

dat_train_prep = receta_trained %>% juice()

# fit ---------------------------------------------------------------------

fit_logistic = function(data) {
  glm(target ~ . - pondera, data=data, family="binomial")  
}

mod_l = fit_logistic(dat_train_prep)

# library(margins)
# margins(mod_l)

summary(mod_l)
broom::tidy(mod_l) %>% arrange(p.value)

# predict -----------------------------------------------------------------

pred_logistic = function(model, newdata, receta) {
  # newdata_prep = newdata %>% receta_simple() %>% prep() %>% bake(new_data=newdata)
  newdata_prep = receta %>% bake(new_data=newdata)
  predict(model, newdata=newdata_prep, type="response") 
}

# predict en holdout
pred = pred_logistic(mod_l, dat_test, receta_trained)


# overall performance ------------------------------------------------------

library(yardstick)

# auroc
metrica_auc = function(target, prob) {
  tab = data.frame(y=factor(target), prob=prob)
  roc_auc(tab, truth=y, prob)$.estimate
} 
metrica_auc(dat_test$target, pred)

# hosmer-lemeshow
tabla_hl = function(target, prob) {
  tab = data.frame(prob, target) %>% 
    mutate(intervalo = cut_interval(prob, n=10)) %>%
    # mutate(intervalo = Hmisc::cut2(prob, seq(0,1,by=0.1))) %>% 
    group_by(intervalo) %>% 
    summarise(
      prob_pred = mean(prob)
      ,prob_obs = mean(target)
      ,n_1 = sum(target)
      ,n_tot = n()
    )
  return(tab)
} 
tabla_hl(dat_test$target, pred)

# overall performance plots -------------------------------------------------

# ejercicio: hesmer-lemeshow plot 

# curva ROC
plot_roc = function(tab_obs_pred) {
  dat = tab_obs_pred %>% 
    pivot_longer(-obs, names_to="modelo", values_to="prob")
  dat %>% 
    group_by(modelo) %>% 
    roc_curve(obs, prob) %>% 
    autoplot()
}
tab = tibble(
  obs = as.factor(dat_test$target)
  ,logistic = pred
  ,azar = runif(nrow(dat_test), 0, 1)
)
plot_roc(tab)

# violin plot
plot_violin = function(tab_obs_pred) {
  dat = tab_obs_pred %>% 
    pivot_longer(-obs, names_to="modelo", values_to="prob")
  ggplot(dat, aes(x=obs, y=prob, group=obs, fill=obs)) +
    facet_wrap(~modelo) +
    geom_violin() +
    theme_bw() +
    guides(fill=FALSE) +
    NULL
}
plot_violin(tab)


# cutoff performance ------------------------------------------------------

metricas_cutoff = function(cutoff, obs, pred) {
  tab = data.frame(
    obs=factor(obs, levels=c(1,0))
    ,pred=factor(ifelse(pred>cutoff, 1, 0), levels=c(1,0))
  ) %>% 
    mutate(
      ganancia = case_when(
        pred == 1 & obs == 1 ~ 1000 - 100
        ,pred == 1 & obs == 0 ~ 0 - 100
        ,pred == 0 & obs == 0 ~ 0 - 0
        ,pred == 0 & obs == 1 ~ -1000
      )
    )
  cm = conf_mat(tab, obs, pred)
  metricas = summary(cm) %>% 
    dplyr::filter(
      .metric %in% c("accuracy","sens","spec","precision")
    ) %>% 
    rename(metrica=.metric, tipo=.estimator, valor=.estimate) %>% 
    rbind(data.frame(metrica="ganancia", tipo="custom", valor=sum(tab$ganancia))) %>% 
    mutate(cutoff = cutoff)
  return(list(cm = cm, metricas = metricas))
}

metricas_cutoff(0.5, dat_test$target, pred)

# plot para todos los cutoff
metricas_plot = function(obs, pred) {
  gdat = seq(0.01,0.98,0.01) %>%
    map_dfr(function(c) metricas_cutoff(c, obs, pred)$metricas)
  ggplot(gdat, aes(x=cutoff, y=valor, color=metrica)) +
    facet_wrap(~tipo, nrow=2, scales="free") +
    geom_line(size=1) +
    theme_bw() +
    NULL
}
metricas_plot(dat_test$target, pred)


# ejercicio:
  # identificar cutoff optimo para cada metrica


# ejercicios --------------------------------------------------------------

# ejercicio: 
  # aplicar el modelo a la eph mas reciente

# ejercicio:
  # probar si sirve incluir al emae y sus interacciones

# ejercicio: modificar la lectura y limpieza de los archivos para que
  # no se lea variables_eph.csv cada vez que se importa
  