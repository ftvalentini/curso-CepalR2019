library(magrittr)
library(janitor)
library(purrr)
library(dplyr)
library(stringr)
library(broom)
library(ggplot2)
library(ggfortify)
library(car)
library(caret)
library(glmnet)

library(rsample)
library(recipes)
library(parsnip)
library(yardstick)


"%+%" = function(a,b) paste(a,b,sep="")

# descargar las bases de 
# https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

# TODO --------------------------------------------------------------------

# REVISAR QUE ES IMPUTA!!!
# GENERAR UN ID_TOT?
# VER SI DEJAR LETRA Y CARAC_OCUP
# VER SI HACER EL CLEAN DATA EN UNA BASE APARTE Y LEVANTAR LA BASE LIMPIA
# preparar comentario breve de comparaciones multiples
# (step_log y step_sqrt pisan las variables -- no generan nuevas
  # ver si se puede resolver dentro de recipes!
# dejar o sacar step_corr? lasso se banca correlaciones?
  # ademas saca variables obvias (las transformaciones creadas) sin
    # mirar la relacion con Y
# SIRVE COR(PRED,OBS) en el resto de los modelos (tipo lasso)?
# mse se calcula en log o en escala original?
# mostrar la forma mala de hacer CV?
# chequear que funcione seleccion de lambda en parsnip!!!
# - sobre step_other
  # a priori parece no tener sentido porque lasso ya descarta
  # categorias no informativas
  # ademas si no se usa, no hay inconsistencias en las dummies de train-test
  # porque al ser factores, se generan todas las dummies
  # ENTONCES PORQUE LO HACEMOS???
# comentario importante:
  # ejercicio supone que
    # (1) DGP es el mismo para los que responden y los que tienen dato faltante
    # (2) los que no responden tienen una distribucion de Y similar (X no importa) -->
        # VER TWIT WSE!

# para ver correlacion entre categoricas y p21:
  # ANOVA y Kruskal-Walis (explicar brevemente)

# notas de Lasso:
  # Lasso can be use to make a inference on predictors. Simplest way is to bootstrap
    # it and count how many times each variable is selected,
    # divide by number of resamples, and you have your p-values
  # ridge es mejor para incluir interacciones porque:
    # if you do not have too many predictors, you might consider ridge regression 
    # instead, which will return coefficients for all variables that may be much 
    # less dependent on the vagaries of your particular data sample.
  # no es sencillo incluir interacciones en lasso asi nomas:
    # ver Learning Interactions via Hierarchical Group-Lasso Regularization 
      # (Lim y Hastie, 2015)
  # en general la incorporacion de no linealidades como interacciones 
    # se resuelve con metodos mas flexibles y menos parametricos:
        # arboles y random-forest
        # SVM
        # projection pursuit regression
        # NN


# read data ---------------------------------------------------------------

# read lista variables relevantes eph
vars_keep = read.delim("resources/variables_eph.csv", sep=",", stringsAsFactors=F
                       ,encoding="UTF-8")
# vector para leer correctamente las columnas
col_types = vars_keep$tipo %>% setNames(toupper(vars_keep$variable))

# file path
file_raw = "data/raw/eph/usu_individual_t418.txt"
# read file
raw = read.delim(file_raw, header=T, sep=";", dec=",", fill=T
                 , encoding="UTF-8"
                 , colClasses=col_types)
# clean names
raw = raw %>% janitor::clean_names()

# read ramas CAE
ramas = read.csv("resources/ramas_cae.csv", colClasses="character")


# clean data --------------------------------------------------------------

# filtros y variables relevantes
temp = raw %>% 
  dplyr::filter(
    # filtro edad
    ch06 %in% 18:65 
    # filtro ocupados
    ,estado==1 
    # filtro ingreso!=0 (revisar!!!)
    ,!(p21 %in% 0)
  ) %>% 
  # keep variables relevantes
  select(vars_keep$variable) 

# nuevas variables
dat_clean = temp %>% 
  # remplazar -9 por NA en ingresos
  mutate_if(is.numeric, function(x) ifelse(x==-9,NA,x)) %>% 
  # agrega 0 si codigo de actividad tiene uno o tres digitos
  mutate(
    pp04b_cod = 
      ifelse(nchar(pp04b_cod) %in% c(1,3), paste("0",pp04b_cod,sep=""),pp04b_cod)
  ) %>% 
  # nuevas variables
  mutate(
    # caracteristicas de la ocupacion
    carac_ocup = str_sub(pp04d_cod,1,2)
    ,calif_ocup = str_sub(pp04d_cod,5,5)
    # rama de actividad a 2 digitos
    ,digito = str_sub(pp04b_cod,1,2)
    # percibe ayuda social (NA imputado como 0)
    ,ayuda = ifelse(v5_m==0 | is.na(v5_m), 0, 1) %>% as.factor()
  ) %>% 
  # join digito con letra CAE
  left_join(ramas, by="digito") %>% 
  # character as factor
  mutate_if(is.character, as.factor) %>% 
  # drop variables no usadas
  select(-c(pp04d_cod, pp04b_cod, v5_m, pp04b_cod, digito)) %>% 
  # POR AHORA SACAMOS LETRA Y CARAC_OCUP (MUCHOS NIVELES Y PVALOR GRANDE)
  select(-letra, -carac_ocup)  

# faltante en t_vi es 0
dat_clean = dat_clean %>% 
  mutate(t_vi = ifelse(is.na(t_vi), 0, t_vi))

# data completa
dat = dat_clean %>% 
  dplyr::filter((!is.na(p21)) & imputa==0) %>% 
  select(-imputa)

# data imputada
dat_imp = dat_clean %>% 
  dplyr::filter(is.na(p21) | imputa==1) %>% 
  select(-imputa)

# exploratorio ------------------------------------------------------------

# distribucion p21
ggplot(dat) +
  geom_density(aes(x=p21), adjust=2, fill="red", alpha=0.5) +
  NULL

# distribucion numericas (por tipo de rpta)
gdat_num = bind_rows(dat, dat_imp, .id="rpta") %>% 
  mutate(rpta = as.numeric(rpta)) %>% 
  select_if(is.numeric) %>% 
  select(-p21) %>% 
  data.table::melt(id.vars="rpta")
  # mutate(id = row_number()) %>% 
  # pivot_longer(-c(id,rpta), names_to="variable", values_to="value")
ggplot(gdat_num) +
  geom_density(aes(x=value, color=rpta), fill="red", alpha=0.5) +
  facet_wrap(~variable, scales="free") +
  NULL

# Correlograma numericas
GGally::ggcorr(dat, label=T, hjust=1, label_size=2.5, layout.exp=10)
GGally::ggcorr(dat_imp, label=T, hjust=1, label_size=2.5, layout.exp=10)

# barplot categoricas
# (mejorar en base a lo que haga para clase exploratorio!)
gdat_cat = dat_clean %>% 
  select_if(function(x) !is.numeric(x)) %>% 
  mutate(id = row_number()) %>% 
  tidyr::pivot_longer(-id, names_to="variable", values_to="value") 
ggplot(gdat_cat %>% filter(variable %in% c("ch04","ch03","ch07","ch16"))) +
  geom_bar(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  NULL

# regresion lineal ---------------------------------------------------------------

# en niveles
mod_lm = lm(p21 ~ ., data=dat)
summary(mod_lm)
broom::tidy(mod_lm) %>% arrange(p.value) %>% print(n=200)
broom::tidy(mod_lm, conf.int=T) %>% arrange(p.value) %>% print(n=200)
glance(mod_lm)
# cor(mod_lm$fitted.values, mod_lm$model$p21)**2

# en logaritmos
  # (no es mejor: ifelse(abs(x)<1, x, sign(x)*log(abs(x))) ????
  # NO PORQUE LOG(1.5)<0.5 POR EJEMPLO --> se rompone la monotonia
siglog = function(x) ifelse(abs(x)<1, 0, sign(x)*log(abs(x)))
sigexp = function(x) ifelse(x<0, sign(x)*exp(abs(x)), exp(x))
  # solo la rpta
mod_lmlog1 = lm(siglog(p21) ~ ., data=dat)
glance(mod_lmlog1)
  # rpta y X
dat2 = dat %>% 
  mutate_if(is.numeric, siglog)
mod_lmlog2 = lm(p21 ~ . , data=dat2)
glance(mod_lmlog2)

# agregando terminos cuadraticos
dat_exp = dat %>% 
  mutate_if(is.numeric, list("sq"=function(x) x**2)) %>% 
  select(-p21_sq)
mod_lmexp = lm(p21 ~ ., data=dat_exp)
glance(mod_lmexp)
broom::tidy(mod_lmexp) %>% print(n=200)

# con interacciones de genero (con todas las variables)
mod_lmexp2 = lm(p21 ~ . + ch04:., data=dat2)
# (ojo con comparaciones multiples!!)
broom::tidy(mod_lmexp2) %>% print(n=200)

glance(mod_lmlog2)
glance(mod_lmexp2)

### diagnostico ###
# residual plot
ggplot(augment(mod_lmlog2), aes(x=.fitted, y=.resid)) +
  geom_point(aes(color=ch04), alpha=0.5, size=1) +
  geom_smooth(se=F)
  # autoplot(mod_lm, which=1, colour="ch04")
# VIF (revisar valores limite)
car::vif(mod_lmlog2)
# metricas de fitted vs obs
rdos = augment(mod_lmlog2) %>% 
  mutate(
    .fitted = sigexp(.fitted)
    ,p21 = dat$p21
    )
yardstick::metrics(rdos, p21, .fitted)

# Hiperparametrizando ----------------------------------------------------

# objetivo: predecir ingreso faltante lo mejor posible con modelo lineal 
# herramienta: feature selection con lasso

# parametros --------------------------------------------------------------

semilla = 150

# train - test split ------------------------------------------------------------

# rsample

set.seed(semilla)
tt_split = dat %>% initial_split(prop=0.8)
dat_train = tt_split %>% training()
dat_test = tt_split %>% testing()

# validation split ----------------------------------------------------------------

# rsample

set.seed(semilla)
cv_split = vfold_cv(dat_train, v=5)
# (analisis y assessment sets)

# preprocesamiento --------------------------------------------------------

# recipes

# recipe() - inicializa con formula --tipo ggplot()--
# prep() - define y aplica transformaciones en base a training data
# bake() - aplica preprocesamiento

# juice() - extrae training preprocesado (= que bake sobre training)


# definimos receta como funcion para aplicar en cada fold de cv
receta = function(dataset) {
  
  recipe(p21 ~ ., data=dataset) %>%
    step_log(all_outcomes(), signed=T) %>%
    step_mutate_at(all_numeric(),-all_outcomes()
                   ,fn = list(log=siglog, sqrt=sqrt, sq=function(x) x**2)) %>% 
    # step_corr(all_numeric(), -all_outcomes(), threshold=0.9) %>% 
    # step_nzv(all_predictors()) %>%
    # step_other(all_nominal(), threshold=0.01, other="otros") %>% 
    step_dummy(all_nominal(), one_hot=F)  

}

# modelo ------------------------------------------------------------------

# ejemplo sobre training
receta(dat_train)
receta_trained = receta(dat_train) %>% prep(retain=T)
recipes::tidy(receta_trained, n=1)
recipes::tidy(receta_trained, n=2)
train_prep = juice(receta_trained)
# train_prep = receta_trained$template #identicos

# parsnip (o tal vez no...)

# ejemplo: entrena modelo y aplica sobre test 

# fit function (parsnip NO TOMA PENALTY :(((( )
fit_lasso = function(data, lambda=NULL) {
  glmnet::glmnet(
    x = data %>% select(-p21) %>% as.matrix()
    ,y = data$p21
    ,alpha = 1
    ,lambda = lambda
  )
}
# con parsnip seria:
#   linear_reg(mode="regression", mixture=1, penalty=lambda) %>% 
#   set_engine("glmnet") %>% 
#   fit(p21 ~ ., data=data)


# ajusta modelo
set.seed(semilla)
mod_lasso = fit_lasso(train_prep)
mod_lasso$lambda

# prepara test (2 versiones --> elegir una)
# test_baked = receta_trained %>% bake(new_data=dat_test)
test_baked = dat_test %>% receta() %>% prep() %>% bake(new_data=dat_test)

# predict function
pred_lasso = function(model, newdata, lambda=NULL) {
  predict(model
          ,newx = newdata %>% select(-p21) %>% as.matrix()
          ,s = lambda) %>% 
    as.vector() %>% sigexp()
} 

# fitted values in test
pred = pred_lasso(mod_lasso, test_baked, lambda=0)

# performance
obs = dat_test$p21
rdos = data.frame(obs=obs, pred=pred)
yardstick::metrics(rdos, obs, pred)

# mean(abs(obs-pred))
# mean((obs-pred)**2) %>% sqrt()
# cor(pred,obs)**2

# plot(pred, obs)

# Una mejora:
# elegir lambda con CV
set.seed(semilla)
lseq = exp(seq(-10,-5,length.out=50))
cv_lasso = cv.glmnet(x = train_prep %>% select(-p21) %>% as.matrix()
                     ,y = train_prep$p21
                     ,alpha=1
                     ,lambda=lseq
                     ,nfolds=5)
plot(cv_lasso)
cv_lasso$lambda.min
cv_lasso$lambda.1se
# (pero puede haber data leakage... hay que reproducir el proceso en cada fold!)

# da right way ------------------------------------------------------------

# si el preprocesamiento usa los param de analysis o assessment depende
  # de como se implemente en la practica!!!

# entrena lasso para lambda dado en analysis set y aplica en assessment
  # el modelo se va ajustar para cada fold para cada valor de lambda
  # es poco eficiente porque por defecto glmnet ajusta para una secuencia de lambdas
  # ejercicio: modificar el codigo para aprovechar esto

train_apply_lasso = function(fold_split, lambda) {

  # get analysis data
  dat_an = fold_split %>% analysis()
  # train receta sobre analysis data
  receta_trained = dat_an %>% receta() %>% prep(retain=T)
  # get analysis preprocesado
  dat_an_prep = juice(receta_trained)
  # entrena modelo 
  mod_lasso = fit_lasso(dat_an_prep, lambda)
  # get assessment data
  dat_as = fold_split %>% assessment()
  # version A. entrena receta y aplica (assessment preprocesado)
  # dat_as_baked = dat_as %>% receta() %>% prep() %>% bake(newdata=dat_as)
  # version B. aplica receta trained sobre assessment (assessment preprocesado)
    # esta sirve mas para online!
    # pero la usamos porque garantiza que tengamos las mismas variables dummy en los sets
    # si no, puede haber distintas categorias cuando agrupa
  dat_as_baked = receta_trained %>% bake(new_data=dat_as)
  # predict  
  out = tibble(
    "id" = fold_split$id$id
    ,"obs" = dat_as$p21
    ,"pred" = pred_lasso(mod_lasso, newdata=dat_as_baked, lambda=lambda)
  )
  return(out)  
  
  # PARA LA VERSION QUE SE HACE CON UNA SECUENCIA DE LAMBDAS:
  # out = crossing(
  #   lambda = lambda_seq
  #   ,id = fold_split$id$id
  # ) %>% 
  #   mutate(
  #     obs = map(lambda, function(x) dat_as_baked$p21)
  #     ,pred = map(lambda, function(l) predict(mod_lasso, dat_as_baked, penalty=l) %>% 
  #                   unlist())
  #   ) %>% 
  #   unnest()
  
}

# function: train and apply lasso para cada split - y get performance metrics
# (yardstick)

# library(furrr)
# plan(multicore)
kfold_lasso = function(cv_splits, lambda) {
  out = list()
  out$fit = map_df(cv_split$splits,
             function(s) train_apply_lasso(fold_split=s, lambda=lambda))
  # out$fit = future_map_dfr(cv_split$splits,
                           # function(s) train_apply_lasso(fold_split=s, lambda=lambda))  
  out$summary = out$fit %>%
    group_by(id) %>%
    metrics(obs, pred)
  return(out)
}

# CV con gridsearch
lseq = exp(seq(-8,-4,length.out=50))
rdos = tibble(
  lambda = lseq
  ,lista = map(lseq, function(l) kfold_lasso(cv_split, lambda=l))
  # ,lista = future_map(lseq, function(l) kfold_lasso(cv_split, lambda=l))
  ,detalle = map(lista, "fit")
  ,metricas = map(lista, "summary")
)
# performance (rmse-rsq-mae) en cada fold y cada lambda
metricas = rdos %>% unnest(metricas)
# MAE: media y desvio para cada lambda
metricas_sum = metricas %>% 
  dplyr::filter(.metric=="mae") %>% 
  group_by(lambda) %>% 
  summarise(m = mean(.estimate)
            ,sd = sd(.estimate)
            ,se1 = m+sd)
# elegimos el modelo menos complejo dentro de los que tienen mean(mae) 
  # menor o igual a 1se minimo
lambda_1se = metricas_sum %>% 
  dplyr::filter(m<=min(se1)) %>% 
  dplyr::filter(lambda==max(lambda)) %>% pull(lambda)
lambda_min = metricas_sum %>% 
  dplyr::filter(m<=min(m)) %>% 
  pull(lambda)

# plot
ggplot(metricas_sum, aes(x=log(lambda), y=m)) + 
  geom_line() +
  geom_errorbar(aes(ymin=m-sd, ymax=m+sd)) +
  geom_point() +
  geom_vline(xintercept=c(log(lambda_min),log(lambda_1se)), color="red")

# performance final
# se ajusta modelo con toda la data y lambda optimo
set.seed(semilla)
mod_lasso = fit_lasso(train_prep, lambda=lambda_1se)
pred = pred_lasso(mod_lasso, test_baked, lambda=lambda_1se)
rdos = data.frame(obs=dat_test$p21, pred=pred)
yardstick::metrics(rdos, obs, pred)


# aplicacion del modelo ---------------------------------------------------

# preprocesa data con ingresos
receta_trained = dat %>% receta() %>% prep(retain=T)
dat_p = receta_trained %>% juice() 

# entrena modelo
mod_lasso = fit_lasso(dat_p, lambda_1se)
mod_lasso %>% broom::tidy() %>% arrange(-abs(estimate)) %>% print(n=100)

# si se usa step_other:
# receta_trained %>% recipes::tidy(n=2) %>% dplyr::filter(terms=="ch08")

# preprocesa data con faltantes (2 versiones --> elegir una)
# new_baked = receta_trained %>% bake(new_data=dat_imp)
new_baked = dat_imp %>% receta() %>% prep() %>% bake(new_data=dat_imp)

# predice ingreso
p21_pred = pred_lasso(mod_lasso, newdata=new_baked, lambda=lambda_1se)
  


# EJERCICIO 1: modificar la funcion de train y la corrida del CV para aprovechar
# que la funcion de glmnet calcula fit para muchos lambda
# PONER ACA LA SOLUCION RECAUCHATADA DEL CODIGO COMENTADO

# EJERCICIO 2: incluir en como hiperparametro si las categoricas se agrupan o no
  # en "otros"
# PONER ACA LA SOLUCION (generar otra version de receta y ponerla en el train_apply_lasso
                       # como parametro y adentro un if)



# para el ejercicio 1:
# esta es la version que usa una seq de lambas en el training
# lseq = c(0,exp(seq(-10,0,length.out=50)))
# aa = map_dfr(cv_split$splits, function(s) train_apply_lasso(s, lseq))
# bb = aa %>%
#   group_by(id, lambda) %>%
#   metrics(obs, pred)
# dd = bb %>%
#   dplyr::filter(.metric %in% "rmse") %>% 
#   group_by(lambda) %>% 
#   summarise(rmse = mean(.estimate))
# 
# dd %>% dplyr::filter(rmse==min(rmse))
# 
# ggplot(dd, aes(x=log(lambda), y=rmse)) +
#   geom_line()

# (A) stepwise selection - caret -------------------------------------------

# DEJARLO PARA DPS PORQUE:
  # ESTO TIRA ERRORES DE LINEAR DEPENDENCIES
  # Y HAY QUE VER QUE HACER CON LAS CATEGORICAS
  # LAS VA PONIENDO COMO DUMMIES DE UNA
  # ENTONCES NO ES FACIL SABER EL NRO DE VARIABLES MAXIMO DE ANTEMANO
# TAL VEZ NI HACERLO!!!

# # caret
# 
# # CV settings
# set.seed(semilla)
# cv_set = trainControl(method="cv", number=10)
# 
# # stepwise linear regression (parametro: nvmax)
# mod_step = train(p21 ~ .
#                  ,data = dat_train
#                  ,method = "leapForward" 
#                  ,tuneGrid = data.frame(nvmax = 5:102)
#                  ,trControl = cv_set
# )
# 
# mod_step$results
# 
# plot(mod_step)
# 
# mod_step_f = train(p21 ~ .
#                  ,data = dat_train
#                  ,method = "leapForward" 
#                  ,tuneGrid = data.frame(nvmax=10:100)
#                  ,trControl = cv_set
# )
# 
# plot(mod_step_f)
# mod_step_f$results
# aa = mod_step_f$finalModel
# 
# mod_step_f$bestTune
# aa$lindep
# 
# # http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
# # usar directamente cross-validation
# # al final se aplica el modelo en toda la data, fijando el # de vars
# # (no las vars mismas!)
# 
# 

# bonus track -------------------------------------------------------------

# para identificar combinaciones lineales
library(recipes)
dat_bin = recipe(p21 ~ ., data=dat) %>% 
  step_dummy(all_nominal()) %>% 
  prep(retain=T) %>% juice()
lc = caret::findLinearCombos(dat_bin)
dat_bin[lc$linearCombos[[1]]]
mod = lm(p21 ~ ., data=dat)
lc2 = alias(mod)



# notas -------------------------------------------------------------------


aa = iris
bb = dplyr::filter(aa, Species=="virginica")

library(magrittr)
library(recipes)
recipes::recipe(bb) %>% 
  recipes::step_dummy(Species) %>% prep() %>% bake(newdata=bb)

