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
# ver como solucionar que performance en escala original da cualquier cosa en CV lasso!
# si el proceso es: entran datos - CV - ajuste con mejor lambda:
  # como se hace CV de ese proceso?? CV dentro del CV??

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

# HACER EXPLORATORIO BREVE
  # (correlograma)
  # distsribuciones univariadas

# regresion lineal ---------------------------------------------------------------

# en niveles
mod_lm = lm(p21 ~ ., data=dat)
summary(mod_lm)
broom::tidy(mod_lm) %>% arrange(p.value) %>% print(n=200)
broom::tidy(mod_lm, conf.int=T) %>% arrange(p.value) %>% print(n=200)
glance(mod_lm)

# en logaritmos
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
rdos = augment(mod_lmlog2) %>% mutate(.fitted = sigexp(.fitted))
yardstick::metrics(rdos, p21, .fitted)

# Hiperparametrizando ----------------------------------------------------

# objetivo: predecir ingreso faltante lo mejor posible con modelo lineal 
# herramienta: feature selection con lasso

# parametros --------------------------------------------------------------

semilla = 150

# nuevas variables  ----------------------------------------------

# solo las no sensibles a data leakage!
# ojo con generar muchas: lasso es sensible a dim alta (p/n>.10)
# (step_log y step_sqrt pisan las variables -- no generan nuevas:( )

datf = dat %>% 
  mutate_if(is.numeric
            ,list("log"=siglog, "sqrt"=sqrt, "sq"=function(x) x**2)) %>% 
  select(-c(p21_log,p21_sqrt,p21_sq))

# train - test split ------------------------------------------------------------

# rsample

set.seed(semilla)
tt_split = datf %>% initial_split(prop=0.8)
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
    # step_corr(all_numeric(), -all_outcomes(), threshold=0.9) %>% 
    step_other(all_nominal(), threshold=0.01, other="otros") %>% 
    step_dummy(all_nominal(), one_hot=F)  

}

# ejemplo sobre training
receta(dat_train)
receta_trained = receta(dat_train) %>% prep(retain=T)
broom::tidy(receta_trained, n=1)
broom::tidy(receta_trained, n=2)
train_prep = juice(receta_trained)
# train_prep = receta_trained$template #identicos

# modelo ------------------------------------------------------------------

# parsnip (o tal vez no...)

# ejemplo: entrena modelo y aplica sobre test 
test_baked = receta_trained %>% bake(newdata=dat_test)
set.seed(semilla)
mod_lasso = glmnet(x=train_prep %>% select(-p21) %>% as.matrix()
                   ,y=train_prep$p21
                   ,alpha=1)
mod_lasso$lambda
pred = predict(mod_lasso
               ,newx = test_baked %>% select(-p21) %>% as.matrix()
               # ,s=mod_lasso$lambda[30]
               ,s=0
) %>% as.vector() %>% sigexp()
obs = dat_test$p21
rdos = data.frame(obs=obs, pred=pred)
yardstick::metrics(rdos, obs, pred)

# mean(abs(obs-pred))
# mean((obs-pred)**2) %>% sqrt()
# cor(pred,obs)**2

# plot(pred, obs)

# elgiendo lambda con CV (ver si dejar esto o sacarlo)
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
# (pero hay data leakage... hay que reproducir el proceso en cada fold!)

# da right way ------------------------------------------------------------

# si el preprocesamiento usa los param de analysis o assessment depende
  # de como se implemente en la practica!!!

# entrena lasso para lambda dado en analysis set y aplica en assessment
  # (lo hago para muchos lambda aca porque el train ya lo hace?)
  # entonces es mucho mas eficiente
  # PERO NO PUEDO DEFINIR LA SECUENCIA DE LAMBDAS :(
  # pero es mas facil explicarlo para un lambda!

train_apply_lasso = function(fold_split, lambda) {

  # get analysis data
  dat_an = fold_split %>% analysis()
  # train receta sobre analysis data
  receta_trained = dat_an %>% receta() %>% prep(retain=T)
  # get analysis preprocesado
  dat_an_prep = juice(receta_trained)
  # A. entrena modelo (con parsnip)
  # mod_lasso =
  #   linear_reg(mode="regression", mixture=1) %>% 
  #   set_engine("glmnet") %>% 
  #   fit(p21 ~ ., data=dat_an_prep)
  # B. entrena modelo (con glmnet porque parsnip no toma penalty!!!)
  mod_lasso =
    glmnet::glmnet(
      x = dat_an_prep %>% select(-p21) %>% as.matrix()
      ,y = dat_an_prep$p21
      ,alpha = 1
      ,lambda = lambda
    )
  # get assessment data
  dat_as = fold_split %>% assessment()
  # version A. entrena receta y aplica (assessment preprocesado)
  # dat_as_baked = dat_as %>% receta() %>% prep() %>% bake(newdata=dat_as)
  # version B. aplica receta trained sobre assessment (assessment preprocesado)
    # esta sirve mas para online!
    # pero la usamos porque garantiza que tengamos las mismas variables dummy en los sets
    # si no, puede haber distintas categorias cuando agrupa
  dat_as_baked = receta_trained %>% bake(newdata=dat_as)
  
  # cuando se aplica sigexp() la performance de CV empieza a mejorar apde un punto
    # en lugar de empeorar!!!
    # creo que pasa por los valores extremos!
  
  # version en log
  # out = tibble(
  #   "id" = fold_split$id$id
  #   ,"obs" = dat_as_baked$p21 
  #   ,"pred" = predict(mod_lasso
  #                     ,newx = dat_as_baked %>% select(-p21) %>% as.matrix()
  #                     ,s = lambda) %>% as.vector() 
  #   # %>% sigexp()
  # )
  
  # version en escala original (para mi va esta..)
  out = tibble(
    "id" = fold_split$id$id
    ,"obs" = dat_as$p21
    ,"pred" = predict(mod_lasso
                      ,newx = dat_as_baked %>% select(-p21) %>% as.matrix()
                      ,s = lambda) %>% as.vector() %>% sigexp()
  )
  
  # SI FUNCIONA PARSNIP:
  # out =   tibble(
  #   "id" = fold_split$id$id
  #   ,"obs" = dat_as_baked$p21
  #   ,"pred" = predict(mod_lasso, dat_as_baked, penalty=0.00065) %>% unlist()
  # )
  
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

  return(out)  
  
}

# yardstick

# # train and apply lasso para cada split - y get performance metrics
#   # ESTO ES PARA LA VERSION LENTA (CUANDO SE CORRE EL FIT PARA CADA LAMBDA)
kfold_lasso = function(cv_splits, lambda) {
  out = list()
  out$fit = map_df(cv_split$splits,
             function(s) train_apply_lasso(fold_split=s, lambda=lambda))
  out$summary = out$fit %>%
    group_by(id) %>%
    metrics(obs, pred)
  return(out)
}
lseq = exp(seq(-8,-4,length.out=50))
rdos = tibble(
  lambda = lseq
  ,lista = map(lseq, function(l) kfold_lasso(cv_split, lambda=l))
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
mod_lasso = glmnet(x=train_prep %>% select(-p21) %>% as.matrix()
                   ,y=train_prep$p21
                   ,alpha=1
                   ,lambda=lambda_1se)
pred = predict(mod_lasso
               ,newx = test_baked %>% select(-p21) %>% as.matrix()
               ,s = lambda_1se
) %>% as.vector() %>% sigexp()
rdos = data.frame(obs=dat_test$p21, pred=pred)
yardstick::metrics(rdos, obs, pred)


# aplicacion del modelo ---------------------------------------------------

# preprocesa train
receta_trained = datf %>% receta() %>% prep(retain=T)
dat_p = receta_trained %>% juice() 

# entrena modelo
mod_lasso = glmnet(x=dat_p %>% select(-p21) %>% as.matrix()
                   ,y=dat_p$p21
                   ,alpha=1
                   ,lambda=lambda_1se)
mod_lasso %>% broom::tidy() %>% arrange(-abs(estimate)) %>% print(n=100)
receta_trained %>% broom::tidy(n=2) %>% dplyr::filter(terms=="ch08")

# preprocesa newdata
  # (hay que generarle los atributos! porque no pude ponerlo como step :( )
dat_impf = dat_imp %>% 
  mutate_if(is.numeric
            ,list("log"=siglog, "sqrt"=sqrt, "sq"=function(x) x**2)) %>% 
  select(-c(p21_log,p21_sqrt,p21_sq))
new_baked = receta_trained %>% bake(newdata=dat_impf)

# predice ingreso
p21_pred = predict(mod_lasso
               ,newx = new_baked %>% select(-p21) %>% as.matrix()
               ,s = lambda_1se
) %>% as.vector() %>% sigexp()





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



# EJERCICIO 1: modificar la funcion de train y la corrida del CV para aprovechar
# que la funcion de glmnet calcula fit para muchos lambda
# PONER ACA LA SOLUCION RECAUCHATADA DEL CODIGO COMENTADO

# EJERCICIO 2: incluir en como hiperparametro si las categoricas se agrupan o no
  # en "otros"
# PONER ACA LA SOLUCION (generar otra version de receta y ponerla en el train_apply_lasso
                       # como parametro y adentro un if)


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

