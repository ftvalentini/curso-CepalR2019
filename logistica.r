library(magrittr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

"%+%" = function(a,b) paste(a,b,sep="")

# descargar las bases de 
# https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos
# y unzip
# from cmd:
  # cd data\raw\ 
  # unzip \*.zip

# nombres de archivos
files = c(
   "data/raw/usu_individual_t119.txt"
  ,"data/raw/usu_individual_t418.txt"
  ,"data/raw/usu_individual_T318.txt"
  ,"data/raw/usu_individual_t218.txt"
  ,"data/raw/usu_individual_t118.txt"
  ,"data/raw/usu_individual_T417.txt"
  ,"data/raw/usu_individual_t317.txt"
  ,"data/raw/usu_individual_t217.txt"
  ,"data/raw/usu_individual_t117.txt"
)

# lee cada archivo iterando
raws = files %>% 
  map(function(f)
    read.delim(f, header=T, sep=";", dec=",", fill=T, stringsAsFactors=F))
# nombre de cada elemento de la lista
names(raws) = files

# append/concatenate de las tablas
indiv = raws %>% bind_rows()
# limpia nombres
indiv = indiv %>% janitor::clean_names()

# limpia base
indivc = indiv %>% 
  # filtro edad y estado
  filter(ch06 %in% 18:65 & estado!=0) %>% 
  # id de individuo y de tiempo
  mutate(
    id_indiv = paste(codusu, nro_hogar, componente, sep="_")
    ,id_temp = ano4 %+% "_" %+% trimestre
  ) %>% 
  # tiempo como variable ordinal (2 variantes)
  mutate(
    periodo = match(id_temp, sort(unique(id_temp)))
    # t = as.numeric(factor(id_temp))
  ) %>%
  # ordena
  arrange(id_indiv, periodo) %>% 
  # filtro: solo los que estan mas de una vez
  group_by(id_indiv) %>% 
  dplyr::filter(n()>1) %>% 
  # variable t: periodo 0,..,n
  mutate(t = periodo - min(periodo)) %>% 
  ungroup()

# estado en cada periodo (0,1,..,n) de cada id (formato wide)
estado_wide = indivc %>% 
  select(id_indiv, t, estado) %>% 
  data.table::dcast(id_indiv ~ t, value.var="estado") %>% 
  rename_at(vars(-id_indiv), function(n) "temp_"%+%n)

# join con primer periodo de cada id de la base
dat_temp = indivc %>% 
  group_by(id_indiv) %>% 
  dplyr::filter(periodo == min(periodo)) %>% 
  ungroup() %>% 
  left_join(estado_wide, by="id_indiv")

# filter: individuos ocupados en temp_0
# genera clase (VER COMO HACERLO SIN TENER QUE ESCRIBIR TODOS LOS PERIODOS!)
dat = dat_temp %>% 
  dplyr::filter(temp_0 %in% 1) %>% 
  mutate(clase = ifelse(
    temp_1 %in% 2 |
      # temp_2 %in% 2 |
      temp_3 %in% 2 |
      temp_4 %in% 2 |
      temp_5 %in% 2 
    , 1, 0))

# UNA FORMA DE EVITARLO: DFRAME INTERMEDIO:
# temp = dat_temp %>% 
#   select(id_indiv, starts_with("temp_"), -temp_0) %>% 
#   filter_at(vars(starts_with("temp_")), any_vars(.==2)) %>% 
#   mutate(clase = 1)
# dat = dat_temp %>% 
#   left_join(temp, by="id_indiv") %>% 
#   dplyr::filter(temp_0 %in% 1) %>%
#   mutate(clase = ifelse(is.na(clase), 0, clase))




# COMO SE ENTIENDEN CASOS CON periodo=2 (sin periodo=1) Y CON periodo=5 ???
# tal vez no necesariamente se cumple el 2-2-2
# estado_long %>% 
#   group_by(id_indiv) %>% 
#   dplyr::filter(any(t %in% 3) & any(periodo %in% 2)) %>% 
#   # dplyr::filter(n()>2) %>% 
#   ungroup()
