# MODIFICAR LECTURA DE CSVs DE VARIABLES Y RAMAS PARA QUE SEA MAS TRANSPARENTE

library(magrittr)
library(dplyr)
library(stringr)

# lee eph individual con columnas relevantes con el tipo correcto
read_eph = function(path) {

  # lista variables relevantes
  vars_keep = read.delim("resources/variables_eph.csv", sep=",", stringsAsFactors=F
                         ,encoding="UTF-8")
  # tipo de cada variable relevante
  col_types = vars_keep$tipo %>% setNames(toupper(vars_keep$variable))
  # read file
  raw = read.delim(path, header=T, sep=";", dec=",", fill=T, encoding="UTF-8"
                   ,colClasses=col_types, stringsAsFactors=F)

  return(raw)
}

# limpia eph
clean_eph = function(df_raw) {

  # lista variables relevantes
  vars_keep = read.delim("resources/variables_eph.csv", sep=",", stringsAsFactors=F
                         ,encoding="UTF-8")
  # ramas CAE
  ramas = read.csv("resources/ramas_cae.csv", colClasses="character")

  # clean names
  tab = df_raw %>% janitor::clean_names()
  # filtros
  tab = tab %>%
    dplyr::filter(
      # filtro edad
      ch06 %in% 18:65 &
        # filtro estado
        estado!=0
    )
  # variables relevantes
  tab = tab %>%
    select(vars_keep$variable
           ,codusu, nro_hogar, componente
           , ano4, trimestre
           ,pondera)
  # define NAs as such en numericas
  tab = tab %>%
    mutate_if(is.numeric, function(x) ifelse(x==-9, NA, x))
  # define NAs as such en categoricas
  tab = tab %>%
    mutate_if(function(x) !is.numeric(x), function(x) ifelse(x=="" | x==" ",NA,x))
  # faltante en t_vi es 0
  tab = tab %>%
    mutate(t_vi = ifelse(is.na(t_vi), 0, t_vi))
  # agrega 0 si codigo de actividad tiene uno o tres digitos
  tab = tab %>% mutate(
    pp04b_cod =
      ifelse(nchar(pp04b_cod) %in% c(1,3), paste("0",pp04b_cod,sep=""),pp04b_cod)
  )
  # nuevas variables
  tab = tab %>%
    mutate(
      # id individuo
      id_indiv = paste(codusu, nro_hogar, componente, sep="_")
      # periodo (anio-trimestre)
      ,periodo = paste0(ano4, "-", trimestre)
      # caracteristicas de la ocupacion
      ,calif_ocup = str_sub(pp04d_cod,5,5)
      # rama de actividad a 2 digitos
      ,digito = str_sub(pp04b_cod,1,2)
      # percibe ayuda social (NA imputado como 0)
      ,ayuda = ifelse(v5_m==0 | is.na(v5_m), 0, 1) %>% as.factor()
    )
  # renames
  tab = tab %>%
    rename(anio = ano4)
  # join digito con letra CAE (menos desagregacion)
  tab = tab %>% left_join(ramas, by="digito")
  # character as factor
  tab = tab %>% mutate_if(is.character, as.factor)
  # salvo id_indiv y periodo
  tab = tab %>% mutate_at(vars(id_indiv, periodo), as.character)
  # drop variables no usadas
  tab = tab %>%
    select(-c(pp04d_cod, pp04b_cod, v5_m, pp04b_cod, digito, letra
              ,codusu, nro_hogar, componente, imputa))

  return(tab)
}
