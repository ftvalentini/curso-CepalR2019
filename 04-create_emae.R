library(dplyr)
library(stringr)
library(magrittr)

query = 
  "https://apis.datos.gob.ar/series/api/series/?collapse=quarter&collapse_aggregation=avg&ids=143.3_NO_PR_2004_A_21&limit=5000&representation_mode=change_a_year_ago&format=csv"

raw = read.delim(query, sep=",", stringsAsFactors=F)

emae = raw %>% 
  mutate(
    periodo = paste0(
      str_sub(indice_tiempo,1,5)
      ,case_when(
        str_sub(indice_tiempo,6,7)=="01" ~ 1
        ,str_sub(indice_tiempo,6,7)=="04" ~ 2
        ,str_sub(indice_tiempo,6,7)=="07" ~ 3
        ,str_sub(indice_tiempo,6,7)=="10" ~ 4
      ))) %>% 
  rename(emae = indice_serie_original_var_ia) %>% 
  select(periodo, emae)

saveRDS(emae, "data/working/emae_ia.rds")
