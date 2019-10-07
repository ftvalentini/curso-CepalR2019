library(data.table)
library(magrittr)
library(purrr)
library(dplyr)
library(stringr)

# file names
files = list.files("data/raw/brasil/ipea/", full.names=T)

# read data and correct names
dats = list()
for (f in files) {
  basef = basename(f)
  basef_sinext = str_remove(basef, "(\\.csv)")
  dats[[basef]] = fread(f, skip=1, fill=T, encoding="UTF-8")
  dats[[basef]] %<>% rename(!!basef_sinext := "2016")
  dats[[basef]] %<>% janitor::clean_names()
  dats[[basef]][, v5 := NULL]
}

# joins
dat = Reduce(f=function(a,b) merge(a,b,all=F), x=dats)

# integer64 as numeric
dat = dat %>% mutate_if(is.numeric, as.numeric)

# save data as RDS
saveRDS(dat, file="data/working/brasil/varios_municipios_2016.rds")
