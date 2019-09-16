library(data.table)
library(magrittr)

# read raw data
dat = fread("unzip -p data/raw/brasil/portal_transparencia/201612_BolsaFamilia_Pagamentos.zip"
            ,dec=",", fill=T)
# clean names
dat = dat %>% janitor::clean_names()

# group data by municipio
datg = dat[ ,
            .(num_beneficiarios = uniqueN(nis_beneficiario)
              ,suma_valor = sum(valor_beneficio))  
            ,by = .(uf, codigo_municipio_siafi, nome_municipio_siafi)
            ]

# save data as RDS
saveRDS(datg, file="data/working/brasil/bolsafamilia_municipios_2016-12.rds")
