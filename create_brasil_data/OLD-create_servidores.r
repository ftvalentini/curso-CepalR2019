library(data.table)
library(magrittr)

# read raw data
cadastro = fread("unzip -p data/raw/201612_Servidores.zip 201612_Cadastro.csv"
                 , fill=T)
salarios = fread("unzip -p data/raw/201612_Servidores.zip 201612_Remuneracao.csv"
                 , fill=T)

# clean names
cadastro = cadastro %>% janitor::clean_names()
salarios = salarios %>% janitor::clean_names()


cadastro[,
         ,.(id_servidor_portal,cod_uorg_lotacao,uorg_lotacao,
            cod_uorg_exercicio, uorg_exercicio, situacao_vinculo
            ,regime_juridico, jornada_de_trabalho)]


ID_SERVIDOR_PORTAL



# group data by municipio
datg = dat[ ,
            .(num_beneficiarios = uniqueN(nis_beneficiario)
              ,suma_valor = sum(valor_beneficio))  
            ,by = .(uf, codigo_municipio_siafi, nome_municipio_siafi)
            ]

# save data as csv
fwrite(datg, file="data/working/bolsafamilia_municipios_2016-12.csv")
