library(readxl)
library(magrittr)
library(dplyr)

# read excel and clean names
filep = "data/raw/brasil/ibge/base_de_dados_2010_2016_xls/copia_pib_municipios_FV.xls"
dat = readxl::read_excel(filep, guess_max=5000)
dat = dat %>% janitor::clean_names()

# keep useful vars
dat = dat %>% select(
ano
,codigo_da_grande_regiao
,nome_da_grande_regiao
,codigo_da_unidade_da_federacao
,sigla_da_unidade_da_federacao
,nome_da_unidade_da_federacao
,codigo_do_municipio
,nome_do_municipio
,semiarido
,tipologia_rural_urbana
,hierarquia_urbana_principais_categorias
,valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000
,valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000
,produto_interno_bruto_a_precos_correntes_r_1_000
,populacao_nº_de_habitantes
,produto_interno_bruto_per_capita_r_1_00
,atividade_com_maior_valor_adicionado_bruto
)

# rename vars
dat = dat %>% rename(
  vab_agro = valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000
  ,vab_industria = valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000
  ,pib = produto_interno_bruto_a_precos_correntes_r_1_000
  ,pib_percapita = produto_interno_bruto_per_capita_r_1_00
  ,atividade_maior_vab = atividade_com_maior_valor_adicionado_bruto
)  

# filter 2016
dat = dat %>% dplyr::filter(ano == 2016)

# save data as rds
saveRDS(dat, file="data/working/brasil/pib_municipios_2016.rds")
