library(data.table)
library(stringr)
library(dplyr)
library(fuzzyjoin)
library(tibble)

# read data rds
files = fs::dir_ls("data/working/brasil/",regexp="rds")
dats = list()
for (f in files) {
  dats[[basename(f)]] = readRDS(f)
}

# clean string function
clean_str = function(vchar) {
  vchar %>% 
    tolower() %>% 
    janitor::make_clean_names()
}

# nombres pib-bolsa ---------------------------------------------------------

# clean names
n_bolsa = dats$`bolsafamilia_municipios_2016-12.rds`$nome_municipio_siafi %>% 
  sort() %>% clean_str() %>% tibble(n_bolsa=.)
n_pib = dats$pib_municipios_2016.rds$nome_do_municipio %>% 
  sort() %>% clean_str() %>% tibble(n_pib=.)

# no matches
nomatch_bolsa = n_bolsa %>% anti_join(n_pib, by=c("n_bolsa"="n_pib"))
nomatch_pib = n_pib %>% anti_join(n_bolsa, by=c("n_pib"="n_bolsa"))

# join by strdist
matches_bolsapib = stringdist_inner_join(nomatch_bolsa, nomatch_pib
                                         , by=c("n_bolsa"="n_pib")
                                         ,max_dist=3)

# no matches 2
nomatch_bolsa2 = nomatch_bolsa %>% dplyr::filter(!n_bolsa %in% matches_bolsapib$n_bolsa)
nomatch_pib2 = nomatch_pib %>% dplyr::filter(!n_pib %in% matches_bolsapib$n_pib)

# join by regex match (y faltantes a mano)
matches2_bolsapib = 
  regex_inner_join(nomatch_bolsa2, nomatch_pib2, by=c("n_bolsa"="n_pib")) %>% 
  bind_rows(
    regex_inner_join(nomatch_pib2, nomatch_bolsa2, by=c("n_pib"="n_bolsa"))
  ) %>% 
  bind_rows(
    tribble(~n_bolsa,                 ~n_pib
            ,"sao_domingos_de_pombal", "sao_domingos_5"
    )
  )

# tabla correspondencia final
tab_noms_bolsapib = bind_rows(matches_bolsapib, matches2_bolsapib)


# match pib-bolsa -------------------------------------------------------------------

# clean names (new municipio column)
bolsa = dats$`bolsafamilia_municipios_2016-12.rds` %>% 
  mutate(municipio = clean_str(nome_municipio_siafi))
pib = dats$pib_municipios_2016.rds %>% 
  mutate(municipio = clean_str(nome_do_municipio))

# se conservan nombres pib (Modificamos bolsa)
bolsa_f = bolsa %>% left_join(tab_noms_bolsapib, by=c("municipio"="n_bolsa")) %>% 
  mutate(municipio = coalesce(n_pib, municipio)) %>% 
  select(-n_pib)

# chequeo:
# setdiff(bolsa_f$municipio, pib$municipio)

# nombres pib-varios ---------------------------------------------------------

# clean names
n_varios = dats$varios_municipios_2016.rds$municipio %>% 
  sort() %>% clean_str() %>% tibble(n_varios=.)

# no matches
nomatch_varios = n_varios %>% anti_join(n_pib, by=c("n_varios"="n_pib"))
nomatch_pib = n_pib %>% anti_join(n_varios, by=c("n_pib"="n_varios"))

# join by strdist
matches = stringdist_inner_join(nomatch_varios, nomatch_pib, by=c("n_varios"="n_pib")
                                ,max_dist=3)

# no matches 2
nomatch_varios2 = nomatch_varios %>% dplyr::filter(!n_varios %in% matches$n_varios)
nomatch_pib2 = nomatch_pib %>% dplyr::filter(!n_pib %in% matches$n_pib)

# join by regex match (y faltantes a mano)
matches2 = 
  regex_inner_join(nomatch_varios2, nomatch_pib2, by=c("n_varios"="n_pib")) %>% 
  bind_rows(
    regex_inner_join(nomatch_pib2, nomatch_varios2, by=c("n_pib"="n_varios"))
  ) %>% 
  bind_rows(
    tribble(~n_varios,                    ~n_pib
            ,"presidente_juscelino_3",    "serra_caiada"
            ,"vila_alta",                 "alto_paraiso_2"
            ,"governador_lomanto_junior", "barro_preto"
            ,"santarem_2",                "joca_claudino"
            ,"sao_miguel_de_touros",      "sao_miguel_do_gostoso"
            ,"campo_de_santana",          "tacima"
            ,"sao_domingos_de_pombal",    "sao_domingos_5"
    )
  )

# tabla correspondencia final
tab_noms_pibvarios = bind_rows(matches, matches2)

# match pib-varios -------------------------------------------------------------------

# clean names (new municipio column)
varios = dats$varios_municipios_2016.rds %>% 
  rename(municipio_old = municipio) %>% 
  mutate(municipio = clean_str(municipio_old))

# se conservan nombres pib (Modificamos varios)
varios_f = varios %>% left_join(tab_noms_pibvarios, by=c("municipio"="n_varios")) %>% 
  mutate(municipio = coalesce(n_pib, municipio)) %>% 
  select(-n_pib)

# chequeo:
# setdiff(varios_f$municipio, pib$municipio)
  # solo quedan los municipios demas en varios

# ajustes finales ---------------------------------------------------------

# drop irrelevant vars / rename
varios_f = varios_f %>% 
  select(-c(
    codigo,municipio_old,sigla
  ))
bolsa_f = bolsa_f %>% 
  select(-c(
    codigo_municipio_siafi,nome_municipio_siafi
  )) %>% 
  rename(
    num_beneficiarios_dic2016 = num_beneficiarios
    ,suma_valor_dic2016 = suma_valor
  )
pib_f = pib %>% 
  select(-c(
    ano,codigo_da_grande_regiao,codigo_da_unidade_da_federacao,codigo_do_municipio
    ,nome_do_municipio,sigla_da_unidade_da_federacao
  ))


# write csvs --------------------------------------------------------------

varios_f %>% readr::write_csv("data/working/brasil/varios_municipios_2016.csv")
pib_f %>% readr::write_csv("data/working/brasil/pib_municipios_2016.csv")
bolsa_f %>% readr::write_csv("data/working/brasil/bolsafamilia_municipios_2016-12.csv")

