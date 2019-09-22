library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(forcats)


# TODO --------------------------------------------------------------------

# hacer todos los scatter plots en tibble o pdf (ver mimic) 

# working directory -------------------------------------------------------

getwd()
path = "C:/Users/Fran/Documents/R_scripts_FV/curso-CepalR2019"  
# o usar proyectos!

# read data ---------------------------------------------------------------

# paths
p_bolsa = "data/working/brasil/bolsafamilia_municipios_2016-12.csv"
p_pib = "data/working/brasil/pib_municipios_2016.csv"
p_varios = "data/working/brasil/varios_municipios_2016.csv"
# read
bolsa = read.delim(p_bolsa, header=T, sep=",", dec=".", stringsAsFactors=F
                     ,encoding="UTF-8")
pib = read.delim(p_pib, header=T, sep=",", dec=".", stringsAsFactors=F
                   ,encoding="UTF-8")
varios = read.delim(p_varios, header=T, sep=",", dec=".", stringsAsFactors=F
                      ,encoding="UTF-8")

# EJERCICIO:
  # LEER ARCHIVOS ITERANDO (SIN REPETIR EL CODIGO R VECES)

# queries / dplyr ---------------------------------------------------------

# inspeccionar
dim(bolsa)
str(varios)
glimpse(pib)
head(bolsa)
View(bolsa)

# filter/order rows
bolsa %>% 
  dplyr::filter(uf == "RJ" & num_beneficiarios_dic2016 > 50000)
bolsa %>% 
  arrange(desc(num_beneficiarios_dic2016),municipio) %>% 
  head()

# columns
pib %>% select(municipio,vab_agro) %>% head()
pib %>% select(-c(nome_da_grande_regiao,nome_da_unidade_da_federacao))
pib %>% pull(municipio)
pib$municipio

# rename
pib = pib %>%
  rename(
    nome_regiao = nome_da_grande_regiao
    ,nome_uf = nome_da_unidade_da_federacao
    ,hierarquia = hierarquia_urbana_principais_categorias
    ,tipologia = tipologia_rural_urbana
    ,populacao = populacao_nº_de_habitantes
  )
  # pib %>% janitor::clean_names()

# joins
dat = bolsa %>%
  inner_join(pib, by="municipio") %>% 
  left_join(varios, by="municipio")

# nueva columna
dat %>% mutate(
  prueba = 0
  ,despesa_transporte_2016_pc = despesa_transporte_2016/populacao
)
dat = dat %>%
  mutate(
    densidad = populacao/area_geografica
    ,suma_perbenef = suma_valor_dic2016/num_beneficiarios_dic2016
  ) %>% 
  mutate_at(
    vars(
      vab_agro,vab_industria,despesa_ciencia_tecnologia_2016
    ,despesa_custeio_pessoal_2016,despesa_educacao_cultura_2016
    ,despesa_transporte_2016,exportacoes_2016,importacoes_2016
    ), list(pc=function(x) x/.$populacao))

# ifelse
dat %>% 
  mutate(
    semiarido = ifelse(semiarido == "Não", F, T)
  )

# case_when
dat %>% 
  mutate(
    regiao = case_when(
      nome_regiao == "Norte" ~ "N"
      ,nome_regiao == "Nordeste" ~ "NE"
      ,nome_regiao == "Centro-oeste" ~ "CO"
      ,nome_regiao == "Sudeste" ~ "SE"
      ,nome_regiao == "Sul" ~ "S"
      ,TRUE ~ "error"
    )
  )

# agrupar categorias poco frecuentes
dat %>% 
  mutate(
    atividade = forcats::fct_lump(atividade_maior_vab, prop=0.1, other_level="Otros")
  )

# medidas resumen
dat %>% summarise(
  suma_media = mean(suma_valor_dic2016)
  ,pop_max = max(populacao)
  ,area_min = min(area_geografica, na.rm=T)
)
dat %>% 
  summarise_if(is.numeric, list(media=mean, max=max, min=min))

# replace NAs
dat = dat %>% mutate_if(is.numeric
                        , function(x) ifelse(is.na(x),0,x))

# group by
tab = dat %>% 
  group_by(nome_uf, semiarido) %>% 
  summarise(s = sum(populacao)) %>% 
  ungroup() %>% 
  arrange(-s)

# EJERCICIO:
  # hacer consultas interesantes!

# reshape / tidyr ---------------------------------------------------------

# revisar si ya existe tidyr::pivot_longer y tidyr::pivot_wider !!!

# long to wide
tabw = tab %>% 
  spread(key=semiarido, value=s, fill=0, sep="_")
# wide to long
tabw %>% 
  gather(key="semiarido", value="pop", -nome_uf)


# exploratorio ------------------------------------------------------------

# general
skimr::skim(dat)
explora = skimr::skim_to_list(dat)

d_num = dat %>% select_if(is.numeric)
d_cat = dat %>% select_if(function(x) !is.numeric(x))

# categoricas
table(d_cat$tipologia)
table(d_cat$tipologia,d_cat$semiarido)
d_cat %>% janitor::tabyl(hierarquia)
janitor::tabyl(d_cat, hierarquia, tipologia)
tabs = d_cat %>% purrr::map(function(x) table(x, useNA="ifany"))
tabs = d_cat %>% purrr::map(function(x) janitor::tabyl(x))

# cuantiles
quantile(d_num$populacao, seq(0,1,0.05))
cuants = d_num %>% map(function(x) quantile(x, seq(0,1,0.05)))

# correlograma
GGally::ggcorr(dat, label=T, hjust=1, label_size=2.5, layout.exp=10)
# GGally::ggcorr(dat, method=c("everything","spearman"), label=T, hjust=1, label_size=2)

# bivariados
bdat = dat %>% 
  select(nome_regiao,suma_perbenef,densidad,exportacoes_2016_pc
         ,importacoes_2016_pc,despesa_ciencia_tecnologia_2016)
  
GGally::ggpairs(bdat, mapping=aes(color=nome_regiao))
GGally::ggpairs(bdat, mapping=aes(color=nome_regiao)
                ,columns=c("nome_regiao","suma_perbenef"))
                

# plots -------------------------------------------------------------------

# scatter plot
ggplot(dat, aes(x=log(pib_percapita), y=suma_perbenef, color=nome_regiao)) +
  geom_point() +
  NULL
ggplot(dat, aes(x=log(pib), y=log(suma_valor_dic2016))) +
  geom_point(aes(color=nome_regiao)) +
  geom_smooth(se=F, method="lm") +
  # facet_grid(~ nome_regiao) +
  facet_wrap(~ nome_regiao) +
  # theme_minimal() +
  NULL
ggpubr::ggscatterhist(
  dat %>% mutate(pessoal_log=log(despesa_custeio_pessoal_2016_pc))
  , x="suma_perbenef", y="pessoal_log", color="nome_regiao"
  ,size=3, alpha=0.6
  # ,margin.plot="boxplot"
  ,ggtheme=theme_bw()
)

# box plot
ggplot(dat, aes(x=nome_regiao, y=suma_perbenef, fill=nome_regiao)) +
  geom_boxplot()+
  facet_wrap(~ tipologia) +
  NULL

# histogram/density plot
ggplot(dat, aes(x=log(vab_agro_pc), fill=semiarido)) +
  # geom_histogram(alpha=0.5) + 
  # geom_density(alpha=0.5, adjust=0.1) +
  geom_density(alpha=0.5, adjust=2) +
  NULL
ggplot(dat, aes(x=log(populacao), y=tipologia, fill =..x..)) +
  geom_density_ridges_gradient(scale=1) +
  viridis::scale_fill_viridis() +
  NULL
ggplot(dat, aes(x=log(populacao), y=tipologia, fill=tipologia)) +
  stat_density_ridges(quantile_lines=T, quantiles=c(.25,.5,.75), alpha=0.7) +
  NULL
ggplot(dat, aes(x=log(populacao), y=tipologia, fill=tipologia)) +
  geom_density_ridges(jittered_points=T) +
  NULL

# interactive
g = ggplot(dat,aes(x=log(exportacoes_2016_pc), y=log(importacoes_2016_pc)
                    ,color=semiarido
                    ,label=municipio, label2=nome_regiao)) +
  geom_point(alpha=0.8, size=0.8)
plotly::ggplotly(g)


# bonus track -------------------------------------------------------------

# todos los scatter en un pdf

# genera log de todas las vars
d_num_exp = d_num %>%
  mutate_all(list("log" = function(x) ifelse(x==0, 0, sign(x)*log(abs(x))) ))

# hay combn(48,2)=2256 scatterplots posibles!!!
  # --> elegimos las de alta cor (pearson)

# Cor en niveles (only lower triangle of matrix)
mcor_n = cor(d_num, method="pearson")
mcor_n[upper.tri(mcor_n, diag=T)] = NA
tcor_n = mcor_n %>% data.table::melt()

# Cor en logs (only lower triangle of matrix)
mcor_l = cor(d_num_exp %>% select(ends_with("_log")), method="pearson")
mcor_l[upper.tri(mcor_l, diag=T)] = NA
tcor_l = mcor_l %>% data.table::melt()

# tabla con todas las cors altas
tcor = rbind(tcor_n, tcor_l) %>% 
  dplyr::filter(value>0.5) %>% 
  arrange(-value)

# lista de plots
g_cor = map2(
  .x=tcor$Var1, .y=tcor$Var2
  ,function(x,y) qplot(d_num_exp[[x]], d_num_exp[[y]], xlab=x, ylab=y,
                       size=I(0.5), geom="jitter", alpha=I(0.5))
)

# save plots
ggsave("output/scatter_brasil.pdf",width=6,height=6,
       gridExtra::marrangeGrob(grobs=g_cor, nrow=2, ncol=1))
