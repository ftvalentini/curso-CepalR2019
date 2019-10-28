library(magrittr)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)


# working directory -------------------------------------------------------

getwd()

# opcion a
path = "C:/Users/Fran/Documents/R_scripts_FV/curso-CepalR2019"  
# opcion b: usar proyectos!

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

# EJERCICIO 1:
  # LEER ARCHIVOS ITERANDO (SIN REPETIR EL CODIGO 3 VECES)

# queries (dplyr) ---------------------------------------------------------

# inspeccionar
dim(bolsa)
names(pib)
str(varios)
head(bolsa)
View(bolsa)

# filas
bolsa %>% 
  dplyr::filter(uf == "RJ" & num_beneficiarios_dic2016 > 50000)
bolsa %>% 
  arrange(desc(uf), -num_beneficiarios_dic2016) %>% 
  head()
bolsa %>% 
  slice(20:30)

# columnas
pib %>% select(municipio,vab_agro) %>% head()
pib %>% select(-c(nome_da_grande_regiao,nome_da_unidade_da_federacao)) %>% head()
pib %>% select(municipio)
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

# setdiff(pib$municipio, bolsa$municipio)
# setdiff(bolsa$municipio, pib$municipio)
# setdiff(varios$municipio, bolsa$municipio)

# EJERCICIO 2:
  # que municipios tienen area_geografica faltante? (usar funcion is.na)

# nueva columna
dat %>% mutate(
  prueba = 0
  ,despesa_transporte_2016_pc = despesa_transporte_2016/populacao
)
dat = dat %>%
  mutate(
    densidad = populacao/area_geografica
    ,suma_perbenef = suma_valor_dic2016/num_beneficiarios_dic2016
  ) 
dat = dat %>% 
  mutate_at(
    vars(
      vab_agro,vab_industria,despesa_ciencia_tecnologia_2016
      ,despesa_custeio_pessoal_2016,despesa_educacao_cultura_2016
      ,despesa_transporte_2016,exportacoes_2016,importacoes_2016
      ,num_beneficiarios_dic2016
    ), list(pc=function(x) x/.$populacao)
  ) 
dat = dat %>% 
  mutate_at(vars(codigo_municipio, codigo_uf), as.character)

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
dat$atividade_maior_vab %>% unique()
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
dat = dat %>% mutate_if(is.numeric, function(x) ifelse(is.na(x),0,x))
  # replace(dat, is.na(dat) & is.numeric(dat), 0)

# group by
tab = dat %>% 
  group_by(nome_uf, semiarido) %>% 
  summarise(
    pop = sum(populacao)
    ,n = n()
    ) %>% 
  ungroup() %>% 
  arrange(-pop)

# EJERCICIO 3:
  # obtener cantidad de metropolis (a) por estado y (b) por region
  # obtener densidad poblacional (a) por estado y (b) por region
  # obtener la proporcion de la poblacion del estado que representa cada municipio

# reshape (tidyr) ---------------------------------------------------------

# long to wide
tabw = tab %>% 
  pivot_wider(id_cols=nome_uf, names_from=semiarido, values_from=c(pop,n))

# wide to long
tabl = tab %>% 
  pivot_longer(cols=-c(nome_uf, semiarido), names_to="variable", values_to="value")

# exploratorio ------------------------------------------------------------

# general
skimr::skim(dat)
explora = skimr::skim_to_list(dat)

d_num = dat %>% select_if(is.numeric)
d_cat = dat %>% select_if(function(x) !is.numeric(x))

# categoricas
table(d_cat$tipologia)
table(d_cat$tipologia, d_cat$semiarido)
tabs = d_cat %>% map(function(x) table(x, useNA="ifany"))

# d_cat %>% janitor::tabyl(hierarquia)
# janitor::tabyl(d_cat, hierarquia, tipologia) %>% janitor::adorn_percentages("row")
# tabs = d_cat %>% map(function(x) janitor::tabyl(x))

# cuantiles
quantile(d_num$populacao, seq(0,1,0.05))
cuants = d_num %>% map(function(x) quantile(x, seq(0,1,0.05)))

# correlograma
GGally::ggcorr(dat, label=T, hjust=1, label_size=2, layout.exp=10, size=3)
# GGally::ggcorr(dat, method=c("everything","spearman"), label=T, hjust=1, label_size=2)

# bivariados
bdat = dat %>% 
  select(
    nome_regiao,suma_perbenef,densidad,exportacoes_2016,importacoes_2016
    ,despesa_ciencia_tecnologia_2016
  )
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
  # facet_wrap(semiarido ~ nome_regiao) +
  # theme_minimal() +
  NULL
ggpubr::ggscatterhist(
  dat %>% mutate(pessoal_log=log(despesa_custeio_pessoal_2016_pc))
  , x="suma_perbenef", y="pessoal_log", color="nome_regiao"
  ,size=3, alpha=0.6
  # ,margin.plot="boxplot"
  # ,ggtheme=theme_bw()
)

# box plot
ggplot(dat, aes(x=nome_regiao, y=suma_perbenef, fill=nome_regiao)) +
  geom_boxplot()+
  facet_wrap(~ tipologia) +
  NULL

# histogram/density plot
ggplot(dat, aes(x=log(vab_agro_pc), fill=semiarido)) +
  geom_density() +
  # geom_density(alpha=0.5, adjust=0.1) +
  # geom_density(alpha=0.5, adjust=2) +
  # geom_histogram(alpha=0.5) +
  NULL
ggplot(dat, aes(x=log(populacao), y=tipologia, fill =..x..)) +
  ggridges::geom_density_ridges_gradient(scale=1) +
  scale_fill_viridis_c() +
  NULL
ggplot(dat, aes(x=log(populacao), y=tipologia, fill=tipologia)) +
  ggridges::stat_density_ridges(quantile_lines=T, quantiles=c(.25,.5,.75), alpha=0.7) +
  NULL
ggplot(dat, aes(x=log(populacao), y=tipologia, fill=tipologia)) +
  ggridges::geom_density_ridges(jittered_points=T, point_size=0.5) +
  NULL

# barplot
ggplot(dat) +
  geom_bar(aes(x=nome_regiao, fill=semiarido)) +
  # geom_bar(aes(x=nome_regiao, fill=semiarido), position="fill") +
  # geom_bar(aes(x=nome_regiao, fill=semiarido), position="dodge") +
  NULL
ggplot(dat) +
  geom_col(aes(x=nome_regiao, y=num_beneficiarios_dic2016, fill=semiarido)) + 
  NULL
ggplot(dat) +
  geom_bar(aes(x=nome_regiao, fill=semiarido)
           , position="fill")

# interactive
g = ggplot(dat
           ,aes(x=log(exportacoes_2016), y=log(importacoes_2016)
                ,color=semiarido
                ,label=municipio, label2=nome_regiao)) +
  geom_point(alpha=0.8, size=0.8)
plotly::ggplotly(g)


# mapas -------------------------------------------------------------------

# read shapefiles
sf_mun = sf::read_sf("data/raw/brasil/ibge/br_municipios/BRMUE250GC_SIR.shp")
sf_uf = sf::read_sf("data/raw/brasil/ibge/br_unidades_da_federacao/BRUFE250GC_SIR.shp")

# join con datos
sf_mun = sf_mun %>%
  select(-NM_MUNICIP) %>% 
  left_join(dat, by=c("CD_GEOCMU"="codigo_municipio"))

# create and save map
g = ggplot() +
  geom_sf(data=sf_mun
          ,aes(fill=num_beneficiarios_dic2016_pc), color=NA) +
  geom_sf(data=sf_uf
          ,fill=NA, color="black", size=0.5) +
  # scale_fill_gradient(trans="log10") +
  scale_fill_viridis_c(trans="log", na.value="gray60") +
  theme_minimal() +
  # coord_sf(xlim=c(-60,-48), ylim=c(-35, -27)) +
  theme(legend.position=c(0.15,0.2)) +
  # guides(fill=FALSE) +
  labs(fill="benef. per capita") +
  NULL
ggsave("output/prueba_sf.png", g, width=10, height=8)

# EJERCICIO 4:
  # usar Hmisc::cut2() para mejorar efecto visual del color  
  # HACER MAPA COLOREANDO POR ALGUNA VARIABLE A NIVEL ESTADO
  # HACER MAPA PARA UN SOLO ESTADO COLOREANDO POR ALGUNA VARIABLE A NIVEL municipio


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
