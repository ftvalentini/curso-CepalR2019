
# EJERCICIO 1:
# LEER ARCHIVOS ITERANDO (SIN REPETIR EL CODIGO 3 VECES)
# paths
ps = c(
  "data/working/brasil/bolsafamilia_municipios_2016-12.csv"
  ,"data/working/brasil/pib_municipios_2016.csv"
  ,"data/working/brasil/varios_municipios_2016.csv"
)
# read
dats = map(
  ps
  ,function(p) read.delim(p, header=T, sep=",", dec=".", stringsAsFactors=F
                          ,encoding="UTF-8")
)

# EJERCICIO 2:
# ¿que municipios tienen area_geografica faltante?
dat %>%
  filter(is.na(area_geografica)) %>% 
  pull(municipio)

# EJERCICIO 3:
# obtener cantidad de metropolis (a) por estado y (b) por region
# obtener densidad poblacional (a) por estado y (b) por region
# obtener la proporcion de la poblacion del estado que representa cada municipio
dat %>% 
  group_by(nome_uf) %>% 
  summarise(cant = sum(ifelse(hierarquia=="Metrópole",1,0)))
dat %>% 
  group_by(nome_regiao) %>% 
  summarise(cant = sum(ifelse(hierarquia=="Metrópole",1,0)))
dat %>% 
  group_by(nome_uf) %>% 
  summarise(dens = sum(populacao)/sum(area_geografica))
dat %>% 
  group_by(nome_regiao) %>% 
  summarise(dens = sum(populacao)/sum(area_geografica))
dat %>% 
  group_by(nome_uf) %>% 
  mutate(prop_pop = populacao/sum(populacao)) %>% 
  ungroup() %>% 
  select(municipio, nome_uf, prop_pop)

# EJERCICIO 4:
# (a) usar Hmisc::cut2() para mejorar efecto visual del color  
# (b) HACER MAPA COLOREANDO POR ALGUNA VARIABLE A NIVEL ESTADO
# (c) HACER MAPA PARA UN SOLO ESTADO COLOREANDO POR ALGUNA VARIABLE A NIVEL municipio
# (a)
gdat_mun = sf_mun %>% 
  mutate(num_benef_int = Hmisc::cut2(num_beneficiarios_dic2016_pc, g=5))
g1 = ggplot() +
  geom_sf(data=gdat_mun, aes(fill=num_benef_int), color=NA) +
  geom_sf(data=sf_uf, fill=NA, color="black", size=0.5) +
  scale_fill_viridis_d() +
  theme(legend.position=c(0.15,0.2)) +
  NULL
ggsave("output/prueba_1.png", g1, width=10, height=8)
# (b)
gdat_uf = dat %>% 
  group_by(codigo_uf) %>% 
  summarise(num_benef_pc = sum(num_beneficiarios_dic2016)/sum(populacao)) %>% 
  right_join(sf_uf, by=c("codigo_uf"="CD_GEOCUF"))
g2 = ggplot() +
  geom_sf(data=gdat_uf, aes(fill=num_benef_pc), color="black") +
  scale_fill_viridis_c() +
  theme(legend.position=c(0.15,0.2)) +
  NULL
ggsave("output/prueba_2.png", g2, width=10, height=8)
# (c)
gdat_rio = sf_mun %>%
  dplyr::filter(nome_uf == "Rio de Janeiro")
g3 = ggplot() +
  geom_sf(data=sf_uf, fill="gray", color="black", size=0.5) +
  geom_sf(data=gdat_rio, aes(fill=num_beneficiarios_dic2016_pc), color="black") +
  scale_fill_viridis_c() +
  coord_sf(xlim=c(-45,-40.5), ylim=c(-23.5, -20.5)) +
  NULL
ggsave("output/prueba_3.png", g3, width=10, height=5)

