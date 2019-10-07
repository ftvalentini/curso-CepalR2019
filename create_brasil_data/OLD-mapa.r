
library(ggplot2)
library(magrittr)
library(dplyr)
library(sf)

# download shp from
# ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2016/



# OLD ---------------------------------------------------------------------

# # read file ---------------------------------------------------------------
# 
# sh = sf::st_read("data/raw/brasil/ibge/br_municipios/BRMUE250GC_SIR.shp",)
# sh2 = rgdal::readOGR("data/raw/brasil/ibge/br_municipios/BRMUE250GC_SIR.shp"
#                      , encoding="UTF-8")
# 
# 
# # mapa --------------------------------------------------------------------
# 
# dat = data.frame(
#   CD_GEOCMU = sh2@data$CD_GEOCMU
#   ,var = sample(10, length(sh2@data$CD_GEOCMU), rep=T)
# )
# mapa2 = ggplot2::fortify(sh2, region="CD_GEOCMU")
# 
# aa = ggplot(dat) +
#   geom_map(map=mapa2, aes(map_id = CD_GEOCMU)) +
#   expand_limits(x = mapa2$long, y = mapa2$lat) +
#   coord_map()
# ggsave("output/prueba.png", aa, width=5, height=10)
# 
# 
# 
# # mapa2 -------------------------------------------------------------------
# 
# sh_mun = rgdal::readOGR("data/raw/brasil/ibge/br_municipios/BRMUE250GC_SIR.shp")
# mun = ggplot2::fortify(sh_mun)
# 
# aa2 = ggplot() + 
#   borders("world", regions="Brazil") +
#   geom_path(data=mun, aes(x=long, y=lat, group=group)) +
#   NULL
# ggsave("output/prueba2.png", aa2, width=5, height=10)
# 

