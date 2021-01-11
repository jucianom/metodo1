library(sf)
library(raster)
library(geobr)
library(tidyverse)
library(slopes)
library(tmap)
library(osmextract)


# obtendo os dados das vias da cidade do Rio

# download dos dados das vias no open street maps - o conjunto de dados disponível é de toda Regiao Sudeste do Brasil

# sd_osm <-  oe_get(place = "sudeste", provider = "geofabrik", stringsAsFactors = FALSE, quiet = FALSE,
#                  force_download = TRUE, force_vectortranslate = TRUE) # no meu caso esse procedimento ja foi realizado

# salvando os dados do Sudeste no diretorio de trabalho

st_write(sd_osm, "C:/Users/Juciano Rodrigues/Documents/declive_roads/sd_osm.shp")


# importar os dados das vias da Regiao Sudeste

sd_osm <- st_read("C:/Users/Juciano Rodrigues/Documents/declive_roads/sd_osm.shp")

crs(sd_osm) # verificar o crs

# importa limites do municipio do rio de janeiro atraves do pacote geobr

rio_muni <- read_municipality(
  code_muni = 3304557,
  year = 2010)

rio_muni <-st_transform(rio_muni, crs = 4326) # transforma o crs

# recorta apenas os dados das vias do municipio do Rio

rio_osm <- st_intersection(sd_osm, rio_muni)
plot(rio_osm$geometry) # plotar para simples conferencia


# salvando apenas os dados do Sudeste no diretorio de trabalho

st_write(rio_osm, "C:/Users/Juciano Rodrigues/Documents/declive_roads/rio_osm.shp")


# especiona a variavel highways

highways <- rio_osm %>% 
  group_by(highway) %>% 
  count() %>% 
  st_drop_geometry()

# filtra apenas as vias de interesse para a analise

rio_osm_filter <- rio_osm%>% 
  dplyr::filter(highway %in% c("cicleway", "primary", "primary_link", "residential", "secondary", "secondary_link", "tertiary", 
                               "tertiary_link", "unclassified"))

plot(rio_osm_filter$geometry) # plota o mapa para simples conferencia

# verifica as vias desconectadas e seleciona apenas 
rio_osm_filter$group = rnet_group(rio_osm_filter)
table(rio_osm_filter$group) 

groupos <- rio_osm_filter %>% 
  group_by(group) %>% 
  count() %>% 
  st_drop_geometry()

# salvando os dados do Rio no diretorio de trabalho

st_write(rio_osm_filter, "C:/Users/Juciano Rodrigues/Documents/declive_roads/rio_osm_filter.shp")

# importa os dados raster - Obs.: as quatro folhas que cobrem o municipio do rio foram mescladas no QGis

dem_rio <- raster("C:/Users/Juciano Rodrigues/Documents/declive_roads/dados_raster/rio_de_janeiro/mesclado.tif")
plot(dem_rio)
crs(rio)

# atribui sistema de referencia de coordenadas ao objeto raster
projection(dem_rio) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # atribui crs ao raster
crs(dem_rio)

# visualiza para simples conferencia
raster::plot(dem_rio)
plot(sf::st_geometry(rio_muni), add = TRUE)


# recorta a parte da imagem que cobre apenas o municipio do Rio
altitude_rio <- raster::crop(dem_rio, rio_muni)

# visualiza para simples conferencia
raster::plot(altitude_rio)
plot(sf::st_geometry(rio_muni), add = TRUE)

# verifica compatilizacao dos crs
crs(altitude_rio)
crs(rio_osm_filter)


# calcular a declividade

rio_osm_filter = rio_osm_filter %>% 
  st_cast("LINESTRING")

# quebra a rede em varios segmentos para aumentar a precisao do calculo
nrow(rio_osm_filter)
rede_viaria_rio = stplanr::rnet_breakup_vertices(rio_osm_filter)
nrow(rede_viaria_rio)

# calcula a declividade
rede_viaria_rio$slope = slope_raster(rede_viaria_rio, e = altitude_rio) #28 segundos

rede_viaria_rio$declive = rede_viaria_rio$slope*100
summary(rede_viaria_rio$declive)


rede_viaria_rio$declive_class =  rede_viaria_rio$declive %>%
  cut(
    breaks = c(0, 3, 5, 8, 10, 20, Inf),
    labels = c("0-3: plano", "3-5: leve","5-8: médio", "8-10: exigente", "10-20: terrível", ">20: impossível"),
    right = F
  )
round(prop.table(table(rede_viaria_rio$declive_class))*100,1)


declive_rio <- tm_shape(rede_viaria_rio) +
tm_lines(
       col = "declive_class",
       palette = palredgreen, #palete de cores
       lwd = 1, #espessura das linhas
       title.col = "Declive [%]") +
     tm_shape(rio_muni) +
     tm_borders()

tmap_save(declive_rio, "C:/Users/Juciano Rodrigues/Documents/declive_roads/dados_raster/rio_de_janeiro/declive_rio1.png", dpi = 900)


st_write(rede_viaria_rio, "C:/Users/Juciano Rodrigues/Documents/declive_roads/rede_viaria_rio.shp")
