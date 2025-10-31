require(tidyverse)
require(magrittr)
require(janitor)
require(leaflet)
require(sf)
require(htmlwidgets)

# Leamos la BD de saber 11

agricola <- read_csv(r"(C:\Users\Victus\Documents\DOC_UNAL\SemestreVIII\PLE_2025-2\Trabajo1\Datos\Evaluaciones_Agropecuarias_Municipales_EVA_20250927.csv)")



agricola %>% tail()

agricola %<>% clean_names()

agricola %>% names()


# Rendimiento total por departamento entre 2006 y 2018 en Colombia

agricola %>% filter(cultivo == "CAFE") %>% View()

cafe <- agricola %>% filter(subgrupo_de_cultivo == "CAFE") %>% 
  group_by(departamento) %>%
  summarise(produccion_total = sum(produccion_t),
           area_sum_tot = sum(area_sembrada_ha)) %>% 
  mutate(ren_tot = produccion_total/area_sum_tot)


# Mapa estatico

Colombia <- read_sf(r"(C:\Users\Victus\Downloads\MGN2023_DPTO_POLITICO\MGN_ADM_DPTO_POLITICO.shp)")

# Eliminar los acentos a los registros

Colombia$dpto_cnmbr %<>%  iconv(to = "ASCII//TRANSLIT")
cafe$departamento %<>% iconv(to = "ASCII//TRANSLIT")

# str_trans_general("Latin-ASCII")

setdiff(Colombia$dpto_cnmbr, cafe$departamento)
setdiff(cafe$departamento, Colombia$dpto_cnmbr)


# inner_join()    interseccion
# full_join()     todo
# left_join()     izquierda
# right_join()    derecha

Colombia$departamento <- Colombia$dpto_cnmbr

cafe_col <- full_join(Colombia, cafe, by = c("departamento"))


cafe_col %>% 
  ggplot()+geom_sf(aes(fill=ren_tot))+
  geom_sf_label(aes(label=dpto_cnmbr), size=2)




# Mapa dianamico

color <- cafe_col$ren_tot %>%
  cut(breaks = 3, labels = c("#663000", "#996136", "#CC9B7A"))



mapa1 <- cafe_col %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1, color = "black", fillColor = color, fillOpacity = 0.8,
              popup = paste(cafe_col$dpto_cnmbr, "<br/>", cafe_col$ren_tot))


saveWidget(mapa1, file = r"(C:\Users\Victus\Documents\DOC_UNAL\SemestreVIII\PLE_2025-2\cafe_col.html)")














