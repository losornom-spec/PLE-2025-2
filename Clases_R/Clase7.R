require(tidyverse)
require(janitor)
require(magrittr)
require(maps)
require(ggplot2)
require(leaflet)
require(sf)
require(countrycode)


migrantes <- read_csv(r"(C:\Users\Victus\Downloads\Colombianos_registrados_en_el_exterior_20240201.csv)")


migrantes %>% dim()

migrantes %>% names()

migrantes %>% View()

migrantes %<>% clean_names()

migrantes %>% View()

# Cual es el pais con mas migrantes colombianos?

migrantes %$% table(pais) %>% sort(decreasing = T) %>% prop.table()

migrantes %>%
  mutate(pais = fct_lump(pais, n=30, other_level = "OTROS"))%>%
  group_by(pais) %>%
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  mutate(porc = total/sum(total) * 100) %>% 
  ggplot(aes(x=reorder(pais, porc), y=porc))+
  geom_col()+
  coord_flip()


# Exploremos el paquete maps


paises %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons()
  

# Tarea: Crear mapa con colores Rojo %>10, Amarillo %<5

tabla1 <- migrantes %>% 
  group_by(codigo_iso_pais) %>% 
  count() %>% 
  mutate(porcentaje = 100 * n/nrow(migrantes)) %>% 
  arrange(desc(n)) %>% 
  mutate(color =  cut(porcentaje, breaks = c(0, 1, 5, 100),
                     labels = c("green", "yellow", "red")))

paises <- maps::map(region = tabla1$codigo_iso_pais, fill=TRUE, plot = FALSE)


paises %<>% st_as_sf()

paises$codigo_iso_pais <- paises$ID %>% countrycode(., origin = 'country.name', destination = 'iso3c')

union <- merge(paises, tabla1, by="codigo_iso_pais", all = FALSE)


union %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(fillColor = union$color, weight = 1) %>% 
  addLegend()

# Tarea: examinar a addlegend


































