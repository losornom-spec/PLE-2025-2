require(tidyverse)
require(sf)
require(magrittr)
require(janitor)
require(leaflet)




migrantes <- read_csv(r"(C:\Users\Victus\Downloads\Colombianos_registrados_en_el_exterior_20240201.csv)")

migrantes %<>% clean_names()

migrantes %>% View()

df <- str_split(migrantes$localizacion, ", ", simplify = FALSE)

migrantes %<>% separate(col="localizacion", into = c("latitud", "longitud"), sep = ",")

migrantes %<>% mutate(latitud = as.numeric(str_replace_all(latitud, "\\(", "")),
                      longitud = as.numeric(str_replace_all(longitud, "\\)", "")))



migrantes %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lat = ~latitud, lng = ~longitud)



migrantes %>% filter(pais=="ESPAÑA") %>% View()



tabla <- migrantes %>% filter(genero %in% c("MASCULINO", "FEMENINO")) %>%
  group_by(genero, pais) %>% summarise(n = n()) %>% 
  mutate(porcentaje = round(100*n/sum(n), 10)) %>% select(-n) %>% 
  spread(genero, porcentaje) %>% 
  filter(FEMENINO <= MASCULINO)


















