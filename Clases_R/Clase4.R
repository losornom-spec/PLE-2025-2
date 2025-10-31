require(tidyverse)
require(magrittr)
require(janitor)
require(leaflet)
require(sf)

# Leamos la BD de desempleo


desempleo <- read_delim(r"(C:\Users\Victus\Downloads\DESEMPLEO_ECV_2017_MUNICIPIOS.txt)",
                      delim = ";", locale = locale(encoding="latin1"))


colombia <- read_sf(r"(C:\Users\Victus\Downloads\MGN_ADM_MPIO_GRAFICO.shp)")



# Mapa estatico de antioquia


antioquia <- colombia %>% filter(dpto_cnmbr == "ANTIOQUIA")

antioquia %>% 
  ggplot() +
  geom_sf()

# Mapa dinamico de antioquia

antioquia %>% 
  leaflet() %>%
  addTiles() %>% 
  addPolygons(weight = 1, color = "black",
              popup = ~mpio_cnmbr)


# Unimos la BD de desempleo con la de municipios

antioquia %<>% mutate(mpio_cnmbr = str_to_upper(mpio_cnmbr))
desempleo %<>% mutate(Municipio = str_to_upper(Municipio))

setdiff(desempleo$Municipio, antioquia$mpio_cnmbr)
setdiff(antioquia$mpio_cnmbr, desempleo$Municipio)

# Hay 4 municipios en la base de desempleo que no estan en la base de municipios

# Corregir el problema

antioquia$mpio_cnmbr %<>% recode("CAROLINA" = "CAROLINA DEL PRÍNCIPE",
                                 "PEÑOL" = "EL PEÑOL",
                                 "SAN ANDRÉS DE CUERQUÍA" = "SAN ANDRÉS DE CUERQUIA",
                                 "RETIRO" = "EL RETIRO"
                                 )


desempleo$Municipio %<>% recode("LA CEJA DEL TAMBO" = "LA CEJA",
                                "SANTA FE DE ANTIOQUIA" = "SANTA FÉ DE ANTIOQUIA")

antioquia_desempleo <- merge(antioquia, desempleo, by.x = "mpio_cnmbr", by.y = "Municipio", all=TRUE)


antioquia_desempleo %>% View()

antioquia_desempleo %>% 
  ggplot(aes(fill=Total)) +
  geom_sf()+
  geom_sf_label(aes(label=mpio_cnmbr), size=2)


color <- rep(c("red", "orange", "green"), each=50)

antioquia_desempleo$TotAux <- cut(antioquia_desempleo$Total,
                                  breaks = c(0, 5, 10, 100),
                                  labels = c("green", "orange", "red"))



antioquia_desempleo %>%
  leaflet() %>%
  addTiles() %>% 
  addPolygons(weight = 1, 
              popup = paste("Municipio: ", antioquia_desempleo$mpio_cnmbr,
                            "<br>",
                            "Total Desempleados: ", antioquia_desempleo$Total),
              fillColor = ~TotAux,
              opacity = 1,
              fillOpacity = 0.7)



