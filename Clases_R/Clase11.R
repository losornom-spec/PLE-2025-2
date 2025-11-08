require(rvest)
require(magrittr)
require(tidyverse)
require(ggplot2)
require(plotly)



url <- "https://creg.gov.co/publicaciones/15565/precios-de-combustibles-liquidos/"




tablas <- url %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE, convert = FALSE)


fechas_dma <- c( "01/01/2022", "01/02/2022","12/03/2022",
                "01/07/2022", "01/10/2022",
                "01/11/2022", "01/12/2022","01/01/2023",
                "01/02/2023", "01/04/2023",
                "03/05/2023", "03/06/2023","01/07/2023",
                "05/08/2023", "02/09/2023",
                "07/10/2023", "04/11/2023","01/01/2024",
                "24/02/2024", "13/04/2024",
                "26/06/2024", "03/08/2024","31/08/2024",
                "07/09/2024", "17/10/2024",
                "07/11/2024", "01/12/2024","01/01/2025",
                "01/02/2025", "22/03/2025",
                "23/05/2025", "17/06/2025","12/06/2025",
                "17/08/2025", "24/10/2025")

dates <- fechas_dma[35:1]


for(i in 1:35){
  tablas[[i]]["fecha"] = dates[i] %>% strptime(format = "%d/%m/%Y") %>% as.Date()
}

tablas %<>% do.call(rbind, .)

tablas$`Gasolina MC ($/gal)` %<>%  str_replace_all("\\.", "") %>% as.numeric()
tablas$`ACPM ($/gal)`%<>%  str_replace_all("\\.", "") %>% as.numeric()
tablas$Ciudad %<>% str_replace_all("Bogotá.*", "Bogotá")


tablas %>% ggplot(aes(x=fecha, y = `Gasolina MC ($/gal)`, color = Ciudad)) +
  geom_point()+
  geom_line()

p1 <- tablas %>% 
  filter(Ciudad %in% c("Cúcuta", "Medellín", "Bogotá", "Cali", "Barranquilla")) %>% 
  ggplot(aes(x=fecha, y = `Gasolina MC ($/gal)`, color = Ciudad)) +
  geom_point()+
  geom_line()


ggplotly(p1)



p2 <- tablas %>% 
  filter(Ciudad %in% c("Cúcuta", "Medellín", "Bogotá", "Cali", "Barranquilla")) %>% 
  ggplot(aes(x=fecha, y = `ACPM ($/gal)`, color = Ciudad)) +
  geom_point()+
  geom_line()


ggplotly(p2)










