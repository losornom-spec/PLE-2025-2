require(tidyverse)
require(ggplot2)
require(plotly)
require(rAmCharts4)


datos <- read_csv(r"(C:\Users\Victus\Downloads\MEN_MATRICULA_ESTADISTICA_ES_Limpio.csv)")
datos$principal_o_seccional


gf1 <- datos %>% group_by(principal_o_seccional) %>% summarise(n = n()) %>% 
  mutate(porcentaje = n/sum(n))%>% 
  ggplot(aes(y=porcentaje, x=principal_o_seccional))+
  geom_col()

gf1 %>% ggplotly()


datos %>% filter(ano == 2021 & institucion_de_educacion_superior_ies == "UNIVERSIDAD NACIONAL DE COLOMBIA") %>% 
  group_by(id_genero) %>% summarise(suma = sum(total_matriculados)) %>% mutate(porcentaje = suma/sum(suma)) %>% 
  amPieChart(category = "id_genero", value="porcentaje")


gf2 <- datos %>% filter(ano == 2021) %>% group_by(departamento_de_domicilio_de_la_ies) %>% 
  summarise(suma = sum(total_matriculados)) %>% mutate(porcentaje = 100*(suma/sum(suma)))%>% 
  ggplot(aes(x=porcentaje, y=departamento_de_domicilio_de_la_ies))+
  geom_col()


gf2 %>% ggplotly()



gf3 <- datos %>% filter(ano == 2021 & institucion_de_educacion_superior_ies == "UNIVERSIDAD NACIONAL DE COLOMBIA") %>%
  group_by(departamento_de_domicilio_de_la_ies) %>% 
  summarise(suma = sum(total_matriculados)) %>% 
  arrange(desc(suma)) %>% 
  ggplot(aes(x=suma, y =reorder(departamento_de_domicilio_de_la_ies,suma)))+
  geom_col()
  
gf3 %>% ggplotly()











