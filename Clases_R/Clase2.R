library(readxl)     # Lectura de datos
library(magrittr)   # Pipes
library(tidyverse)  # Herramientas de preprocesamiento y graficos
library(janitor)    # Contiene clean_names
library(Amelia)     # Visualizar datos faltantes
library(naniar)     # Visualizar datos faltantes 
library(plotly)     # Visualizacion dinamica



master <- read_csv("DOC_UNAL/SemestreVIII/PLE_2025-2/master.csv")

# Obtenemos informacion sobre la BD

master %>% str() # Forma 1

master %>% glimpse()  # Forma 2

# Dimensiones de la BD


master %>% dim()



# Nombres de la BD

master %>% colnames()

master %>% names()

# Cambiemos los nombres de las variables


master %>% clean_names()


master %>% head()

master %<>% clean_names()

master %>% colnames()




# Entendamos que es cada fila en la BD

master %>% View()



# Filtramos Colombia


master %>% filter(country=="Colombia") %>%
  group_by(year, sex) %>% 
  summarise(poblacion = sum(population),
            n_suicidios = sum(suicides_no),
            sucidios100k = (n_suicidios/poblacion)*100000) %>% 
  ggplot(aes(x=year, y = sucidios100k, col=sex)) + geom_line()
  

p1 <- master %>% filter(country %in% c("Colombia", "Japan", "United States")) %>%
  group_by(year, country) %>% 
  summarise(poblacion = sum(population),
            n_suicidios = sum(suicides_no),
            sucidios100k = (n_suicidios/poblacion)*100000) %>% 
  ggplot(aes(x=year, y = sucidios100k, col=country)) + geom_line()


ggplotly(p1)



# Visualizar los datos faltantes

# Forma 1 del paquete Amelia


master %>% missmap()


# Forma 2 del paquete naniar


master %>% vis_miss()


# Para contar el numero de filas con datos completos usamos


master %>% complete.cases() %>% table() %>% {./nrow(master)}

master %>% complete.cases() %>% table() %>% prop.table()


# Para contar las filas con datos inclompletos 

master %>% complete.cases() %>%  not() %>% table() %>% {./nrow(master)}


master %>% complete.cases() %>% not() %>% sum()










