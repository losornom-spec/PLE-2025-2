install.packages("magrittr")
library(magrittr)   # Paquete de pipes
library(catdata)    # Librería con datos categoricos


# %!>%	Eager pipe
# %$%	  Exposition pipe
# %<>%	Assignment pipe
# %>%	  Pipe
# %T>%	Tee pipe


# Veamos el pipe %>% 


log(sqrt(abs(-2)))

-2 %>% abs() %>% sqrt() %>% log()


# Veamos un ejemplo con datos:

data()   # Permite visualizar las BD de R

data(heart)

heart <- data.frame(heart)

?heart

dim(heart)

# Encabezado de la BD

heart %>% head()


heart %>% tail()


heart$y %>% table()

heart$y %>% table() %>% barplot()


heart$y %>% table() %>% prop.table() %>% round(2)
heart$y %>% table() %>% prop.table() %>% round(2) %>% barplot()


# Pipe de exposicion  %$% 


ifelse(heart$y == 1, "Si", "No")  # Forma clasica


heart$y <- heart %$% ifelse(y== 1, "Si", "No")



heart %$% boxplot(sbp ~ y) 

heart %$% boxplot(tobacco ~ y) 

heart %$% boxplot(obesity ~ y) 

heart %$% boxplot(age ~ y) 

heart %$% min(age)

heart %$% max(age)

# Categorizemos la variable edad

# Trabajemos con el pipe de asignacion %<>%

heart$age %<>% cut(., breaks = c(0, 18, 40, 50, 100), right = FALSE,
                   labels = c("<18", "[18,40)", "[40,50)", ">50"))

heart %>% View()

heart %$% table(y, age) %>% mosaicplot(.,color = TRUE, las = 1)


heart %$% table(y, age) %>% prop.table(margin = 1) %>% mosaicplot(., color = TRUE, las = 1)


heart %$% table(y, age) %>% prop.table(margin = 2) %>% barplot()


# Veamos el pipe Tee %T>%

heart %T>% summary()

# El pipe Tee %T>% permite hacer operaciones intermedias sin afectar
# el flujo del pipe principal


-2 %!>% abs() %!>% sqrt() %!>% log() # entrega lo mismo en ambos casos

-2 %>% abs() %>% sqrt() %>%  log()

f1 <- function(x){
  print("Hola")
  x
}

f2 <- function(x){
  print("como")
  x
}


f3 <- function(x){
  print("estas")
  x
}



-2 %>% f1() %>% f2() %>% f3()
-2 %!>% f1() %!>% f2() %!>% f3()

# Pipe %T>% 


heart %T>% summary() %$%  plot(sbp, tobacco)

heart %T>%  plot(sbp, tobacco) %>% summary()

