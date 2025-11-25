library(readxl)
library(magrittr)
library(janitor)
library(tidyverse)


datos <- read.csv(r"(C:\Users\Victus\Documents\DOC_UNAL\SemestreVIII\PLE-2025-2\Trabajo3\datos\MEN_MATRICULA_ESTADISTICA.csv)")

datos %<>% clean_names() 


datos %$% table(principal_o_seccional)

datos %<>% mutate(principal_o_seccional = str_to_upper(principal_o_seccional))

datos %$% unique(departamento_de_domicilio_de_la_ies)

lista_dep = c("SAN ANDRES Y PROVI", "ARCHIPIELAGO DE SA", "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
              "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA", "SAN ANDRES Y PROVIDENCIA")

datos %<>% mutate(departamento_de_domicilio_de_la_ies = str_to_upper(departamento_de_domicilio_de_la_ies),
                  departamento_de_domicilio_de_la_ies = iconv(departamento_de_domicilio_de_la_ies, from = "UTF-8", to="ASCII//TRANSLIT"),
                  departamento_de_domicilio_de_la_ies = recode(departamento_de_domicilio_de_la_ies,
                                                               "BOGOTA, D.C." = "BOGOTÁ",
                                                               "BOGOTA D.C" = "BOGOTÁ",
                                                               "NARINIO" = "NARINO",
                                                               "LA GUAJIRA" = "GUAJIRA",
                                                               "BOGOTA D.C." = "BOGOTÁ"
                  ),
                  departamento_de_domicilio_de_la_ies = ifelse(departamento_de_domicilio_de_la_ies %in% lista_dep, "SAN ANDRÉS", departamento_de_domicilio_de_la_ies))


# codigo_del_municipio_ies

datos %$% unique(codigo_del_municipio_ies)

# municipio_dedomicilio_de_la_ies

datos %<>% mutate(municipio_dedomicilio_de_la_ies = iconv(municipio_dedomicilio_de_la_ies, from="UTF-8", to="ASCII//TRANSLIT"),
                  municipio_dedomicilio_de_la_ies = str_to_upper(municipio_dedomicilio_de_la_ies),
                  municipio_dedomicilio_de_la_ies = str_replace_all(municipio_dedomicilio_de_la_ies,",", ""),
                  municipio_dedomicilio_de_la_ies = recode(municipio_dedomicilio_de_la_ies,
                                                           "OCA?A" = "OCANA",
                                                           "SANTA FE DE ANTIOQU" = "SANTA FE DE ANTIOQUIA",
                                                           "SANTAFE DE ANTIOQUIA" = "SANTA FE DE ANTIOQUIA",
                                                           "SANTAFE DE BOGOTA" = "SANTA FE DE BOGOTA",
                                                           "EL CARMEN DE VIBORA" = "EL CARMEN DE VIBORAL",
                                                           "CARMEN DE VIBORAL" = "EL CARMEN DE VIBORAL",
                                                           "CARTAGENA DE INDIAS" = "CARTAGENA",
                                                           "COVEAS (SUCRE)" = "COVENAS",
                                                           "VILLA DE SAN DIEGO DE UBATE" = "VILLA DE SAN DIEGO",
                                                           "ESPINAL (CHICORAL)" = "ESPINAL"))

datos %<>% mutate(programa_academico = iconv(programa_academico, from="UTF-8", to="ASCII//TRANSLIT"),
                  programa_academico = str_to_upper(programa_academico))


datos %<>% mutate(Nivel_Academico = case_when(
  str_detect(programa_academico, "DOCTOR") ~ "DOCTORADO",
  str_detect(programa_academico, "MAEST") ~ "MAESTRIA",
  str_detect(programa_academico, "ESPECIA") ~ "ESPECIALIZACION",
  str_detect(programa_academico, "TECNOL") ~ "TECNOLOGO",
  str_detect(programa_academico, "TECNIC") ~ "TECNICO",
  TRUE ~ "PROFESIONAL"
  ))

datos %<>% mutate(id_genero=case_when(
  id_genero== 2 ~ "FEMENINO",
  id_genero== 1 ~ "MASCULINO"
))

datos %$% unique(institucion_de_educacion_superior_ies)


datos %<>% mutate(IES = fct_lump(institucion_de_educacion_superior_ies, n = 10, other_level = "OTRAS"))



write.csv(datos, file = r"(C:\Users\Victus\Documents\DOC_UNAL\SemestreVIII\PLE-2025-2\Trabajo3\datos\MEN_MATRICULA_ESTADISTICA_LIMPIA.csv)")













