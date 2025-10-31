require(tidyverse)
require(magrittr)
require(ggplot2)
require(janitor)
require(naniar)
require(visdat)
require(Amelia)

datos <- read_csv(r"(C:\Users\Victus\Downloads\MEN_MATRICULA_ESTADISTICA_ES_20251029.csv)")

# Identificar valores missing

datos %<>% clean_names() 

datos %>% miss_var_summary() %>% print(n=26)

datos %>% gg_miss_var()

# Revisar que las observaciones de las variables estén bien escritas

## institucion_de_educacion_superior_ies

datos %$% unique(institucion_de_educacion_superior_ies)

## principal_o_seccional

datos %$% unique(principal_o_seccional)

datos %<>% mutate(principal_o_seccional = str_to_upper(principal_o_seccional))

## id_sector

datos %$% unique(id_sector)

# id_character

datos %$% unique(id_caracter)

# codigo_del_departamento_ies

datos %$% unique(codigo_del_departamento_ies)

# departamento_de_domicilio_de_la_ies

datos %$% unique(departamento_de_domicilio_de_la_ies)

lista_dep = c("SAN ANDRES Y PROVI", "ARCHIPIELAGO DE SA", "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
              "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA")

datos %<>% mutate(departamento_de_domicilio_de_la_ies = str_to_upper(departamento_de_domicilio_de_la_ies),
                  departamento_de_domicilio_de_la_ies = iconv(departamento_de_domicilio_de_la_ies, from = "UTF-8", to="ASCII//TRANSLIT"),
                  departamento_de_domicilio_de_la_ies = recode(departamento_de_domicilio_de_la_ies,
                                                               "BOGOTA, D.C." = "BOGOTA D.C.",
                                                               "BOGOTA D.C" = "BOGOTA D.C.",
                                                               "NARINIO" = "NARINO",
                                                               "LA GUAJIRA" = "GUAJIRA"
                                                               ),
                  departamento_de_domicilio_de_la_ies = ifelse(departamento_de_domicilio_de_la_ies %in% lista_dep, "SAN ANDRES Y PROVIDENCIA", departamento_de_domicilio_de_la_ies))

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

# codigo_snies_delprograma

datos %$% unique(codigo_snies_delprograma)

# programa_academico

datos %<>% mutate(programa_academico = iconv(programa_academico, from="UTF-8", to="ASCII//TRANSLIT"),
                          programa_academico = str_to_upper(programa_academico))

# id_nivel

datos %$% unique(id_nivel) 

# id_nivel_formacion

datos %$% unique(id_nivel_formacion)

# id_metodologia

datos %$% unique(id_metodologia)

# id_area

datos %$% unique(id_area)

# id_nucleo

datos %$% unique(id_nucleo)

# nucleo_basico_del_conocimiento_nbc


datos %<>% mutate(nucleo_basico_del_conocimiento_nbc = str_to_upper(nucleo_basico_del_conocimiento_nbc),
                          nucleo_basico_del_conocimiento_nbc = iconv(nucleo_basico_del_conocimiento_nbc, from="UTF-8", to = "ASCII//TRANSLIT"),
                          nucleo_basico_del_conocimiento_nbc = str_replace_all(nucleo_basico_del_conocimiento_nbc, ",", ""),
                          nucleo_basico_del_conocimiento_nbc = recode(nucleo_basico_del_conocimiento_nbc,
                                                                      "ATES PLASTICAS VISUALES Y AFINES"="ARTES PLASTICAS VISUALES Y AFINES",
                                                                      "IINGENIERIA INDUSTRIAL ALIMENTOS Y AFINES" = "INGENIERIA AGROINDUSTRIAL ALIMENTOS Y AFINES", 
                                                                      "INGENIERIA ADMNISTRATIVA Y AFINES" = "INGENIERIA ADMINISTRATIVA Y AFINES",
                                                                      "ANTROPOLOGIA Y  ARTES LIBERALES" = "ANTROPOLOGIA ARTES LIBERALES"))

# codigo_del_departamento_programa

datos %$% unique(codigo_del_departamento_programa)

# departamento_de_oferta_del_programa

datos %<>% mutate(departamento_de_oferta_del_programa = str_to_upper(departamento_de_oferta_del_programa),
                  departamento_de_oferta_del_programa = iconv(departamento_de_oferta_del_programa,from = "UTF-8",to = "ASCII//TRANSLIT"),
                  departamento_de_oferta_del_programa = recode(departamento_de_oferta_del_programa,
                                                               "BOGOTA, D.C." = "BOGOTA D.C.",
                                                               "BOGOTA D.C" = "BOGOTA D.C.",
                                                               "NARINIO" = "NARINO",
                                                               "LA GUAJIRA" = "GUAJIRA",), 
                  departamento_de_oferta_del_programa = ifelse(departamento_de_oferta_del_programa %in% lista_dep, "SAN ANDRES Y PROVIDENCIA", departamento_de_oferta_del_programa)
                  )


# id_genero

datos %$% unique(id_genero)

# ano

datos %$% unique(ano)

# semestre

datos %$% unique(semestre)


datos %$% unique(total_matriculados)


# Lectura de mapas de departamento y municipios

departamento <- read









# Guardado de datos


write.csv(datos, r"(C:\Users\Victus\Downloads\MEN_MATRICULA_ESTADISTICA_ES_Limpio.csv)")




