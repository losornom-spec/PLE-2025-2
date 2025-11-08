require(reticulate)  # Trabajar con python en R
repl_python()

import numpy as np

matrix_python = np.array([[2, 7, 4], [-1, 3, 5]])

matrix_python

matrix_python.shape


quit



matrix_R = matrix(c(0.8, 0.5, 1, 3, -1, 2), byrow = TRUE, nrow = 2)


# Para acceder desde R a los objetos de Python


py$matrix_python + matrix_R  # Sumar en R



require(rvest)

url <- "https://creg.gov.co/publicaciones/15565/precios-de-combustibles-liquidos/"

acceso1 <-  read_html(url)

elementos <- html_nodes(acceso1, "table")


tablas <- elementos %>% html_table(fill = TRUE)


tablas[[1]]







