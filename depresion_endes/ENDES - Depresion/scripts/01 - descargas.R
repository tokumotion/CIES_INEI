# Descarga de Archivos####

library(here)
library(purrr)

# Funciones para la descarga - monta las funciones dentro del environment
source(here("scripts", "funciones_descarga_ENDES.R"))

# Lista de enlaces solicitados
urls_descarga <- solicitud(rango_anos = 2013:2022, 
                           modulos = c("1629", "1640"), 
                           formato = "SPSS")

# Verificacion de tamanos de descarga
urls_descarga$tamano <- sapply(urls_descarga$urls, verif_url)

# Descarga
walk2(urls_descarga$urls, 
      urls_descarga$yr, 
      ~ descarga_descompresion(.x, .y, here("datos")))
?walk2

# Retorno de directorio de trabajo a la carpeta del proyecto
setwd(here())
