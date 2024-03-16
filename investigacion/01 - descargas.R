# investigacion ####
pacman::p_load(tidyverse, purrr, haven, labelled, stringr, rrapply, here)
# jalar las funciones de descarga
source(here('investigacion', 
            'scripts', 
            'funciones_descarga_ENAHO.R'))

# los codigos de modulos de gobernanza son 
url_descarga <- solicitud(rango_anos = 2013:2022,
                          modulos = c('34', '85'),
                          formato = 'SPSS')

# Verificacion de tamanos de descarga
url_descarga$tamano <- sapply(url_descarga$urls, verif_url)

# Descarga
walk2(url_descarga$urls, 
      url_descarga$yr, 
      ~ descarga_descompresion(.x, .y, here('investigacion', "datos")))