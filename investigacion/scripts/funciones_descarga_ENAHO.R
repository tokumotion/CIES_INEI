# Funciones de Soporte - ENDES ####

library(httr)
library(gdata)
library(dplyr)

verif_url <- function(url_descarga) {
  
  encabezado <- HEAD(url_descarga)
  
  tamaño_descarga <- as.numeric(encabezado$headers$`content-length`)
  
  humanReadable(tamaño_descarga, units = "auto")
  
}

solicitud <- function(rango_anos, modulos, formato = "STATA") {
  
  # Usar codificación nueva.
  
  indice_codigos_ENDES <- data.frame(yr = 2004:2022, 
                                     cod_encuesta = c("280", "281", "282", "283", "284",
                                                      "285", "279", "291", "324", "404", 
                                                      "440", "498", "546", "603",
                                                      "634", "687", "737", "759", "784"))
  
  hermes <- expand.grid(yr = rango_anos, mod = modulos)
  
  anos_reemplazo <- data.frame(yr = c(2004:2019))
  
  cods_reemplazo <- data.frame(
    
    mod = c("1629", "1630", "1631", "1632", "1633", "1634", "1635", "1636", "1637", "1638", "1639", "1640", "1641"),
    mods_url = c("64", "65", "66", "67", "69", "70", "71", "72", "73", "74", "413", "414", "569")
    
  )
  
  a <- inner_join(cods_reemplazo %>% mutate(key=1), 
                  anos_reemplazo %>% mutate(key=1), 
                  by="key", 
                  relationship = "many-to-many") %>% 
    select(-key)
  
  hermes <- left_join(hermes, a, by = c("yr", "mod")) %>% 
    mutate(mods_url = if_else(is.na(mods_url), mod, mods_url))
  
  hermes <- dplyr::left_join(x = hermes, 
                             y = dplyr::filter(indice_codigos_ENDES, 
                                               yr %in% rango_anos), 
                             by = "yr")
  
  hermes$urls <- paste0("https://proyectos.inei.gob.pe/iinei/srienaho/descarga/",
                        formato,
                        "/",
                        hermes$cod_encuesta,
                        "-Modulo",
                        hermes$mods_url,
                        ".zip")
  
  hermes
  
}


descarga_descompresion <- function(url_descarga,
                                   nombre_carpeta_descompresion,
                                   ruta_principal, 
                                   patron_tablas_separar = "*.sav|*.SAV") {
  
  # Carpeta de destino
  omega <- ruta_principal
  
  # Creamos las carpetas que hagan falta para completar la ruta (recursive = TRUE)
  dir.create(path = omega, recursive = TRUE)
  
  setwd(omega)
  
  t_zip <- tempfile(tmpdir = omega)
  
  download.file(url      = url_descarga,
                destfile = t_zip, 
                mode     = "wb")
  
  carpeta_descompresion <- file.path(omega, nombre_carpeta_descompresion)
  
  nombres_archivos_por_unzip <- unzip(zipfile = t_zip, list = TRUE) # Sacar Nombres de Archivos dentro del ZIP
  
  que_elementos_estan_bien <- validEnc(nombres_archivos_por_unzip$Name) # Verificar qué elementos tienen problemas
  
  archivos_con_nombre_correcto <- nombres_archivos_por_unzip$Name[que_elementos_estan_bien] # Filtramos y dejamos archivos sin problemas
  
  archivos_con_nombre_INcorrecto <- nombres_archivos_por_unzip$Name[!que_elementos_estan_bien]
  
  unzip(zipfile = t_zip, exdir = carpeta_descompresion, files = archivos_con_nombre_correcto)
  
  setwd(carpeta_descompresion)
  
  archivos <- list.files(recursive = TRUE, 
                         pattern = patron_tablas_separar)
  
  archivos <- archivos[stringr::str_detect(string = archivos, "tablas/", negate = TRUE)]
  
  ruta_subcarpeta <- file.path(carpeta_descompresion, "tablas")
  
  dir.create(ruta_subcarpeta)
  
  file.copy(from = archivos,
            to = ruta_subcarpeta)
  
  file.remove(archivos) # CUIDADO!
  
  unlink(t_zip)
  
}

print("Librerías cargadas: dplyr, httr, gdata")