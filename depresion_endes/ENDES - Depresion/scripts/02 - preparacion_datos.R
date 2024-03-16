# Consolidacion de Tablas

library(here)

source(here("scripts", "herramientas_INEI.R"))

# Identificacion de archivos ####

rec0 <- ruteador(carpeta = here("datos"), 
                 patron_regex = "rech0.sav") # Hogar

rech1  <- ruteador(carpeta = here("datos"), 
                   patron_regex = "rech1.sav") # Persona - General

csalud <- ruteador(carpeta = here("datos"), 
                   patron_regex = "csalud01.sav") # Persona - Salud

# Lectura de archivos y creacion de listas ####

csal_l <- map(csalud, leer_sav) %>% set_names(2013:2022)

rec0_l <- map(rec0, leer_sav) %>% set_names(2013:2022)

rech1_l <- map(rech1, leer_sav) %>% set_names(2013:2022)

# Seleccion de variables y estimacion de variable depresion ####

csal_l <- map(csal_l, ~ .x %>% 
                 mutate(caseid = paste0(hhid, "  ", qsnumero)) %>% 
                 select(any_of(c("caseid", "hhid", "qhcluster", "qhnumber", "qhhome", "qsnumero", 
                                 "qsninos", "qs20c", "qssexo", "qsmef", "qhcluster" = "qscluster",
                                 "qsdia", "qs22m", "qs22a", "qs23", "qs24", "qs25aa", "qs25bb", 
                                 "qs26", "qs200", "qs201", "qs202", "qs211c", "qs402", 
                                 "qs700a", "qs700b", "qs700c", "qs700d", "qs700e", "qs700f", 
                                 "qs700g", "qs700h", "qs700i", "qs702", "qs703", "qs704prv", "qs704t", 
                                 "qs704a", "qs704b", "qs704c", "qs704d", "qs704e", "qs704f", "qs704g", 
                                 "qs704h", "qs704i", "qs706", "qs707", "qs708", "qs709", "qs710", 
                                 "qs711", "qs713", "qs714", "qs715", "qs716", "qs717", "qs900", 
                                 "peso15_amas","peso15_amas" = "peso15años", "peso15_amas" = "peso_may15años", "peso15_amas" = "peso15_ajus"))))

dep_calc <- function(df, names = c("qs700a", "qs700b", "qs700c", "qs700d", "qs700e", "qs700f", "qs700g", "qs700h", "qs700i")){
  
  df <- data.table::as.data.table(df)
  
  df <- df[,dep_score := rowSums(.SD, na.rm = TRUE),.SDcols = names]
  
  df[, dep_cats := cut(dep_score, 
                      breaks = c(-Inf, 4, 9, 14, 19, 27), 
                      labels = c("Ninguna", "Leve", "Moderada", "Moderadamente Severa", "Severa"))]
  
  df[, dep_dummy := cut(dep_score, 
                        breaks = c(-Inf, 9, 27), 
                        labels = c("Sin Depresion", "Depresion"))]
  
  return(df)
  
}

csal_l <- csal_l %>% map(dep_calc)

etiquetas <- val_labels(rec0_l$"2018"$hv025)
rec0_l <- reemplazar_etiquetas(rec0_l, "hv025", etiquetas)

rech1_l <- map(rech1_l, ~ .x %>% 
                mutate(caseid = paste0(hhid, "  ", hvidx)))

# Union de tablas

consolidado <- map2(csal_l, rech1_l, ~ left_join(.x, .y, by = c("caseid", "hhid")))

consolidado <- map2(consolidado, rec0_l, ~ left_join(.x, .y, by = "hhid"))

save(consolidado, file = here("datos", "consolidado.RData"))
