# Consolidacion de Tablas

library(here); library(purrr)

source(here('investigacion', 
            'scripts', 
            "herramientas_INEI.R"))

# Identificacion de archivos ####
gober <- ruteador(carpeta = here('investigacion', "datos"), 
                           patron_regex = "(?i)Enaho01B-[0-9]{4}-1.sav$") # Gobernabilidad

sumaria  <- ruteador(carpeta = here('investigacion', "datos"), 
                   patron_regex = "(?i)Sumaria-[0-9]{4}.sav$") # Sumarias

# Lectura de archivos y creacion de listas ####
gob_l <- map(gober, leer_sav) %>% set_names(2013:2022)

sum_l <- map(sumaria, leer_sav) %>% set_names(2013:2022)

# Creacion de indices de data frames ####
crear_indice <- function(tabla){
  indice = data.frame(
    name = names(tabla),
    label = rrapply::rrapply(sapply(tabla, attributes), how = 'bind')[1]
  )
  indice$label = tolower(indice$label)
  return(indice)
}

indices_gob_l <- map(gob_l, crear_indice) %>% 
  set_names(2013:2022)

indices_sum_l <- map(sum_l, crear_indice) %>% 
  set_names(2013:2022)
# el numero de variables para cada encuesta cambia a través de los años
# como estandarizo eso?
# esta funcion me devuelve los labels que son iguales en todas las encuestas en 
# un data.frame
find_repeated_observations <- function(df_list, column) {
  unique_values <- lapply(df_list, function(df) unique(df[[column]]))
  common_values <- Reduce(intersect, unique_values)
  
  repeated_data <- lapply(df_list, function(df) df[df[[column]] %in%
                                                     common_values, ]) %>% 
    reduce(intersect)
  return(repeated_data)
}

find_repeated_observations(indices_gob_l, 'label')
find_repeated_observations(indices_sum_l, 'label')

# identificador ####
identificador <- function(df){
  df %>% 
    mutate(ubigeo = stringr::str_pad(trimws(ubigeo),
                                     width = 6,
                                     side = 'left',
                                     pad = 0),
           conglome = stringr::str_pad(trimws(conglome),
                                       width = 4,
                                       side = 'left',
                                       pad = 0),
           vivienda = stringr::str_pad(trimws(vivienda),
                                       width = 3,
                                       side = 'left',
                                       pad = 0),
           id_hogar= paste(año, 
                           ubigeo, 
                           estrato, 
                           conglome, 
                           vivienda, 
                           hogar, sep = "__"),
           cod_dep = substr(ubigeo, start = 1, stop = 2),
           cod_prov = substr(ubigeo, start = 1, stop = 4))
}

gob_l <- map(gob_l, identificador)
sum_l <- map(sum_l, identificador)

region_indice <- data.frame(
  cod_dep = str_pad(seq(1, 25, by = 1), width = 2, side = 'left', pad = '0'),
  region = gsub("^\\s*\\*\\s*", "", c("*   Amazonas", "*   Áncash", 
                                      "*   Apurímac", "*   Arequipa", 
                                      "*   Ayacucho", "*   Cajamarca", 
                                      "Lima", "*   Cusco", "*   Huancavelica", 
                                      "*   Huánuco", "*   Ica", "*   Junín", 
                                      "*   La Libertad", "*   Lambayeque", 
                                      "*   Lima", "*   Loreto", 
                                      "*   Madre de Dios", "*   Moquegua", 
                                      "*   Pasco", "*   Piura", "*   Puno", 
                                      "*   San Martín", "*   Tacna", 
                                      "*   Tumbes", "*   Ucayali"))
)

# Seleccion de variables y estimacion de variable depresion ####
csal_l <- map(csal_l, ~ .x %>% 
                 mutate(caseid = paste0(hhid, "  ", qsnumero)) %>% 
                 select(any_of(c("caseid", "hhid", "qhcluster", "qhnumber", 
                                 "qhhome", "qsnumero", "qsninos", "qs20c", 
                                 "qssexo", "qsmef", "qhcluster" = "qscluster",
                                 "qsdia", "qs22m", "qs22a", "qs23", "qs24", 
                                 "qs25aa", "qs25bb", "qs26", "qs200", "qs201", 
                                 "qs202", "qs211c", "qs402", "qs700a", "qs700b", 
                                 "qs700c", "qs700d", "qs700e", "qs700f", 
                                 "qs700g", "qs700h", "qs700i", "qs702", "qs703", 
                                 "qs704prv", "qs704t", "qs704a", "qs704b", 
                                 "qs704c", "qs704d", "qs704e", "qs704f", 
                                 "qs704g", "qs704h", "qs704i", "qs706", "qs707", 
                                 "qs708", "qs709", "qs710", "qs711", "qs713", 
                                 "qs714", "qs715", "qs716", "qs717", "qs900", 
                                 "peso15_amas",
                                 "peso15_amas" = "peso15años", 
                                 "peso15_amas" = "peso_may15años", 
                                 "peso15_amas" = "peso15_ajus"))))

dep_calc <- function(df, names = c("qs700a", "qs700b", "qs700c", "qs700d", 
                                   "qs700e", "qs700f", "qs700g", "qs700h", 
                                   "qs700i")){
  
  df <- data.table::as.data.table(df)
  
  df <- df[,dep_score := rowSums(.SD, na.rm = TRUE),.SDcols = names]
  
  df[, dep_cats := cut(dep_score, 
                      breaks = c(-Inf, 4, 9, 14, 19, 27), 
                      labels = c("Ninguna", "Leve", "Moderada", 
                                 "Moderadamente Severa", "Severa"))]
  
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
