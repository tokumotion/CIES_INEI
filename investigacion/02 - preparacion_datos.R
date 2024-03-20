# Consolidacion de Tablas

# helper functions y librerias ####
pacman::p_load(questionr, here, hutils, srvyr, tidyverse, rrapply, labelled, sjmisc)

source(here('investigacion', 
            'scripts', 
            "herramientas_INEI.R"))

# Identificacion de archivos ####
gober <- ruteador(carpeta = here('investigacion', "datos"), 
                           patron_regex = "(?i)Enaho01B-[0-9]{4}-1.sav$") # Gobernabilidad

sumaria  <- ruteador(carpeta = here('investigacion', "datos"), 
                   patron_regex = "(?i)Sumaria-[0-9]{4}.sav$") # Sumarias

# Lectura de archivos y creacion de listas ####
gob_l <- map(gober, leer_sav) %>% set_names(2013:2022) %>% 
  keep(names(.) %in% 2013:2022)

sum_l <- map(sumaria, leer_sav) %>% set_names(2013:2022) %>% 
  keep(names(.) %in% 2013:2022)

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
  common_values <- reduce(map(df_list, \(x) pluck(x, "name")), intersect)   
  
  repeated_data <- map(df_list, 
                       function(x) filter(x, name %in% common_values)) %>% 
    .[length(.)]
  
  return(tibble(repeated_data[[1]]))
}

rep_obs_gob_l <- find_repeated_observations(indices_gob_l, 'name')
rep_obs_sum_l <- find_repeated_observations(indices_sum_l, 'name')

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
# cambiar los titulos de los dataframes
names_gob_l <- map(gob_l, ~ .x %>% names) %>% 
  map(., ~ .x %>% 
        str_replace_all(patter = '\\$', replacement = "_"))
# se cambia el nombre de las columnas en cada dataframe usando set_names()
gob_l <- map2(.x = gob_l, .y = names_gob_l, .f = ~ set_names(.x, .y))
sum_l <- map(sum_l, identificador)
# tabla de regiones ####
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

# funciones para agregar variables explicativas a sumarias ####

`%nin%` <- Negate(`%in%`)

sum_l <- map(sum_l, ~ .x %>% group_by(pobreza) %>% mutate(fac_pob = factor07 * mieperho))
sum_l <- map(sum_l, ~ .x %>% mutate(ingreso_1 = ingmo1hd/(mieperho * 12 * ld)))
sum_l <- map(sum_l, ~ .x %>% mutate(decil_ingreso = weighted_ntile(ingreso_1, 
                                                        weights = fac_pob, 
                                                        n = 10)))
sum_l <- map(sum_l, ~ .x %>% left_join(y = region_indice, by = 'cod_dep'))
sum_l <- map(sum_l, ~ .x %>% mutate(rural = labelled(as.numeric(estrato > 5)),
                                    urbano = labelled(as.numeric(estrato <= 5))) %>% 
               set_value_labels(rural = c(Rural = 1, Urbano = 0),
                                urbano = c(Rural = 0, Urbano = 1)))
sum_l <- map(sum_l, ~ .x %>% mutate(reg_nat = rec(dominio,
                                                  rec = c('1:3 = 1 [Costa];
                                                  4:6 = 2 [Sierra];
                                                  7 = 3 [Selva];
                                                  8 = 4 [Lima Metropolitana]'),
                                                  var.label = "Region Natural",
                                                  as.num = FALSE)))
sum_l <- map(sum_l, ~ .x %>% mutate(dominio_2 = str_to_title(as_factor(dominio))))
sum_l <- map(sum_l, ~ .x %>% mutate(dominio_2 = replace(dominio_2,
                                                        dominio_2 == 'Selva' & cod_dep %in% c(16, 17, 25),
                                                       "Selva Baja"),
                                    dominio_2 = replace(dominio_2,
                                                        dominio_2 == 'Selva' & cod_dep %nin% c(16, 17, 25),
                                                       "Selva Alta")))
sum_l <- map(sum_l, ~ .x %>% mutate(dominio_ld = paste(dominio_2, to_character(urbano)),
                                    dominio_ld = replace(dominio_ld,
                                                         dominio_ld == 'Lima Metropolitana Urbano',
                                                         "Lima Metropolitana")))
sum_l <- map(sum_l, ~ .x %>% mutate(limareg = as.numeric(substr(ubigeo, start = 1, stop = 4) == '1501'),
                                    limareg = replace(limareg,
                                                      cod_dep == '07', 'Prov Const. Callao'),
                                    limareg = replace(limareg,
                                                      substr(ubigeo, 1, 4) >= '1502' & substr(ubigeo, 1, 4) < '1599', 
                                                      'Region Lima'),
                                    limareg = replace(limareg,
                                                      limareg == 1,
                                                      'Lima Provincia'),
                                    limareg = replace(limareg,
                                                      limareg == 0,
                                                      'Peru sin Lima')))

# unir sum_l con gob_l ####
common_values <- intersect(names(sum_l[[1]]), names(gob_l[[1]]))
consolidado <- map2(sum_l, gob_l, ~ left_join(.x, .y, by = common_values))

save(consolidado, file = here('investigacion', "datos", "consolidado.RData"))
