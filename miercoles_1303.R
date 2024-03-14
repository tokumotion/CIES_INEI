library(tidyverse); library(haven); library(here)
library(rrapply); library(purrr)

sumarias_sav <- list.files(recursive = TRUE, pattern = "*.sav")[6]

# here() nos sirve para ubicar la direccion del archivo o carpeta donde trabajo
# o crear carpetas nuevas cuando trabajo

enaho_22 <- read_sav(file = sumarias_sav)

# otra forma de llamar el archivo con here()
enaho_22 <- read_sav(here(list.files(recursive = TRUE, pattern = "*.sav")[6]))

# crear el indice con rrapply
indice_enaho_22 <- data.frame(
  name = names(enaho_22),
  label = rrapply(sapply(enaho_22, attributes), how = 'bind')[1],
  tolower = tolower(names(enaho_22))
)

# usar purrr para poner los nombres del dataframe en minusculas
enaho_22 <- read_sav(here(list.files(recursive = TRUE, pattern = "*.sav")[6])) %>% 
  set_names(tolower)

# como crear un indentificador unico para cada vivienda en enaho
# enaho no te da un identificador unico para cada vivienda
# usamos la variable conglome para empezar en la creador del identificador
length(unique(enaho_22$conglome))
# quiero ver como se ve esta variable
head(enaho_22$conglome)
# para hacer el id unico debo concatenar conglome, vivienda y hogar
length(unique(with(enaho_22,paste0(conglome, vivienda, hogar))))
# viendo esto, tengo los ids unicos para cada fila del dataframe

enaho_22$id_hogar <- with(enaho_22,paste(año, 
                                         ubigeo, 
                                         estrato, 
                                         conglome, 
                                         vivienda, 
                                         hogar, sep = "__"))
# pongo año porque voy a analizar diferentes años
# pongo ubigeo y estrato para poder hacer comparaciones entre años y tablas
# le meto separadores para poder diferenciar entre variables
head(enaho_22$id_hogar)

# Vamos a crear codigos de departamentos ####
enaho_22$cod_dep <- substr(enaho_22$ubigeo, start = 1, stop = 2)
head(enaho_22$cod_dep)
length(unique(enaho_22$cod_dep))

# str_pad() nos ayuda a convertir los numeros en strings y le lleno de ceros a 
# la izquierda
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
# ahora agrego la columna de departamentos en la data.frame
enaho_22 <- enaho_22 %>% 
  left_join(y = region_indice, by = 'cod_dep') 

# variable estrato en enaho: diferenciacion entre urbano y rural, si esta 
# entre 6 y 8 es rural, el resto es urbano
# tenemos que convertir estos labels de la variable estrato a variable 
# categorica usando la libreria labelled
install.packages('labelled')
library(labelled)
# usando val_labels() podemos sacar los labels de la variable
val_labels(enaho_22$estrato)
# para hacer una tabla con los labes usamos as_factor() de haven
table(as_factor(enaho_22$estrato))
# ahora creamos una variable dummy para distinguir entre rural y urbano
enaho_22 <- enaho_22 %>% 
  mutate(rural = as.numeric(estrato > 5),
         urbano = as.numeric(estrato <= 5))
# le creamos una variable numerica con labels con labelled() y le ponemos los 
# valores de los labels con set_value_labels()
enaho_22 <- enaho_22 %>% 
  mutate(rural = labelled(as.numeric(estrato > 5)),
         urbano = labelled(as.numeric(estrato <= 5))) %>% 
  set_value_labels(rural = c(Rural = 1, Urbano = 0),
                   urbano = c(Rural = 0, Urbano = 1))
# pruebo si el codigo funciona, osea si urbano y rural tienen la misma cantidad
table(as_factor(enaho_22$rural))
table(as_factor(enaho_22$urbano))
# si quiero que la variable me bote el label, uso as_factor() de haven
# si quiero que la variable me bote el numero, uso as.numeric() de base
# si quiero que la variable me bote el texto, uso to_character() de labbeled

# variables de geografia y economia ####
enaho_22 %>% 
  group_by(dominio, ld, rural) %>% 
  summarise(n())
# ld es el deflactor espacial

# Hago un recoding de variables para encontrar de que region es cada dominio
library(sjmisc)
enaho_22$reg_nat <- rec(enaho_22$dominio,
    rec = c('1:3 = 1 [Costa];
            4:6 = 2 [Sierra];
            7 = 3 [Selva];
            8 = 4 [Lima Metropolitana]'),
    var.label = "Region Natural",
    as.num = FALSE)

# quiero crear una copia de dominio que tenga los titulos, no en lower
enaho_22$dominio2 <- str_to_title(as_factor(enaho_22$dominio))
# creo un operador llama %nin%
`%nin%` <- Negate(`%in%`)
# y le separo selva alta y selva baja en dominio2
enaho_22 <- enaho_22 %>% 
  mutate(dominio2 = replace(dominio2,
                            dominio2 == 'Selva' & cod_dep %in% c(16, 17, 25),
                            "Selva Baja"),
         dominio2 = replace(dominio2,
                            dominio2 == 'Selva' & cod_dep %nin% c(16, 17, 25),
                            "Selva Alta"))
# le quito a lima metropolitana lo todo lo que no es urbano, porque no hay rural
enaho_22 <- enaho_22 %>% 
  mutate(dominio_ld = paste(dominio2, to_character(urbano)),
         dominio_ld = replace(dominio_ld,
                              dominio_ld == 'Lima Metropolitana Urbano',
                              "Lima Metropolitana"))

# aca estamos crenao una variable limareg que me identifica si esto en lima, y 
# tambien me dice si estoy en el callao
enaho_22 <- enaho_22 %>% 
  mutate(limareg = as.numeric(substr(ubigeo, start = 1, stop = 4) == '1501'),
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
                           'Peru sin Lima'))

#### purr ####
library(purrr)
# queremos usar funciones de forma iterativa
vect_ej <- c(1, 2, 3, 4)
# R vectoriza las funciones de forma automatica si tener que crear loops
paste('prefijo', vect_ej, 'sufijo')
tremenda_funcion <- function(x){
  x ^ 3
}
tremenda_funcion(vect_ej)

# para que sirve map()
# map() me devuelve una lista con los resultados de la función que va en map()
# esto se aplica al vector
map(vect_ej, tremenda_funcion)
# es mas estricto que sapply(), pero te bota error si no puede darte la 
# respuesta a la funcion de forma estricta
# tambien puedo meterle la funcion directamente a map()
map(vect_ej, function(x) {x ^ 3})
# hay diferentes maps, ejemplos
map_dbl(vect_ej, ~ {.x ^ 3}) 
# en map_dbl() se puede poner ~ {.x} en vez de function(x){x}
# map_dbl() te devuelve un vector de numeros reales, no te va a dar nada mas y 
# te va a dar error si el resultado de la formula no es un dbl
map_int(vect_ej, ~ {.x ^ 3})
# map_int() te devuelve un vector de numeros enteros, no te va a dar nada mas y 
# te va a dar error si el resultado de la formula no es un dbl
map_int(vect_ej, ~ {.x ^ 1/3}) # si el resultado NO es un entero, te bota error
# ahora puedo usar map_df() para crear tibbles
map_df(vect_ej, ~ tibble(name = .x ^ 3))
# le puedo meter el nombre de la columna si lo declaro en el tibble

# quiero abrir varias tablas al mismo tiempo con map(), para que me devuelva 
# una lista de data.frames
list.files(recursive = TRUE, pattern = "*.sav")[c(2, 7, 13)]
list_tables <- map(list.files(recursive = TRUE, pattern = "*.sav")[c(2, 7, 13)], 
                    read_sav)
# podemos encadenar las funciones en map()
list_tables <- map(list.files(recursive = TRUE, pattern = "*.sav")[c(2, 7, 13)], 
                   read_sav) %>% 
  map(set_names, tolower) 
# aca estoy pasando la funcion tolower() dentro de set_names(), encadeno el 
# map() original con otro map(), es mas rapido así
list_tables <- map(list.files(recursive = TRUE, pattern = "*.sav")[c(2, 7, 13)], 
                   read_sav) %>% 
  map(set_names, tolower) %>% 
  set_names(paste0('a', 2020:2022))
# aca estoy pasando nombres a cada miembro de la lista de map() me bota, esto 
# tiene que estar fuera del map()

# para pasar objeto sobre el que trabajo a otro map() al que le quiero aplicar 
# un mutate u otra función parecida, le pongo . para que reconozca qué objeto 
# debe jalar
list_tables <- map(list.files(recursive = TRUE, pattern = "*.sav")[c(2, 7, 13)], 
                   read_sav) %>% 
  map(set_names, tolower) %>% 
  map(. %>% mutate(id_hogar = paste(año, 
                              ubigeo, 
                              estrato, 
                              conglome, 
                              vivienda, 
                              hogar, sep = "__"))) %>% 
  set_names(paste0('a', 2020:2022))

identificador <- function(df){
  df %>% 
    mutate(id_hogar= paste(año, 
                           ubigeo, 
                           estrato, 
                           conglome, 
                           vivienda, 
                           hogar, sep = "__"),
           cod_dep = substr(ubigeo, start = 1, stop = 2),
           cod_prov = substr(ubigeo, start = 1, stop = 4))
}

# puedo meterle a map() una funcion propia y encadenar todo para que me bote el 
# resultado. Encadenar diferentes map() es mas rapido que meter toda la funcion 
# en un solo map()
tablas <- list.files(recursive = TRUE, pattern = "*.sav")[c(2, 7, 13)]
map(tablas, read_sav) %>% 
  map(set_names, tolower) %>% 
  map(identificador) %>% 
  set_names(paste0('a', 2020:2022)) %>% 
  map_df(~ head(.x$id_hogar))