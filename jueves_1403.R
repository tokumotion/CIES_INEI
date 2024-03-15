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

# modulo de gobernabilidad ####

gobernabilidad <- read_sav(here(list.files(recursive = TRUE, pattern = "*.sav")[2])) %>% 
  set_names(tolower)

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

gobernabilidad <- identificador(gobernabilidad)
head(gobernabilidad$id_hogar)      
        
# hay espacios en blancos, hay que quitarlos y llenar los 0s donde deben estar
# ubigeo debe tener 6 digitos, si hay espacio en blanco hay que llenarlo con 0s
# usamos str_pad(para llenar de 0s a la izquierda todo string que no sea de 6 
# caracteres)
# trimws() quita los espacios en blanco
stringr::str_pad(trimws(gobernabilidad$ubigeo),
                 width = 6,
                 side = 'left',
                 pad = 0) %>% head()
# ahora esto se mete en a funcion identificador() para cada variable que 
# queremos arreglar (conglome, vivienda, ubigeo)
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
gobernabilidad <- identificador(gobernabilidad)
head(gobernabilidad$id_hogar)      
# hay que comparar con el id_hogar de la enaho 2020:2022

# podemos trabajar a nivel hogar y a nivel persona, cada hogar tiene diferentes 
# personas
# tenemos que crear variables que puedan identificar ciertas caracteristicas de
# cada miembro del hogar por separado y luego traer esa variable de vuelta a la 
# encuesta enaho madre

# educacion ####
# educacion es una tabla de resultados por persona

pacman::p_load(tidyverse, purr, haven, labelled, stringr, rrapply)
educ_sav <- list.files(pattern = '*.sav', recursive = TRUE)[13]
# para evitar usar la misma funcion una y otra vez, como list.files() puedes 
# usar partial() con los argumentos especiales que tu usas!
buscar_sav <- partial(list.files, recursive = TRUE, pattern = '*.sav')
buscar_sav()

# hay que definir la formula que vamos a usar ANTES de usar compose
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

# otra forma de cargar la base de datos con purrr, pasarle el tolower y la 
# funcion identificador, se crea un funcion nueva usando compose()
# esta funcion es la que vamos a meter en map() en vez de usar function() para
# trabajar con la sintaxis de dplyr
leer_enaho <- compose(~ identificador(.x),
                      ~ set_names(.x, tolower),
                      ~ read_sav(.x))

educ <- leer_enaho(educ_sav)
head(educ$id_hogar)

# crear indice de tabla educ
# para crear los indices hay que jalar el sav directo y aplicarle la formula, 
# no a traves de compose()
edu_raw <- read_sav(educ_sav)
indice_educ <- data.frame(
  name = names(edu_raw),
  label = rrapply(sapply(edu_raw, attributes), how = 'bind')[1],
  lower = tolower(names(edu_raw))
)


# un par de variables importantes para la tabla de educacion de enaho
# tiempo de educacion
# la columna p301b son los años de estudios, pero hay muchos NAs, hay que 
# reemplazarlos con 0
# acá podemos ver los atributos de las colummnas
attributes(educ$p301b)
attributes(educ$p301a)
attributes(educ$p301c)
# ahora como podemos empezar a calcular los años educativos
# a_gr es el ultimo año aprobado por la persona en la encuesta
educ <- educ %>% 
  mutate_at(vars(p301b:p301c), ~ replace(.x, is.na(.x), 0)) %>% 
  mutate(a_gr = p301b + p301c,
         educacion = 0,
         educacion = case_when(
         p301a %in% c(1, 2, 3, 4, 12) ~ 0 + a_gr,
         p301a %in% c(5, 6) ~ 6 + a_gr,
         p301a %in% c(7, 8, 9, 10) ~ 11 + a_gr,
         p301a %in% c(11) ~ 16 + a_gr)
         )
# esto lo vamos a convertir en una funcion para poder aplicarlo en todos los 
# años con los que vamos a trabajar
table(educ$educacion)

# como calculo cuantos jefes del hogar hay?
val_labels(educ$p203)
jefes_hogar <- educ %>% 
  filter(p203 == 1) %>% 
  select(id_hogar, educ_jefe = educacion)

# sumo esto a la enaho_22 de ayer
buscar_sav()[15] # este es el enaho que busco
enaho_22 <- leer_enaho(buscar_sav()[15]) # corro funcion identificador

enaho_22 <- jefes_hogar %>% 
  left_join(x = enaho_22, by = 'id_hogar')

# ruteador con patrones REGEX (USAR CHATGPT PARA QUE ME BOTE EL PATRON)
ruteador <- function(ruta, patron_regex){
  list.files(path = ruta, recursive = TRUE, full.names = TRUE) %>% 
    tibble(rutas = ., rutas_minusculas = tolower(.)) %>% 
    filter(str_detect(rutas_minusculas, patron_regex)) %>% 
    purrr::pluck('rutas')
}

#### esto hay que revisarlo ####
list.files(path = '~/Documents/CIES_INEI/', recursive = TRUE, full.names = TRUE) %>% 
  tibble(rutas = ., rutas_minusculas = tolower(.)) %>% 
  filter(str_detect(rutas_minusculas, "^(?i)sumaria-\\d+\\.sav$"))

rutas_sumarias <- ruteador(ruta = '~/Documents/CIES_INEI/', 
                           patron_regex = "^(?i)sumaria-\\d+\\.sav$")

# depresion +  reproducibilidad ####
