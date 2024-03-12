# Como abrir archivos de STATA y SPSS ####
library(readr); library(tidyverse); library(magrittr)

# funcion para crear rutas (directorios) cuando necesitemos escribir archivos 
# file.path()
file.path(getwd(), 'datos')

# me da una lista de los archivos en la carpeta
list.files()

# puedo pedirle que me abras carpetas que estan dentro y qué archivos hay con
# arg recursive = TRUE
list.files(recursive = TRUE)

# también puedo pedirle que me de archivos específicos con el arg 
# pattern = "*.filetype"
list.files(recursive = TRUE, pattern = "*.sav")

# creo un vector con todos los objetos
archivos <- list.files(recursive = TRUE)

# creo un subset del vector archivos con str_subset()
library(stringr)

# uso to_lower() para standarizar los nombres
str_subset(tolower(archivos), "sumaria")

# ENCONTRAR LOS ARCHIVOS QUE NECESITAMOS
sumarias_sav <- list.files(recursive = TRUE, pattern = '*.sav')[5]
sumarias_csv <- list.files(recursive = TRUE, pattern = '*.csv')[2]
sumarias_dta <- list.files(recursive = TRUE, pattern = '*.dta')[3]

# ABRIR EN CSV ####
CSV_ENAHO <- read.csv(sumarias_csv, encoding = 'latin1', header = T, 
                      stringsAsFactors = FALSE)

# ABRIR CON DATA.TABLE QUE ES MAS RAPIDO
CSV_fread_ENAHO <- data.table::fread(sumarias_csv, encoding = 'Latin-1')

# PODEMOS VER QUE CORRE MAS RAPIDO
library(microbenchmark)
# pegamos las dos funciones que hacen lo mismo y comparamos qué es más rapido
# en milisegundos, corriendo el codigo 5 veces
microbenchmark(read.csv(sumarias_csv, encoding = 'latin1', header = T, 
                        stringsAsFactors = FALSE),
               data.table::fread(sumarias_csv, encoding = 'Latin-1'),
               times = 5, unit = 'milliseconds')

# abrir archivos .dta y .sav ####
library(foreign)
DTA_foreign_ENAHO <- read.dta(file = sumarias_dta)
SAV_foreign_ENAHO <- read.spss(file = sumarias_sav, to.data.frame = TRUE)
# hay que ponerle el arg to.data.frame = TRUE, sino te trae una lista

# otra libreria que podemos usar es haven
library(haven)
DTA_haven_ENAHO <- read_dta(file = sumarias_dta)
SAV_haven_ENAHO <- read_spss(file = sumarias_sav)

microbenchmark(read.dta(file = sumarias_dta),
               read.spss(file = sumarias_sav, to.data.frame = TRUE),
               read_dta(file = sumarias_dta),
               read_spss(file = sumarias_sav),
               times = 5)
# foreign es mucho mas rapido, pero haven te lee la data latina de mejor manera
# osea no se come la ñs o las tildes

# Metadatos ####
attributes(CSV_ENAHO)
attributes(CSV_fread_ENAHO)
class(attributes(DTA_foreign_ENAHO)$label.table$dominio)
# cuando uso foreign para importar data de SPSS o STATA, la función va a ponerle
# un vector de integers nombrados que PARECEN factores pero no son

# es importante jalar los metadatos de la base de datos para poder entender los 
# valores que recibamos cuando hagamos un analisis exploratorio de la data y 
# tener una visión rapida del data frame. Esto se hace con haven
atributos_haven_dta <- sapply(DTA_haven_ENAHO, attributes)

# con foreign puedo hacer algo similar, creamos un indice de las variables
# ESTO ES SUPER IMPORTANTE PORQUE NOS AYUDA A DEFINIR QUÉ VARIABLES SON LAS
# QUE NECESITAMOS!!!
indice_SAV_foreign <- data.frame(
  names = names(SAV_foreign_ENAHO),
  labels = attributes(SAV_foreign_ENAHO)$variable.labels,
  lower_names = tolower(names(SAV_foreign_ENAHO))
)

# OJO, cuando importas un CSV, ese archivo NO TIENE METADATA por lo que USAR 
# SPSS O STATA TE TRAE METADATA

# Estimaciones estadisticas ####
# Vamos a trabajar con haven y el archivo .sav
install.packages('sjmisc')
library(sjmisc); library(tidyverse); library(haven)
sumarias_sav <- list.files(recursive = TRUE, pattern = '*.sav')[5]

enaho_22 <- read_sav(file = sumarias_sav)
names(enaho_22) <- tolower(names(enaho_22))

# Calculos basicos
enaho_22$pobreza
# aca puedo ver los labels de la data, esto lo saco del SPSS
# Histograma para ver los niveles de pobreza
table(as_factor(enaho_22$pobreza))

# o uso el dplyr
enaho_22 %>% 
  group_by(pobreza) %>% 
  summarise(obs = n()) %>% 
  mutate(prop = obs * 100/sum(obs))
# aca hay que tomar en cuenta que como ENAHO es una encuesta, se usa una muestra
# por lo que las proporciones que se saca de este analisis no son correctas 
# cuando se quiere inferir algo de la poblacion
# Debido a esto, se debe aplicar un factor de expansion, osea que tienes que
# aplicar un peso aplicado que esta relacionado con la densidad poblacional de 
# cada comunidad
# aca se muestra como se aplica - factor07 es el factor de expansión en el ENAHO
enaho_22 %>% 
  group_by(pobreza) %>% 
  summarise(obs = sum(factor07))
# TOMA EN CUENTA QUE ENAHO ES ENCUESTA DE HOGARES!!! POR ESO SALEN 10 MILLONES 
# DE OBSERVACIONES
# AHORA VOY A CREAR EL FACTOR POBLACIONAL PARA PERSONAS, NO HOGARES
# enaho$mieperho es miembros por hogar
# enaho$factor07 es el factor de expansión que usa el ENAHO (que aplica a 
# HOGARES)
enaho_22$facpob <- enaho_22$factor07*enaho_22$mieperho
head(enaho_22$facpob)
# quiero ver si tengo algún NA en la nueva columna
table(is.na(enaho_22$facpob))

# ESTA ES LA FORMA CORRECTA DE CALCULAR LAS VARIABLES A NIVEL POBLACIONAL DE 
# PERU SEGUN LA ENAHO
enaho_22 %>% 
  group_by(pobreza) %>% 
  mutate(facpob1 = factor07*mieperho) %>% 
  summarise(obs = sum(facpob1)) %>% 
  mutate(prop = obs * 100/sum(obs))

# questionr ####
install.packages('questionr')
library(questionr)
# nos permite usar tablas con peso (?)
# ENAHO es weighted survey data, factor07 es el PESO, mieperho es miembros del 
# hogar
wtd.table(x = as_factor(enaho_22$pobreza), 
          weights = enaho_22$facpob)
# ESTA FUNCION TE DA LOS VALORES POBLACIONALES YA MULTIPLCIADOS POR EL FACTOR 
# POBLACIONAL, TOMA EN CUENTA QUE LA COLUMNA DE POBREZA DEBE ESTA as_factor()
frq(as_factor(enaho_22$pobreza), 
    show.na = FALSE, 
    weights = enaho_22$facpob)
# esta funcion me deja crear una tabla para mostrar los valores poblacionales
# siempre hay que pner los pesos creados por facpob

# srvyr ####
install.packages('srvyr')
library(srvyr)
# crear objeto
dstrata <- enaho_22 %>% 
  as_survey_design(ids = conglome,
                   strata = estrato,
                   weights = facpob)
# analisis, aca puedes ver el error standard ponderado y de ahi se puede 
# calcular el intervalo de confianza de la variable
dstrata %>% 
  group_by(año, pobreza) %>% 
  summarise(cantidad = survey_total())

# evolucion del gasto real per capital (pobreza se calcula por ingreso, 
# no gasto)
attributes(enaho_22$gashog2d)

enaho_22$gasto_1 <- enaho_22$gashog2d/(enaho_22$mieperho*12)
# para calcular CUALQUIER VARIABLE a partir de data de ENAHO, HAY QUE SACAR 
# PROMEDIOS PONDERADOS
weighted.mean(enaho_22$gasto_1, w = enaho_22$facpob)
# este numero no es el correcto, hay que meter el deflactor espacial, ya que en 
# diferentes partes del pais, el valor de la moneda no es la misma
# variable gasto_1 es el gasto anual por miembros del hogar con el deflactor 
# espacial aplicado
enaho_22$gasto_1 <- enaho_22$gashog2d/(enaho_22$mieperho*12*enaho_22$ld)
weighted.mean(enaho_22$gasto_1, w = enaho_22$facpob)
# enaho_22$ld es el deflactor espacial en la encuesta

# ahora hagamos el analisis del gasto promedio usando srvyr
dstrata_2 <- enaho_22 %>% 
  as_survey_design(ids = conglome,
                   strata = estrato,
                   weights = facpob)

dstrata_2 %>% 
  summarise(gasto = survey_mean(gasto_1, vartype = 'ci'))
# esta funcion me permite ver los intervalos de confianza sin tener que 
# calcularlos

# hutils ####
install.packages('hutils')
library(hutils)
dstrata_2 %>% 
  mutate(decil = weighted_ntile(gasto_1, weights = facpob, n = 10)) %>% 
  group_by(decil) %>% 
  summarise(gasto = survey_mean(gasto_1, vartype = 'ci'))
# analisis de deciles para entender los niveles de gasto a nivel nacional

# ahora calcular el ingreso bruto per capital
attributes(enaho_22$inghog1d)
enaho_22$ingreso_1 <- enaho_22$inghog1d/(enaho_22$mieperho*12*enaho_22$ld)
dstrata_3 <- enaho_22 %>% 
  as_survey_design(ids = conglome,
                   strata = estrato,
                   weights = facpob)
dstrata_3 %>% 
  mutate(decil = weighted_ntile(ingreso_1, weights = facpob, n = 10)) %>% 
  group_by(decil) %>% 
  summarise(gasto = survey_mean(ingreso_1, vartype = 'ci'))

# TO DO, HACER UNA FUNCION PARA SACAR LA TABLA DE VARIABLES Y ATRIBUTOS DEL 
# DATA.FRAME EN HAVEN!!!!
library(rrapply)
install.packages('rrapply')

indice_enaho_22 <- data.frame(
  name = names(enaho_22),
  metadata = rrapply(sapply(enaho_22, attributes), how = 'bind')[,1],
  lower = tolower(names(enaho_22))
)

# ITERACIONES ####
# funciones explicitas, usas return() dentro de la función para que te de 
# exactamente el valor que deseas