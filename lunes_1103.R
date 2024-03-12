# Nos enseñan a instalar librerias
# se va a utilizar tidyverse y dplyr

# Objetivos del curso, desarrollar un workflow para data wrangling de ENAHO
# Este curso no es para aplicar modelos sobre la data, podemos usar tidymodels
# para construir el workflow

enaho <- read.csv('~/Downloads/Enaho01-2022-100.csv', fileEncoding = "latin1", 
                  header = T, stringsAsFactors = FALSE)
# Usar el styleguide de R: https://google.github.io/styleguide/Rguide.html
library(DataExplorer)
create_report(enaho)
data()

# que hace esto? parse() lee el texto que está dentro de la función y eval() 
# lo va a ejecutar. Osea, eval() evalua si el resultado de parse() es TRUE or 
# FALSE. En el ejemplo, la funcion en parse() dice is.character(3L + 5L), osea
# si 3 + 5 es character, eval() lo "evalúa" y te dice que no (FALSE) porque 
# 3 + 5 es 8, no un character
eval(parse(text = "is.character(3L + 5L)"))

# cuando tienes una lista de data.frame y quieres correr una funcion para cada 
# una, puedes usar sapply() de forma recursiva
data1 <- data.frame(unif = runif(100), norm = rnorm(n = 100, mean = 5, sd = 2))
data2 <- data.frame(unif = runif(100), norm = rnorm(n = 100, mean = 4, sd = 1))
data3 <- data.frame(unif = runif(100), norm = rnorm(n = 100, mean = 3, sd = 3))
lista_tablas <- list(data1, data2, data3)
# si usas sapply directo a la lista, te sale error
sapply(lista_tablas, mean)
# hay que usar sapply de formma recursiva, osea, meterle un sapply dentro del 
# sapply con la función que quieres correr
sapply(lista_tablas, sapply, mean)
# esta es la forma correcta de hacer un loop dentro de un loop en R

# R sirve para hacer algebra lineal y de matrices de forma muy facil
# preguntar a chatgpt para que te explique

# dplyr - grammar for making operations in dataframes
library(tidyverse); library(magrittr)
gms <- read.csv('~/Downloads/gms.csv', header = T, stringsAsFactors = FALSE)
write.csv(x = gms, file = '~/Documents/CIES_INEI/gms.csv')
write.csv(x = enaho, file = '~/Documents/CIES_INEI/enaho_testdata.csv')
str(gms)
# usar DataExplorer para ver que hay en el dataframe
create_report(gms)
# usar glimpse() para ver la estructura de datos
glimpse(gms)
# en general se sugiere usar tibble() cuando se llama a un data.frame por orden
tibble(gms) %>% 
  filter(Federation %in% c('Peru', 'Chile')) %>% 
  arrange(desc(Federation), desc(Title.Year))
# crear nuevas variables con mutate
tibble(gms) %>% 
  mutate(nacimiento = ymd(Born)) %>% 
  select(nacimiento, everything()) 
# si pones everything() en el select(), mandas la columna que creaste en 
# mutate() al principio del tibble
# sacar la edad en la que se volvieron grand masters
tibble(gms) %>% 
  mutate(nacimiento = ymd(Born),
         edad_gm = Title.Year - year(nacimiento)) %>% 
  select(edad_gm, everything()) 
# tomar en cuenta que cuando hacer un left_join() o right_join() se pierden las
# variables creadas anteriormente (en este caso se pierde edad_gm), si es 
# necesario mantener la variable hay que insertarla dentro del objeto original

# insertar la variable edad_gm en gms
gms_tibble <- tibble(gms) %>% 
  mutate(nacimiento = ymd(Born),
         edad_gm = Title.Year - year(nacimiento)) 
# crear variables dummy, agrupando la edad promedio en que se vuelven grand 
# masters por cada federación y creando una variable dummy que me da 1 si es 
# menor a la edad promedio o no
gms_tibble %>% 
  group_by(Federation) %>% 
  summarise(promedio_edad =  mean(edad_gm, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(x = gms_tibble, by = 'Federation') %>% 
  mutate(dummy_var_joven = as.numeric(promedio_edad > edad_gm)) %>% 
  select(dummy_var_joven, everything())

# fuentes para revisar investigaciones litmaps + elicitc + research rabbit
  