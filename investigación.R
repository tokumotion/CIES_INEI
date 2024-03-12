library(tidyverse); library(magrittr); library(DataExplorer)

# quiero saber la correlacion entre las variables de transporte y pobreza en 
# los diferentes UBIGEOS y CONGLOMERADO/SUBCONGLOMERADO

#GASTOS DEL HOGAR
gastos <- read.csv(file = '~/Downloads/2022/784-Modulo07/Enaho01-2022-601.csv',
                   header = T, stringsAsFactors = FALSE, 
                   fileEncoding = 'latin1')
glimpse(gastos)

# TRANSPORTE
transporte <- read.csv(file = '~/Downloads/2022/784-Modulo10/Enaho01-2022-604.csv', 
                       header = T, stringsAsFactors = FALSE, 
                       fileEncoding = 'latin1')

glimpse(transporte)
create_report(transporte)
