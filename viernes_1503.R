# ggplot2 ####
pacman::p_load(ggplot2, tidyverse, ggthemes)

depas <- read_csv('~/Documents/CIES_INEI/vta_inmob_2022_2.csv')

depas %>% 
  filter(periodo == '2022',
         trimestre == 2,
         distrito %in% c('Surco', 'San Miguel', 'Miraflores')) %>% 
  ggplot(data = ., aes(x = as.factor(habitaciones), 
                       y = precio_dolares_corrientes)) + 
  geom_col() +
  facet_wrap(distrito ~ .)

depas %>% 
  filter(periodo == '2022',
         trimestre == 2,
         distrito %in% c('Surco', 'San Miguel', 'Miraflores')) %>% 
  ggplot(data = ., aes(x = superficie, 
<<<<<<< HEAD
                       y = precio_soles_corrientes/1000)) + 
=======
                       y = precio_soles_corrientes)) + 
>>>>>>> 882b0c484dcb97e24bd70dcff50d577bc9018090
  geom_point(aes(color = as.factor(habitaciones))) +
  stat_smooth(method = loess) +
  facet_grid(distrito ~ .) + 
  labs(title = 'Precios y Superficie de Departamentos Vendidos',
       subtitle = 'Segundo Trimestre de 2022',
       x = 'Superficie (m2)',
<<<<<<< HEAD
       y = 'Precio en miles de (S/)',
       color = 'Habitaciones',
       caption = 'Fuente: Estadísticas BCRP') + 
  scale_color_colorblind()+ 
  theme_minimal() + 
  ylim(c(0, 1500))
=======
       y = 'Precio (S/)',
       color = 'Habitaciones',
       caption = 'Fuente: Estadísticas BCRP') + 
  scale_color_colorblind() + 
  theme_minimal()
>>>>>>> 882b0c484dcb97e24bd70dcff50d577bc9018090

# como hacer varios tipos de geometrias sobre la base de una sola variable
p <- depas %>% 
  filter(periodo == '2022',
         trimestre == 2,
         distrito %in% c('Surco', 'San Miguel', 'Miraflores')) %>%  
  ggplot(aes(precio_soles_constantes_2009/superficie, 
             label = distrito)) + 
  labs(x = "S/ de 2009 por m2")
p + geom_histogram() + ggtitle('Histograma')
p + geom_density() + ggtitle('Densidad')
p + geom_freqpoly() + ggtitle('Poligono de Frecuencias')
p + geom_boxplot() + ggtitle('Boxplot')
p + geom_boxplot(aes(y = distrito, color = distrito)) + 
  ggtitle('Boxplot Distritos')
p + geom_boxplot(aes(y = distrito, fill = distrito)) + 
  ggtitle('Boxplot Distritos')

# ggplots de 2 o más variables
s <- depas %>% 
  filter(periodo == '2022',
         trimestre == 2,
         distrito %in% c('Surco', 'San Miguel', 'Miraflores')) %>%  
  group_by(distrito) %>% 
  summarise(y = mean(precio_soles_constantes_2009/superficie), 
            x = mean(superficie)) %>% 
  ggplot(aes(x, y, label = distrito)) + 
  labs(x = 'Superficie Promedio (m2)',
       y = 'S/ de 2009 por m2')

s + geom_point() + ggtitle('Puntos')
s + geom_point() + ggtitle('Puntos') + ylim(c(0, 6000))
s + geom_text() + geom_point() + ggtitle('Texto') # usar ggrepel para esto

s + geom_col() + ggtitle('Barras')
<<<<<<< HEAD

library(paletteer); library(scales)
show_col(paletteer_d(`"MoMAColors::Warhol"`))

depas %>% 
  filter(periodo == '2022',
         trimestre == 2,
         distrito %in% c('Surco', 'San Miguel', 'Miraflores')) %>% 
  ggplot(data = ., aes(x = superficie, 
                       y = precio_soles_corrientes/1000)) + 
  geom_point(aes(color = as.factor(habitaciones))) +
  stat_smooth(method = loess) +
  facet_grid(distrito ~ .) + 
  labs(title = 'Precios y Superficie de Departamentos Vendidos',
       subtitle = 'Segundo Trimestre de 2022',
       x = 'Superficie (m2)',
       y = 'Precio en miles de (S/)',
       color = 'Habitaciones',
       caption = 'Fuente: Estadísticas BCRP') + 
  scale_color_manual(values = paletteer_d(`"MoMAColors::Warhol"`)) + 
  theme_minimal(base_size = 16) + 
  ylim(c(0, 1500))

rrapply::rrapply # asi puedes ver el codigo que se usa para la funcion
=======
>>>>>>> 882b0c484dcb97e24bd70dcff50d577bc9018090
