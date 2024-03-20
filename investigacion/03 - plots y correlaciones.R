# correlaciones y graficos ####
pacman::p_load(here, tidyverse, ggplot2, car)

source(here('investigacion', 
            "02 - preparacion_datos.R"))

# Exploratory Data Analysis
# escoger las variables que queremos evaluar
rep_obs_gob_l

# create a dataframe to do EDA
conf_jne <- map(consolidado, ~ .x %>% 
                  mutate(p1_01 = replace(p1_01, is.na(p1_01), 5)) %>% 
                  group_by(p1_01) %>% 
                  mutate(pp = mieperho * factor07) %>% 
                  summarise(pp_tot = sum(pp)) %>% 
                  mutate(prop = pp_tot*100/sum(pp_tot)) %>% 
                  ungroup()) %>% 
  bind_rows() %>% 
  mutate(year = rep(as.numeric(names(consolidado)), 
                    each = length(unique(p1_01)))) %>% 
  ggplot(data = ., aes(x = ymd(year, truncated = 2L), y = prop, fill = as_factor(p1_01))) +
  geom_area() +
  labs(title = attributes(conf_jne$p1_01)$label, 
       subtitle = 'Períodos 2013 - 2022', 
       caption = 'Fuente: ENAHO 2013 - 2022', 
       x = 'Período', y = 'Porcentaje Población (%)',
       fill = attributes(conf_jne$p1_01)$label) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_discrete(labels = c('Nada', 'Poco', 'Suficiente', 'Bastante','No Sabe')) + 
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 16))
