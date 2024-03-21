# correlaciones y graficos ####
pacman::p_load(here, tidyverse, ggplot2, car, patchwork, ggthemes)

source(here('investigacion', 
            "02 - preparacion_datos.R"))
class(gober)
class(lista_conf)
# Exploratory Data Analysis
# escoger las variables que queremos evaluar
rep_obs_gob_l$name <- str_replace_all(rep_obs_gob_l$name, pattern = '\\$', 
                                      replacement = '_')
# convert the list of confidence variables
lista_conf <- filter(rep_obs_gob_l, 
                             grepl("p1_", rep_obs_gob_l$name))[,1]
lista_conf <- lapply(lista_conf, as.character)[[1]]

# create a dataframe to do EDA
#Crear funcion que genera los dataframes para plotear
confianza <- function(lista, variable){
  data <- map(lista, ~.x %>% 
        mutate_at(variable, ~ replace(., is.na(!!rlang::sym(variable)), 5)) %>% 
        group_by_at(variable) %>% 
        mutate(pp = mieperho * factor07) %>% 
        summarise(pp_tot = sum(pp)) %>% 
        mutate(prop = pp_tot * 100 / sum(pp_tot)) %>% 
        ungroup()) %>% 
    bind_rows() %>% 
    mutate(year = rep(as.numeric(names(!!rlang::enexpr(lista))),
                      each = length(unique(!!rlang::sym(variable))))) 
  return(data)
}
conf_list <- map(lista_conf, \(nombre) confianza(consolidado, nombre)) %>% 
  set_names(lista_conf)

# crear lista de plots de ggplot
graph_conf <- function(conf, variable){
  plot <- ggplot(conf[[variable]], aes(x = ymd(as.numeric(.data[['year']]), truncated = 2L), 
                                      y = as.numeric(.data[['prop']]), 
                           fill = as_factor(.data[[variable]]))) +
    geom_area() +
    labs(title = var_label(conf[[variable]][,1]),
         subtitle = 'Períodos 2013 - 2022', 
         caption = 'Fuente: ENAHO 2013 - 2022', 
         x = 'Período', y = 'Porcentaje Población (%)') +
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_colorblind(labels = c('Nada', 'Poco', 'Suficiente', 'Bastante', 
                                     'No Sabe')) +
    theme_minimal() +
    theme(plot.title = element_text(face = 'bold'))
}

plot_list <- map(lista_conf, \(nombre) graph_conf(conf_list, nombre))

(plot_list[[16]] | plot_list[[13]] | plot_list[[12]]) / (plot_list[[9]] | plot_list[[8]] | plot_list[[7]] | plot_list[[6]])
