# Caja de Herramientas ####

pacman::p_load(dplyr, purrr, stringr, haven, labelled)

ruteador <- function(carpeta = getwd(), patron_regex){
 
  # lista todas las rutas a archivos en la carpeta "ruta" y sub carpetas
  list.files(path = carpeta, recursive = TRUE, full.names = TRUE) %>% 
    
    # se crea tabla con rutas originales y con rutas en minúsculas
    tibble(rutas = ., rutas_minusculas = tolower(.)) %>% 
    
    # se filtran rutas buscadas con la columna de minúsculas
    filter(str_detect(rutas_minusculas, patron_regex)) %>%
    
    # se retorna el vector filtrado de rutas originales
    purrr::pluck("rutas")

}

# Lectura de archivos ####

leer_sav <- function(ruta){
  
  set_names(read_sav(ruta), tolower)
  
}


leer_varias_sav <- function(rutas, nombres_de_tablas){
  
  lista_de_tablas <- map(rutas, leer_sav)
  
  lista_de_tablas <- set_names(lista_de_tablas, paste0("a", nombres_de_tablas))
  
  return(lista_de_tablas)
  
}

# Identificación y Caracterización de Variables ####

buscar_variable <- function(lista_tablas, variable_a_buscar){
  
  # Genera un vector lógico que indica TRUE para cada elemento de la lista
  # que tiene una variable con el nombre definido en variable_a_buscar
  map_lgl(lista_tablas, function(df) variable_a_buscar %in% names(df))
  
}

saca_a <- function(texto){
  
  as.numeric(str_sub(texto, start = 2, end = 5))
  
}

descripciones_de_variable <- function(lista_tablas,
                                      variable_a_buscar,
                                      nombre_atributo_a_sacar = "label"){
  
  # Ojo, tu tabla puede definir etiquetas con otro nombre que no sea "label".
  imap_dfr(lista_tablas, ~ tibble(año = saca_a(.y), nombre = attr(.x[[variable_a_buscar]], nombre_atributo_a_sacar))) 
  
}

formato_de_variable <- function(lista_tablas,
                                variable_a_buscar){
  
  # Ojo, tu tabla puede definir etiquetas con otro nombre que no sea "label".
  imap_dfr(lista_tablas, ~ tibble(año = saca_a(.y), 
                                  formato = paste0(class(.x[[variable_a_buscar]]), 
                                                   collapse = ", "))) 
  
}

# Etiquetas ####

mostrar_etiquetas <- function(lista, variable_a_revisar){
  
  map(lista, ~ val_labels(.x[[variable_a_revisar]]) %>% stack) %>% 
    bind_rows(.id = "año") %>% 
    group_by(valor = values, etiqueta = ind) %>% 
    summarise(año = paste(unique(saca_a(año)), collapse = ", ")) %>% 
    ungroup()
  
}

# Extraer etiquetas que queremos usar para toda la lista:
# (Es necesario verificar que los valores sean consistentes
# Un valor puede variar de significado de un año a otro)

# etiquetas_reemplazo <- val_labels(rech0_lista$a2021[["hv042"]])

reemplazar_etiquetas <- function(lista, 
                                 variable_a_editar,
                                 etiquetas_nuevas){
  
  map(lista, ~ .x %>% 
        mutate(!!sym(variable_a_editar) := set_value_labels(!!sym(variable_a_editar),
                                                            .labels = etiquetas_nuevas)))
  
}

# Anuncio de librerías cargadas ####

print("Librerías cargadas: dplyr, purrr, stringr, haven, labelled")
