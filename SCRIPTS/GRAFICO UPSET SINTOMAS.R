#-----------------------------------------------------------------------------------
#                          🛑 ANALISIS SIGNOS Y SINTOMAS
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#1- Creo vectores que contienen nombres de síntomas
#-----------------------------------------------------------------------------------

sintomas <- c("DOLOR_TORACICO", "DOLOR_MUSCULAR", "DOLOR_ABDOMINAL", "VOMITO", "DIARREA",
              "RECHAZO_ALIMENTO", "TIRAJE", "TOS", "FIEBRE_MAY_38", "FIEBRE_MENOR_38",
              "SIN_FIEBRE", "DISNEA", "DOLOR_GARGANTA", "RINITIS", "INYECCION_CONJUNTIVAL",
              "DIFICULTAD_PARA_RESPIRAR", "SIBILANCIAS", "APNEA", "HIPOXEMIA", "DISGEUSIA",
              "AGEUSIA", "ANOSMIA", "DOLOR_DE_CABEZA", "MALESTAR_GENERAL", "CONFUSION",
              "IRRITABILIDAD", "CONVULSIONES", "TAQUIPNEA")


#------------------------------------------------------------------------------------------
#2- Reemplazo "9" y "NA" por "0" para tener un df binario donde 1 es presencia y 0 ausencia
#------------------------------------------------------------------------------------------

base_sintomas <- DATA_UC_LISTA %>%
  mutate(across(
    all_of(sintomas),
    ~ replace_na(ifelse(as.numeric(.) == 9, 0, as.numeric(.)), 0)
  ))


#------------------------------------------------------------------------------------------
#3- Selecciono variables de interés para el gráfico
#------------------------------------------------------------------------------------------

base_sintomas <- base_sintomas %>% select(all_of(sintomas)) %>%
  mutate(ID = row_number()) %>%
  relocate(ID)

#Renombro las columnas para cambiar los "-" por espacios
base_sintomas <- base_sintomas %>% 
  rename_with(~ str_replace_all(., "_", " "))%>%
  rename_with(~ str_to_sentence(.,))

#Corrijo casos particulares

base_sintomas <- base_sintomas %>%
  rename_with(~str_replace_all(.,"toracico","torácico") %>%
                str_replace_all("Vomito", "Vómito") %>%
                str_replace_all("may 38", "mayor a 38°") %>%
                str_replace_all("menor 38", "menor a 38°") %>%
                str_replace_all("Inyeccion", "Inyección") %>%
                str_replace_all("Confusion", "Confusión"))

#--------------------------------------------------------------------------------------------------
#4-Selecciono aquellas columnas para las que haya al menos un registro == 1 (presencia sintomas)
#--------------------------------------------------------------------------------------------------

base_sintomas_filtrados <- base_sintomas %>%
  select(Id, where(~ any(. == 1, na.rm = TRUE)))


base_sintomas_filtrados <- base_sintomas_filtrados %>%
  filter(if_any(everything(), ~ . == 1))



# Nombres de comorbilidades (intersecciones)
#Excluyo la primera columna (ID)
variables_sintomas <- colnames(base_sintomas_filtrados)[-1]

#--------------------------------------------------------------------------------------------------
#5- Grafico upset
#--------------------------------------------------------------------------------------------------

GRAFICO_UPSET_SINTOMAS <- upset(
  data = base_sintomas_filtrados, #base de datos 
  intersect = variables_sintomas, #variables que se cruzan para ver intersecciones
  min_size = 4,
  max_degree= 6,
  name = "Signos y síntomas", #nombre del eje horizontal del gráfico
  base_annotations = list(
    'Intersecciones' = intersection_size( #nombre del eje vertical del gráfico
      mapping = aes(),   
      fill = "#1F77B4",  # color de relleno de las barras
      color = "black",   # color del borde de las barras
      text = list(size = 4))),
  
  themes =  upset_modify_themes( #modifica la estética de componentes específicos del gráfico
    list(
      'intersections_matrix' = theme(
        axis.text.y = element_text(size = 8),  # etiquetas del eje Y
        axis.title.x = element_text(size = 8)  # etiquetas del eje x
      ),
      'overall_sizes' = theme(
        axis.text.x = element_text(angle = 90, size = 8) #tamaño y rotación del gráfico de barras horizontales
      ))
  )
  
) + labs(caption = "Fuente: Elaboración propia en base a los datos provenientes 
  del Sistema Nacional de Vigilancia de la Salud SNVS 2.0") +
  theme(plot.caption = element_text(size = 8, hjust = 0))

#Gráfico

GRAFICO_UPSET_SINTOMAS

#--------------------------------------------------------------------------------------------------
#                                         TEXTO ENRIQUECIDO
#--------------------------------------------------------------------------------------------------

# Crear tabla de combinaciones

tabla_combinaciones_sintomas <- base_sintomas_filtrados %>%
  select(-Id) %>%                         # Quito ID
  mutate(across(everything(), as.numeric)) %>% 
  
  # Crear strings con los sintomas presentes por fila
  
  mutate(Combinacion = apply(., 1, function(x){
    paste(names(.)[which(x == 1)], collapse = " + ")
  })) %>%
  group_by(Combinacion) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Obtener la combinación más frecuente

top_4_combinaciones_sintomas <- tabla_combinaciones_sintomas %>% 
  slice(1:4)

# Generar objetos individuales para cada combinación

for(i in 1:nrow(top_4_combinaciones_sintomas)){
  combo_name_sintoma <- paste0("COMBINACION_SINTOMA", i)
  assign(combo_name_sintoma, top_4_combinaciones_sintomas$Combinacion[i])
  
  freq_name_sintoma <- paste0("FRECUENCIA_SINTOMA", i)
  assign(freq_name_sintoma, top_4_combinaciones_sintomas$Frecuencia[i])
}


#--------------------------------------------------------------------------------------------------
#                                         N GRAFICO SIGNOS Y SÍNTOMAS
#--------------------------------------------------------------------------------------------------

upset_sintomas <- upset_data(
  base_sintomas_filtrados,
  intersect = variables_sintomas
)

# vector con tamaños de cada barra
observaciones_barra <- upset_sintomas$sizes$exclusive_intersection

# obtener los nombres de las intersecciones
nombres <- names(observaciones_barra)

# obtener el grado 
grado <- str_count(nombres, "-") + 1  

# aplicar filtros igual que en el gráfico
observaciones_barra_filtrado <- observaciones_barra[ observaciones_barra >= 4 & grado <= 6 ]

# sumar valores
suma_barras_sintomas <- sum(observaciones_barra_filtrado)



