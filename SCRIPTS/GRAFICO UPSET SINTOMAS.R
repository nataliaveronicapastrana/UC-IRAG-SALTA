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

#--------------------------------------------------------------------------------------------------
#4-Selecciono aquellas columnas para las que haya al menos un registro == 1 (presencia sintomas)
#--------------------------------------------------------------------------------------------------

base_sintomas_filtrados <- base_sintomas %>%
  select(ID, where(~ any(. == 1, na.rm = TRUE)))


base_sintomas_filtrados <- base_sintomas_filtrados %>%
  filter(if_any(everything(), ~ . == 1))


#Renombro las columnas para cambiar los "-" por espacios
base_sintomas_filtrados <- base_sintomas_filtrados %>% 
  rename_with(~ str_replace_all(., "_", " ")) 


# Nombres de comorbilidades (intersecciones)
#Excluyo la primera columna (ID)
variables_sintomas <- colnames(base_sintomas_filtrados)[-1]

#--------------------------------------------------------------------------------------------------
#5- Grafico upset
#--------------------------------------------------------------------------------------------------

GRAFICO_UPSET_SINTOMAS <- upset(
  data = base_sintomas_filtrados, #base de datos 
  intersect = variables_sintomas, #variables que se cruzan para ver intersecciones
  min_size = 2,
  max_degree= 4,
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
  
)

#Gráfico

GRAFICO_UPSET_SINTOMAS
