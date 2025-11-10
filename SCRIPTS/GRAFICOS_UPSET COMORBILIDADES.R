#-----------------------------------------------------------------------------------
#                        🛑   GRÁFICOS UPSET PARA COMORBILIDADES Y SÍNTOMAS 
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#Selecciono registros solo con resultado positivo para al menos una determinacion
#-----------------------------------------------------------------------------------

columnas <- c("VSR_FINAL","COVID_19_FINAL","INFLUENZA_FINAL")

resultado <- c("Negativo","Sin resultado","En estudio")

DATA_UC_LISTA<- DATA_UC_LISTA %>%
  mutate(DETERMINACION_POSITIVA = if_else(
    if_any(all_of(columnas), ~ !.x %in% resultado),
    "1", "0"))

DATA_UC_LISTA <- DATA_UC_LISTA %>% filter(DETERMINACION_POSITIVA == "1")


#-----------------------------------------------------------------------------------
#                        🛑   ANALISIS COMORBILIDADES
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#1- Creo vectores que contienen nombres de comorbilidades
#-----------------------------------------------------------------------------------

comorbilidades <- c(
  "DIABETES", "BAJO_PESO_NACIMIENTO", "ASMA", "TUBERCULOSIS", "ENF_RESPIRATORIA",
  "CARDIOPATIA_CONGENITA", "VIH", "ASPLENIA", "DESNUTRICION", "CANCER",
  "TRASPLANTADO", "BRONQUIOLITIS_PREVIA", "EMBARAZO_PUERPERIO", "EMBARAZO_COMORBILIDAD",
  "ENF_NEUROLOGICA_CRONICA", "ENF_HEPATICA", "HIPERTENSION", "ENF_CEREBROVASCULAR",
  "ENF_NEUROMUSCULAR", "DISCAPACIDAD_INTELECTUAL", "ENF_CARDIACA", "ENF_REUMATOLOGICA",
  "DBP", "ASPIRINA", "ENF_RENAL", "OBESIDAD", "PREMATURIDAD_MEN33SG",
  "PREMATURIDAD_33A36SG", "INMUNOCOMPROMETIDO_OTRAS_CAUSAS", "S_DOWN",
  "FUMADOR", "OTRAS_COMORBILIDADES", "SIN_COMORBILIDADES"
)


#------------------------------------------------------------------------------------------
#2- Reemplazo "9" y "NA" por "0" para tener un df binario donde 1 es presencia y 0 ausencia
#------------------------------------------------------------------------------------------

base_comorbilidades <- DATA_UC_LISTA %>%
  mutate(across(
    all_of(comorbilidades),
    ~ as.numeric(.) %>%                    
      replace_na(0) %>%                     
      replace(. == 9, 0)                    
  ))

#------------------------------------------------------------------------------------------
#3- Selecciono variables de interés para el gráfico
#------------------------------------------------------------------------------------------

base_comorbilidades <- base_comorbilidades %>% select(all_of(comorbilidades)) %>%
  mutate(ID = row_number()) %>%
  relocate(ID)


#Renombro las columnas para cambiar los "-" por espacios
base_comorbilidades <- base_comorbilidades %>% 
  rename_with(~ str_replace_all(., "_", " ")) 


#--------------------------------------------------------------------------------------------------
#4-Selecciono aquellas columnas para las que haya al menos un registro == 1 (presencia comorbilidad)
#--------------------------------------------------------------------------------------------------

base_comorbilidades_filtradas <- base_comorbilidades %>%
  select(ID, where(~ any(. == 1, na.rm = TRUE)))


base_comorbilidades_filtradas <- base_comorbilidades_filtradas %>%
  filter(if_any(everything(), ~ . == 1))


# Nombres de comorbilidades (intersecciones)
#Excluyo la primera columna (ID)
variables_comorbilidades <- colnames(base_comorbilidades_filtradas)[-1]

#--------------------------------------------------------------------------------------------------
#5- Grafico upset
#--------------------------------------------------------------------------------------------------

GRAFICO_UPSET_COMORBILIDADES <- upset(
  data = base_comorbilidades_filtradas, 
  intersect = variables_comorbilidades,
  min_size = 2, #tamaño minimo de interseccion
  name = "Comorbilidades",
  base_annotations = list(
    'Intersecciones' = intersection_size(
      mapping = aes(),
      fill = "#9467BD",
      color = "black",
      text = list(size = 4)
    )
  ),
  themes = upset_modify_themes(
    list(
      'intersections_matrix' = theme(
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)
      ),
      'overall_sizes' = theme(
        axis.text.x = element_text(angle = 90, size = 8)
      )
    )
  )
) + labs(caption = "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0
  *Se excluyeron las combinaciones de comorbilidades con un único caso (n=1)") +
  theme(plot.caption = element_text(size = 8, hjust = 0))


#Gráfico

GRAFICO_UPSET_COMORBILIDADES

#--------------------------------------------------------------------------------------------------
# TEXTO ENRIQUECIDO INTERSECCION DE COMORBILIDADES
#--------------------------------------------------------------------------------------------------

# Crear tabla de combinaciones

tabla_combinaciones <- base_comorbilidades_filtradas %>%
  select(-ID) %>%                         # Quito ID
  mutate(across(everything(), as.numeric)) %>% 
  
  # Crear strings con las comorbilidades presentes por fila
  mutate(Combinacion = apply(., 1, function(x){
    paste(names(.)[which(x == 1)], collapse = " + ")
  })) %>%
  group_by(Combinacion) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

#Obtener la combinación más frecuente

top_4_combinaciones <- tabla_combinaciones %>% 
  slice(1:4)

# Generar objetos individuales para cada combinación

for(i in 1:nrow(top_4_combinaciones)){
  combo_name <- paste0("COMBINACION_", i)
  assign(combo_name, top_4_combinaciones$Combinacion[i])
  
  freq_name <- paste0("FRECUENCIA_", i)
  assign(freq_name, top_4_combinaciones$Frecuencia[i])
}
