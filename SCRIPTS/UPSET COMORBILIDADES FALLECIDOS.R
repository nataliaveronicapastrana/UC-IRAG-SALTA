#-----------------------------------------------------------------------------------
#                        🛑   GRÁFICOS UPSET PARA COMORBILIDADES EN FALLECIDOS 
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Selecciono solo los registros con FALLECIDOS == "SI"
#-----------------------------------------------------------------------------------

DATA_UC_LISTA_FALLECIDOS <- DATA_UC_LISTA %>%
  filter(FALLECIDO == "SI")

#-----------------------------------------------------------------------------------
#                        🛑   ANÁLISIS DE COMORBILIDADES EN FALLECIDOS
#-----------------------------------------------------------------------------------

# 1. Creo vector con nombres de comorbilidades
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
# 2. Reemplazo "9" y "NA" por "0" (df binario: 1 presencia / 0 ausencia)
#------------------------------------------------------------------------------------------

base_comorbilidades_defunciones <- DATA_UC_LISTA_FALLECIDOS %>%
  mutate(across(
    all_of(comorbilidades),
    ~ as.numeric(.) %>%
      replace_na(0) %>%
      replace(. == 9, 0)
  ))

#------------------------------------------------------------------------------------------
# 3. Selecciono variables de interés para el gráfico
#------------------------------------------------------------------------------------------

base_comorbilidades_defunciones <- base_comorbilidades_defunciones %>%
  select(all_of(comorbilidades)) %>%
  mutate(ID = row_number()) %>%
  relocate(ID)

# Renombro columnas (quita "_")
base_comorbilidades_defunciones <- base_comorbilidades_defunciones %>%
  rename_with(~ str_replace_all(., "_", " "))

#--------------------------------------------------------------------------------------------------
# 4. Selecciono aquellas columnas con al menos un registro == 1 (presencia de comorbilidad)
#--------------------------------------------------------------------------------------------------

base_comorbilidades_filtradas_def <- base_comorbilidades_defunciones %>%
  select(ID, where(~ any(. == 1, na.rm = TRUE))) %>%
  filter(if_any(everything(), ~ . == 1))

# Nombres de comorbilidades presentes
variables_comorbilidades_defunciones <- colnames(base_comorbilidades_filtradas_def)[-1]

#--------------------------------------------------------------------------------------------------
# 5. Gráfico UpSet de comorbilidades en fallecidos
#--------------------------------------------------------------------------------------------------

GRAFICO_UPSET_COMORBILIDADES_FALLECIDOS <- upset(
  data = base_comorbilidades_filtradas_def,
  intersect = variables_comorbilidades_defunciones,
  min_size = 1,
  name = "Comorbilidades en fallecidos",
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
) +
  labs(caption = "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud (SNVS 2.0).
  *Se excluyeron las combinaciones de comorbilidades con un único caso (n=1)") +
  theme(plot.caption = element_text(size = 8, hjust = 0))

# Mostrar gráfico
GRAFICO_UPSET_COMORBILIDADES_FALLECIDOS


#--------------------------------------------------------------------------------------------------
# TEXTO ENRIQUECIDO INTERSECCION DE COMORBILIDADES
#--------------------------------------------------------------------------------------------------

# Crear tabla de combinaciones

tabla_combinaciones_defunciones <- base_comorbilidades_filtradas_def %>%
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

top_4_combinaciones_defunciones <- tabla_combinaciones_defunciones %>% 
  slice(1:4)

# Generar objetos individuales para cada combinación

for(i in 1:nrow(top_4_combinaciones_defunciones)){
  combo_name_defunciones <- paste0("DEFUNCION_", i)
  assign(combo_name_defunciones, top_4_combinaciones_defunciones$Combinacion[i])
  
  freq_name_defunciones <- paste0("DEFUNCIONFRE_", i)
  assign(freq_name_defunciones, top_4_combinaciones_defunciones$Frecuencia[i])
}