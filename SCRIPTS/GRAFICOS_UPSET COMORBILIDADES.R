#-----------------------------------------------------------------------------------
#                        🛑   ANALISIS COMORBILIDADES GRAFICO UPSET
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


#Renombro las columnas para cambiar los "-" por espacios y aplicar mayúscula solo a la 
#primera palabra

base_comorbilidades <- base_comorbilidades %>% 
  rename_with(~ str_replace_all(., "_", " ")) %>%
  rename_with(~str_to_sentence(.,))

#Correcciones a casos específicos

base_comorbilidades <- base_comorbilidades %>%
  rename_with(~ str_replace_all(.,"Enf", "Enfermedad") %>%
                str_replace_all("Vih", "VIH") %>%
                str_replace_all("Dbp", "DBP") %>%
                str_replace_all("men33sg", "menor a 33 SG") %>%
                str_replace_all("33a36sg", "33 a 36 SG") %>%
                str_replace_all("S down", "Síndrome de Down") %>%
                str_replace_all("Cardiopatia congenita", "Cardiopatía congénita") %>%
                str_replace_all("Desnutricion", "Desnutrición") %>%
                str_replace_all("Hipertension", "Hipertensión") %>%
                str_replace_all("reumatologica","reumatológica") %>%
                str_replace_all("Cancer", "Cáncer") %>%
                str_replace_all("neurologica cronica", " neurológica crónica") %>%
                str_replace_all("cardiaca","cardíaca") %>%
                str_replace_all ("hepatica", "hepática") %>%
                str_replace_all("hipertension", "hipertensión"))



#--------------------------------------------------------------------------------------------------
#4-Selecciono aquellas columnas para las que haya al menos un registro == 1 (presencia comorbilidad)
#--------------------------------------------------------------------------------------------------

base_comorbilidades_filtradas <- base_comorbilidades %>%
  select(ID, where(~ any(. == 1, na.rm = TRUE)))


base_comorbilidades_filtradas <- base_comorbilidades_filtradas %>%
  filter(if_any(everything(), ~ . == 1))


# Nombres de comorbilidades (intersecciones)
# Excluyo la primera columna (ID)

variables_comorbilidades <- colnames(base_comorbilidades_filtradas)[-1]

#--------------------------------------------------------------------------------------------------
#5- Grafico upset
#--------------------------------------------------------------------------------------------------

GRAFICO_UPSET_COMORBILIDADES <- upset(
  data = base_comorbilidades_filtradas, 
  intersect = variables_comorbilidades,
  min_size = 6, #tamaño minimo de interseccion
  name = "Comorbilidades",
  base_annotations = list(
    'Intersecciones' = intersection_size(
      mapping = aes(),
      fill = "#9467BD",
      color = "black",
      text = list(size = 2.5)
    )
  ),
  themes = upset_modify_themes(
    list(
      'intersections_matrix' = theme(
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6)
      ),
      'overall_sizes' = theme(
        axis.text.x = element_text(angle = 90, size = 6)
      )
    )
  )
) + labs(caption = "Fuente: Elaboración propia en base a los datos provenientes 
   del Sistema Nacional de Vigilancia de la Salud SNVS 2.0") +
  theme(plot.caption = element_text(size = 8, hjust = 0)) 


#Gráfico

GRAFICO_UPSET_COMORBILIDADES

#--------------------------------------------------------------------------------------------------
# TEXTO ENRIQUECIDO INTERSECCION DE COMORBILIDADES
#--------------------------------------------------------------------------------------------------

# Crear tabla de combinaciones

tabla_combinaciones_comorb <- base_comorbilidades_filtradas %>%
  select(-ID) %>%                         # Quito ID
  mutate(across(everything(), as.numeric)) %>% 
  
# Crear strings con las comorbilidades presentes por fila
  
  mutate(Combinacion = apply(., 1, function(x){
    paste(names(.)[which(x == 1)], collapse = " + ")
  })) %>%
  group_by(Combinacion) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Obtener la combinación más frecuente

top_4_combinaciones_comorb <- tabla_combinaciones_comorb %>% 
  slice(1:4)

# Generar objetos individuales para cada combinación

for(i in 1:nrow(top_4_combinaciones_comorb)){
  combo_name_comorb <- paste0("COMBINACION_COMORB", i)
  assign(combo_name_comorb, top_4_combinaciones_comorb$Combinacion[i])
  
  freq_name_comorb <- paste0("FRECUENCIA_COMORB", i)
  assign(freq_name_comorb, top_4_combinaciones_comorb$Frecuencia[i])
}


#--------------------------------------------------------------------------------------------------
# N GRÁFICO DE COMORBILIDADES
#--------------------------------------------------------------------------------------------------

upset_comorbilidades <- upset_data(
  base_comorbilidades_filtradas,
  intersect = variables_comorbilidades
)

# vector con tamaños de cada barra
observaciones_barra_comorb <- upset_comorbilidades$sizes$exclusive_intersection

# filtrar las barras que aparecen en el gráfico (≥ 6)
observaciones_barra_comorb_filtrado <- observaciones_barra_comorb[observaciones_barra_comorb>= 6]

# sumar todas las barras violetas
suma_barras <- sum(observaciones_barra_comorb_filtrado)



