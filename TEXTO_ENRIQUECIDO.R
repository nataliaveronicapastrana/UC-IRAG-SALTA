# ===============================
# 1. SEMANA EPIDEMIOLOGICA MINIMA Y MAXIMA DESDE DATA_UC_LISTA
# ===============================

# Asegurar que SEPI_FECHA_INTER sea numérica

DATA_UC_LISTA$SEPI_MIN_INTERNACION <- as.numeric(DATA_UC_LISTA$SEPI_MIN_INTERNACION)

# Calcular semanas mínima y máxima

SE_MIN_2025 <- min(DATA_UC_LISTA$SEPI_MIN_INTERNACION, na.rm = TRUE)
SE_MAX_2025 <- max(DATA_UC_LISTA$SEPI_MIN_INTERNACION, na.rm = TRUE)

# Mostrar resultados

cat("Semana epidemiológica mínima:", SE_MIN_2025, "\n")
cat("Semana epidemiológica máxima:", SE_MAX_2025, "\n")

# ===============================
# 2. VALOR UNICO DE ESTABLECIMIENTO_INTERNACION
# ===============================

# Nombre del establecimiento sin repetir

ESTABLECIMIENTO <- unique(na.omit(DATA_UC_LISTA$ESTABLECIMIENTO_INTERNACION)) 

# =================================
# 3.PROMEDIO SEMANAL DE CASOS TOTALES (IRAG + IRAG EXTENDIDA)
# =================================

PROMEDIO_SEMANAL_CASOS <- DATA_UC_LISTA %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)", 
                                     "IRAG extendida")) %>%
  group_by(SEPI_MIN_INTERNACION) %>%
  summarise(casos_semanales = n(), .groups = "drop") %>%
  summarise(promedio = mean(casos_semanales, na.rm = TRUE)) %>%
  pull(promedio) %>%
  round()

# ================================
# 4.SE CON MAYOR NUMERO DE CASOS TOTALES 
# ===============================

# Semana con mayor número de casos
SE_MAYOR_N_CASOS <- DATA_UC_LISTA %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)", 
                                     "IRAG extendida")) %>%
  group_by(SEPI_MIN_INTERNACION) %>%
  summarise(casos_semanales = n(), .groups = "drop") %>%
  arrange(desc(casos_semanales)) %>%
  slice(1)

# Guardar la semana y el número de casos en objetos separados

SEMANA_PICO <- SE_MAYOR_N_CASOS$SEPI_MIN_INTERNACION
CASOS_SEMANA_PICO <- SE_MAYOR_N_CASOS$casos_semanales


# =================================
# 5. FRECUENCIAS ABSOLUTAS Y RELATIVAS
# ===============================

# Crear tabla de frecuencias y porcentajes SOLO para el año 2025
FRECUENCIAS <- DATA_UC_LISTA %>%
  filter(ANIO_MIN_INTERNACION == 2025) %>%
  count(CLASIFICACION_MANUAL) %>%
  mutate(prop = round(100 * n / sum(n), 0))  # porcentaje sin decimales

# Frecuencias individuales

FRECUENCIA_IRAG<- FRECUENCIAS %>%
  filter(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)") %>%
  pull(n)

FRECUENCIA_IRAG_EXT <- FRECUENCIAS %>%
  filter(CLASIFICACION_MANUAL == "IRAG extendida") %>%
  pull(n)

# Porcentajes individuales

PORCENTAJE_IRAG <- FRECUENCIAS %>%
  filter(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)") %>%
  pull(prop) 


PORCENTAJE_IRAG_EXT <- FRECUENCIAS %>%
  filter(CLASIFICACION_MANUAL == "IRAG extendida") %>%
  pull(prop) 

# Total de casos (IRAG + IRAG extendida)

TOTAL_CASOS <- FRECUENCIA_IRAG + FRECUENCIA_IRAG_EXT

# =================================
# 6. GRAFICO 2
# ===============================

# Grupo etario con mayor número de casos

CASOS_POR_EDAD <- DATA_UC_LISTA %>%
  filter(!is.na(EDAD_UC_IRAG)) %>%
  group_by(EDAD_UC_IRAG) %>%
  summarise(
    IRAG = sum(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)", na.rm = TRUE),
    IRAG_EXTENDIDA = sum(CLASIFICACION_MANUAL == "IRAG extendida", na.rm = TRUE),
    .groups = "drop"
  )

# Grupo etario con mayor número de casos de IRAG
GRUPO_MAYOR_IRAG <- CASOS_POR_EDAD %>%
  filter(IRAG == max(IRAG)) %>%
  pull(EDAD_UC_IRAG)

# Grupo etario con mayor número de casos de IRAG extendida
GRUPO_MAYOR_IRAG_EXT <- CASOS_POR_EDAD %>%
  filter(IRAG_EXTENDIDA == max(IRAG_EXTENDIDA)) %>%
  pull(EDAD_UC_IRAG)

# Cantidad de casos de IRAG en el grupo con mayor número de IRAG
GRUPO_CANTIDAD_IRAG <- CASOS_POR_EDAD %>%
  filter(EDAD_UC_IRAG == GRUPO_MAYOR_IRAG) %>%
  pull(IRAG)

# Cantidad de casos de IRAG extendida en el grupo con mayor número de IRAG extendida
GRUPO_CANTIDAD_IRAG_EXT <- CASOS_POR_EDAD %>%
  filter(EDAD_UC_IRAG == GRUPO_MAYOR_IRAG_EXT) %>%
  pull(IRAG_EXTENDIDA)

# Mayor tasa de letalidad y grupo de edad correspondiente

CASOS_TOTALES_POR_EDAD <- DATA_UC_LISTA %>%
  filter(!is.na(EDAD_UC_IRAG)) %>%
  group_by(EDAD_UC_IRAG) %>%
  summarise(casos_totales = sum(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)",
                                                            "IRAG extendida")), .groups = "drop")

FALLECIDOS_POR_EDAD <- DATA_UC_LISTA %>%
  filter(!is.na(EDAD_UC_IRAG)) %>%
  group_by(EDAD_UC_IRAG) %>%
  summarise(fallecidos = sum(FALLECIDO == "SI", na.rm = TRUE), .groups = "drop")

LETALIDAD_POR_EDAD <- FALLECIDOS_POR_EDAD %>%
  left_join(CASOS_TOTALES_POR_EDAD, by = "EDAD_UC_IRAG") %>%
  mutate(tasa_letalidad = round((fallecidos / casos_totales) * 100, 0))

MAYOR_TASA_LETALIDAD <- max(LETALIDAD_POR_EDAD$tasa_letalidad, na.rm = TRUE)

GRUPO_MAYOR_TASA <- LETALIDAD_POR_EDAD %>%
  filter(tasa_letalidad == MAYOR_TASA_LETALIDAD) %>%
  pull(EDAD_UC_IRAG)

# =================================
# 7. GRAFICO 3
# ===============================

#Crear variables binarias

DATA_UC_LISTA_PROCESADA <- DATA_UC_LISTA %>%
  # Variable binaria para Influenza
  mutate(detectable_influenza = case_when(
    INFLUENZA_FINAL %in% c("Influenza A (sin subtipificar)", "Influenza A H3N2",
                           "Influenza positivo-Sin Tipo", "Influenza B (sin linaje)",
                           "Influenza A H1N1") ~ 1,
    INFLUENZA_FINAL %in% c("Negativo") ~ 0,
    TRUE ~ NA_real_ # Valores no contemplados quedan como NA
  )) %>%
  # Variable binaria para VSR
  mutate(detectable_VSR = case_when(
    VSR_FINAL %in% c("VSR", "VSR B", "VSR A") ~ 1,
    VSR_FINAL %in% c("Negativo") ~ 0,
    TRUE ~ NA_real_
  )) %>%
  # Variable binaria para COVID
  mutate(detectable_COVID = case_when(
    COVID_19_FINAL == "Positivo" ~ 1,
    COVID_19_FINAL %in% c("Negativo") ~ 0,
    TRUE ~ NA_real_
  ))

# Total de casos positivos para algún virus (Influenza, VSR o COVID-19)

DATA_VIRUS <- DATA_UC_LISTA_PROCESADA %>% # Ahora trabajamos con el dataframe que tiene todas las nuevas columnas
  group_by(SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION) %>%
  summarise(
    casos_influenza = sum(detectable_influenza == 1, na.rm = TRUE),
    casos_vsr = sum(detectable_VSR == 1, na.rm = TRUE),
    casos_covid = sum(detectable_COVID == 1, na.rm = TRUE),
    casos_negativos = sum(
      detectable_influenza == 0 &
        detectable_VSR == 0 &
        detectable_COVID == 0,
      na.rm = TRUE
    ),
    .groups = "drop" # Eliminar la agrupación después de resumir
  ) %>%
  arrange(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION)

TOTAL_CASOS_POSITIVOS <- sum(DATA_VIRUS$casos_influenza, na.rm = TRUE) +
  sum(DATA_VIRUS$casos_vsr, na.rm = TRUE) +
  sum(DATA_VIRUS$casos_covid, na.rm = TRUE)

# Total de casos de Influenza
TOTAL_CASOS_INFLUENZA <- sum(DATA_VIRUS$casos_influenza, na.rm = TRUE)

# Total de casos de VSR
TOTAL_CASOS_VSR <- sum(DATA_VIRUS$casos_vsr, na.rm = TRUE)

# Total de casos de COVID-19
TOTAL_CASOS_COVID <- sum(DATA_VIRUS$casos_covid, na.rm = TRUE)

# =================================
# 8. TABLA 1
# ===============================

# Vector con las variables de comorbilidades

vars_comorb <- c(
  "BAJO_PESO_NACIMIENTO", "ASMA", "DIABETES", "TUBERCULOSIS",
  "ENF_RESPIRATORIA", "CARDIOPATIA_CONGENITA", "VIH", "ASPLENIA",
  "DESNUTRICION", "CANCER", "TRASPLANTADO", "BRONQUIOLITIS_PREVIA",
  "EMBARAZO_PUERPERIO", "EMBARAZO_COMORBILIDAD", "ENF_NEUROLOGICA_CRONICA",
  "ENF_HEPATICA", "HIPERTENSION", "ENF_CEREBROVASCULAR", "ENF_NEUROMUSCULAR",
  "DISCAPACIDAD_INTELECTUAL", "ENF_CARDIACA", "ENF_REUMATOLOGICA", "DBP",
  "ASPIRINA", "ENF_RENAL", "OBESIDAD", "PREMATURIDAD_MEN33SG",
  "PREMATURIDAD_33A36SG", "INMUNOCOMPROMETIDO_OTRAS_CAUSAS",
  "S_DOWN", "FUMADOR", "OTRAS_COMORBILIDADES"
)

# Calcular total de casos

TOTAL_CASOS <- nrow(DATA_UC_LISTA)

# Procesar tabla y quedarse con las 10 más frecuentes

TABLA_COMORB <- DATA_UC_LISTA %>%
  select(all_of(vars_comorb)) %>%
  mutate(across(everything(), ~ na_if(., 9))) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to = "Comorbilidad",
    values_to = "Frecuencia"
  ) %>%
  mutate(
    Porcentaje = round((Frecuencia / TOTAL_CASOS) * 100, 1)
  ) %>%
  arrange(desc(Frecuencia)) %>%
  slice_head(n = 10)  # Solo las 10 más frecuentes

# Renombrar las comorbilidades que definiste

renombrar_comorbs <- c(
  "ASMA" = "Asma",
  "HIPERTENSION" = "Hipertensión",
  "OBESIDAD" = "Obesidad",
  "DIABETES" = "Diabetes",
  "ENF_RESPIRATORIA" = "Enf. respiratoria crónica",
  "VIH" = "VIH",
  "TUBERCULOSIS" = "Tuberculosis",
  "ENF_REUMATOLOGICA" = "Enf. reumatológica",
  "INMUNOCOMPROMETIDO_OTRAS_CAUSAS" = "Inmunocomprometido otras causas",
  "ENF_CARDIACA" = "Enf. cardíaca"
)

TABLA_COMORB <- TABLA_COMORB %>%
  mutate(Comorbilidad = ifelse(Comorbilidad %in% names(renombrar_comorbs),
                               renombrar_comorbs[Comorbilidad],
                               Comorbilidad))

# Crear objeto con la comorbilidad de mayor porcentaje

COMORB_MAYOR <- TABLA_COMORB %>%
  slice_max(Porcentaje, n = 1)

# Esto te da un dataframe con la comorbilidad más frecuente y su porcentaje
COMORB_MAYOR

# Nombre de la comorbilidad más frecuente
COMORB_MAYOR_NOMBRE <- COMORB_MAYOR$Comorbilidad

# Porcentaje de la comorbilidad más frecuente
COMORB_MAYOR_FRE <- COMORB_MAYOR$Frecuencia
COMORB_MAYOR_PCT <- COMORB_MAYOR$Porcentaje

# =================================
# 9. TOTAL DE DETERMINACIONES POSITIVAS
# =================================

# Total de determinaciones positivas (SARS-CoV-2, Influenza y VSR)
TOTAL_DETERMINACIONES_POSITIVAS <- DATA_VIRUS %>%
  summarise(
    total_positivas = sum(casos_influenza + casos_vsr + casos_covid, na.rm = TRUE)
  ) %>%
  pull(total_positivas)

TOTAL_DETERMINACIONES_POSITIVAS
