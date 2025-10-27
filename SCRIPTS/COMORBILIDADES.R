# ------------------------------------------------------------------------
# TABLA DE COMORBILIDADES MAS FRECUENTES
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Vector con las variables de comorbilidades
# ------------------------------------------------------------------------

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


# ------------------------------------------------------------------------
# Calcular total de casos
# ------------------------------------------------------------------------

TOTAL_CASOS <- nrow(DATA_UC_LISTA)

# ------------------------------------------------------------------------
# Procesar tabla y quedarse con las 10 más frecuentes
# ------------------------------------------------------------------------

TABLA_COMORB <- DATA_UC_LISTA %>%
  select(any_of(vars_comorb)) %>%
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


# ------------------------------------------------------------------------
# Renombrar las comorbilidades que definiste
# ------------------------------------------------------------------------

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

# ------------------------------------------------------------------------
# Crear tabla gt
# ------------------------------------------------------------------------
TABLA_COMORB_GT <- TABLA_COMORB %>%
  gt() %>%
  cols_label(
    Comorbilidad = "Comorbilidad",
    Frecuencia = "n",
    Porcentaje = "%"
  ) %>%
  fmt_number(
    columns = Porcentaje,
    decimals = 1
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.align = "center",
    data_row.padding = px(2)  # reduce espacio vertical
  ) %>%
  cols_width(
    Comorbilidad ~ px(180),   # reducimos ancho de columna
    Frecuencia ~ px(60),
    Porcentaje ~ px(60)
  ) %>%
  tab_source_note(
    source_note = "Nota: Cada paciente puede presentar más de una comorbilidad, por lo que los porcentajes no suman 100%."
  )

TABLA_COMORB_GT

