# ------------------------------------------------------------------------
# TABLA DE CODETECCIONES POR SEMANA (1 FILA POR SEMANA Y COMBINACION)
# ------------------------------------------------------------------------

# 1. Crear variables binarias si no las tenés
DATA_UC_LISTA_PROCESADA <- DATA_UC_LISTA %>%
  mutate(
    detectable_influenza = ifelse(INFLUENZA_FINAL %in% c("Influenza A (sin subtipificar)", 
                                                         "Influenza A H3N2",
                                                         "Influenza positivo-Sin Tipo", 
                                                         "Influenza B (sin linaje)",
                                                         "Influenza A H1N1"), 1, 0),
    detectable_VSR = ifelse(VSR_FINAL %in% c("VSR", "VSR B", "VSR A"), 1, 0),
    detectable_COVID = ifelse(COVID_19_FINAL == "Positivo", 1, 0)
  )

# 2. Identificar codecciones
CODETECCIONES <- DATA_UC_LISTA_PROCESADA %>%
  filter((detectable_influenza + detectable_VSR + detectable_COVID) > 1) %>%
  mutate(
    combinacion = case_when(
      detectable_influenza == 1 & detectable_VSR == 1 & detectable_COVID == 1 ~ "Influenza + VSR + COVID",
      detectable_influenza == 1 & detectable_VSR == 1 ~ "Influenza + VSR",
      detectable_influenza == 1 & detectable_COVID == 1 ~ "Influenza + COVID",
      detectable_VSR == 1 & detectable_COVID == 1 ~ "VSR + COVID"
    )
  ) %>%
  select(IDEVENTOCASO, SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION, combinacion)

# 3. Resumen por semana y codección (una fila por semana y combinación)
RESUMEN_CODETECCIONES_SEMANAL <- CODETECCIONES %>%
  group_by(SEPI_MIN_INTERNACION, combinacion) %>%
  summarise(casos = n(), .groups = "drop") %>%
  arrange( SEPI_MIN_INTERNACION, combinacion)

# 4. Crear tabla estilo OPS/OMS
TABLA_CODETECCIONES_GT <- RESUMEN_CODETECCIONES_SEMANAL %>%
  gt()  %>%
  fmt_number(
    columns = vars(casos),
    decimals = 0
  ) %>%
  cols_label(
    SEPI_MIN_INTERNACION = "SE",
    combinacion = "Codetección",
    casos = "N°"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12,
    table.border.top.width = 1,
    table.border.bottom.width = 1
  )

# Mostrar la tabla en RStudio
TABLA_CODETECCIONES_GT
