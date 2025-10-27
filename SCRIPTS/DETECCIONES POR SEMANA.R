# ------------------------------------------------------------------------
# GRÁFICO N°3: CASOS DE IRAG E IRAG EXTENDIDA POR TIPO DE VIRUS RESPIRATORIO 
# Y SEMANA EPIDEMIOLÓGICA
# ------------------------------------------------------------------------

sum(DATA_UC_LISTA$INFLUENZA_FINAL == "Sin resultado", na.rm = TRUE)

sum(DATA_UC_LISTA$VSR_FINAL == "Sin resultado", na.rm = TRUE)

sum(DATA_UC_LISTA$COVID_19_FINAL == "Sin resultado", na.rm = TRUE)

# ------------------------------------------------------------------------
# 1. CREAR VARIABLES BINARIAS 
# ------------------------------------------------------------------------

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

# ------------------------------------------------------------------------
# 2. AGRUPAR POR SEMANA EPIDEMIOLÓGICA Y AÑO PARA SUMAR LOS CASOS DETECTABLES
# DE CADA VIRUS Y NEGATIVOS
# ------------------------------------------------------------------------

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


semanas <- as.character(1:35)

# ------------------------------------------------------------------------
# 3. CREAR GRÁFICO 
# ------------------------------------------------------------------------

GRAFICO_INTERACTIVO_VIRUS <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(
    # Usamos la nueva columna sepi_label para las categorías si la creamos
    categories = as.character(DATA_VIRUS$SEPI_MIN_INTERNACION),
    title = list(text = "Semana epidemiológica"),
    labels = list(rotation = -0, style = list(fontSize = "9px")),
    tickInterval = 1 # Mostrar todas las semanas
  ) %>%
  hc_yAxis(
    title = list(text = "Determinaciones"),
    reversedStacks = FALSE # Para que el apilamiento sea de abajo hacia arriba (negativos abajo)
  ) %>%
  hc_plotOptions(column = list(
    stacking = "normal", # Apilar las series
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,
    groupPadding = 0
  )) %>%
  # Ahora usamos las columnas de DATA_VIRUS directamente
  hc_add_series(name = "Influenza", data = DATA_VIRUS$casos_influenza, color = "#1F77B4") %>%
  hc_add_series(name = "VSR", data = DATA_VIRUS$casos_vsr, color = "#1B9E77") %>%
  hc_add_series(name = "SARS-CoV-2", data = DATA_VIRUS$casos_covid, color = "#9467BD") %>%
  hc_add_series(name = "Negativos", data = DATA_VIRUS$casos_negativos, color = "#E0E0E0") %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  ) %>%
  hc_tooltip(
    shared = TRUE,
    valueSuffix = " casos"
  ) %>%
  hc_credits(
    enabled = TRUE,
    text = "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0",
    style = list(fontSize = "10px")
  ) %>%
  hc_exporting(enabled = TRUE)


print(GRAFICO_INTERACTIVO_VIRUS)







