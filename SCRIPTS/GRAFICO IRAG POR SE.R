# ----------------------------------------------------------------
# GRAFICO N°1: CASOS DE IRAG E IRAG EXTENDIDA POR SE
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# PASOS PARA GRAFICO CASOS DE IRAG/IRAG EXTENDIDA POR SEMANA EPIDEM
# ----------------------------------------------------------------

## PASO 1: CREAR TABLA CON LOS CASOS DE IRAG E IRAG EXT POR AÑO Y SEMANA

CASOS_SEMANA_ANIO <- DATA_UC_LISTA %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION, CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n(), .groups = "drop") %>%

# Crear la etiqueta de semana solo con el número (dos dígitos)
  mutate(SEMANA_LABEL = str_pad(SEPI_MIN_INTERNACION, 2, pad = "0"))

## PASO 2: ORDENAR FACTOR Y PIVOTEAR A FORMATO ANCHO
CASOS_SEMANA_ANIO <- CASOS_SEMANA_ANIO %>%
  arrange(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  mutate(SEMANA_LABEL = factor(SEMANA_LABEL, levels = unique(SEMANA_LABEL)))

CASOS_PIVOT <- CASOS_SEMANA_ANIO %>%
  pivot_wider(
    names_from = CLASIFICACION_MANUAL,
    values_from = CASOS,
    values_fill = list(CASOS = 0)
  )

## PASO 3: ASIGNAR COLORES

colores <- c("Infección respiratoria aguda grave (IRAG)" = "#1F77B4",  # azul
             "IRAG extendida" = "#9ACFD9")  # naranja suave


## PASO 4: GRAFICO INTERACTIVO

GRAFICO_INTERACTIVO_SE <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(
    categories = as.character(CASOS_PIVOT$SEMANA_LABEL),
    title = list(text = "Semana epidemiológica"),
    labels = list(rotation = 0, style = list(fontSize = "9px")),
    tickInterval = 1
  ) %>%
  hc_yAxis(title = list(text = "Número de casos")) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,
    groupPadding = 0.07
  )) %>%
  hc_add_series(
    name = "Infección respiratoria aguda grave (IRAG)",
    data = CASOS_PIVOT$`Infección respiratoria aguda grave (IRAG)`,
    color = "#1F77B4"
  ) %>%
  hc_add_series(
    name = "IRAG extendida",
    data = CASOS_PIVOT$`IRAG extendida`,
    color = "#9ACFD9"
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  ) %>%
  hc_tooltip(shared = TRUE, valueSuffix = " casos") %>%
  hc_credits(
    enabled = TRUE,
    text = "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0",
    style = list(fontSize = "10px"),
    align = "left",
    verticalAlign = "bottom",
    x = 10
  ) %>%
  hc_exporting(enabled = TRUE)

GRAFICO_INTERACTIVO_SE
