# ----------------------------------------------------------------
# GRAFICO N°2: CASOS DE IRAG E IRAG EXTENDIDA POR GRUPO DE EDAD
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# ORDENAR LA VARIABLE GRUPO ETARIO
# ----------------------------------------------------------------
orden_edades <- c("15 a 19 Años","20 a 24 Años","25 a 29 Años", 
                  "30 a 34 Años","35 a 39 Años","40 a 44 Años", 
                  "45 a 49 Años","50 a 54 Años","55 a 59 Años",
                  "60 a 64 Años","65 a 69 Años","70 a 74 Años","75 y más Años")

DATA_UC_LISTA$EDAD_UC_IRAG <- factor(DATA_UC_LISTA$EDAD_UC_IRAG,
                                     levels = orden_edades,
                                     ordered = TRUE)

# ----------------------------------------------------------------
# AGRUPAR DATOS DE CASOS POR GRUPO ETARIO
# ----------------------------------------------------------------
CASOS_POR_EDAD <- DATA_UC_LISTA %>%
  filter(!is.na(EDAD_UC_IRAG), !is.na(CLASIFICACION_MANUAL)) %>%
  group_by(EDAD_UC_IRAG, CLASIFICACION_MANUAL) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = CLASIFICACION_MANUAL, 
              values_from = n, values_fill = 0)

# ----------------------------------------------------------------
# CALCULAR TASA DE LETALIDAD POR GRUPO ETARIO
# ----------------------------------------------------------------
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

# ----------------------------------------------------------------
# GRAFICO INTERACTIVO HORIZONTAL
# ----------------------------------------------------------------
GRAFICO_INTERACTIVO_EDAD <- highchart() %>%
  hc_chart(
    type = "column",
    inverted = FALSE,
    height = 450
  ) %>%
  hc_xAxis(
    categories = as.character(CASOS_POR_EDAD$EDAD_UC_IRAG),
    title = list(text = "Grupo etario"),
    labels = list(style = list(fontSize = "10px"))
  ) %>%
  hc_yAxis_multiples(
    list( # Eje principal para casos (barras)
      title = list(text = "Número de casos"),
      opposite = FALSE,
      gridLineColor = "#E6E6E6"
    ),
    list( # Eje secundario para tasa de letalidad
      title = list(text = "Tasa de letalidad (%)"),
      opposite = TRUE,
      gridLineWidth = 0
    )
  ) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,
    groupPadding = 0
  )) %>%
  # Barras
  hc_add_series(
    name = "IRAG",
    data = CASOS_POR_EDAD$`Infección respiratoria aguda grave (IRAG)`,
    color = "#1F77B4",
    yAxis = 0,
    tooltip = list(pointFormat = "<b>{series.name}:</b> {point.y} casos")
  ) %>%
  hc_add_series(
    name = "IRAG extendida",
    data = CASOS_POR_EDAD$`IRAG extendida`,
    color = "#9ACFD9",
    yAxis = 0,
    tooltip = list(pointFormat = "<b>{series.name}:</b> {point.y} casos")
  ) %>%
  # Línea de tasa de letalidad
  hc_add_series(
    name = "Tasa de letalidad (%)",
    data = LETALIDAD_POR_EDAD$tasa_letalidad,
    type = "line",
    color = "#FFB3B3",
    marker = list(symbol = "circle", radius = 4),
    yAxis = 1,
    tooltip = list(pointFormat = "<b>{series.name}:</b> {point.y} %")
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal",
    itemStyle = list(fontWeight = "normal", fontSize = "10px")
  ) %>%
  hc_tooltip(shared = TRUE, useHTML = TRUE) %>%
  hc_credits(
    enabled = TRUE,
    text = "Fuente: Elaboración propia en base a los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0",
    style = list(fontSize = "10px"),
    align = "left",
    verticalAlign = "bottom",
    x = 10
  ) %>%
  hc_exporting(enabled = TRUE)

# ----------------------------------------------------------------
# MOSTRAR GRÁFICO
# ----------------------------------------------------------------
print(GRAFICO_INTERACTIVO_EDAD, width = 550, height = 450)


