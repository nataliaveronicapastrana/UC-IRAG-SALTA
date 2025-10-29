# ----------------------------------------------------------------
# GRAFICO: Defunciones por IRAG, IRAG extendida y otras causas
# ----------------------------------------------------------------

GRAFICO_IRAG_DEFUNCIONES <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(
    categories = DATA_UC_IRAG_LISTA$SEMANA_LABEL,
    title = list(text = "Semana epidemiológica"),
    labels = list(rotation = -45, style = list(fontSize = "9px")),
    tickInterval = 1
  ) %>%
  hc_yAxis(
    title = list(text = "Número de casos"),
    min = 0
  ) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,
    groupPadding = 0.07
  )) %>%
  # 1️⃣ IRAG (abajo)
  hc_add_series(
    name = "Defunciones por IRAG",
    data = DATA_UC_IRAG_LISTA$`defunciones por irag`,
    color = "#1F77B4",
    index = 2
  ) %>%
  # 2️⃣ IRAG extendida (en el medio)
  hc_add_series(
    name = "Defunciones por IRAG extendida",
    data = DATA_UC_IRAG_LISTA$`defunciones por irag extendida`,
    color = "#9ACFD9",
    index = 1
  ) %>%
  # 3️⃣ Otras defunciones (arriba)
  hc_add_series(
    name = "Defunciones por otras causas",
    data = DATA_UC_IRAG_LISTA$`defunciones totales`,
    color = "#C7C7C7",
    index = 0
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  ) %>%
  hc_tooltip(shared = TRUE, valueSuffix = " casos") %>%
  hc_credits(
    enabled = TRUE,
    text = "Fuente: Elaboración propia en base a datos del Sistema Nacional de Vigilancia de la Salud (SNVS 2.0)",
    style = list(fontSize = "10px"),
    align = "left",
    verticalAlign = "bottom",
    x = 10
  ) %>%
  hc_exporting(enabled = TRUE)

# ----------------------------------------------------------------
# MOSTRAR EL GRAFICO
# ----------------------------------------------------------------
GRAFICO_IRAG_DEFUNCIONES
