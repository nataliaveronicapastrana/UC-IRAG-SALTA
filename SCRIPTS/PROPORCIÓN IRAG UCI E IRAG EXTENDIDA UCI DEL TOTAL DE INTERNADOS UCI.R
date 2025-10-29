# ----------------------------------------------------------------
# GRAFICO: Porcentaje IRAG UCI, IRAG extendida UCI  y otros ingresos UCI
# ----------------------------------------------------------------

GRAFICO_IRAG_INTERNADOS_UCI <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(
    categories = DATA_UC_IRAG_LISTA$SEMANA_LABEL,
    title = list(text = "Semana epidemiológica"),
    labels = list(rotation = -45, style = list(fontSize = "9px")),
    tickInterval = 1
  ) %>%
  hc_yAxis(
    title = list(text = "Porcentaje (%)"),
    max = 100
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
    name = "IRAG",
    data = round(DATA_UC_IRAG_LISTA$prop_irag_uci, 2),
    color = "#1F77B4",
    index = 2
  ) %>%
  # 2️⃣ IRAG extendida (en el medio)
  hc_add_series(
    name = "IRAG extendida",
    data = round(DATA_UC_IRAG_LISTA$prop_irag_ext_uci, 2),
    color = "#9ACFD9",
    index = 1
  ) %>%
  # 3️⃣ Otros ingresos (arriba)
  hc_add_series(
    name = "Otros ingresos",
    data = round(DATA_UC_IRAG_LISTA$prop_otros_uci, 2),
    color = "#C7C7C7",
    index = 0
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  ) %>%
  hc_tooltip(shared = TRUE, valueSuffix = " %") %>%
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
GRAFICO_IRAG_INTERNADOS_UCI
