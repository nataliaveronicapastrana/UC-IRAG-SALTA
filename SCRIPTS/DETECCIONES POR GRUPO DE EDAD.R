# ------------------------------------------------------------------------
# 1. CREAR VARIABLES BINARIAS PARA INFLUENZA, VSR Y COVID (IGUAL QUE ANTES)
# ------------------------------------------------------------------------

DATA_UC_LISTA_PROCESADA <- DATA_UC_LISTA %>%
  # Influenza
  mutate(detectable_influenza = case_when(
    INFLUENZA_FINAL %in% c("Influenza A (sin subtipificar)", "Influenza A H3N2",
                           "Influenza positivo-Sin Tipo", "Influenza B (sin linaje)",
                           "Influenza A H1N1") ~ 1,
    INFLUENZA_FINAL %in% c("Negativo") ~ 0,
    TRUE ~ NA_real_
  )) %>%
  # VSR
  mutate(detectable_VSR = case_when(
    VSR_FINAL %in% c("VSR", "VSR B", "VSR A") ~ 1,
    VSR_FINAL %in% c("Negativo") ~ 0,
    TRUE ~ NA_real_
  )) %>%
  # COVID
  mutate(detectable_COVID = case_when(
    COVID_19_FINAL == "Positivo" ~ 1,
    COVID_19_FINAL %in% c("Negativo") ~ 0,
    TRUE ~ NA_real_
  ))

# ------------------------------------------------------------------------
# 2. ORDENAR VARIABLE DE GRUPO ETARIO
# ------------------------------------------------------------------------

orden_edades <- c("15 a 19 Años","20 a 24 Años","25 a 29 Años",
                  "30 a 34 Años","35 a 39 Años","40 a 44 Años",
                  "45 a 49 Años","50 a 54 Años","55 a 59 Años",
                  "60 a 64 Años","65 a 69 Años","70 a 74 Años","75 y más Años")

DATA_UC_LISTA_PROCESADA$EDAD_UC_IRAG <- factor(DATA_UC_LISTA_PROCESADA$EDAD_UC_IRAG,
                                               levels = orden_edades,
                                               ordered = TRUE)

# ------------------------------------------------------------------------
# 3. AGRUPAR POR GRUPO ETARIO Y CALCULAR CASOS POR VIRUS
# ------------------------------------------------------------------------

DATA_VIRUS_EDAD <- DATA_UC_LISTA_PROCESADA %>%
  filter(!is.na(EDAD_UC_IRAG)) %>%
  group_by(EDAD_UC_IRAG) %>%
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
    .groups = "drop"
  ) %>%
  arrange(EDAD_UC_IRAG)


# ------------------------------------------------------------------------
# 4. GRAFICO INTERACTIVO DE VIRUS POR GRUPO ETARIO
# ------------------------------------------------------------------------

GRAFICO_INTERACTIVO_VIRUS_EDAD <- highchart() %>%
  hc_chart(type = "column", inverted= TRUE,  height = 400) %>%
  hc_xAxis(
    categories = as.character(DATA_VIRUS_EDAD$EDAD_UC_IRAG),
    title = list(text = "Grupo etario"),
    labels = list(rotation = -45, style = list(fontSize = "9px")),
    tickInterval = 1
  ) %>%
  hc_yAxis(
    title = list(text = "Determinaciones"),
    reversedStacks = FALSE
  ) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#000000",
    borderWidth = 0.5,
    pointPadding = 0,
    groupPadding = 0
  )) %>%
  hc_add_series(name = "Influenza", data = DATA_VIRUS_EDAD$casos_influenza, color = "#1F77B4") %>%
  hc_add_series(name = "VSR", data = DATA_VIRUS_EDAD$casos_vsr, color = "#1B9E77") %>%
  hc_add_series(name = "SARS-CoV-2", data = DATA_VIRUS_EDAD$casos_covid, color = "#9467BD") %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  ) %>%
  hc_tooltip(shared = TRUE, valueSuffix = " casos") %>%
  hc_credits(
    enabled = TRUE,
    text = "Fuente: Elaboración propia  en base a 
    los datos provenientes del Sistema Nacional de Vigilancia de la Salud SNVS 2.0",
    style = list(fontSize = "10px")
  ) %>%
  hc_exporting(enabled = TRUE)

print(GRAFICO_INTERACTIVO_VIRUS_EDAD, width = 500, height = 400)
