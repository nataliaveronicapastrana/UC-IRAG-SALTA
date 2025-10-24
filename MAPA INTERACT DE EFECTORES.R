# ------------------------------------------------------------------------
# CARGAR LISTADO DE EFECTORES
# ------------------------------------------------------------------------
LISTADO_EFECTORES <- read_excel("EFECTORES.xlsx")

# Separar coordenadas en latitud y longitud
LISTADO_EFECTORES <- LISTADO_EFECTORES %>%
  mutate(
    lat = as.numeric(sub(",.*", "", Coordenadas)),
    lon = as.numeric(sub(".*,", "", Coordenadas))
  )

# ------------------------------------------------------------------------
# CREAR MAPA BASE CON CARTODB POSITRON
MAPA_EFECTORES <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  # Fondo limpio y profesional
  setView(lng = -64.8, lat = -24.8, zoom = 7) # Centrado en Salta

# ------------------------------------------------------------------------
# AGREGAR HOSPITALES (CIRCULOS AZULES)
# ------------------------------------------------------------------------
MAPA_EFECTORES <- MAPA_EFECTORES %>%
  addCircleMarkers(
    data = LISTADO_EFECTORES %>% filter(`tipo de establecimiento` == "HOSPITAL"),
    ~lon, ~lat,
    popup = ~paste0(
      "<b>Nombre:</b> ", Nombre, "<br>",
      "<b>Código SISA:</b> ", `Código SISA EFECTOR`, "<br>",
      "<b>Nivel Complejidad:</b> ", `NIVEL COMPL`, "<br>",
      "<b>Zona Sanitaria:</b> ", `zona sanitaria`, "<br>",
      "<b>Localidad:</b> ", LOCALIDAD, "<br>",
      "<b>Establecimiento:</b> ", `tipo de establecimiento`, "<br>",
      "<b>Estrategia:</b> ", Estrategia
    ),
    color = "blue",
    radius = 5,
    fillOpacity = 0.7
  )

# ------------------------------------------------------------------------
# AGREGAR UNIDADES CENTINELA (CRUCES ROJAS)
# ------------------------------------------------------------------------
MAPA_EFECTORES <- MAPA_EFECTORES %>%
  addAwesomeMarkers(
    data = LISTADO_EFECTORES %>% filter(Estrategia == "CENTINELA"),
    ~lon, ~lat,
    popup = ~paste0(
      "<b>Nombre:</b> ", Nombre, "<br>",
      "<b>Código SISA:</b> ", `Código SISA EFECTOR`, "<br>",
      "<b>Nivel Complejidad:</b> ", `NIVEL COMPL`, "<br>",
      "<b>Zona Sanitaria:</b> ", `zona sanitaria`, "<br>",
      "<b>Localidad:</b> ", LOCALIDAD, "<br>",
      "<b>Tipo de Establecimiento:</b> ", `tipo de establecimiento`, "<br>",
      "<b>Estrategia:</b> ", Estrategia
    ),
    icon = awesomeIcons(
      icon = "plus",
      library = "fa",
      markerColor = "red"
    )
  )

# ------------------------------------------------------------------------
# LEYENDA PERSONALIZADA
# ------------------------------------------------------------------------
custom_legend <- htmltools::HTML('
<div style="background:white; padding: 10px; border-radius: 5px; 
            box-shadow: 2px 2px 6px rgba(0,0,0,0.3); font-size: 14px;">
  <b>Tipos de Establecimientos</b><br>
  <i class="fa fa-plus" style="color:red; margin-right:6px;"></i> Unidad Centinela de IRAG
</div>
')

MAPA_EFECTORES <- MAPA_EFECTORES %>%
  addControl(custom_legend, position = "bottomright")

# ------------------------------------------------------------------------
# MOSTRAR MAPA
# ------------------------------------------------------------------------

MAPA_EFECTORES










