# ----------------------------------------------------------------
# MODIFICACION BASE DE DATOS AGRUPADA: RENOMBRAR COLUMNAS
# ----------------------------------------------------------------

# Vector con los nombres originales (como aparecen en tu dataframe)
originales <- c("0 a 2 m", "3 a 5 m", "6 a 11 m", "12 a 23 m",
                "2 a 4 años", "5 a 9 años", "10 a 14 años", "15 a 19 años",
                "20 a 24 años", "25 a 29 años", "30 a 34 años", "35 a 39 años",
                "40 a 44 años", "45 a 49 años", "50 a 54 años", "55 a 59 años",
                "60 a 64 años", "65 a 69 años", "70 a 74 años", ">= a 75 años",
                "Sin especificar")

# Vector con los nuevos nombres
nuevos <- c("edad_0_2m", "edad_3_5m", "edad_6_11m", "edad_12_23m",
            "edad_2_4a", "edad_5_9a", "edad_10_14a", "edad_15_19a",
            "edad_20_24a", "edad_25_29a", "edad_30_34a", "edad_35_39a",
            "edad_40_44a", "edad_45_49a", "edad_50_54a", "edad_55_59a",
            "edad_60_64a", "edad_65_69a", "edad_70_74a", "edad_75a_mas",
            "sin_especificar")

# Renombrar columnas
DATA_UC_IRAG_AGRUPADA <- DATA_UC_IRAG_AGRUPADA %>%
  rename_with(~ nuevos[match(., originales)], .cols = all_of(originales))

# Revisar nombres
colnames(DATA_UC_IRAG_AGRUPADA)


# ----------------------------------------------------------------
# GENERACION DE NUEVO DATAFRAME
# ----------------------------------------------------------------

#dataframe con las variables a utilizar

DATA_UC_IRAG_AGRUPADA <-DATA_UC_IRAG_AGRUPADA %>%
  select(ANIO, SEMANA, ORIGEN, NOMBREEVENTOAGRP, edad_0_2m, edad_3_5m, edad_6_11m, edad_12_23m,
         edad_2_4a, edad_5_9a, edad_10_14a, edad_15_19a,
         edad_20_24a, edad_25_29a, edad_30_34a, edad_35_39a,
         edad_40_44a, edad_45_49a, edad_50_54a, edad_55_59a,
         edad_60_64a, edad_65_69a, edad_70_74a, edad_75a_mas,
         sin_especificar
  ) %>%
  filter(
    ANIO == 2025) 


View(DATA_UC_IRAG_AGRUPADA)

# ----------------------------------------------------------------
# TRANSFORMACION DE DATAFRAME
# ----------------------------------------------------------------

#Transformar formato ancho → largo

DATA_UC_IRAG_LARGO <- DATA_UC_IRAG_AGRUPADA %>% 
  pivot_longer( cols = starts_with("edad_"), names_to = "GRUPO_EDAD", values_to = "CASOS" )%>% mutate(CASOS = as.numeric(CASOS))

#Agrupar y sumar casos por tipo de evento, año y semana 

DATA_UC_IRAG_AGREGADA<- DATA_UC_IRAG_LARGO %>% group_by(ANIO, SEMANA, NOMBREEVENTOAGRP) %>% 
  summarise(TOTAL_CASOS = sum(CASOS, na.rm = TRUE)) %>% ungroup()

#Pasar filas de eventos a columnas

DATA_UC_IRAG_LISTA <- DATA_UC_IRAG_AGREGADA %>% pivot_wider( names_from = NOMBREEVENTOAGRP, values_from = TOTAL_CASOS ) 
names(DATA_UC_IRAG_LISTA) <- tolower(names(DATA_UC_IRAG_LISTA))

#Calcular proporciones de interés

DATA_UC_IRAG_LISTA <- DATA_UC_IRAG_LISTA%>%
  mutate(
    prop_irag = (`casos de irag entre los internados` / `pacientes internados por todas las causas`) * 100,
    prop_irag_ext = (`casos de irag extendida entre los internados` / `pacientes internados por todas las causas`) * 100,
    prop_irag_uci = (`casos de irag entre los ingresados a uci` / `pacientes ingresados a uci`) * 100,
    prop_irag_ext_uci = (`casos de irag extendida entre los ingresados a uci` / `pacientes ingresados a uci`) * 100,
    prop_def_irag = (`defunciones por irag` / `defunciones totales`) * 100,
    prop_def_irag_ext = (`defunciones por irag extendida` / `defunciones totales`) * 100,
    prop_otros_internados = pmax(0, 100 - (prop_irag + prop_irag_ext)), 
    prop_otros_uci = pmax(0, 100 - (prop_irag_uci + prop_irag_ext_uci))
  )

# Asegurar orden por semana

DATA_UC_IRAG_LISTA <- DATA_UC_IRAG_LISTA %>%
  arrange(semana)

# Crear etiqueta de semana

DATA_UC_IRAG_LISTA <- DATA_UC_IRAG_LISTA %>%
  mutate(SEMANA_LABEL = as.character(semana))








