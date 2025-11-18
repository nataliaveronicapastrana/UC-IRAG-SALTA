# ------------------------------------------------------------------------------
# IMPORTAR DE BASE DE DATOS NOMINAL Y AGRUPADA
# ------------------------------------------------------------------------------

DATA_UC_IRAG <- read_delim(here("TEMPLATES", "UC_IRAG_HSM.csv"), delim = ";",
  locale = locale(encoding = "latin1"),
  trim_ws = TRUE)

View(DATA_UC_IRAG)



DATA_UC_IRAG_AGRUPADA <- read_excel( here("TEMPLATES", "UC_IRAG_AGRUPADA_HSM.xlsx"),
  col_types = c("text", "numeric", "numeric",
    "text", "text", "text", "text",
    "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric"
  ))

View(DATA_UC_IRAG_AGRUPADA)

