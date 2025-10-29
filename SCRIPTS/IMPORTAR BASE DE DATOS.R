# ----------------------------------------------------------------
# IMPORTAR DE BASE DE DATOS
# ----------------------------------------------------------------

DATA_UC_IRAG <- read_delim("C:/Users/Natal/Documents/ANALISIS DE DATOS UC IRAG HSM/UC-IRAG-SALTA/TEMPLATES/UC_IRAG_HSM.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                          trim_ws = TRUE) 
View(DATA_UC_IRAG)



DATA_UC_IRAG_AGRUPADA<- read_excel("~/ANALISIS DE DATOS UC IRAG HSM/UC-IRAG-SALTA/TEMPLATES/UC_IRAG_AGRUPADA_HSM.xlsx", 
                                   col_types = c("text", "numeric", "numeric", 
                                                 "text", "text", "text", "text", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric"))
View(DATA_UC_IRAG_AGRUPADA)

