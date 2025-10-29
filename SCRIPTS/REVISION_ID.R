#-----------------------------------------------------------------------------------
#                          🛑 IDENTIFIACIÓN DE ID´S A REVISAR 
#-----------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#                          1-CASOS DE GRUPO ETARIO FUERA DE LA COBERTURA DEL ESTABLECIMIENTO 
#-------------------------------------------------------------------------------------------


revisar_edades <- function(data,variable) {
  
  data_revision <- data %>%
    mutate(edad_min = as.numeric(str_extract(.data[[variable]], "\\d+"))) %>%
    filter(edad_min < 15)
  
  return(data_revision)
}

resultado_edades <- revisar_edades(base, "EDAD_UC_IRAG")

id_revision <- unique(resultado_edades$ID)

mensaje_revision_1 <- paste("Se realizó una exploración de la base.\n 
                            Los siguientes ID:",paste (id_revision, collapse = ","),"no coinciden con la edad de cobertura del hospital.
                            Revisar individualmente estos casos.")

cat(mensaje_revision_1)

writeLines(mensaje_revision_1, "mensaje_revision.txt") 


#-------------------------------------------------------------------------------------------
#                          2-CASOS DE GRUPO ETARIO FUERA DE LA COBERTURA DEL ESTABLECIMIENTO 
#-------------------------------------------------------------------------------------------

establecimiento_internacion<- "HOSPITAL SEÑOR DEL MILAGRO"

revisar_establecimiento <- function(data,variable) {
  
  data_establecimiento<-data %>% filter(ESTABLECIMIENTO_INTERNACION != establecimiento_internacion)
  
  return(data_establecimiento)
}

resultado_establecimiento <- revisar_establecimiento (base,"ESTABLECIMIENTO_INTERNACION")

id_chequeo<- unique(resultado_establecimiento$ID)

mensaje_revision_2 <- paste("Se revisaron los establecimientos de carga, los siguientes ID", paste(id_chequeo, collapse = ","), "no corresponden a la UC-IRAG.\n
                            Revisar individualmente estos ID.")

cat(mensaje_revision_2)



#-------------------------------------------------------------------------------------------
#                          3-#EXPORTACION DE MENSAJES CON REVISIONES
#-------------------------------------------------------------------------------------------


#Fecha del día en el que se hizo el anáilisis
fecha<- format(Sys.Date(),"%d-%m-%Y")

#Nombre del archivo
archivo <- paste0("Salidas/Consideraciones_", fecha, ".txt")

# Si el archivo ya existe, lo elimina para comenzar limpio
if (file.exists(archivo)) {
  file.remove(archivo)
}

# Función para agregar mensajes al archivo
guardar_mensaje <- function(mensaje) {
  write(paste0(mensaje, "\n\n"), file = archivo, append = TRUE)
}

# Ejemplo de mensajes
mensaje <- c(mensaje_revision_1,
             mensaje_revision_2)

# Itera por los mensajes y los guarda en el archivo
for (mensaje in mensaje) {
  guardar_mensaje(mensaje)
}
