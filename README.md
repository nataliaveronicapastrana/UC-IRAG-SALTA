# UNIDAD CENTINELA DE INFECCIÓN RESPIRATORIA AGUDA GRAVE (UC-IRAG)
## Autores: Lic. Natalia Pastrana-Lic. Cinthya Villagomez
## Introducción: 
### La Estrategia de Vigilancia de Infecciones Respiratorias Agudas en Argentina se estructura a partir de componentes de vigilancia centinela, vigilancia universal y redes de establecimientos que operan de manera complementaria y coordinada. Su propósito es consolidar un sistema integrado que permita generar información oportuna y de calidad para la toma de decisiones sanitarias.

### En este marco, el análisis presentado tiene como objetivo producir información técnica basada en los registros del Sistema Nacional de Vigilancia de la Salud (SNVS 2.0), específicamente en el contexto de la estrategia de Unidades Centinela de Infecciones Respiratorias Agudas Graves (UC-IRAG). En la provincia de Salta, esta estrategia es implementada por los equipos de salud del Hospital Señor del Milagro (ciudad de Salta) y del Hospital San Vicente de Paul (ciudad de Orán).

### Con el fin de facilitar la elaboración del reporte automatizado “Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)” para cada unidad centinela, se desarrolló la presente caja de herramientas denominada UC-IRAG-SALTA, que reúne el repositorio y los archivos necesarios para la obtención del informe.

### La incorporación de herramientas automatizadas resulta fundamental para procesar, analizar y visualizar los datos de manera oportuna, estandarizada y reproducible, fortaleciendo así la vigilancia epidemiológica y respaldando la toma de decisiones basada en evidencia.

## Objetivos:
### Describir el perfil clínico, epidemiológico y de diagnóstico etiológico de las infecciones respiratorias agudas graves.

## Propósito: La caja de herramientas UC-IRAG-SALTA fue desarrollada con el propósito de proporcionar un marco metodológico estandarizado y reproducible que pueda ser utilizado tanto por las Unidades Centinela de la provincia para generar informes periódicos de manera oportuna, regular y consistente.

## Destinatarios: Unidad centinela de IRAG Hospital Señor del Milagro- Unidad centinela de IRAG Hospital San Vicente de Paul

## Contenido de la caja de herramientas: 
### Carpeta "DOCUMENTACION ADICIONAL": Contiene el Plan de Análisis UC-IRAG y la Guía Operativa UC-IRAG.
### Carpeta "SCRIPTS": Contiene los archivos .R necesarios para la ejecución del reporte automatizado.
### Carpeta "TEMPLATES": Contiene:
### • Carpeta "CSS": incluye el archivo estilo.css y los logos institucionales.
### • Archivos .xlsx con los insumos necesarios para el análisis (por ejemplo, base para mapas).
### Archivos de “ANALISIS SITUACION IRAG MILAGRO”: Incluye los archivos del reporte en formato .html y .qmd.
### Archivo "README.md": Incluye la descripción del proyecto, los objetivos, los requisitos, instrucciones básicas de uso y la explicación de la estructura del repositorio.
### Archivo "UC-IRAG-SALTA.Rproj": Proyecto de R.
### Archivo "encabezado_documento.html": Encabezado utilizado para la presentación del reporte.

## Productos esperados: Reporte automatizado “Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)”
## Descarga de la caja de herramientas:
### Opción 1: Descargar sin usar GitHub (recomendado para usuarios sin cuenta)
###   1.Ingresar al repositorio: https://github.com/nataliaveronicapastrana/UC-IRAG-SALTA
###   2.Hacer clic en el botón Code (verde).
###   3.Seleccionar Download ZIP.
###   4.Descomprimir el archivo en la computadora.
###   5.Esto permite acceder a todos los archivos, pero no permite hacer commit ni sincronizar cambios.

### Opción 2: Acceder con cuenta de GitHub
###   1.Ingresar a https://github.com y realizar login.
###   2.Buscar el repositorio: UC-IRAG-SALTA o acceder directamente: https://github.com/nataliaveronicapastrana/UC-IRAG-SALTA
### Si tiene permisos de colaborador, podrá clonar, crear ramas y enviar pull requests.

### Opción 3: Clonar el repositorio en RStudio (recomendado para desarrollo)
###   1.Abrir RStudio.
###   2.Ir a: File → New Project → Version Control → Git
###   3.Pegar la URL del repositorio: https://github.com/nataliaveronicapastrana/UC-IRAG-SALTA.git
###   4.Seleccionar la carpeta local donde se guardará el proyecto.
### RStudio creará el proyecto y habilitará la pestaña Git para realizar pull, push, commit y gestionar versiones.

## Uso de la caja: Para la obtención del reporte automatizado se requiere realizar en primera instancia y por única vez la instalación de los siguientes paquetes en caso de que no se encuentren instalados: dplyr; ggplot2; lubridate; stringr; readxl; readr; writexl; highcharter; tidyr; tidyverse; mapview; leaflet; geoAr; gt; htmltools; devtools; ComplexUpset; here. Posteriormente se deberán renderizar el archivo .qmd

## Anexo: 
## VIGILANCIA CENTINELA DE INFECCION RESPIRATORIA AGUDA GRAVE (IRAG) - GUIA OPERATIVA 2024 - DIRECCIÓN DE EPIDEMIOLOGÍA. DISPONIBLE EN: https://www.argentina.gob.ar/sites/default/files/guia-uc-irag-vff.pdf


<span style="font-size:22px; color:#103A5C; font-weight:bold;">
UNIDAD CENTINELA DE INFECCIÓN RESPIRATORIA AGUDA GRAVE (UC-IRAG)
</span>

<span style="font-size:14px; color:#555;">
<strong>Autores:</strong> Lic. Natalia Pastrana – Lic. Cinthya Villagomez
</span>

---

<p style="text-align: justify;">
La Estrategia de Vigilancia de Infecciones Respiratorias Agudas en Argentina se estructura a partir de componentes de vigilancia centinela, vigilancia universal y redes de establecimientos que operan de manera complementaria y coordinada. Su propósito es consolidar un sistema integrado que permita generar información oportuna y de calidad para la toma de decisiones sanitarias.
</p>

<p style="text-align: justify;">
En este marco, el análisis presentado tiene como objetivo producir información técnica basada en los registros del Sistema Nacional de Vigilancia de la Salud (SNVS 2.0), específicamente en el contexto de la estrategia de Unidades Centinela de Infecciones Respiratorias Agudas Graves (UC-IRAG). En la provincia de Salta, dicha estrategia es implementada por los equipos de salud del Hospital Señor del Milagro (Salta capital) y del Hospital San Vicente de Paul (Orán).
</p>

<p style="text-align: justify;">
Con el fin de facilitar la elaboración del reporte automatizado “Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)” para cada unidad centinela, se desarrolló la presente caja de herramientas UC-IRAG-SALTA, que reúne el repositorio y los archivos necesarios para la obtención del informe.
</p>

<p style="text-align: justify;">
La incorporación de herramientas automatizadas resulta fundamental para procesar, analizar y visualizar los datos de manera oportuna, estandarizada y reproducible, fortaleciendo así la vigilancia epidemiológica y respaldando la toma de decisiones basada en evidencia.
</p>

---

<span style="font-size:18px; color:#0A5275;"><strong>Objetivos</strong></span>

<p style="text-align: justify;">
Describir el perfil clínico, epidemiológico y de diagnóstico etiológico de las infecciones respiratorias agudas graves.
</p>

---

<span style="font-size:18px; color:#0A5275;"><strong>Propósito</strong></span>

<p style="text-align: justify;">
La caja de herramientas UC-IRAG-SALTA fue desarrollada con el propósito de proporcionar un marco metodológico estandarizado y reproducible que pueda ser utilizado por las Unidades Centinela de la provincia para generar informes periódicos de manera oportuna, regular y consistente.
</p>

---

<span style="font-size:18px; color:#0A5275;"><strong>Destinatarios</strong></span>

- Unidad Centinela IRAG – Hospital Señor del Milagro  
- Unidad Centinela IRAG – Hospital San Vicente de Paul  

---

<span style="font-size:18px; color:#0A5275;"><strong>Contenido de la caja de herramientas</strong></span>

- **DOCUMENTACION ADICIONAL**: Plan de Análisis UC-IRAG y Guía Operativa UC-IRAG.  
- **SCRIPTS**: Archivos `.R` necesarios para el reporte automatizado.  
- **TEMPLATES**  
  - Carpeta **CSS** con `estilo.css` y logos institucionales.  
  - Archivos `.xlsx` con insumos (por ejemplo, base para mapas).  
- **ANALISIS SITUACION IRAG MILAGRO**: archivos `.html` y `.qmd`.  
- **README.md**: Descripción del proyecto, estructura, instrucciones y objetivos.  
- **UC-IRAG-SALTA.Rproj**: Proyecto de R.  
- **encabezado_documento.html**: Encabezado del reporte.

---

<span style="font-size:18px; color:#0A5275;"><strong>Productos esperados</strong></span>

<p style="text-align: justify;">
Reporte automatizado “Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)”.
</p>

---

<span style="font-size:18px; color:#0A5275;"><strong>Descarga de la caja de herramientas</strong></span>

### Opción 1: Descargar sin usar GitHub (usuarios sin cuenta)
1. Ingresar al repositorio:  
   https://github.com/nataliaveronicapastrana/UC-IRAG-SALTA  
2. Hacer clic en **Code (verde)**.  
3. Seleccionar **Download ZIP**.  
4. Descomprimir.  
5. Acceder a todos los archivos.  
*(No permite commits ni sincronización de cambios.)*

### Opción 2: Acceder con cuenta GitHub
- Ingresar a https://github.com  
- Buscar **UC-IRAG-SALTA** o acceder al enlace directo  
- Si tiene permisos, podrá: clonar, crear ramas y enviar *pull requests*

### Opción 3: Clonar desde RStudio (recomendado para desarrollo)


