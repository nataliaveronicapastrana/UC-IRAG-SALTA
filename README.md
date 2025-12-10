
<ins>**UNIDAD CENTINELA DE INFECCIÓN RESPIRATORIA AGUDA GRAVE (UC-IRAG)**</ins>


---

<span style="font-size:14px; color:#555;">
<strong>Autores:</strong> Lic. Natalia Pastrana – Lic. Cynthia Villagomez
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

<span style="font-size:18px; color:#0A5275;"><strong>Objetivo</strong></span>

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
  - Archivos `.xlsx` con insumos (por ejemplo, base de datos para mapas).  
- **ARCHIVOS** `.html` y `.qmd`.  
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

### <span style="font-size:18px; color:#0A5275;"><strong> Descargar sin usar GitHub (usuarios sin cuenta)</strong></span>
1. Ingresar al repositorio: https://github.com/nataliaveronicapastrana/UC-IRAG-SALTA  
2. Hacer clic en **Code (verde)**.  
3. Seleccionar **Download ZIP**.  
4. Descomprimir.  
5. Acceder a todos los archivos.  
*(No permite commits ni sincronización de cambios.)*

---

<span style="font-size:18px; color:#0A5275;"><strong>Uso de la caja</strong></span>

<p style="text-align: justify;">
Para obtener el reporte automatizado se requiere instalar por única vez los siguientes paquetes en caso de no estar instalados:
</p>

`dplyr`, `ggplot2`, `lubridate`, `stringr`, `readxl`, `readr`,  
`writexl`, `highcharter`, `tidyr`, `tidyverse`, `mapview`,  
`leaflet`, `geoAr`, `gt`, `htmltools`, `devtools`,  
`ComplexUpset`, `here`.

<p style="text-align: justify;">
Posteriormente, deberá agregar en la carpeta TEMPLATES la base de datos correspondiente y finalmente renderizarse el archivo <strong>.qmd</strong>.
</p>

---

<span style="font-size:18px; color:#0A5275;"><strong>Anexo</strong></span>

<p style="text-align: justify;">
<strong>Vigilancia Centinela de Infección Respiratoria Aguda Grave (IRAG) – Guía Operativa 2024</strong>, Dirección de Epidemiología.  
Disponible en:  
https://www.argentina.gob.ar/sites/default/files/guia-uc-irag-vff.pdf
</p>

