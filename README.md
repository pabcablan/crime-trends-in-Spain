# Tendencias Delictivas en España
![R Version](https://img.shields.io/badge/R-%3E%3D4.3.1-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Status](https://img.shields.io/badge/status-finished-success)

## 📖 Descripción general
Este proyecto analiza la evolución temporal de los delitos en España durante el período 2013-2022, estudiando la relación entre el nivel de renta y paro con la incidencia de delitos ajustados por población. Proporciona herramientas visuales y analíticas que permitan explorar y comunicar los resultados de forma clara.


## 📌 Antecedentes y propósito

El vínculo entre *factores socioeconómicos* y criminalidad ha sido objeto de debate académico y político durante décadas. Diversos estudios han señalado que la *pobreza, el desempleo y la desigualdad en la distribución de la renta* pueden influir en la incidencia de delitos de distinta naturaleza.  

En el caso español, la **renta per cápita** y el **nivel educativo** aparecen como variables clave en la explicación de las tasas delictivas. Al mismo tiempo, se observan diferencias notables entre comunidades autónomas, tanto en la frecuencia como en el tipo de delitos.

El proyecto se centra en los años 2013-2022, un período de cambios económicos y sociales importantes (incluida la pandemia), con el objetivo de:

- Explorar cómo las diferencias en renta y desempleo se relacionan con la evolución de la criminalidad en las comunidades autónomas.  
- Analizar la distribución de distintos tipos de delitos y su evolución temporal.  
- Aportar evidencias que ayuden a *diseñar políticas públicas más efectivas y equitativas*.  
- Ofrecer herramientas visuales y analíticas que faciliten la interpretación de los resultados tanto para investigadores como para responsables de la toma de decisiones.  


## 📊 Fuentes de datos

Este proyecto se basa en *fuentes oficiales y de acceso abierto*, lo que garantiza la fiabilidad y consistencia de la información:

- **Instituto Nacional de Estadística (INE):** población por comunidades autónomas, renta neta de los hogares y tasas de paro.  
- **Ministerio del Interior (MIR):** estadísticas de criminalidad con series anuales de delitos registrados por las fuerzas de seguridad.  

El rango temporal de ambos conjuntos es *2013-2022*, permitiendo un análisis comparativo homogéneo entre indicadores socioeconómicos y criminalidad.


## 🗂️ Organización de los datos

Los datos utilizados en este proyecto fueron recopilados, procesados y normalizados para permitir **comparaciones entre comunidades autónomas** y análisis temporal. Incluyen información sobre criminalidad, población, renta y desempleo.  

- Se creó una **tabla inicial en valores absolutos** para análisis exploratorio y limpieza de datos.  
- Posteriormente se elaboró una **tabla final ajustada por cada 100.000 habitantes**, unificando todas las variables.  
- La frecuencia de los datos es **anual**, lo que permite un análisis homogéneo tanto temporal como territorial.

<details>
<summary> 📋 Ver detalles de las variables </summary>

| Variable        | Tipo        | Descripción |
|-----------------|------------|-------------|
| **Anyo**        | Numérica   | Año de referencia (2013-2022). |
| **Tot_Del**     | Numérica   | Tasa de delitos totales registrados anualmente por CCAA. |
| **Cntr_Pat**    | Numérica   | Tasa de delitos contra el Patrimonio. |
| **Cntr_Per**    | Numérica   | Tasa de delitos contra las Personas. |
| **Cntr_Lib**    | Numérica   | Tasa de delitos contra la Libertad (incluye delitos sexuales). |
| **Cntr_Segcol** | Numérica   | Tasa de delitos contra la Seguridad Colectiva. |
| **Rest_Del**    | Numérica   | Otros delitos (Administración de Justicia, Orden Público, falsedades, etc.). |
| **Pob**         | Numérica   | Número de habitantes por Comunidad Autónoma. |
| **Rent_Net_Mh** | Numérica   | Renta neta media por hogar (€ anuales). |
| **Tasa_Paro**   | Numérica   | Porcentaje de población activa desempleada. |
| **CCAA**        | Categórica | Nombre de cada Comunidad Autónoma (limpio y sin duplicidades). |
| **Rent_Net_Cat**| Categórica | Clasificación de renta en Alta, Media o Baja según PIB per cápita relativo a la media nacional. |
| **Tasa_Paro_Cat**| Categórica| Clasificación de paro en Alta, Media o Baja usando media y desviación estándar. |

</details>


## 📂 Estructura del Repositorio

El repositorio contiene los siguientes archivos y recursos organizados por tipo:

| Archivo | Tipo | Descripción |
|---------|------|-------------|
| [.Rhistory](.Rhistory) | ⏳ Historial | Historial de la sesión de R. |
| [001Delitos_CCAA_2013_2022.xlsx](001Delitos_CCAA_2013_2022.xlsx) | 📊 Datos | Delitos por CCAA (2013-2022) en valores absolutos. |
| [002PoblacionCenso2013_2022.xlsx](002PoblacionCenso2013_2022.xlsx) | 📊 Datos | Población por CCAA, series 2013-2022. |
| [003renta_Media_CCAA.xlsx](003renta_Media_CCAA.xlsx) | 📊 Datos | Renta media por hogar y CCAA. |
| [004Tasa_Paro_CCAA.xlsx](004Tasa_Paro_CCAA.xlsx) | 📊 Datos | Tasas de paro por CCAA. |
| [CuadroDeMandos.Rmd](CuadroDeMandos.Rmd) | 💻 Código | Dashboard interactivo (Shiny + flexboard). |
| [LICENSE](LICENSE) | 📄 Documento | Archivo de licencia del proyecto. |
| [README.md](README.md) | 📄 Documento | Descripción completa del proyecto. |
| [islas_20170101.json](islas_20170101.json) | 🗺️ Geodatos | Archivo GeoJSON con límites de CCAA/islas. |
| [proyecto_semestral_delitos_Pablo_Cabeza.Rmd](proyecto_semestral_delitos_Pablo_Cabeza.Rmd) | 💻 Código | Script principal del proyecto en R Markdown. |
| [proyecto_semestral_delitos_Pablo_Cabeza.html](proyecto_semestral_delitos_Pablo_Cabeza.html) | 🌐 HTML | Versión renderizada del R Markdown principal. |
| [tabla_final.xlsx](tabla_final.xlsx) | 📊 Datos | Base de datos final normalizada. |
| [tabla_final_mapa.xlsx](tabla_final_mapa.xlsx) | 📊 Datos | Base de datos preparada para visualizaciones en mapa. |
| [tabla_final_miles.xlsx](tabla_final_miles.xlsx) | 📊 Datos | Base con valores ajustados por miles de habitantes. |
| [tabla_final_miles_mapa.xlsx](tabla_final_miles_miles_mapa.xlsx) | 📊 Datos | Base para mapas con valores ajustados. |
| [utilidades.R](utilidades.R) | 💻 Código | Funciones auxiliares para el proyecto. |


## ⚙️ Requisitos

- **R:** versión ≥ 4.3.1
- **Paquetes de R:** openxlsx, dplyr, ggplot2, scales, tidyr, qcc, RColorBrewer, lubridate, plotly, kableExtra, geojsonio, leaflet, fpp3, tidyverse, highcharter, shinydashboard, datasets, MASS, sp

  > La mayoría de estos paquetes se instalarán automáticamente al ejecutar los R Markdown del proyecto si no están presentes.

- **Archivos necesarios:** `utilidades.R` en el mismo directorio que el proyecto
- **Conexión a Internet:** solo necesaria si algún paquete debe descargarse


## 🚀 Instalación

Para clonar este repositorio en tu máquina local, ejecuta:

```bash
git clone https://github.com/pabcablan/crime-trends-in-Spain.git
cd crime-trends-in-Spain
```


## ▶️ Ejecución

Existen dos formas principales de ejecutar el proyecto según el recurso que quieras visualizar:

### 📝 Memoria
- **Opción recomendada:** abrir directamente el archivo HTML generado:  
  [proyecto_SEMESTRAL_DELITOS_PABLO_CABEZA.html](proyecto_SEMESTRAL_DELITOS_PABLO_CABEZA.html) en cualquier navegador.  
- **Opción alternativa:** abrir el archivo fuente en RStudio:  
  [proyecto_SEMESTRAL_DELITOS_PABLO_CABEZA.Rmd](proyecto_SEMESTRAL_DELITOS_PABLO_CABEZA.Rmd) y compilarlo manualmente para generar el `.html`.

### 📊 Cuadro de Mandos
- Siempre debe ejecutarse desde RStudio.  
- Abrir el archivo: [CuadroDeMandos.Rmd](CuadroDeMandos.Rmd) y ejecutar.  
- No existe versión precompilada, ya que requiere entorno interactivo.

 
## 📊 Cuadro de Mandos

Herramienta interactiva desarrollada en **Shiny** y diseñada con **flexboard**, que permite explorar la relación entre criminalidad y factores socioeconómicos en España.

Se organiza en cinco secciones principales:  

- **🏠 Inicio:** mapa coroplético interactivo.  
- **📈 Tendencias Nacionales:** evolución temporal de los delitos (líneas y mapas de calor).  
- **🌍 Análisis por Región:** desglose por comunidades autónomas con tablas interactivas.  
- **📊 Análisis de Atributos:** correlación entre indicadores delictivos y socioeconómicos.  
- **⚖️ Comparaciones de Atributos:** diagramas de dispersión con regresiones lineales.


### 📷 Vista previa

![](https://i.imgur.com/LuYSsZv.gif)


## 📄 Licencia

Este proyecto está licenciado bajo la licencia MIT. Consulta el archivo [LICENSE](LICENSE) para obtener más detalles.
