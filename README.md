# TFG

# Modelo de probabilidad para el análisis de resultados de Liga Española

Este proyecto implementa un modelo probabilístico basado en la distribución de Poisson para analizar y predecir resultados de la Liga Española de fútbol.

Con una precisión de **más del 85%** se logra obtener la clasificación final de una temporada así como otras estadísticas (goles a favor, goles en contra, puntos) en base al rendimiento en temporadas pasadas.

## Descripción

El modelo analiza datos históricos de partidos de la Liga Española para:

- Verificar si los goles en fútbol siguen una distribución de Poisson
- Calcular parámetros de capacidad ofensiva y defensiva para cada equipo
- Simular temporadas completas mediante el método de Monte Carlo
- Predecir resultados futuros y tablas de clasificación

## Características principales

- Análisis diferenciado para rendimiento como local y visitante
- Modelo probabilístico con base estadística sólida
- Simulación de múltiples temporadas para obtener distribuciones de probabilidad
- Capacidad de predicción para partidos futuros

## Requisitos

- R 4.0+
- Paquetes requeridos:
  - dplyr
  - engsoccerdata
  - readxl
  - vcd
  - tidyverse
  - ggplot2

## Estructura del proyecto

- `src/`: Scripts organizados por funcionalidad
- `data/`: Archivos de datos incluyendo `ultimas2temporadas.xlsx`
- `liga_espanola_analisis.Rmd`: Documento R Markdown con análisis completo
- `TFG Pablo Legerén.pdf`: Documento final .pdf donde están los resultados del estudio

## Uso

```r
# Instalar dependencias
install.packages(c("dplyr", "engsoccerdata", "readxl", "vcd", "tidyverse", "ggplot2"))

# Ejecutar análisis completo
rmarkdown::render("liga_espanola_analisis.Rmd")
```

## Metodología

El modelo se basa en la distribución de Poisson para modelar los goles marcados en cada partido. Se calcula:

1. Capacidad atacante y defensiva de cada equipo
2. Parámetros lambda para cada enfrentamiento específico
3. Simulación de 1000 temporadas completas para obtener distribuciones de probabilidad
4. Predicción ponderada utilizando datos históricos y de la temporada actual
