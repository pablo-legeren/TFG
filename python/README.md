# Versión en Python del modelo de probabilidad para resultados de La Liga

Este directorio contiene una reimplementación en Python del modelo original en R desarrollado para analizar y predecir resultados de la Liga Española de fútbol usando un enfoque probabilístico basado en la distribución de Poisson.

**Estado del proyecto:** En desarrollo y revisión. La lógica estadística ha sido replicada paso a paso, y actualmente se está validando la equivalencia con la versión original en R.

## Estructura del modelo

El flujo principal del modelo incluye:

1. Carga y exploración de datos (`analisis_poisson.py`)  
2. Cálculo de fuerzas ofensivas y defensivas por equipo (`modelo_poisson.py`)  
3. Simulación de temporadas completas (`simulador_temporadas.py`)  
4. Visualización de probabilidades de clasificación (`visualizaciones.py`)  
5. Análisis histórico opcional con datos de engsoccerdata (`historico_engsoccerdata.py`)  
6. Ejecución global (`main.py`)  

## Requisitos

Para replicar el proyecto, se recomienda usar un entorno virtual.

### 1. Crear entorno virtual (opcional pero recomendado)

```bash
python -m venv env
env\Scripts\activate
```

Con el entorno virtual activado, instalar las dependencias:

```bash
pip install -r requirements.txt
```

## Ejecución

Para ejecutar el flujo completo de análisis, simulación y visualización:

``` bash
python main.py
```

Esto realizará los siguientes pasos:

- Carga de datos y análisis de distribución de goles  
- Cálculo de parámetros ofensivos y defensivos  
- Simulación de una temporada  
- Simulación de 1000 temporadas con Monte Carlo  
- Generación de un heatmap de probabilidades por posición  