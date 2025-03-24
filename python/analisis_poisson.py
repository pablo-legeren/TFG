import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from scipy.stats import poisson

# Configuración gráfica
sns.set(style="whitegrid")

def cargar_datos(ruta_excel):
    """Carga los datos desde un archivo Excel"""
    datos = pd.read_excel(ruta_excel)
    print("Primeras filas del dataset:")
    print(datos.head())
    return datos

def graficar_distribucion_poisson(variable, nombre_variable="Goles", color="skyblue"):
    """Grafica la distribución observada y Poisson teórica de una variable de conteo"""
    media = variable.mean()
    valores = np.arange(0, variable.max() + 1)
    pmf = poisson.pmf(valores, mu=media)

    plt.figure(figsize=(8,5))
    sns.histplot(variable, bins=np.arange(0, variable.max() + 2) - 0.5,
                 stat='probability', color=color, edgecolor='black', label='Observado')
    plt.plot(valores, pmf, 'o-', color='red', label=f'Poisson teórica (λ={media:.2f})')
    plt.title(f"Distribución de {nombre_variable} vs Poisson")
    plt.xlabel(nombre_variable)
    plt.ylabel("Probabilidad")
    plt.legend()
    plt.tight_layout()
    plt.show()

def analizar_goles(datos):
    """Analiza si los goles locales y visitantes siguen una distribución de Poisson"""
    goles_local = datos["FTHG"]
    goles_visitante = datos["FTAG"]

    graficar_distribucion_poisson(goles_local, "Goles locales", color="skyblue")
    graficar_distribucion_poisson(goles_visitante, "Goles visitantes", color="salmon")

if __name__ == "__main__":
    # Ruta al archivo de datos (ajústala según tu estructura)
    ruta = "data/ultimas2temporadas.xlsx"
    datos = cargar_datos(ruta)
    analizar_goles(datos)
