import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd

def graficar_probabilidades_posicion(tabla_posiciones):
    """
    Genera un heatmap con la probabilidad (%) de que cada equipo
    termine en cada posición según las simulaciones de temporada.
    """
    plt.figure(figsize=(14, 10))
    sns.heatmap(tabla_posiciones, annot=True, fmt=".1f", cmap="YlGnBu", cbar_kws={'label': 'Probabilidad (%)'})
    plt.title("Probabilidad de quedar en cada posición (simulación Monte Carlo)")
    plt.xlabel("Posición final")
    plt.ylabel("Equipo")
    plt.tight_layout()
    plt.show()

if __name__ == "__main__":
    # Supone que ya has generado tabla_posiciones
    from simulador_temporadas import simular_varias_temporadas
    from modelo_poisson import calcular_fuerzas
    from analisis_poisson import cargar_datos

    datos = cargar_datos("data/ultimas2temporadas.xlsx")
    modelo = calcular_fuerzas(datos)
    media_local = datos["FTHG"].mean()
    media_visitante = datos["FTAG"].mean()

    print("Simulando 500 temporadas...")
    tabla_posiciones = simular_varias_temporadas(500, datos, modelo, media_local, media_visitante)

    graficar_probabilidades_posicion(tabla_posiciones)
