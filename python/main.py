from analisis_poisson import cargar_datos, analizar_goles
from modelo_poisson import calcular_fuerzas
from simulador_temporadas import simular_temporada, simular_varias_temporadas
from visualizaciones import graficar_probabilidades_posicion

def main():
    print("Cargando datos...")
    datos = cargar_datos("data/ultimas2temporadas.xlsx")

    print("Analizando distribución de goles...")
    analizar_goles(datos)

    print("Calculando fuerzas ofensivas y defensivas...")
    modelo = calcular_fuerzas(datos)
    media_local = datos["FTHG"].mean()
    media_visitante = datos["FTAG"].mean()

    print("Simulando una temporada...")
    clasificacion = simular_temporada(datos, modelo, media_local, media_visitante)
    print("\nClasificación simulada (Top 10):")
    print(clasificacion.head(10))

    print("\nSimulando 1000 temporadas...")
    tabla_posiciones = simular_varias_temporadas(1000, datos, modelo, media_local, media_visitante)

    print("Generando heatmap de probabilidades por posición...")
    graficar_probabilidades_posicion(tabla_posiciones)

if __name__ == "__main__":
    main()
