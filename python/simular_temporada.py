import pandas as pd
import numpy as np
from scipy.stats import poisson
from modelo_poisson import calcular_fuerzas, estimar_lambda
from analisis_poisson import cargar_datos

def simular_partido(home_team, away_team, modelo, media_local, media_visitante):
    """
    Simula un único partido usando distribución de Poisson.
    Devuelve los goles simulados para local y visitante.
    """
    lambda_home, lambda_away = estimar_lambda(home_team, away_team, modelo, media_local, media_visitante)
    goles_home = poisson.rvs(mu=lambda_home)
    goles_away = poisson.rvs(mu=lambda_away)
    return goles_home, goles_away

def asignar_puntos(goles_home, goles_away):
    """Devuelve los puntos del partido: (local, visitante)"""
    if goles_home > goles_away:
        return 3, 0
    elif goles_home < goles_away:
        return 0, 3
    else:
        return 1, 1

def simular_temporada(datos, modelo, media_local, media_visitante):
    """
    Simula una temporada completa a partir del calendario real.
    Retorna una tabla de clasificación.
    """
    equipos = sorted(set(datos["HomeTeam"]) | set(datos["AwayTeam"]))
    clasificacion = pd.DataFrame(0, index=equipos,
                                 columns=["Puntos", "GF", "GC", "DG", "PJ", "Victorias"])

    for _, row in datos.iterrows():
        home = row["HomeTeam"]
        away = row["AwayTeam"]

        goles_home, goles_away = simular_partido(home, away, modelo, media_local, media_visitante)
        puntos_home, puntos_away = asignar_puntos(goles_home, goles_away)

        clasificacion.loc[home, "Puntos"] += puntos_home
        clasificacion.loc[away, "Puntos"] += puntos_away
        clasificacion.loc[home, "GF"] += goles_home
        clasificacion.loc[away, "GF"] += goles_away
        clasificacion.loc[home, "GC"] += goles_away
        clasificacion.loc[away, "GC"] += goles_home
        clasificacion.loc[home, "PJ"] += 1
        clasificacion.loc[away, "PJ"] += 1
        if puntos_home == 3:
            clasificacion.loc[home, "Victorias"] += 1
        elif puntos_away == 3:
            clasificacion.loc[away, "Victorias"] += 1

    clasificacion["DG"] = clasificacion["GF"] - clasificacion["GC"]
    clasificacion = clasificacion.sort_values(by=["Puntos", "DG", "GF"], ascending=False)
    return clasificacion.reset_index().rename(columns={"index": "Equipo"})

def simular_varias_temporadas(n, datos, modelo, media_local, media_visitante):
    """
    Simula n temporadas y devuelve cuántas veces cada equipo quedó en cada posición.
    """
    equipos = sorted(set(datos["HomeTeam"]) | set(datos["AwayTeam"]))
    posiciones = pd.DataFrame(0, index=equipos, columns=range(1, len(equipos)+1))

    for _ in range(n):
        clasif = simular_temporada(datos, modelo, media_local, media_visitante)
        for pos, equipo in enumerate(clasif["Equipo"], start=1):
            posiciones.loc[equipo, pos] += 1

    # Convertir a porcentajes
    posiciones = posiciones.div(n).multiply(100).round(2)
    return posiciones

if __name__ == "__main__":
    datos = cargar_datos("data/ultimas2temporadas.xlsx")
    modelo = calcular_fuerzas(datos)
    media_local = datos["FTHG"].mean()
    media_visitante = datos["FTAG"].mean()

    print("Simulando una temporada...")
    clasificacion = simular_temporada(datos, modelo, media_local, media_visitante)
    print(clasificacion.head(10))

    print("\nSimulando 1000 temporadas...")
    tabla_posiciones = simular_varias_temporadas(1000, datos, modelo, media_local, media_visitante)
    print(tabla_posiciones)
