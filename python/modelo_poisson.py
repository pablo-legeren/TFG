import pandas as pd

def calcular_fuerzas(datos):
    """
    Calcula los factores ofensivos y defensivos para cada equipo
    a partir de los goles marcados y encajados en casa y fuera.
    """
    # Goles promedio por partido
    media_goles_local = datos["FTHG"].mean()
    media_goles_visitante = datos["FTAG"].mean()

    # Goles anotados y recibidos en casa
    ataque_local = datos.groupby("HomeTeam")["FTHG"].mean() / media_goles_local
    defensa_local = datos.groupby("AwayTeam")["FTAG"].mean() / media_goles_visitante

    # Goles anotados y recibidos fuera
    ataque_visitante = datos.groupby("AwayTeam")["FTAG"].mean() / media_goles_visitante
    defensa_visitante = datos.groupby("HomeTeam")["FTHG"].mean() / media_goles_local

    equipos = sorted(set(datos["HomeTeam"].unique()) | set(datos["AwayTeam"].unique()))

    modelo = pd.DataFrame(index=equipos)
    modelo["ataque_local"] = ataque_local
    modelo["defensa_local"] = defensa_local
    modelo["ataque_visitante"] = ataque_visitante
    modelo["defensa_visitante"] = defensa_visitante

    return modelo.fillna(1.0)

def estimar_lambda(home_team, away_team, modelo, media_local, media_visitante):
    """
    Calcula las lambdas (goles esperados) para un partido dado un modelo de fuerzas.
    """
    lambda_home = (modelo.loc[home_team, "ataque_local"] *
                   modelo.loc[away_team, "defensa_visitante"] *
                   media_local)

    lambda_away = (modelo.loc[away_team, "ataque_visitante"] *
                   modelo.loc[home_team, "defensa_local"] *
                   media_visitante)

    return lambda_home, lambda_away

if __name__ == "__main__":
    from analisis_poisson import cargar_datos

    datos = cargar_datos("data/ultimas2temporadas.xlsx")
    modelo = calcular_fuerzas(datos)

    # Promedios generales de goles
    media_local = datos["FTHG"].mean()
    media_visitante = datos["FTAG"].mean()

    # Ejemplo: Real Madrid vs Barcelona
    equipo_casa = "Real Madrid"
    equipo_fuera = "Barcelona"
    lam_home, lam_away = estimar_lambda(equipo_casa, equipo_fuera, modelo, media_local, media_visitante)

    print(f"Goles esperados para {equipo_casa} vs {equipo_fuera}:")
    print(f"  {equipo_casa}: {lam_home:.2f} goles esperados")
    print(f"  {equipo_fuera}: {lam_away:.2f} goles esperados")
