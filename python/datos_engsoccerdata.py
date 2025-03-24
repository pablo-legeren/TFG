import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def cargar_datos_spain_engsoccerdata(url=None):
    """
    Carga datos históricos de la liga española desde engsoccerdata (GitHub).
    """
    if url is None:
        url = "https://raw.githubusercontent.com/jalapic/engsoccerdata/master/data-raw/spain.csv"
    df = pd.read_csv(url)
    return df

def filtrar_temporadas_recientes(df, desde=2018):
    """
    Filtra el DataFrame para conservar solo las temporadas a partir de un año específico.
    """
    return df[df["Season"] >= desde].copy()

def analizar_media_goles_por_decada(df):
    """
    Agrupa partidos por década y calcula la media de goles por partido.
    """
    df["Decada"] = (df["Season"] // 10) * 10
    df["TotalGoles"] = df["hgoal"] + df["vgoal"]
    resumen = df.groupby("Decada")["TotalGoles"].mean().reset_index()
    resumen.columns = ["Década", "Goles promedio por partido"]

    plt.figure(figsize=(8,5))
    sns.barplot(x="Década", y="Goles promedio por partido", data=resumen, palette="crest")
    plt.title("Promedio de goles por partido en La Liga por década")
    plt.ylabel("Goles por partido")
    plt.xlabel("Década")
    plt.tight_layout()
    plt.show()

    return resumen

if __name__ == "__main__":
    print("Cargando datos históricos de La Liga desde engsoccerdata...")
    df = cargar_datos_spain_engsoccerdata()

    print("Filtrando temporadas desde 2018...")
    recientes = filtrar_temporadas_recientes(df)
    print(recientes[["Season", "home", "visitor", "hgoal", "vgoal"]].head())

    print("Analizando goles por partido por década...")
    resumen = analizar_media_goles_por_decada(df)
    print(resumen)
