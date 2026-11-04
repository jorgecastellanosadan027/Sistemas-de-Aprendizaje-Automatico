

# ============================================================
# PRACTICA 01 - ANALISIS EXPLORATORIO DE DATOS (EDA)
# Conjuntos: Wine Quality y Palmer Penguins
# Herramienta: Python / Spyder
# ============================================================
from pathlib import Path
from sqlalchemy import create_engine, text
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# ============================================================
# 1. CONFIGURACION DE RUTAS
# ============================================================
ROOT = Path.home() / "curso_ia" / "datos"
WINE_RED = ROOT / "raw/wine/winequality-red.csv"
WINE_WHITE = ROOT / "raw/wine/winequality-white.csv"
PENGUINS_FILE = ROOT / "raw/penguins/penguins.csv"
FIGURES = ROOT / "figures/python"
REPORTS = ROOT / "docs"

USER = "root"
PASSWORD = "root"
HOST = "localhost"
PORT = 3306
DATABASE = "practica01"

FIGURES.mkdir(parents=True, exist_ok=True)
REPORTS.mkdir(parents=True, exist_ok=True)
sns.set_theme(style="whitegrid")

plt.rcParams["figure.figsize"] = (10, 6)
plt.rcParams["font.size"] = 10

print("\n============================================")
print("PRACTICA 01 - ANALISIS EXPLORATORIO DE DATOS")
print("============================================")
print("\nDirectorio principal:", ROOT)
archivos = {
    "Wine tinto": WINE_RED,
    "Wine blanco": WINE_WHITE,
    "Penguins": PENGUINS_FILE
}
print("\nCOMPROBACION DE ARCHIVOS:")

for nombre, archivo in archivos.items():
    estado = "OK" if archivo.exists() else "NO ENCONTRADO"
    print(f"{nombre}: {estado}")
    if not archivo.exists():
        raise FileNotFoundError(f"No se encuentra el archivo: {archivo}")
        
# ============================================================
# 2. CARGA DE LOS DATOS
# ============================================================
def cargar_datos():
    print("\n========== CARGA DE DATOS ==========")
    red = pd.read_csv(WINE_RED, sep=";")
    white = pd.read_csv(WINE_WHITE, sep=";")
    red["wine_type"] = "red"
    white["wine_type"] = "white"
    wine = pd.concat([red, white], ignore_index=True)
    penguins = pd.read_csv(PENGUINS_FILE)
    print("\nWine tinto:", red.shape)
    print("Wine blanco:", white.shape)
    print("Wine conjunto:", wine.shape)
    print("Penguins:", penguins.shape)
    return red, white, wine, penguins

# ============================================================
# 3. INSPECCION GENERAL DE LOS DATOS
# ============================================================
def inspeccion_general(nombre, df):
    print("\n" + "=" * 70)
    print("CONJUNTO DE DATOS:", nombre)
    print("=" * 70)
    print("\n1. DIMENSIONES")
    print("Filas:", df.shape[0])
    print("Columnas:", df.shape[1])
    print("\n2. PRIMERAS 5 FILAS")
    print(df.head())
    print("\n3. ULTIMAS 5 FILAS")
    print(df.tail())
    print("\n4. NOMBRES DE LAS VARIABLES")
    print(df.columns.tolist())
    print("\n5. TIPOS DE DATOS")
    print(df.dtypes)
    print("\n6. INFORMACION GENERAL")
    df.info()
    print("\n7. ESTADISTICA DESCRIPTIVA")
    print(df.describe(include="all").T)
    print("\n8. NUMERO DE VALORES DISTINTOS")
    print(df.nunique())
    df.describe(include="all").T.to_csv(REPORTS / f"descriptiva_{nombre}.csv")
    
# ============================================================
# 4. ANALISIS DE CALIDAD DE LOS DATOS
# ============================================================
def calidad_datos(nombre, df):
    print("\n" + "=" * 70)
    print("CALIDAD DE LOS DATOS:", nombre)
    print("=" * 70)
    print("\n1. VALORES AUSENTES")
    ausentes = df.isna().sum()
    print(ausentes)
    print("\n2. PORCENTAJE DE VALORES AUSENTES")
    porcentaje = (df.isna().mean() * 100).round(2)
    print(porcentaje)
    print("\n3. REGISTROS DUPLICADOS")
    duplicados = df.duplicated().sum()
    print("Duplicados:", duplicados)
    print("\n4. REGISTROS COMPLETOS")
    completos = df.notna().all(axis=1).sum()
    print("Registros completos:", completos)
    print("\n5. TOTAL DE REGISTROS")
    print(len(df))
    informe = pd.DataFrame({
        "variable": df.columns,
        "valores_ausentes": df.isna().sum().values,
        "porcentaje_ausentes": porcentaje.values,
        "valores_distintos": df.nunique().values
    })
    informe.to_csv(REPORTS / f"calidad_{nombre}.csv", index=False)
    plt.figure(figsize=(12, 5))
    sns.barplot(data=informe, x="variable", y="porcentaje_ausentes")
    plt.xticks(rotation=45, ha="right")
    plt.ylabel("Porcentaje de valores ausentes")
    plt.title(f"Valores ausentes - {nombre}")
    plt.tight_layout()
    plt.savefig(FIGURES / f"ausentes_{nombre}.png", dpi=300)
    plt.close()
    
# ============================================================
# 5. DETECCION DE VALORES ATIPICOS MEDIANTE IQR
# ============================================================
def detectar_outliers(nombre, df):
    print("\n" + "=" * 70)
    print("DETECCION DE VALORES ATIPICOS:", nombre)
    print("=" * 70)
    numericas = df.select_dtypes(include=np.number)
    resultados = []
    for columna in numericas.columns:
        serie = numericas[columna].dropna()
        q1 = serie.quantile(0.25)
        q3 = serie.quantile(0.75)
        iqr = q3 - q1
        limite_inferior = q1 - 1.5 * iqr
        limite_superior = q3 + 1.5 * iqr
        outliers = (serie < limite_inferior) | (serie > limite_superior)
        numero_outliers = int(outliers.sum())
        porcentaje_outliers = numero_outliers / len(serie) * 100 if len(serie) > 0 else 0
        resultados.append({
            "variable": columna,
            "Q1": q1,
            "Q3": q3,
            "IQR": iqr,
            "limite_inferior": limite_inferior,
            "limite_superior": limite_superior,
            "outliers": numero_outliers,
            "porcentaje_outliers": porcentaje_outliers
        })
    resultado = pd.DataFrame(resultados)
    print(resultado.to_string(index=False))
    resultado.to_csv(REPORTS / f"outliers_{nombre}.csv", index=False)
    
# ============================================================
# 6. MATRIZ DE CORRELACIONES
# ============================================================
def analizar_correlaciones(nombre, df):
    print("\n" + "=" * 70)
    print("ANALISIS DE CORRELACIONES:", nombre)
    print("=" * 70)
    numericas = df.select_dtypes(include=np.number)
    correlaciones = numericas.corr()
    print("\nMATRIZ DE CORRELACIONES:")
    print(correlaciones.round(2))
    correlaciones.to_csv(REPORTS / f"correlaciones_{nombre}.csv")
    plt.figure(figsize=(12, 9))
    sns.heatmap(correlaciones, annot=True, cmap="coolwarm", center=0, fmt=".2f", linewidths=0.5)
    plt.title(f"Matriz de correlaciones - {nombre}")
    plt.tight_layout()
    plt.savefig(FIGURES / f"correlaciones_{nombre}.png", dpi=300)
    plt.close()
    
# ============================================================
# 7. VISUALIZACION DE VARIABLES NUMERICAS
# ============================================================
def graficos_numericos(nombre, df):
    numericas = df.select_dtypes(include=np.number)
    numericas.hist(figsize=(15, 12), bins=25, edgecolor="black")
    plt.suptitle(f"Histogramas - {nombre}", fontsize=16)
    plt.tight_layout()
    plt.savefig(FIGURES / f"histogramas_{nombre}.png", dpi=300)
    plt.close("all")
    for columna in numericas.columns:
        plt.figure(figsize=(8, 4))
        sns.boxplot(x=df[columna])
        plt.title(f"Boxplot de {columna} - {nombre}")
        plt.tight_layout()
        plt.savefig(FIGURES / f"boxplot_{nombre}_{columna}.png", dpi=300)
        plt.close()
        
# ============================================================
# 8. VISUALIZACIONES ESPECIFICAS
# ============================================================
def graficos_especificos(wine, penguins):
    plt.figure(figsize=(10, 5))
    sns.countplot(data=wine, x="quality", hue="wine_type")
    plt.title("Distribucion de calidad de los vinos")
    plt.xlabel("Calidad")
    plt.ylabel("Frecuencia")
    plt.legend(title="Tipo de vino")
    plt.tight_layout()
    plt.savefig(FIGURES / "wine_calidad_por_tipo.png", dpi=300)
    plt.close()
    plt.figure(figsize=(9, 5))
    sns.countplot(data=penguins, x="species", hue="species", legend=False)
    plt.title("Numero de pingüinos por especie")
    plt.xlabel("Especie")
    plt.ylabel("Frecuencia")
    plt.tight_layout()
    plt.savefig(FIGURES / "penguins_por_especie.png", dpi=300)
    plt.close()
    plt.figure(figsize=(9, 6))
    sns.scatterplot(data=penguins, x="bill_length_mm", y="bill_depth_mm", hue="species", style="sex", s=80)
    plt.title("Longitud y profundidad del pico")
    plt.xlabel("Longitud del pico (mm)")
    plt.ylabel("Profundidad del pico (mm)")
    plt.tight_layout()
    plt.savefig(FIGURES / "penguins_pico.png", dpi=300)
    plt.close()
    plt.figure(figsize=(9, 5))
    sns.boxplot(data=penguins, x="species", y="body_mass_g", hue="species", legend=False)
    plt.title("Masa corporal por especie")
    plt.xlabel("Especie")
    plt.ylabel("Masa corporal (g)")
    plt.tight_layout()
    plt.savefig(FIGURES / "penguins_masa_corporal.png", dpi=300)
    plt.close()

# ============================================================
#  MARIA DB
# ============================================================

def conectarMariaDB():
    engine_server = create_engine(
        f"mysql+pymysql://{USER}:{PASSWORD}@{HOST}:{PORT}/"
    )

    return engine_server

def conectarBBDD():
    engine_db = create_engine(
        f"mysql+pymysql://{USER}:{PASSWORD}@{HOST}:{PORT}/{DATABASE}"
    )
    return engine_db

def volcar_BBDD(dataframes, engine_db):
    for nombre_tabla, dataframe in dataframes.items():
        dataframe.to_sql(
            name=nombre_tabla,
            con=engine_db,
            if_exists="replace",
            index=False
        )

def subir_datos(dfs):
    try:
        engine_server = conectarMariaDB()

        with engine_server.connect() as connection:
            connection.execute(
                text(f"CREATE DATABASE IF NOT EXISTS {DATABASE}")
            )
        print(f"Base de datos '{DATABASE}' comprobada.")

        engine_db = conectarBBDD()

        volcar_BBDD(dfs, engine_db)
    finally:
        engine_db.dispose()
        engine_server.dispose()


    
# ============================================================
# 9. EJECUCION PRINCIPAL
# ============================================================
if __name__ == "__main__":
    red, white, wine, penguins = cargar_datos()
    conjuntos = {
        "wine_red": red,
        "wine_white": white,
        "wine_combined": wine,
        "penguins": penguins
    }
    for nombre, df in conjuntos.items():
        inspeccion_general(nombre, df)
        calidad_datos(nombre, df)
        detectar_outliers(nombre, df)
        analizar_correlaciones(nombre, df)
        graficos_numericos(nombre, df)
    graficos_especificos(wine, penguins)
    print("\n" + "=" * 70)
    print("ANALISIS EXPLORATORIO FINALIZADO CORRECTAMENTE")
    print("=" * 70)
    print("\nDirectorio de datos:", ROOT)
    print("\nFiguras generadas:", FIGURES)
    print("\nInformes generados:", REPORTS)
    print("\nConjuntos analizados:")
    for nombre, df in conjuntos.items():
        print(f"{nombre}: {df.shape[0]} filas y {df.shape[1]} columnas")
    print("\nNo se han eliminado ni modificado registros originales.")

    dataframes = {
        "wine": wine,
        "penguin": penguins
    }

    subir_datos(dataframes)

    print("\nFIN DE LA PRACTICA 01")





