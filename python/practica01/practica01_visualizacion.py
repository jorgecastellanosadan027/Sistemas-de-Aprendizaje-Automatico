

# ============================================================
# PRACTICA 01 - VISUALIZACION DEL ANALISIS EXPLORATORIO (EDA)
# Conjuntos: Wine Quality y Palmer Penguins
# Herramientas: Python / Pandas / Matplotlib / Seaborn
# ============================================================
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
# ============================================================
# 1. CONFIGURACION
# ============================================================
ROOT = Path.home() / "curso_ia" / "datos"
WINE_RED = ROOT / "raw/wine/winequality-red.csv"
WINE_WHITE = ROOT / "raw/wine/winequality-white.csv"
PENGUINS_FILE = ROOT / "raw/penguins/penguins.csv"
FIGURES = ROOT / "figures/visualizacion_eda"
FIGURES.mkdir(parents=True, exist_ok=True)
sns.set_theme(style="whitegrid", palette="Set2")
plt.rcParams["figure.figsize"] = (12, 7)
plt.rcParams["font.size"] = 10
# ============================================================
# 2. CARGA DE DATOS
# ============================================================
red = pd.read_csv(WINE_RED, sep=";")
white = pd.read_csv(WINE_WHITE, sep=";")
penguins = pd.read_csv(PENGUINS_FILE)
red["wine_type"] = "Tinto"
white["wine_type"] = "Blanco"
wine = pd.concat([red, white], ignore_index=True)
print("Datos cargados correctamente")
print("Wine:", wine.shape)
print("Penguins:", penguins.shape)
# ============================================================
# 3. DISTRIBUCION DE CALIDAD DE LOS VINOS
# ============================================================
fig, ax = plt.subplots(figsize=(10, 6))
sns.countplot(data=wine, x="quality", hue="wine_type", ax=ax)
ax.set_title("Distribución de la calidad de los vinos", fontsize=15)
ax.set_xlabel("Calidad del vino")
ax.set_ylabel("Número de registros")
ax.legend(title="Tipo de vino")
plt.tight_layout()
plt.savefig(FIGURES / "01_distribucion_calidad_vinos.png", dpi=300)
plt.show()
# ============================================================
# 4. DISTRIBUCION DEL ALCOHOL
# ============================================================
fig, ax = plt.subplots(figsize=(10, 6))
sns.histplot(data=wine, x="alcohol", hue="wine_type", kde=True, bins=30, element="step", common_norm=False, ax=ax)
ax.set_title("Distribución del contenido de alcohol", fontsize=15)
ax.set_xlabel("Alcohol (%)")
ax.set_ylabel("Frecuencia")
plt.tight_layout()
plt.savefig(FIGURES / "02_distribucion_alcohol.png", dpi=300)
plt.show()
# ============================================================
# 5. RELACION ENTRE ALCOHOL Y CALIDAD
# ============================================================
fig, ax = plt.subplots(figsize=(10, 6))
sns.boxplot(data=wine, x="quality", y="alcohol", hue="wine_type", ax=ax)
ax.set_title("Contenido de alcohol según la calidad del vino", fontsize=15)
ax.set_xlabel("Calidad")
ax.set_ylabel("Alcohol (%)")
ax.legend(title="Tipo de vino")
plt.tight_layout()
plt.savefig(FIGURES / "03_alcohol_calidad.png", dpi=300)
plt.show()
# ============================================================
# 6. MATRICES DE CORRELACION DE LOS VINOS
# ============================================================
fig, axes = plt.subplots(1, 2, figsize=(20, 8))
sns.heatmap(red.select_dtypes(include="number").corr(), annot=True, fmt=".2f", cmap="coolwarm", center=0, ax=axes[0])
axes[0].set_title("Correlaciones - Vino tinto")
sns.heatmap(white.select_dtypes(include="number").corr(), annot=True, fmt=".2f", cmap="coolwarm", center=0, ax=axes[1])
axes[1].set_title("Correlaciones - Vino blanco")
plt.tight_layout()
plt.savefig(FIGURES / "04_correlaciones_vinos.png", dpi=300)
plt.show()
# ============================================================
# 7. RELACION ENTRE VARIABLES DE PENGUINS
# ============================================================
fig, ax = plt.subplots(figsize=(10, 7))
sns.scatterplot(data=penguins, x="bill_length_mm", y="bill_depth_mm", hue="species", style="sex", s=90, ax=ax)
ax.set_title("Longitud y profundidad del pico por especie", fontsize=15)
ax.set_xlabel("Longitud del pico (mm)")
ax.set_ylabel("Profundidad del pico (mm)")
ax.legend(title="Especie / Sexo")
plt.tight_layout()
plt.savefig(FIGURES / "05_penguins_pico.png", dpi=300)
plt.show()
# ============================================================
# 8. MASA CORPORAL POR ESPECIE
# ============================================================
fig, ax = plt.subplots(figsize=(10, 6))
sns.boxplot(data=penguins, x="species", y="body_mass_g", hue="species", legend=False, ax=ax)
ax.set_title("Distribución de la masa corporal por especie", fontsize=15)
ax.set_xlabel("Especie")
ax.set_ylabel("Masa corporal (gramos)")
plt.tight_layout()
plt.savefig(FIGURES / "06_penguins_masa_corporal.png", dpi=300)
plt.show()
# ============================================================
# 9. LONGITUD DE ALETA Y MASA CORPORAL
# ============================================================
fig, ax = plt.subplots(figsize=(10, 6))
sns.scatterplot(data=penguins, x="flipper_length_mm", y="body_mass_g", hue="species", style="sex", s=90, ax=ax)
ax.set_title("Relación entre longitud de aleta y masa corporal", fontsize=15)
ax.set_xlabel("Longitud de aleta (mm)")
ax.set_ylabel("Masa corporal (gramos)")
ax.legend(title="Especie / Sexo")
plt.tight_layout()
plt.savefig(FIGURES / "07_penguins_aleta_masa.png", dpi=300)
plt.show()
# ============================================================
# 10. VALORES AUSENTES
# ============================================================
fig, axes = plt.subplots(1, 2, figsize=(15, 5))
missing_wine = wine.isna().sum()
missing_penguins = penguins.isna().sum()
sns.barplot(x=missing_wine.index, y=missing_wine.values, ax=axes[0])
axes[0].set_title("Valores ausentes - Vinos")
axes[0].set_ylabel("Número de valores ausentes")
axes[0].tick_params(axis="x", rotation=75)
sns.barplot(x=missing_penguins.index, y=missing_penguins.values, ax=axes[1])
axes[1].set_title("Valores ausentes - Penguins")
axes[1].set_ylabel("Número de valores ausentes")
axes[1].tick_params(axis="x", rotation=45)
plt.tight_layout()
plt.savefig(FIGURES / "08_valores_ausentes.png", dpi=300)
plt.show()
# ============================================================
# 11. RESUMEN FINAL
# ============================================================
print("\n============================================")
print("VISUALIZACION EDA FINALIZADA")
print("============================================")
print("Gráficos generados: 8")
print("Directorio:", FIGURES)
for archivo in sorted(FIGURES.glob("*.png")):
    print(archivo.name)
print("\nFIN DE LA VISUALIZACION")