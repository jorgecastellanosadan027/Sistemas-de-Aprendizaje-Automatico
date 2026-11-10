import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

#=====================================
#S1
#====================================

df = pd.read_parquet('../datos/yellow_tripdata_2024-01.parquet')

print('Información del DataFrame:')
print(df.info(memory_usage='deep'))

resultado = df.groupby("payment_type").agg(
    num_viajes=("payment_type", "count"),
    distancia_media=("trip_distance", "mean"),
    importe_medio=("total_amount", "mean")
).reset_index()

print(resultado)

#=====================================
#S2
#====================================

print('\nNúmero de valores nulos por columna:\n')
nulos = pd.DataFrame({
    'nulos':df.isna().sum(),
    'porcentaje':(df.isna().sum() / len(df) * 100)
})
print(nulos)

df['duracion'] = (
    df["tpep_dropoff_datetime"] -
    df["tpep_pickup_datetime"]
).dt.total_seconds()

filas_invalidas = df[
    (df['trip_distance'] <= 0) |
    (df['trip_distance'] <= 0) |
    (df['total_amount'] <= 0) |
    (df['duracion'] <= 0)
]

print(f"\nNúmero de filas con valores inválidos: {len(filas_invalidas)}")

memoria_inicial = df.memory_usage(deep=True).sum()
print(f"Memoria inicial: {memoria_inicial / 1024**2:.2f} MB")

columnas_category = [
    "VendorID",
    "store_and_fwd_flag",
    "RatecodeID",
    "payment_type",
    "PULocationID",
    "DOLocationID",
    "mta_tax",
    "tolls_amount",
    "improvement_surcharge",
    "congestion_surcharge",
    "Airport_fee"
]

for columna in columnas_category:
    if columna in df.columns:
        df[columna] = df[columna].astype("category")

memoria_final = df.memory_usage(deep=True).sum()
print(f"Memoria final: {memoria_final / 1024**2:.2f} MB")
print(f"Reducción de memoria: {(memoria_inicial - memoria_final) / 1024**2:.2f} MB")


mascara = (
    (df["trip_distance"] > 0) &
    (df["fare_amount"] > 0) &
    (df["total_amount"] > 0) &
    (df["duracion"] >= 0) &
    (df["tpep_pickup_datetime"] >= "2024-01-01") &
    (df["tpep_pickup_datetime"] < "2024-02-01") &
    (df["duracion"] <= 86400)
)

viajes_validos = df[mascara].copy()

total_inicial = len(df)
total_validos = len(viajes_validos)
total_descartados = total_inicial - total_validos

print(f"Total inicial: {total_inicial}")
print(f"Viajes válidos: {total_validos}")
print(f"Viajes descartados: {total_descartados}")


#=====================================
#S3
#====================================

plt.figure(figsize=(10, 5))

plt.hist(
    viajes_validos["trip_distance"],
    bins=50,
    log=True,
    edgecolor="black"
)

plt.title("Distribución de distancias de viajes")
plt.xlabel("Distancia (millas)")
plt.ylabel("Frecuencia (escala logarítmica)")

plt.show()


plt.figure(figsize=(10, 5))

sns.boxplot(
    data=viajes_validos,
    x="payment_type",
    y="total_amount"
)

plt.title("Distribución del importe por método de pago")
plt.xlabel("Método de pago")
plt.ylabel("Importe ($)")

plt.show()


plt.figure(figsize=(10, 5))

sns.violinplot(
    data=viajes_validos,
    x="payment_type",
    y="total_amount"
)

plt.title("Distribución de densidad del importe por método de pago")
plt.xlabel("Método de pago")
plt.ylabel("Importe ($)")

plt.show()


plt.figure(figsize=(10, 6))

hexbin = plt.hexbin(
    viajes_validos["trip_distance"],
    viajes_validos["fare_amount"],
    gridsize=50,
    cmap="viridis",
    mincnt=1
)

plt.colorbar(hexbin, label="Número de viajes")

plt.title("Densidad de viajes: distancia frente a tarifa")
plt.xlabel("Distancia (millas)")
plt.ylabel("Tarifa ($)")

plt.show()


fig, axes = plt.subplots(2, 2, figsize=(16, 10))

# Histograma
axes[0, 0].hist(viajes_validos["trip_distance"], bins=50, log=True)
axes[0, 0].set_title("Distribución de distancias")
axes[0, 0].set_xlabel("Distancia (millas)")
axes[0, 0].set_ylabel("Frecuencia")

# Boxplot
sns.boxplot(data=viajes_validos, x="payment_type", y="total_amount", ax=axes[0, 1])
axes[0, 1].set_title("Importe por método de pago")
axes[0, 1].set_xlabel("Método de pago")
axes[0, 1].set_ylabel("Importe ($)")

# Violin plot
sns.violinplot(data=viajes_validos, x="payment_type", y="total_amount", ax=axes[1, 0])
axes[1, 0].set_title("Densidad del importe por método de pago")
axes[1, 0].set_xlabel("Método de pago")
axes[1, 0].set_ylabel("Importe ($)")

# Hexbin
hexbin = axes[1, 1].hexbin(
    viajes_validos["trip_distance"],
    viajes_validos["fare_amount"],
    gridsize=50,
    cmap="viridis",
    mincnt=1
)
axes[1, 1].set_title("Distancia frente a tarifa")
axes[1, 1].set_xlabel("Distancia (millas)")
axes[1, 1].set_ylabel("Tarifa ($)")

fig.colorbar(hexbin, ax=axes[1, 1], label="Número de viajes")

fig.tight_layout()
fig.savefig("graficos_taxi.png", dpi=200, bbox_inches="tight")

#=====================================
#S4
#====================================

df = df.set_index("tpep_pickup_datetime")
df = df.loc["2024-01-01":"2024-01-31"]

# Viajes por hora
viajes_hora = df.resample("h").size()

# Viajes por día
viajes_dia = df.resample("D").size()


fig, ax = plt.subplots(2, 1, figsize=(14, 8))

# Gráfico de viajes por hora
ax[0].plot(viajes_hora.index, viajes_hora.values)
ax[0].set_title("Viajes por hora")
ax[0].set_xlabel("Fecha")
ax[0].set_ylabel("Número de viajes")

# Gráfico de viajes por día
ax[1].plot(viajes_dia.index, viajes_dia.values)
ax[1].set_title("Viajes por día")
ax[1].set_xlabel("Fecha")
ax[1].set_ylabel("Número de viajes")

plt.tight_layout()
plt.show()

media_movil = viajes_hora.rolling(window=24).mean()

plt.figure(figsize=(14, 5))

plt.plot(viajes_hora.index, viajes_hora.values, label="Viajes por hora", alpha=0.5)
plt.plot(media_movil.index, media_movil.values, label="Media móvil 24 h", linewidth=2)

plt.title("Viajes por hora y media móvil de 24 horas")
plt.xlabel("Fecha")
plt.ylabel("Número de viajes")
plt.legend()
plt.grid()

plt.tight_layout()
plt.show()


# Crear DataFrame a partir de la Serie horaria
perfil = viajes_hora.to_frame(name="viajes")

# Obtener la hora y el día de la semana
perfil["hora"] = perfil.index.hour
perfil["dia_semana"] = perfil.index.dayofweek

# Clasificar los días
perfil["tipo_dia"] = perfil["dia_semana"].apply(
    lambda x: "Laborable" if x < 5 else "Fin de semana"
)

comparativa = perfil.groupby(
    ["tipo_dia", "hora"]
)["viajes"].mean().unstack(level=0)


plt.figure(figsize=(12, 5))

plt.plot(comparativa.index, comparativa["Laborable"], label="Laborables")
plt.plot(comparativa.index, comparativa["Fin de semana"], label="Fin de semana")

plt.title("Perfil horario de viajes: laborables vs fin de semana")
plt.xlabel("Hora del día")
plt.ylabel("Media de viajes")
plt.xticks(range(24))
plt.legend()
plt.grid()

plt.tight_layout()
plt.show()