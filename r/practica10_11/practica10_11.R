library(arrow)
library(dplyr)
library(ggplot2)
library(lubridate)

#===========================================
#R1
#==========================================

df <- read_parquet("datos/yellow_tripdata_2024-01.parquet")

# Dimensiones
cat("Dimensiones:\n")
print(dim(df))

# Tipos de columnas
cat("\nTipos de columnas:\n")
print(sapply(df, class))

# Resumen variables numéricas
cat("\nResumen variables numéricas:\n")
print(summary(df[sapply(df, is.numeric)]))

resultado <- df %>%
  group_by(payment_type) %>%
  summarise(
    total_viajes = n(),
    distancia_media = mean(trip_distance, na.rm = TRUE),
    importe_medio = mean(total_amount, na.rm = TRUE)
  )

print(resultado)

#===========================================
#R2
#==========================================

cat("\n========== VALORES NULOS POR COLUMNA ==========\n")
nulos <- colSums(is.na(df))
print(nulos)

cat("\n========== TOTAL DE VIAJES INVÁLIDOS ==========\n")
total_invalidos <- sum(
  df$passenger_count == 0 | is.na(df$passenger_count),
  na.rm = TRUE
)
print(total_invalidos)


cat("\n========== DISTANCIAS NEGATIVAS O CERO ==========\n")
distancias_invalidas <- df %>%
  filter(trip_distance <= 0)

print(nrow(distancias_invalidas))

cat("\n========== TARIFAS NEGATIVAS O CERO ==========\n")
tarifas_invalidas <- df %>%
  filter(fare_amount <= 0)

print(nrow(tarifas_invalidas))

cat("\n========== IMPORTES NEGATIVOS O CERO ==========\n")
importes_invalidos <- df %>%
  filter(total_amount <= 0)

print(nrow(importes_invalidos))

cat("\n========== DURACIONES NEGATIVAS ==========\n")
df <- df %>%
  mutate(
    duracion = as.numeric(
      difftime(tpep_dropoff_datetime,
               tpep_pickup_datetime,
               units = "mins")
    )
  )

duraciones_invalidas <- df %>%
  filter(duracion < 0)

print(nrow(duraciones_invalidas))

cat("\n========== RECOGIDAS FUERA DE ENERO DE 2024 ==========\n")
recogidas_fuera <- df %>%
  filter(
    tpep_pickup_datetime < as.POSIXct("2024-01-01 00:00:00", tz = "UTC") |
    tpep_pickup_datetime >= as.POSIXct("2024-02-01 00:00:00", tz = "UTC")
  )
cat("Total de recogidas fuera de enero:", nrow(recogidas_fuera), "\n")


cat("\n========== VIAJES DE MÁS DE 24 HORAS ==========\n")
viajes_largos <- df %>%
  filter(duracion > 1440)
cat("Total de viajes de más de 24 horas:", nrow(viajes_largos), "\n")


library(dplyr)


df <- df %>%
  mutate(
    duracion = as.numeric(
      difftime(
        tpep_dropoff_datetime,
        tpep_pickup_datetime,
        units = "mins"
      )
    )
  )

df_validacion <- df %>%
  mutate(
    invalid_passenger = is.na(passenger_count) | passenger_count == 0,
    invalid_distance = is.na(trip_distance) | trip_distance <= 0,
    invalid_fare = is.na(fare_amount) | fare_amount <= 0,
    invalid_amount = is.na(total_amount) | total_amount <= 0,
    invalid_pickup = is.na(tpep_pickup_datetime) |
      as.Date(tpep_pickup_datetime) < as.Date("2024-01-01") |
      as.Date(tpep_pickup_datetime) >= as.Date("2024-02-01"),
    invalid_duration = is.na(duracion) | duracion < 0 | duracion > 1440
  )


viajes_validos <- df_validacion %>%
  filter(!if_any(starts_with("invalid_"), identity)) %>%
  select(-starts_with("invalid_"))


cat("Filas originales:", nrow(df), "\n")
cat("Filas válidas:", nrow(viajes_validos), "\n")
cat("Filas descartadas:", nrow(df) - nrow(viajes_validos), "\n")


#===========================================
#R3
#===========================================

ggplot(viajes_validos, aes(x = trip_distance)) +
  geom_histogram(
    bins = 60,
    fill = "steelblue",
    color = "white"
  ) +
  scale_x_log10() +
  labs(
    title = "Distribución de distancias de los viajes",
    subtitle = "Escala logarítmica",
    x = "Distancia del viaje (millas, escala logarítmica)",
    y = "Número de viajes"
  ) +
  theme_minimal()

  ggplot(viajes_validos, aes(x = factor(payment_type), y = total_amount)) +
  geom_boxplot(
    fill = "steelblue",
    color = "black",
    outlier.alpha = 0.3
  ) +
  labs(
    title = "Distribución del importe total por método de pago",
    x = "Método de pago",
    y = "Importe total ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

  ggplot(viajes_validos, aes(x = trip_distance, y = fare_amount)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c(
    name = "Número de viajes",
    trans = "log10"
  ) +
  labs(
    title = "Distancia frente a tarifa de los viajes",
    subtitle = "Mapa de densidad hexagonal",
    x = "Distancia (millas)",
    y = "Tarifa ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


  propina_media <- viajes_validos %>%
  group_by(payment_type) %>%
  summarise(
    propina_media = mean(tip_amount, na.rm = TRUE),
    .groups = "drop"
  )

# Mostrar resultados
cat("\n========== PROPINA MEDIA POR MÉTODO DE PAGO ==========\n")
print(propina_media)

# Crear gráfico
grafico <- ggplot(
  propina_media,
  aes(x = factor(payment_type), y = propina_media)
) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Propina media por método de pago",
    x = "Método de pago",
    y = "Propina media ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    axis.title = element_text(face = "bold")
  )

# Mostrar gráfico
print(grafico)

# Guardar gráfico
ggsave(
  filename = "propina_media_payment_type.png",
  plot = grafico,
  width = 10,
  height = 6,
  dpi = 200
)

#===========================================
#R4
#===========================================

viajes_validos <- viajes_validos %>%
  mutate(
    hora = hour(tpep_pickup_datetime),
    dia_semana = wday(tpep_pickup_datetime, label = TRUE, abbr = FALSE),
    fecha = as.Date(tpep_pickup_datetime)
  )

# Mostrar resultados
cat("\n========== NUEVAS VARIABLES TEMPORALES ==========\n")

print(
  viajes_validos %>%
    select(tpep_pickup_datetime, hora, dia_semana, fecha) %>%
    head(10)
)

viajes_por_dia <- viajes_validos %>%
  group_by(fecha) %>%
  summarise(
    total_viajes = n(),
    .groups = "drop"
  ) %>%
  arrange(fecha)

# Mostrar resultados
cat("\n========== VIAJES POR DÍA ==========\n")
print(viajes_por_dia)

# Crear gráfico
grafico_dias <- ggplot(
  viajes_por_dia,
  aes(x = fecha, y = total_viajes)
) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  labs(
    title = "Evolución diaria de viajes",
    x = "Fecha",
    y = "Número de viajes"
  ) +
  theme_minimal()

# Mostrar gráfico
print(grafico_dias)


viajes_heatmap <- viajes_validos %>%
  group_by(dia_semana, hora) %>%
  summarise(total_viajes = n(), .groups = "drop")

grafico_heatmap <- ggplot(viajes_heatmap,
                          aes(x = hora, y = dia_semana, fill = total_viajes)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Número de viajes") +
  labs(
    title = "Distribución de viajes por día y hora",
    x = "Hora del día",
    y = "Día de la semana"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(grafico_heatmap)

viajes_franjas <- viajes_validos %>%
  mutate(
    franja_horaria = case_when(
      hora >= 6 & hora < 12 ~ "Mañana",
      hora >= 12 & hora < 18 ~ "Tarde",
      hora >= 18 & hora < 24 ~ "Noche",
      TRUE ~ "Madrugada"
    ),
    velocidad = trip_distance / (duracion / 60)
  ) %>%
  group_by(franja_horaria) %>%
  summarise(
    duracion_media = mean(duracion, na.rm = TRUE),
    velocidad_media = mean(velocidad[is.finite(velocidad)], na.rm = TRUE),
    total_viajes = n(),
    .groups = "drop"
  ) %>%
  arrange(velocidad_media)

cat("\n========== DURACIÓN Y VELOCIDAD POR FRANJA ==========\n")
print(viajes_franjas)

cat("\nLa franja con tráfico más lento es:\n")
print(viajes_franjas %>% slice(1))

#===========================================
#R5
#===========================================