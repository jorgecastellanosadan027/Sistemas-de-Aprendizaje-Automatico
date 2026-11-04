# ============================================================
#sudo apt install -y \
#    build-essential \
#    libpq-dev \
#    postgresql-client \
#    pkg-config

paquetes <- c("ggplot2", "dplyr", "tidyr", "readr", "GGally", "corrplot", "DBI", "RPostgres")
instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if(length(instalar) > 0) install.packages(instalar)

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(GGally)
library(corrplot)
library(DBI)
library(RPostgres)
# ============================================================
# 1. CONFIGURACION DE RUTAS
# ============================================================
ROOT <- file.path(path.expand("~"), "curso_ia", "datos")
WINE_RED <- file.path(ROOT, "raw", "wine", "winequality-red.csv")
WINE_WHITE <- file.path(ROOT, "raw", "wine", "winequality-white.csv")
PENGUINS_FILE <- file.path(ROOT, "raw", "penguins", "penguins.csv")
FIGURES <- file.path(ROOT, "figures", "R")
REPORTS <- file.path(ROOT, "docs", "R")
dir.create(FIGURES, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORTS, recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 11))
options(width = 120)
cat("\n============================================\n")
cat("PRACTICA 01 - ANALISIS EXPLORATORIO DE DATOS\n")
cat("============================================\n")
cat("\nDirectorio principal:", ROOT, "\n")
archivos <- c("Wine tinto" = WINE_RED, "Wine blanco" = WINE_WHITE, "Penguins" = PENGUINS_FILE)
cat("\nCOMPROBACION DE ARCHIVOS:\n")
for (nombre in names(archivos)) {
  estado <- if (file.exists(archivos[[nombre]])) "OK" else "NO ENCONTRADO"
  cat(nombre, ":", estado, "\n")
  if (!file.exists(archivos[[nombre]])) stop("No se encuentra el archivo: ", archivos[[nombre]])
}
# ============================================================
# 2. CARGA DE LOS DATOS
# ============================================================
cargar_datos <- function() {
  cat("\n========== CARGA DE DATOS ==========\n")
  red <- read.csv(WINE_RED, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
  blanco <- read.csv(WINE_WHITE, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
  red$wine_type <- "Tinto"
  blanco$wine_type <- "Blanco"
  wine <- bind_rows(red, blanco)
  penguins <- read.csv(PENGUINS_FILE, stringsAsFactors = FALSE, check.names = FALSE)
  cat("\nWine tinto:", paste(dim(red), collapse = " x "), "\n")
  cat("Wine blanco:", paste(dim(blanco), collapse = " x "), "\n")
  cat("Wine conjunto:", paste(dim(wine), collapse = " x "), "\n")
  cat("Penguins:", paste(dim(penguins), collapse = " x "), "\n")
  list(red = red, white = blanco, wine = wine, penguins = penguins)
}
# ============================================================
# 3. FUNCIONES AUXILIARES
# ============================================================
guardar_grafico <- function(grafico, nombre, ancho = 10, alto = 6) {
  ruta <- file.path(FIGURES, nombre)
  ggsave(filename = ruta, plot = grafico, width = ancho, height = alto, dpi = 300)
  cat("Grafico guardado:", ruta, "\n")
}
guardar_tabla <- function(tabla, nombre) {
  write.csv(tabla, file.path(REPORTS, nombre), row.names = FALSE, na = "")
}
columnas_numericas <- function(df) {
  names(df)[vapply(df, is.numeric, logical(1))]
}
# ============================================================
# 4. INSPECCION GENERAL DE LOS DATOS
# ============================================================
inspeccion_general <- function(nombre, df) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("CONJUNTO DE DATOS:", nombre, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("\n1. DIMENSIONES\n")
  cat("Filas:", nrow(df), "\nColumnas:", ncol(df), "\n")
  cat("\n2. PRIMERAS 5 FILAS\n")
  print(head(df, 5))
  cat("\n3. ULTIMAS 5 FILAS\n")
  print(tail(df, 5))
  cat("\n4. NOMBRES DE LAS VARIABLES\n")
  print(names(df))
  cat("\n5. TIPOS DE DATOS\n")
  print(vapply(df, function(x) class(x)[1], character(1)))
  cat("\n6. ESTRUCTURA GENERAL\n")
  str(df)
  cat("\n7. ESTADISTICA DESCRIPTIVA\n")
  print(summary(df))
  cat("\n8. NUMERO DE VALORES DISTINTOS\n")
  distintos <- data.frame(variable = names(df), valores_distintos = vapply(df, function(x) length(unique(x)), integer(1)))
  print(distintos)
  descriptiva <- bind_rows(lapply(names(df), function(variable) {
    x <- df[[variable]]
    if (is.numeric(x)) {
      data.frame(variable = variable, media = mean(x, na.rm = TRUE), desviacion = sd(x, na.rm = TRUE), minimo = min(x, na.rm = TRUE), Q1 = as.numeric(quantile(x, .25, na.rm = TRUE)), mediana = median(x, na.rm = TRUE), Q3 = as.numeric(quantile(x, .75, na.rm = TRUE)), maximo = max(x, na.rm = TRUE))
    } else {
      data.frame(variable = variable, media = NA, desviacion = NA, minimo = NA, Q1 = NA, mediana = NA, Q3 = NA, maximo = NA)
    }
  }))
  guardar_tabla(descriptiva, paste0("descriptiva_", nombre, ".csv"))
  guardar_tabla(distintos, paste0("valores_distintos_", nombre, ".csv"))
}
# ============================================================
# 5. ANALISIS DE CALIDAD DE LOS DATOS
# ============================================================
calidad_datos <- function(nombre, df) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("CALIDAD DE LOS DATOS:", nombre, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  ausentes <- colSums(is.na(df))
  porcentaje <- round(colMeans(is.na(df)) * 100, 2)
  duplicados <- sum(duplicated(df))
  completos <- sum(complete.cases(df))
  cat("\n1. VALORES AUSENTES\n")
  print(ausentes)
  cat("\n2. PORCENTAJE DE VALORES AUSENTES\n")
  print(porcentaje)
  cat("\n3. REGISTROS DUPLICADOS\nDuplicados:", duplicados, "\n")
  cat("\n4. REGISTROS COMPLETOS\nRegistros completos:", completos, "\n")
  cat("\n5. TOTAL DE REGISTROS\n", nrow(df), "\n")
  informe <- data.frame(variable = names(df), valores_ausentes = as.integer(ausentes), porcentaje_ausentes = as.numeric(porcentaje), valores_distintos = vapply(df, function(x) length(unique(x)), integer(1)))
  guardar_tabla(informe, paste0("calidad_", nombre, ".csv"))
  p <- ggplot(informe, aes(x = reorder(variable, porcentaje_ausentes), y = porcentaje_ausentes)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Valores ausentes -", nombre), x = "Variable", y = "Porcentaje de valores ausentes")
  guardar_grafico(p, paste0("ausentes_", nombre, ".png"), 11, 6)
}

# ============================================================
# 6. DETECCION DE VALORES ATIPICOS MEDIANTE IQR
# ============================================================
detectar_outliers <- function(nombre, df) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("DETECCION DE VALORES ATIPICOS:", nombre, "\n")
  numericas <- columnas_numericas(df)
  resultados <- lapply(numericas, function(columna) {
    x <- df[[columna]]
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(data.frame(variable = columna, Q1 = NA, Q3 = NA, IQR = NA, limite_inferior = NA, limite_superior = NA, outliers = 0, porcentaje_outliers = 0))
    }
    q1 <- as.numeric(quantile(x, .25))
    q3 <- as.numeric(quantile(x, .75))
    iqr <- q3 - q1
    inferior <- q1 - 1.5 * iqr
    superior <- q3 + 1.5 * iqr
    n_out <- sum(x < inferior | x > superior)
    data.frame(variable = columna, Q1 = q1, Q3 = q3, IQR = iqr, limite_inferior = inferior, limite_superior = superior, outliers = n_out, porcentaje_outliers = n_out / length(x) * 100)
  })
  resultado <- bind_rows(resultados)
  print(resultado)
  guardar_tabla(resultado, paste0("outliers_", nombre, ".csv"))
}
# ============================================================
# 7. MATRIZ DE CORRELACIONES
# ============================================================
analizar_correlaciones <- function(nombre, df) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("ANALISIS DE CORRELACIONES:", nombre, "\n")
  numericas <- df[, columnas_numericas(df), drop = FALSE]
  correlaciones <- cor(numericas, use = "pairwise.complete.obs")
  cat("\nMATRIZ DE CORRELACIONES:\n")
  print(round(correlaciones, 2))
  write.csv(correlaciones, file.path(REPORTS, paste0("correlaciones_", nombre, ".csv")))
  datos_cor <- as.data.frame(as.table(correlaciones))
  names(datos_cor) <- c("variable_x", "variable_y", "correlacion")
  p <- ggplot(datos_cor, aes(x = variable_x, y = variable_y, fill = correlacion)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", correlacion)), size = 3) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
    labs(title = paste("Matriz de correlaciones -", nombre), x = NULL, y = NULL, fill = "Correlacion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  guardar_grafico(p, paste0("correlaciones_", nombre, ".png"), 12, 9)
  invisible(correlaciones)
}
# ============================================================
# 8. VISUALIZACION DE VARIABLES NUMERICAS
# ============================================================
graficos_numericos <- function(nombre, df) {
  numericas <- columnas_numericas(df)
  if (length(numericas) == 0) return(invisible(NULL))
  datos_largos <- pivot_longer(df[, numericas, drop = FALSE], cols = everything(), names_to = "variable", values_to = "valor")
  p_hist <- ggplot(datos_largos, aes(x = valor)) +
    geom_histogram(bins = 25, fill = "steelblue", color = "black", na.rm = TRUE) +
    facet_wrap(~ variable, scales = "free", ncol = 3) +
    labs(title = paste("Histogramas -", nombre), x = "Valor", y = "Frecuencia")
  guardar_grafico(p_hist, paste0("histogramas_", nombre, ".png"), 15, 12)
  for (columna in numericas) {
    p_box <- ggplot(df, aes(y = .data[[columna]])) +
      geom_boxplot(fill = "lightblue", na.rm = TRUE) +
      labs(title = paste("Boxplot de", columna, "-", nombre), x = NULL, y = columna)
    guardar_grafico(p_box, paste0("boxplot_", nombre, "_", columna, ".png"), 8, 4)
  }
}
# ============================================================
# 9. VISUALIZACIONES ESPECIFICAS Y COMPARATIVAS
# ============================================================
graficos_especificos <- function(wine, red, blanco, penguins) {
  # 9.1 Distribucion de calidad por tipo de vino
  p1 <- ggplot(wine, aes(x = factor(quality), fill = wine_type)) +
    geom_bar(position = "dodge") +
    labs(title = "Distribucion de calidad de los vinos", x = "Calidad", y = "Frecuencia", fill = "Tipo de vino")
  guardar_grafico(p1, "wine_calidad_por_tipo.png", 10, 5)
  # 9.2 Distribucion del alcohol por tipo de vino
  p2 <- ggplot(wine, aes(x = alcohol, fill = wine_type, color = wine_type)) +
    geom_density(alpha = .25, na.rm = TRUE) +
    labs(title = "Distribucion del alcohol por tipo de vino", x = "Alcohol (% vol.)", y = "Densidad", fill = "Tipo de vino", color = "Tipo de vino")
  guardar_grafico(p2, "wine_distribucion_alcohol.png", 9, 5)
  # 9.3 Alcohol segun calidad y tipo de vino
  p3 <- ggplot(wine, aes(x = factor(quality), y = alcohol, fill = wine_type)) +
    geom_boxplot(na.rm = TRUE) +
    labs(title = "Alcohol segun calidad y tipo de vino", x = "Calidad", y = "Alcohol (% vol.)", fill = "Tipo de vino")
  guardar_grafico(p3, "wine_alcohol_calidad.png", 10, 5)
  # 9.4 Comparacion de matrices de correlacion
  cor_red <- cor(red[, columnas_numericas(red), drop = FALSE], use = "pairwise.complete.obs")
  cor_blanco <- cor(blanco[, columnas_numericas(blanco), drop = FALSE], use = "pairwise.complete.obs")
  convertir_cor <- function(matriz, tipo) {
    resultado <- as.data.frame(as.table(matriz))
    names(resultado) <- c("variable_x", "variable_y", "correlacion")
    resultado$tipo <- tipo
    resultado
  }
  cor_largo <- bind_rows(convertir_cor(cor_red, "Tinto"), convertir_cor(cor_blanco, "Blanco"))
  p4 <- ggplot(cor_largo, aes(x = variable_x, y = variable_y, fill = correlacion)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", correlacion)), size = 2.5) +
    facet_wrap(~ tipo) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
    labs(title = "Comparacion de correlaciones: vino tinto y blanco", x = NULL, y = NULL, fill = "Correlacion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  guardar_grafico(p4, "wine_comparacion_correlaciones.png", 16, 8)
  # 9.5 Numero de pinguinos por especie
  p5 <- ggplot(penguins, aes(x = species, fill = species)) +
    geom_bar(show.legend = FALSE) +
    labs(title = "Numero de pinguinos por especie", x = "Especie", y = "Frecuencia")
  guardar_grafico(p5, "penguins_por_especie.png", 9, 5)
  # 9.6 Longitud y profundidad del pico
  p6 <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = sex)) +
    geom_point(size = 2.5, na.rm = TRUE) +
    labs(title = "Longitud y profundidad del pico", x = "Longitud del pico (mm)", y = "Profundidad del pico (mm)", color = "Especie", shape = "Sexo")
  guardar_grafico(p6, "penguins_pico.png", 9, 6)
  # 9.7 Masa corporal por especie
  p7 <- ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
    geom_boxplot(show.legend = FALSE, na.rm = TRUE) +
    labs(title = "Masa corporal por especie", x = "Especie", y = "Masa corporal (g)")
  guardar_grafico(p7, "penguins_masa_corporal.png", 9, 5)
  # 9.8 Longitud de aleta y masa corporal
  p8 <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
    geom_point(size = 2.5, na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE, color = "black", na.rm = TRUE) +
    labs(title = "Longitud de aleta y masa corporal", x = "Longitud de aleta (mm)", y = "Masa corporal (g)", color = "Especie", shape = "Sexo")
  guardar_grafico(p8, "penguins_aleta_masa.png", 9, 6)
  # 9.9 Comparacion de valores ausentes
  aus_wine <- data.frame(variable = names(wine), porcentaje = colMeans(is.na(wine)) * 100, conjunto = "Wine")
  aus_penguins <- data.frame(variable = names(penguins), porcentaje = colMeans(is.na(penguins)) * 100, conjunto = "Penguins")
  ausentes <- bind_rows(aus_wine, aus_penguins)
  p9 <- ggplot(ausentes, aes(x = reorder(variable, porcentaje), y = porcentaje, fill = conjunto)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ conjunto, scales = "free_y") +
    labs(title = "Porcentaje de valores ausentes", x = "Variable", y = "Porcentaje")
  guardar_grafico(p9, "comparacion_valores_ausentes.png", 12, 6)
}
# ============================================================
# 10. EJECUCION PRINCIPAL
# ============================================================
datos <- cargar_datos()
red <- datos$red
blanco <- datos$white
wine <- datos$wine
penguins <- datos$penguins
conjuntos <- list(wine_red = red, wine_white = blanco, wine_combined = wine, penguins = penguins)
for (nombre in names(conjuntos)) {
  df <- conjuntos[[nombre]]
  inspeccion_general(nombre, df)
  calidad_datos(nombre, df)
  detectar_outliers(nombre, df)
  analizar_correlaciones(nombre, df)
  graficos_numericos(nombre, df)
}
graficos_especificos(wine, red, blanco, penguins)
# ============================================================
# 11. RESUMEN FINAL
# ============================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("ANALISIS EXPLORATORIO FINALIZADO CORRECTAMENTE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")


# ============================================================
# 12. CONEXION Y VOLCADO A POSTGRESQL
# ============================================================

# CONFIGURACION POSTGRESQL
USER <- "postgres"
PASSWORD <- "postgres"
HOST <- "localhost"
PORT <- 5432
DATABASE <- "practica01"


# 1. CONECTAR AL SERVIDOR POSTGRESQL
conectarPostgreSQL <- function() {
  dbConnect(
    RPostgres::Postgres(),
    user = USER,
    password = PASSWORD,
    host = HOST,
    port = PORT,
    dbname = "postgres"
  )
}


# 2. CREAR LA BASE DE DATOS SI NO EXISTE
crearBBDD <- function(con) {

  existe <- dbGetQuery(
    con,
    "SELECT 1 FROM pg_database WHERE datname = $1",
    params = list(DATABASE)
  )

  if (nrow(existe) == 0) {
    dbExecute(
      con,
      paste0('CREATE DATABASE "', DATABASE, '"')
    )
    cat("Base de datos creada:", DATABASE, "\n")
  } else {
    cat("La base de datos ya existe:", DATABASE, "\n")
  }
}


# 3. CONECTAR A LA BASE DE DATOS
conectarBBDD <- function() {
  dbConnect(
    RPostgres::Postgres(),
    user = USER,
    password = PASSWORD,
    host = HOST,
    port = PORT,
    dbname = DATABASE
  )
}


# 4. VOLCAR LOS DATAFRAMES
volcar_BBDD <- function(dataframes, con) {

  for (nombre_tabla in names(dataframes)) {

    dataframe <- dataframes[[nombre_tabla]]

    dbWriteTable(
      con,
      name = nombre_tabla,
      value = dataframe,
      overwrite = TRUE,
      row.names = FALSE
    )

    cat("Tabla actualizada:", nombre_tabla, "\n")
  }
}


# 5. EJECUCION PRINCIPAL
subir_datos <- function(dataframes) {

  con_server <- NULL
  con_db <- NULL

  tryCatch({

    con_server <- conectarPostgreSQL()
    crearBBDD(con_server)
    dbDisconnect(con_server)
    con_server <- NULL

    con_db <- conectarBBDD()

    volcar_BBDD(dataframes, con_db)

    cat("Todos los datos se han volcado correctamente.\n")

  }, finally = {
    if (!is.null(con_db) && dbIsValid(con_db)) {
      dbDisconnect(con_db)
    }
    if (!is.null(con_server) && dbIsValid(con_server)) {
      dbDisconnect(con_server)
    }
    cat("Conexiones cerradas.\n")
  })
}

dataframes <- list(wine=wine, penguin=penguins)

subir_datos(dataframes)