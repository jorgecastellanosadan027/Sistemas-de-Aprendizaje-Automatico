# ============================================================
# PRACTICA 01 - VISUALIZACIONES EDA EN R
# DATASETS: WINE QUALITY Y PALMER PENGUINS
# ============================================================
# 1. LIBRERIAS
paquetes <- c("ggplot2", "dplyr", "tidyr", "readr", "GGally", "corrplot")
instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if(length(instalar) > 0) install.packages(instalar)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(GGally)
library(corrplot)
# ============================================================
# 2. RUTAS Y CARGA DE DATOS
# ============================================================
ROOT <- file.path(path.expand("~"), "curso_ia", "datos")
DIR_WINE <- file.path(ROOT, "raw", "wine")
DIR_PENGUINS <- file.path(ROOT, "raw", "penguins")
DIR_PLOTS <- file.path(ROOT, "figures", "R")
dir.create(DIR_PLOTS, recursive = TRUE, showWarnings = FALSE)
wine_red <- read.csv(file.path(DIR_WINE, "winequality-red.csv"), sep = ";")
wine_white <- read.csv(file.path(DIR_WINE, "winequality-white.csv"), sep = ";")
penguins <- read.csv(file.path(DIR_PENGUINS, "penguins.csv"))
wine_red$tipo <- "Tinto"
wine_white$tipo <- "Blanco"
wine <- bind_rows(wine_red, wine_white)
# ============================================================
# 3. FUNCION PARA GUARDAR Y MOSTRAR GRAFICOS
# ============================================================
guardar_plot <- function(grafico, nombre, ancho = 10, alto = 6) {
  print(grafico)
  ggsave(
    filename = file.path(DIR_PLOTS, nombre),
    plot = grafico,
    width = ancho,
    height = alto,
    dpi = 300
  )
}
# ============================================================
# 4. WINE QUALITY - DISTRIBUCIONES
# ============================================================
p1 <- ggplot(wine, aes(x = quality, fill = tipo)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de la calidad del vino", x = "Calidad", y = "Frecuencia", fill = "Tipo de vino") +
  theme_minimal()
guardar_plot(p1, "01_calidad_vinos.png")
p2 <- ggplot(wine, aes(x = alcohol, fill = tipo)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(title = "Distribución del alcohol", x = "Alcohol (%)", y = "Frecuencia", fill = "Tipo de vino") +
  theme_minimal()
guardar_plot(p2, "02_distribucion_alcohol.png")
p3 <- ggplot(wine, aes(x = tipo, y = alcohol, fill = tipo)) +
  geom_boxplot() +
  labs(title = "Alcohol según tipo de vino", x = "Tipo de vino", y = "Alcohol (%)") +
  theme_minimal() +
  theme(legend.position = "none")
guardar_plot(p3, "03_boxplot_alcohol_tipo.png")
p4 <- ggplot(wine, aes(x = factor(quality), y = alcohol, fill = tipo)) +
  geom_boxplot() +
  labs(title = "Alcohol según calidad y tipo de vino", x = "Calidad", y = "Alcohol (%)", fill = "Tipo") +
  theme_minimal()
guardar_plot(p4, "04_alcohol_calidad_tipo.png")
# ============================================================
# 5. WINE QUALITY - VARIABLES FISICOQUIMICAS
# ============================================================
wine_long <- wine %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor")
p5 <- ggplot(wine_long, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Distribución de las variables fisicoquímicas", x = "Valor", y = "Frecuencia") +
  theme_minimal()
guardar_plot(p5, "05_histogramas_variables_vino.png", 14, 10)
p6 <- ggplot(wine_long, aes(x = variable, y = valor)) +
  geom_boxplot(fill = "orange", outlier.alpha = 0.3) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Detección visual de valores atípicos", x = "Variable", y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
guardar_plot(p6, "06_boxplots_variables_vino.png", 14, 10)
# ============================================================
# 6. WINE QUALITY - CORRELACIONES
# ============================================================
wine_numeric <- wine %>% select(where(is.numeric))
cor_wine <- cor(wine_numeric, use = "complete.obs")
png(file.path(DIR_PLOTS, "07_correlaciones_vino.png"), width = 1400, height = 1200, res = 150)
corrplot(cor_wine, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex = 0.6)
dev.off()
cor_red <- cor(wine_red %>% select(where(is.numeric)), use = "complete.obs")
cor_white <- cor(wine_white %>% select(where(is.numeric)), use = "complete.obs")
png(file.path(DIR_PLOTS, "08_correlacion_vino_tinto.png"), width = 1200, height = 1000, res = 150)
corrplot(cor_red, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()
png(file.path(DIR_PLOTS, "09_correlacion_vino_blanco.png"), width = 1200, height = 1000, res = 150)
corrplot(cor_white, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()
# ============================================================
# 7. PALMER PENGUINS - DISTRIBUCIONES
# ============================================================
p7 <- ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Número de pingüinos por especie", x = "Especie", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")
guardar_plot(p7, "10_pingüinos_por_especie.png")
p8 <- ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de especies por isla", x = "Isla", y = "Frecuencia", fill = "Especie") +
  theme_minimal()
guardar_plot(p8, "11_especies_por_isla.png")
# ============================================================
# 8. PALMER PENGUINS - RELACIONES ENTRE VARIABLES
# ============================================================
p9 <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Longitud y profundidad del pico", x = "Longitud del pico (mm)", y = "Profundidad del pico (mm)", color = "Especie") +
  theme_minimal()
guardar_plot(p9, "12_longitud_profundidad_pico.png")
p10 <- ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(title = "Masa corporal según especie", x = "Especie", y = "Masa corporal (g)") +
  theme_minimal() +
  theme(legend.position = "none")
guardar_plot(p10, "13_masa_corporal_especie.png")
p11 <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Longitud de aleta y masa corporal", x = "Longitud de aleta (mm)", y = "Masa corporal (g)", color = "Especie") +
  theme_minimal()
guardar_plot(p11, "14_aleta_masa_corporal.png")
p12 <- ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Longitud del pico y masa corporal", x = "Longitud del pico (mm)", y = "Masa corporal (g)", color = "Especie") +
  theme_minimal()
guardar_plot(p12, "15_pico_masa_corporal.png")
# ============================================================
# 9. PALMER PENGUINS - DISTRIBUCIONES NUMERICAS
# ============================================================
penguins_long <- penguins %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor")
p13 <- ggplot(penguins_long, aes(x = valor)) +
  geom_histogram(bins = 25, fill = "darkseagreen3", color = "white") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(title = "Distribución de variables numéricas de Penguins", x = "Valor", y = "Frecuencia") +
  theme_minimal()
guardar_plot(p13, "16_histogramas_penguins.png", 12, 8)
p14 <- ggplot(penguins_long, aes(x = variable, y = valor)) +
  geom_boxplot(fill = "coral") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(title = "Boxplots de variables numéricas de Penguins", x = "Variable", y = "Valor") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
guardar_plot(p14, "17_boxplots_penguins.png", 12, 8)
# ============================================================
# 10. RESUMEN FINAL
# ============================================================
cat("\n====================================\n")
cat("VISUALIZACIONES GENERADAS\n")
cat("====================================\n")
cat("Directorio:", DIR_PLOTS, "\n")
cat("Total de gráficos:", length(list.files(DIR_PLOTS, pattern = "\\.png$")), "\n")
cat("====================================\n")
