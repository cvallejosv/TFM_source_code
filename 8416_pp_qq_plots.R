# Ajuste de las distribuciones GEV y log-Pearson III a datos de precipitaciones
# máximas diarias anuales de la estación meteorológica de Valencia (8416) y
# generación de gráficos Q-Q y P-P para evaluar la bondad de ajuste.
# =========================================================================
# Descripción:
# Este script ajusta las distribuciones Distribución de Valores Extremos
# Generalizada (GEV) y log-Pearson Tipo III (LP3) a datos de precipitaciones
# máximas diarias anuales de una estación meteorológica de la Comunidad
# Valenciana. El ajuste se realiza mediante L-momentos muestrales. Se generan
# gráficos Q-Q (cuantiles empíricos vs.teóricos) y P-P (probabilidades empíricas
# vs. teóricas) para comparar visualmente la calidad del ajuste de ambas
# distribuciones.
#
# Entradas:
# - CVppmax24_8416_anual_valencia.csv: Archivo CSV con los valores anuales de
#   precipitaciones máximas diarias en mm. Se utiliza la columna PMAX77 de dicho
#   archivo.
#
# Salidas:
# - Cuatro archivos PNG con los gráficos generados:
#   - gev_qq_plot.png: Gráfico Q-Q para la distribución GEV.
#   - lp3_qq_plot.png: Gráfico Q-Q para la distribución LP3.
#   - gev_pp_plot.png: Gráfico P-P para la distribución GEV.
#   - lp3_pp_plot.png: Gráfico P-P para la distribución LP3.
#   Todos se guardan en la ruta especificada.
#
# Requisitos:
# - Paquetes de R:
#   - lmom: para el ajuste de distribuciones usando L-momentos.
#   - readr: para lectura de archivos CSV.
#   - ggplot2: para la generación de gráficos.
#
# Uso:
# 1. Instala los paquetes necesarios.
# 2. Asegúrate de que el archivo CSV se encuentra en la ruta indicada y contiene
#    la columna PMAX77 con los datos precipitaciones máximas diarias anuales.
# 3. Ejecuta el script en R.
# 4. Los gráficos se generarán automáticamente y se guardarán en la carpeta
#    especificada.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 1 de mayo de 2025
# =========================================================================


# Cargar e instalar las librerías necesarias
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(readr)) {
  install.packages("readr")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(lmom)
library(readr)
library(ggplot2)


# Obtener los datos
pmax <- read_csv("path/data/CV_pmax/CVppmax24_8416_anual_valencia.csv", show_col_types = FALSE)$PMAX77
n <- length(pmax)

# Calcular probabilidades empíricas
prob_emp <- (1:n) / (n+1)
pmax_ordenado <- sort(pmax)

# --- Ajuste GEV ---

# Calcular L-momentos muestrales
l_mom_gev <- samlmu(pmax)
# Calcular parámetros de la distribución GEV utilizando L-momentos muestrales
gev_param <- pelgev(l_mom_gev)
# Calcular cuantiles empíricos para el gráfico Q-Q
quant_gev <- quagev(emp_prob, gev_param)
# Calcular probabilidades empíricas para el gráfico P-P
prob_gev <- cdfgev(ordered_pmax, gev_param)


# --- Ajuste Log-Pearson III ---

# Transformar los datos al espacio logarítmico
log_pmax <- log(pmax)
# Calcular L-momentos muestrales
l_mom_lp3 <- samlmu(log_pmax)
# Calcular parámetros de la distribución P3 utilizando L-momentos muestrales
lp3_param <- pelpe3(l_mom_lp3)
# Calcular cuantiles empíricos para el gráfico Q-Q y devolver los resultados al
# espacio original
quant_lp3 <- exp(quape3(emp_prob, lp3_param))
# Calcular probabilidades empíricas para el gráfico P-P
prob_lp3 <- cdfpe3(log(ordered_pmax), lp3_param)

# --- Gráficos ---

# Tema común para los gráficos
theme_custom <- theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.grid.minor = element_blank()
  )

# 1. Gráfico Q-Q para GEV
p1 <- ggplot(data = data.frame(x = quant_gev, y = pmax_ordenado), aes(x = x, y = y)) +
  geom_point(color = "#1F4E79", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "#4D4D4D", linetype = "dashed", size = 1) +
  labs(x = "Cuantiles teóricos", y = "Cuantiles empíricos", title = NULL) +
  theme_custom

ggsave("file_path/gev_qq_plot.png", plot = p1, width = 10, height = 6, dpi = 300)

# 2. Gráfico Q-Q para LP3
p2 <- ggplot(data = data.frame(x = quant_lp3, y = pmax_ordenado), aes(x = x, y = y)) +
  geom_point(color = "#355E3F", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "#4D4D4D", linetype = "dashed", size = 1) +
  labs(x = "Cuantiles teóricos", y = "Cuantiles empíricos", title = NULL) +
  theme_custom

ggsave("file_path/lp3_qq_plot.png", plot = p2, width = 10, height = 6, dpi = 300)

# 3. Gráfico P-P para GEV
p3 <- ggplot(data = data.frame(x = prob_gev, y = prob_emp), aes(x = x, y = y)) +
  geom_point(color = "#1F4E79", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "#4D4D4D", linetype = "dashed", size = 1) +
  labs(x = "Probabilidades teóricas", y = "Probabilidades empíricas", title = NULL) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_custom

ggsave("file_path/gev_pp_plot.png", plot = p3, width = 10, height = 6, dpi = 300)

# 4. Gráfico P-P para LP3
p4 <- ggplot(data = data.frame(x = prob_lp3, y = prob_emp), aes(x = x, y = y)) +
  geom_point(color = "#355E3F", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "#4D4D4D", linetype = "dashed", size = 1) +
  labs(x = "Probabilidades teóricas", y = "Probabilidades empíricas", title = NULL) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_custom

ggsave("file_path/lp3_pp_plot.png", plot = p4, width = 10, height = 6, dpi = 300)

# Mensaje de confirmación
cat("Gráficos guardados.")