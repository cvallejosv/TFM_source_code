# Comparación entre la función de distribución empírica de datos simulados a
# partir de una GEV y la función de distribución teórica de dicha distribución.
# =========================================================================
# Descripción:
# Este script genera un gráfico que compara la función de distribución empírica 
# (ECDF) de una muestra de 100 observaciones simuladas a partir de una
# distribución GEV con parámetros predefinidos con la función de distribución
# teórica (CDF) de la misma distribución. 
#
# Entradas:
# - No se requieren entradas externas. El script simula internamente los datos 
#   a partir de la distribución GEV con parámetros predefinidos.
#
# Salidas:
# - Un archivo PNG llamado ecdf_gev.png con el gráfico comparativo entre:
#   - La ECDF de los datos simulados.
#   - La CDF teórica de la distribución GEV.
#   El archivo se guarda automáticamente en la ruta especificada.
#
# Requisitos:
# - Paquetes de R:
#   - ggplot2: para crear y personalizar el gráfico.
#   - lmom: para simular datos y calcular la función de distribución teórica.
#
# Uso:
# 1. Asegúrate de que los paquetes ggplot2 y lmom estén instalados.
# 2. Ejecuta el script en R.
# 3. El archivo PNG se generará en la ruta indicada.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 30 de abril de 2025
# =========================================================================


# Cargar e instalar las librerías necesarias
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(ggplot2)) {
  install.packages("lmom")
}

# Fijar una semilla para reproducibilidad
set.seed(123)

# Definir los parámetros de la distribución GEV
gev_params <- c(loc = 50, scale = 15, shape = 0.2)

# Generar 100 datos simulados a partir de una distribución GEV
n <- 100
sim_data <- quagev(runif(n), gev_params)  
datos <- data.frame(data = sim_data)  

# Crear un rango de valores para graficar la CDF teórica de GEV
min_val <- min(datos$data)
max_val <- max(datos$data)
x_vals <- seq(min_val, max_val * 1.2, length.out = 100)

# Calcular la CDF teórica para GEV
cdf_gev <- cdfgev(x_vals, gev_params)

# Crear un data frame con los valores de la CDF
cdf_data <- data.frame(
  x = x_vals,
  cdf = cdf_gev,
  distribution = "GEV"
)

# Crear el gráfico: ECDF + CDF de GEV
p <- ggplot() +
  # ECDF de los datos simulados
  stat_ecdf(data = datos, aes(x = data, color = "FDE"), geom = "step", size = 1) +
  # CDF teórica de GEV
  geom_line(data = cdf_data, aes(x = x, y = cdf, color = distribution), size = 1) +
  labs(x = "Valor simulado (mm)", 
       y = "Probabilidad acumulada") +
  scale_x_continuous(breaks = seq(0, ceiling(max(x_vals) / 10) * 10, by = 10),
                     limits = c(0, ceiling(max(x_vals) / 10) * 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = c("FDE" = "black", "GEV" = "#1F4E79"),
                     name = "Curva") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

# Mostrar el gráfico
print(p)

# Guardar el gráfico en un archivo
ggsave("file_path/ecdf_gev.png", plot = p, width = 8, height = 6, dpi = 300)