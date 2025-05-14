# Visualización de funciones de densidad de la distribución GEV para distintos 
# valores del parámetro de forma ξ.
# =========================================================================
# Descripción:
# Este script genera un gráfico comparativo de las funciones de densidad de la 
# distribución de Valores Extremos Generalizada (GEV) para tres valores distintos 
# del parámetro de forma ξ:
#   - Gumbel (ξ = 0)
#   - Fréchet (ξ = 0.5)
#   - Weibull (ξ = -0.5)
# 
# El gráfico se crea utilizando el paquete ggplot2, representando las densidades 
# sobre un rango definido de valores z y diferenciando cada caso mediante estilos 
# de línea. El resultado se guarda como imagen en formato PNG.
#
# Entradas:
# - No se requieren entradas externas. El script define internamente:
#     - El rango de valores z.
#     - Los parámetros de la distribución.
#
# Salidas:
# - Un archivo PNG llamado gev_density.png con el gráfico comparativo de las
#   funciones de densidad, guardado en la ruta especificada.
#
# Requisitos:
# - Paquetes de R:
#   - evd: para calcular las densidades de la distribución GEV.
#   - ggplot2: para generar el gráfico.
#
# Uso:
# 1. Verifica que los paquetes evd y ggplot2 estén instalados.
# 2. Ejecuta el script en R.
# 3. El gráfico se generará y guardará automáticamente en la ruta definida.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 30 de abril de 2025
# =========================================================================


# Instalar los paquetes necesarios si no están instalados
if (!require(evd)) install.packages("evd")
if (!require(ggplot2)) install.packages("ggplot2")
library(evd)
library(ggplot2)

# Definir el rango de valores para z
z <- seq(-3, 6, length.out = 1000)

# Calcular las densidades para los tres casos
# Gumbel: xi = 0
# Frechet: xi > 0
# Weibull: xi < 0
data <- data.frame(
  z = rep(z, 3),
  density = c(
    dgev(z, loc = 0, scale = 1, shape = 0),           # Gumbel (xi = 0)
    dgev(z, loc = 0, scale = 1, shape = 0.5),         # Frechet (xi = 0.5)
    dgev(z, loc = 0, scale = 1, shape = -0.5)         # Weibull (xi = -0.5)
  ),
  type = rep(c("Gumbel (ξ = 0)", "Fréchet (ξ = 0.5)", "Weibull (ξ = -0.5)"), each = length(z))
)

# Reordenar los niveles del factor
data$type <- factor(data$type, levels = c("Gumbel (ξ = 0)", "Fréchet (ξ = 0.5)", "Weibull (ξ = -0.5)"))

# Crear el gráfico con ggplot2
p <- ggplot(data, aes(x = z, y = density, linetype = type)) +
  geom_line(color = "black", linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  labs(x = "x", y = "Densidad") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)), 
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
  )

# Mostrar el gráfico
print(p)

# Guardar el gráfico en la ruta especificada
ggsave("file_path/gev_density.png", plot = p, width = 6, height = 4, dpi = 300)