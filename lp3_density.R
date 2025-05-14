# Visualización de funciones de densidad de la distribución Log-Pearson Tipo III 
# con ξ = 0 y distintas combinaciones de los parámetros α y β.
# =========================================================================
# Descripción:
# Este script genera dos gráficos que comparan las funciones de densidad de la 
# distribución Log-Pearson Tipo III (LP3) bajo el supuesto de ξ = 0, considerando 
# diferentes combinaciones de los parámetros α (forma) y β (escala):
# 
# - Gráfico 1: Funciones de densidad con β > 0 (escala positiva), para tres 
#   configuraciones distintas de α y β.
# - Gráfico 2: Funciones de densidad con β < 0 (escala negativa), también con 
#   tres configuraciones de α y β.
# 
# Los gráficos se generan con el paquete ggplot2, representando las densidades 
# sobre rangos específicos de valores ajustados según el signo de β, y 
# diferenciando cada curva con distintos estilos de línea. Las salidas se 
# guardan como archivos PNG.
#
# Entradas:
# - No se requieren archivos externos. El script define internamente:
#     - Las combinaciones de parámetros α y β.
#     - Los rangos de valores z adecuados para cada caso.
#
# Salidas:
# - Dos archivos PNG:
#   - lp3_density_positive_beta.png: Funciones de densidad para β > 0.
#   - lp3_density_negative_beta.png: Funciones de densidad para β < 0.
#   Ambos se guardan en la ruta especificada.
#
# Requisitos:
# - Paquetes de R:
#   - PearsonDS: para calcular las densidades de la distribución Pearson Tipo III.
#   - ggplot2: para la visualización de los gráficos.
#
# Uso:
# 1. Asegúrate de tener instalados los paquetes PearsonDS y ggplot2.
# 2. Ejecuta el script en R.
# 3. Los gráficos se generarán automáticamente y se guardarán en la ruta indicada.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 30 de abril de 2025
# =========================================================================

# Instalar y cargar los paquetes necesarios
if (!require(PearsonDS)) {
  install.packages("PearsonDS")
  library(PearsonDS)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# --- Gráfico 1: scale positivo (β > 0) ---

# Definir tres configuraciones de parámetros (β positivo)
params1_1 <- c(shape = 3, location = 0, scale = 0.2)
params1_2 <- c(shape = 4, location = 0, scale = 0.3)
params1_3 <- c(shape = 5, location = 0, scale = 0.4)

# Vector y para el caso positivo: exp(ξ) < x < ∞
y1 <- seq(1, 10, length.out = 1000)  

# Aplicar transformación logarítmica al dominio para utilizar la función de
# densidad de la distribución Pearson tipo III.
x1 <- log(y1)

# Calcular las densidades de la Pearson tipo III para cada configuración
dens_pearson1_1 <- dpearsonIII(x1, params = params1_1, log = FALSE)
dens_pearson1_2 <- dpearsonIII(x1, params = params1_2, log = FALSE)
dens_pearson1_3 <- dpearsonIII(x1, params = params1_3, log = FALSE)

# Calcular las densidades de la Log-Pearson tipo III: f(y) = f(x) * (1/y)
dens_logpearson1_1 <- dens_pearson1_1 / y1
dens_logpearson1_2 <- dens_pearson1_2 / y1
dens_logpearson1_3 <- dens_pearson1_3 / y1

# Crear data frame para el primer gráfico
data1 <- data.frame(
  y = rep(y1, 3),
  density = c(dens_logpearson1_1, dens_logpearson1_2, dens_logpearson1_3),
  type = rep(c("α=3, β=0.2", "α=4, β=0.3","α=5, β=0.4"), each = length(y1))
)


# Reordenar los niveles del factor para el orden en la leyenda
data1$type <- factor(data1$type, levels = c("α=3, β=0.2", "α=4, β=0.3", "α=5, β=0.4"))

# Graficar las densidades para β > 0
plot1 <- ggplot(data1, aes(x = y, y = density, linetype = type)) +
  geom_line(color = "black", linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  labs(x = "x", y = "Densidad") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 12, margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, margin = margin(r = 15)),
    axis.text = element_text(size = 10)
  )

# Guardar el primer gráfico
ggsave("file_path/lp3_density_positive_beta.png", plot = plot1, width = 6, height = 4, dpi = 300)

# --- Gráfico 2: scale negativo (β < 0) ---

# Definir tres configuraciones de parámetros (β negativo)
params2_1 <- c(shape = 3, location = 0, scale = -0.2)
params2_2 <- c(shape = 4, location = 0, scale = -0.3)
params2_3 <- c(shape = 5, location = 0, scale = -0.4)


# Vector y para el caso negativo: 0 < x < exp(ξ)
y2 <- seq(0.001, 1, length.out = 1000)

# Aplicar transformación logarítmica al dominio para utilizar la función de
# densidad de la distribución Pearson tipo III.
x2 <- log(y2)

# Calcular las densidades de la Pearson tipo III para cada configuración
dens_pearson2_1 <- dpearsonIII(x2, params = params2_1, log = FALSE)
dens_pearson2_2 <- dpearsonIII(x2, params = params2_2, log = FALSE)
dens_pearson2_3 <- dpearsonIII(x2, params = params2_3, log = FALSE)

# Calcularlas densidades de la Log-Pearson tipo III: f(y) = f(x) * (1/y)
dens_logpearson2_1 <- dens_pearson2_1 / y2
dens_logpearson2_2 <- dens_pearson2_2 / y2
dens_logpearson2_3 <- dens_pearson2_3 / y2

# Crear data frame para el segundo gráfico
data2 <- data.frame(
  y = rep(y2, 3),
  density = c(dens_logpearson2_1, dens_logpearson2_2, dens_logpearson2_3),
  type = rep(c("α=3, β=-0.2", "α=4, β=-0.3", "α=5, β=-0.4"), each = length(y2))
)

# Reordenar los niveles del factor para el orden en la leyenda
data2$type <- factor(data2$type, levels = c("α=3, β=-0.2", "α=4, β=-0.3", "α=5, β=-0.4"))

# Graficar las densidades para β < 0
plot2 <- ggplot(data2, aes(x = y, y = density, linetype = type)) +
  geom_line(color = "black", linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  labs(x = "x", y = "Densidad") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 12, margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, margin = margin(r = 15)),
    axis.text = element_text(size = 10)
  )

# Guardar el segundo gráfico
ggsave("file_path/lp3_density_negative_beta.png", plot = plot2, width = 6, height = 4, dpi = 300)

# Mostrar ambos gráficos
print(plot1)
print(plot2)