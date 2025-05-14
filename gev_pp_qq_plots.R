# Gráficos P–P y Q–Q para comparar datos simulados con una distribución GEV.
# =========================================================================
# Descripción:
# Este script genera dos gráficos que permiten comparar las probabilidades
# acumuladas y cuantiles teóricos de una distribución GEV con parámetros μ = 50,
# σ = 15 y ξ = 0,2 frente a las probabilidades y cuantiles empíricas obtenidos
# a partir de una muestra simulada de 100 observaciones generadas bajo la misma
# distribución.
#
# Los gráficos generados son:
# - Gráfico P–P (Probability–Probability): compara las probabilidades empíricas
#   de los datos simulados con las probabilidades teóricas de la GEV.
# - Gráfico Q–Q (Quantile–Quantile): compara los cuantiles empíricos con los
#   cuantiles teóricos de la distribución GEV.
#
#
# Entradas:
# - No se requieren entradas externas. La simulación de datos se realiza 
#.  internamente.
#
# Salidas:
# - Dos archivos PNG:
#   - gev_pp_plot.png: Gráfico P–P.
#   - gev_11_plot.png: Gráfico Q–Q.
#   Ambos se guardan en la ruta especificada.
#
# Requisitos:
# - Paquetes de R:
#   - ggplot2: para la generación y personalización de gráficos.
#   - lmom: para simular los datos y calcular cuantiles y probabilidades
#      teóricas.
#
# Uso:
# 1. Asegúrate de tener instalados los paquetes ggplot2 y lmom.
# 2. Ejecuta el script en R.
# 3. Los gráficos se generarán y guardarán automáticamente en la ruta
#.   especificada.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 30 de abril de 2025
# =========================================================================


# Cargar e instalar las librerías necesarias
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(lmom)) {
  install.packages("lmom")
}
library(ggplot2)  
library(lmom)     

# Fijar una semilla para reproducibilidad
set.seed(123)

# Definir los parámetros de la distribución GEV
gev_params <- c(loc = 50, scale = 15, shape = 0.2)

# Generar 100 datos simulados a partir de una distribución GEV
n <- 100
sim_data <- quagev(runif(n), gev_params)  
data <- data.frame(gev_data = sim_data)   

# Ordenar los datos
ordered_data <- sort(data$gev_data)

# Calcular las probabilidades empíricas
emp_prob <- (1:n) / (n + 1)

# --- Gráfico P-P ---

# Calcular las probabilidades teóricas para el gráfico P-P
gev_prob <- cdfgev(ordered_data, gev_params)

# Crear un data frame para graficar el P-P
pp_data <- data.frame(
  teorical_prob = gev_prob,
  empirical_prob = emp_prob
)

# Crear el gráfico P-P
p_pp <- ggplot(data = pp_data, aes(x = teorical_prob, y = empirical_prob)) +
  geom_point(color = "#1F4E79", size = 2) +  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) +  
  labs(x = "Probabilidades teóricas (GEV)",
       y = "Probabilidades empíricas") +  
  scale_x_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +  
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +  
  theme_minimal(base_size = 14) + 
  theme(
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 14),   
    axis.title.x = element_text(margin = margin(t = 15)),  
    axis.title.y = element_text(margin = margin(r = 15)),  
    panel.grid.minor = element_blank(),  
    legend.position = "none"
  )

# --- Gráfico Q-Q ---

# Calcular los cuantiles teóricos de la distribución GEV para el gráfico Q-Q
gev_quant <- quagev(emp_prob, gev_params)

# Crear un data frame para graficar el Q-Q
qq_data <- data.frame(
  teorical_quant = gev_quant,
  empirical_quant = ordered_data
)

# Determinar los límites de los ejes
lim_inf <- min(min(gev_quant), min(ordered_data))
lim_sup <- max(max(gev_quant), max(ordered_data))
lim_inf <- floor(lim_inf / 10) * 10 
lim_sup <- ceiling(lim_sup / 10) * 10  

# Crear el gráfico Q-Q
p_qq <- ggplot(data = qq_data, aes(x = teorical_quant, y = empirical_quant)) +
  geom_point(color = "#1F4E79", size = 2) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) +  
  labs(x = "Cuantiles teóricos (GEV)",
       y = "Cuantiles empíricos") +  
  scale_x_continuous(breaks = seq(lim_inf, lim_sup, by = 10), limits = c(lim_inf, lim_sup)) +  
  scale_y_continuous(breaks = seq(lim_inf, lim_sup, by = 10), limits = c(lim_inf, lim_sup)) +  
  theme_minimal(base_size = 14) + 
  theme(
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 14),   
    axis.title.x = element_text(margin = margin(t = 15)),  
    axis.title.y = element_text(margin = margin(r = 15)),  
    panel.grid.minor = element_blank(),  
    legend.position = "none" 
  )

# Mostrar ambos gráficos
print(p_pp)
print(p_qq)

# Guardar los gráficos en archivos
ggsave("file_pàth/gev_pp_plot.png", plot = p_pp, width = 8, height = 6, dpi = 300)
ggsave("file_pàth/gev_qq_plot.png", plot = p_qq, width = 8, height = 6, dpi = 300)