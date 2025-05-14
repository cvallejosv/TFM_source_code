# Visualización de las precipitaciones máximas diarias anuales en la estación 
# meteorológica de Valencia (8416).
# =========================================================================
# Descripción:
# Este script genera dos gráficos para analizar las precipitaciones máximas
# diarias anuales registradas en la estación meteorológica de Valencia (8416):
# 
# - Gráfico (a): Serie temporal que muestra la evolución de las precipitaciones 
#   máximas diarias a lo largo de los años, incluyendo una línea de referencia 
#   en 150 mm.
# - Gráfico (b): Histograma de frecuencias relativas de las precipitaciones 
#   máximas diarias, representado en orientación horizontal y sin relleno.
# 
# Ambos gráficos se generan utilizando el paquete ggplot2 y se exportan como 
# imágenes PNG.
#
# Entradas:
# - CVppmax24_8416_anual_valencia.csv: Archivo CSV con los datos de
#   precipitaciones. El archivo debe contener al menos las siguientes columnas:
#   - AÑO: año de la medición.
#   - PMAX77: precipitación máxima diaria anual (en mm).
#
# Salidas:
# - Dos archivos PNG con los gráficos generados:
#   - temporal_series.png: Gráfico de la serie temporal.
#   - histogram.png: Histograma de frecuencias relativas.
#   Ambos se guardarán en la ruta especificada.
#
# Requisitos:
# - Paquetes de R:
#   - ggplot2: para la generación de gráficos.
#
# Uso:
# 1. Verifica que el archivo CSV se encuentre en la ruta indicada.
# 2. Ejecuta el script en R.
# 3. Los gráficos se generarán y se guardarán automáticamente en la ubicación 
#    especificada.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 30 de abril de 2025
# =========================================================================




# Instalar y cargar los paquetes necesarios
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Obtener datos
datos <- read.csv("path/data/CV_pmax/CVppmax24_8416_anual_valencia.csv")

# Definir umbral
x_tau <- 150

# Gráfico (a): Serie temporal de precipitaciones máximas diarias anuales
p1 <- ggplot(datos, aes(x = AÑO, y = PMAX77)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = x_tau, linetype = "dashed", color = "black", size = 1) +
  labs(x = "Año", y = "Precipitación máxima diaria (mm)") +
  scale_x_continuous(breaks = seq(min(datos$AÑO), max(datos$AÑO), by = 10)) +
  scale_y_continuous(breaks = seq(0, max(datos$PMAX77, na.rm = TRUE), by = 50)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14),  
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.grid.minor = element_blank()
  )

# Gráfico (b): Histograma de frecuencia relativa
p2 <- ggplot(datos, aes(y = PMAX77)) +
  geom_histogram(aes(x = ..count../sum(..count..)), 
                 bins = 20, 
                 fill = NA, color = "black") +
  labs(x = "Frecuencia relativa", y = "Precipitación máxima diaria (mm)") +
  scale_x_continuous(breaks = seq(0, max(table(cut(datos$PMAX77, breaks = 20))) / nrow(datos), by = 0.05)) +
  scale_y_continuous(breaks = seq(0, max(datos$PMAX77, na.rm = TRUE), by = 50)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 14),   
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.grid.minor = element_blank()
  )

# Guardar los gráficos
ggsave("file_path/temporal_series.png", 
       plot = p1, width = 10, height = 6, dpi = 300)
ggsave("file_path/histogram.png", 
       plot = p2, width = 10, height = 6, dpi = 300)

# Mensaje de confirmación
cat("Gráficos guardados.\n")