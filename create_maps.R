# Representación espacial de las precipitaciones máximas diarias estimadas en la
# Comunidad Valenciana para períodos de retorno de 200 y 500 años.
# ============================================================================
# Descripción:
# Este script genera dos gráficos que representan las precipitaciones máximas 
# diarias estimadas en la Comunidad Valenciana para períodos de retorno de 
# 200 y 500 años. Cada gráfico incluye dos mapas de dicha región dispuestos en
# paralelo, uno por cada distribución de probabilidad empleada: la Distribución
# de Valores Extremos Generalizada (GEV) y la log-Pearson Tipo III (LP3).
#
# A cada municipio se le asigna un valor estimado de precipitación máxima diaria 
# (en mm), determinado mediante la estación meteorológica más cercana usando el 
# método de unión espacial `st_nearest_feature` (basado en distancia euclídea). 
# En los casos en que no se identifica una estación próxima, se imputa el valor 
# medio de las precipitaciones registradas por el conjunto de estaciones
# disponibles.
#
# Los mapas muestran los valores asignados mediante un gradiente de color 
# (de azul claro a azul oscuro), y las ubicaciones de las estaciones
# meteorológicas se indican con puntos rojos.
#
# Entradas:
# - ca_municipios_20240505.shp: Archivo shapefile con las geometrías de los
#   municipios de la Comunidad Valenciana, transformado al sistema de referencia
#   WGS84 (EPSG:4326).
# - map_data.csv: Archivo CSV con los datos de la ubicación de las estaciones
#   meteorólogicas y las precipitaciones máximas probables asociadas. Concreta-
#   mente, debe contener los siguientes campos: 
#     -"Longitude": Longitud de la estación en formato de grados, minutos y
#                    segundos (e.g., "010320W" para 1°03'20" Oeste).
#     -"Latitude": Latitud de la estación en formato de grados, minutos y
#                  segundos (e.g., "390334N" para 39°03'34" Norte).
#     -"Rainfall": Precipitación máxima probable estimada (en mm).
#     -"Return_Period": Período de retorno de la estimación (valores: "200" o
#                        "500").
#     -"Distribution": Distribución utilizada (valores: "GEV" o "LP3").
#  
#
# Salidas:
# - Archivos PNG con los siguientes gráficos:
#   - map_200.png: Mapas para un período de retorno de 200 años.
#   - map_500.png:Mapas para un período de retorno de 500 años.
#   Ambos se guardan automáticamente en la ruta indicada.
#
# Requisitos:
# - Paquetes de R necesarios:
#   - ggplot2: para la generación de los mapas.
#   - sf: para la manipulación de datos espaciales.
#   - dplyr: para el procesamiento de datos.
#   - patchwork: para combinar múltiples mapas en un solo gráfico.
#
# Uso:
# 1. Asegúrate de que los paquetes necesarios están instalados.
# 2. Verifica que el shapefile y el archivo CSV estén disponibles en las rutas
#    correspondientes.
# 3. Ejecuta el script en R.
# 4. Los mapas se generarán y se guardarán automáticamente.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 30 de abril de 2025
# ============================================================================

# Cargar e instalar las librerías necesarias
if (!require(sf)) {
  install.packages("sf")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(patchwork)) {
  install.packages("patchwork")
}
library(sf)        
library(ggplot2)  
library(dplyr)     
library(patchwork) 

# 1. Cargar el shapefile de municipios de la Comunidad Valenciana
shapefile_path <- "file_path/data/ca_municipios_20240505.shp"
municipalities <- st_read(shapefile_path)

# Asegurarse de que el shapefile esté en WGS84 (EPSG:4326)
municipalities <- st_transform(municipalities, crs = 4326)

# Reparar geometrías inválidas
municipalities <- st_make_valid(municipalities)

# 2. Función para convertir coordenadas a grados decimales
convert_coordinates <- function(longitude, latitude) {
  lon_num <- substr(longitude, 1, nchar(longitude) - 1)
  lat_num <- substr(latitude, 1, nchar(latitude) - 1)
  
  lon_degrees <- as.numeric(substr(lon_num, 1, 2)) + 
    as.numeric(substr(lon_num, 3, 4)) / 60 + 
    as.numeric(substr(lon_num, 5, 6)) / 3600
  
  lat_degrees <- as.numeric(substr(lat_num, 1, 2)) + 
    as.numeric(substr(lat_num, 3, 4)) / 60 + 
    as.numeric(substr(lat_num, 5, 6)) / 3600
  
  if (grepl("W", longitude)) lon_degrees <- -lon_degrees
  if (grepl("S", latitude)) lat_degrees <- -lat_degrees
  
  return(c(lon_degrees, lat_degrees))
}

# 3. Función para generar un mapa
create_map <- function(data, lower_limit, upper_limit, breaks, labels) {
  # Convertir coordenadas
  coords <- mapply(convert_coordinates, data$Longitude, data$Latitude, SIMPLIFY = FALSE)
  longitudes <- sapply(coords, `[`, 1)
  latitudes <- sapply(coords, `[`, 2)
  rainfall <- data$Rainfall
  
  # Crear objeto sf para estaciones
  stations <- st_as_sf(data.frame(
    Rainfall = as.numeric(rainfall),
    geometry = st_sfc(lapply(seq_along(longitudes), function(i) st_point(c(longitudes[i], latitudes[i]))), crs = 4326)
  ))
  
  # Asignar precipitaciones a municipios
  municipalities_rainfall <- st_join(municipalities, stations, join = st_nearest_feature)
  
  # Imputar valores NA con la media
  media_precip <- mean(stations$Rainfall, na.rm = TRUE)
  municipalities_rainfall <- municipalities_rainfall %>%
    mutate(Rainfall = ifelse(is.na(Rainfall), media_precip, Rainfall))
  
  # Generar el mapa
  p <- ggplot() +
    geom_sf(data = municipalities_rainfall, aes(fill = Rainfall), color = "black", size = 0.2) +
    geom_sf(data = stations, color = "red", size = 1.2, alpha = 0.7) +
    scale_fill_steps(low = "#E6F0FF", high = "#08306B", 
                     breaks = breaks,
                     labels = labels,
                     limits = c(lower_limit, upper_limit),
                     name = "",
                     guide = guide_colourbar(direction = "horizontal", 
                                             title.position = "top", 
                                             barwidth = 15,
                                             barheight = 0.5)) +
    labs() +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank(),
      plot.caption = element_blank(),
      panel.grid = element_blank()
    )
  
  return(p)
}

# 4. Leer los datos del CSV
precipitation_data <- read.csv("file_path/data/map_data.csv")

# 5. Preparar datos para T=500 años
data_500_gev <- precipitation_data %>% filter(Return_Period == "500" & Distribution == "GEV")
data_500_lp3 <- precipitation_data %>% filter(Return_Period == "500" & Distribution == "LP3")

# Calcular límites y breaks para T=500 años
all_rainfall_500 <- precipitation_data %>% filter(Return_Period == "500") %>% pull(Rainfall)
lower_limit_500 <- floor(min(all_rainfall_500, na.rm = TRUE) / 10) * 10  
upper_limit_500 <- ceiling(max(all_rainfall_500, na.rm = TRUE) / 10) * 10  

# Definir breaks uniformes para T=500
total_range_500 <- upper_limit_500 - lower_limit_500  
num_intervals_500 <- 6  
interval_500 <- total_range_500 / num_intervals_500 
breaks_500 <- seq(lower_limit_500, upper_limit_500, by = interval_500)
breaks_500 <- round(breaks_500)  
breaks_500[length(breaks_500)] <- upper_limit_500  
labels_500 <- as.character(breaks_500)

# 7. Generar los mapas para T=500 años
p1_500 <- create_map(data_500_gev, lower_limit_500, upper_limit_500, breaks_500, labels_500)
p2_500 <- create_map(data_500_lp3, lower_limit_500, upper_limit_500, breaks_500, labels_500)

# Combinar los mapas para T=500 años
combined_plot_500 <- (p1_500 + plot_spacer() + p2_500) +
  plot_layout(guides = "collect", widths = c(1, 0.0005, 1)) &
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

# 8. Preparar datos para T=200 años
data_200_gev <- precipitation_data %>% filter(Return_Period == "200" & Distribution == "GEV")
data_200_lp3 <- precipitation_data %>% filter(Return_Period == "200" & Distribution == "LP3")

# Calcular límites y breaks para T=200 años
all_rainfall_200 <- precipitation_data %>% filter(Return_Period == "200") %>% pull(Rainfall)
lower_limit_200 <- floor(min(all_rainfall_200, na.rm = TRUE) / 10) * 10  
upper_limit_200 <- ceiling(max(all_rainfall_200, na.rm = TRUE) / 10) * 10  

# Definir breaks uniformes para T=200
total_range_200 <- upper_limit_200 - lower_limit_200  
num_intervals_200 <- 6  
interval_200 <- total_range_200 / num_intervals_200 
breaks_200 <- seq(lower_limit_200, upper_limit_200, by = interval_200)
breaks_200 <- round(breaks_200)  
breaks_200[length(breaks_200)] <- upper_limit_200  
labels_200 <- as.character(breaks_200)

# 9. Generar los mapas para T=200 años
p1_200 <- create_map(data_200_gev, lower_limit_200, upper_limit_200, breaks_200, labels_200)
p2_200 <- create_map(data_200_lp3, lower_limit_200, upper_limit_200, breaks_200, labels_200)

# Combinar los mapas para T=200 años
combined_plot_200 <- (p1_200 + plot_spacer() + p2_200) +
  plot_layout(guides = "collect", widths = c(1, 0.0005, 1)) &
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

# 10. Mostrar los gráficos
print(combined_plot_500)
print(combined_plot_200)

# 11. Guardar los gráficos
ggsave("file_path/map_500.png", combined_plot_500, width = 9, height = 6, dpi = 300)
ggsave("file_path/map_200.png", combined_plot_200, width = 9, height = 6, dpi = 300)