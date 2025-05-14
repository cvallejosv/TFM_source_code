# Estimación de precipitaciones máximas diarias para diferentes períodos de
# retorno en distintas estaciones meteorológicas de la Comunidad Valenciana
# utilizando las distribuciones GEV y log-Pearson Tipo III.
# =========================================================================
# Descripción:
# Este script calcula las precipitaciones máximas diarias probables para
# diversos períodos de retorno (5, 10, 25, 50, 100, 200 y 500 años) en una serie
# de estaciones meteorológicas de la Comunidad Valenciana. Para ello, se utiliza 
# el ajuste de los datos empíricos a la distribución de Valores Extremos
# Generalizada (GEV) y la distribución log-Pearson Tipo III (LP3). Para la
# estimación de los parámetros de las distribuciones se utiliza el método de los
# L-momentos. Además, se evalúa la bondad del ajuste de los datos empíricos a
# ambas distribuciones mediante la prueba de Kolmogórov-Smirnov (K-S), empleando
# simulaciones Monte Carlo. Los resultados se presentan en una tabla generada en
# formato LaTeX.
#
# Entradas:
# - CV_pmax: carpeta que contiene los archivos CSV correspondientes a cada
#   estación meteorológica.
#   Cada archivo debe incluir dos columnas:
#     - INDICATIVO: identificador climatológico de la estación.
#     - PMAX77: precipitación máxima diaria anual (en mm).
#
# Salidas:
# - pmax_table.tex: Un archivo en formato LaTeX con una tabla que incluye:
#   - Indicativo de la estación.
#   - Distribución empleada (GEV o LP3).
#   - Valores estimados de precipitación para los distintos períodos de retorno.
#   - P-valor de la prueba K-S.
#
# Requisitos:
# - Paquetes de R:
#   - lmom: cálculo de L-momentos muestrales.
#   - readr: lectura de archivos CSV.
#   - stats: ejecución de la prueba K-S.
#
# Uso:
# 1. Asegúrate de que los archivos CSV estén ubicados en la carpeta especificada.
# 2. Ejecuta este script en R.
# 3. El archivo en formato LaTeX se generará en la ruta de salida indicada.
#
# Autor: Cristina Vallejos Valdor
# Fecha: 30 de abril de 2025
# =========================================================================

# 1. Cargar e instalar los paquetes necesarios
if (!require(lmom)) install.packages("lmom")
if (!require(readr)) install.packages("readr")
if (!require(stats)) install.packages("stats")
library(lmom)      
library(readr)    
library(stats)     

# 2. Definir la carpeta y listar los archivos CSV
folder <- "folder_path/data/CV_pmax"
csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

# 3. Definir los períodos de retorno y las probabilidades de no excedencia
return_periods <- c(5, 10, 25, 50, 100, 200, 500)
prob_no_exced <- 1 - 1/return_periods

# 4. Almacenar resultados
results <- data.frame()

# 5. Función para procesar cada archivo
process_file <- function(file) {
  # Fijar la semilla para asegurar reproducibilidad en todas las simulaciones
  set.seed(123) 
  
  # Leer el archivo CSV
  datos <- read_csv(file, show_col_types = FALSE)
  
  # Extraer indicativo del campo INDICATIVO
  code <- unique(datos$INDICATIVO)[1]
  
  # Extraer la columna PMAX77 y limpiar datos
  pmax <- datos$PMAX77
  pmax <- pmax[!is.na(pmax)]
  
  # --- Ajuste GEV ---
  
  # Calcular L-momentos muestrales
  l_mom_gev <- samlmu(pmax)
  # Estimar parámetros de la distribución GEV a partir de los L-momentos
  # muestrales
  gev_param <- pelgev(l_mom_gev)
  
  # Calcular niveles de retorno 
  return_levels_gev <- quagev(prob_no_exced, gev_param)
  
  # Prueba KS para GEV usando simulaciones Monte Carlo
  # Calcular el estadístico K-S observado
  ks_gev_observed <- ks.test(pmax, function(x) cdfgev(x, gev_param))$statistic
  
  # Simulaciones Monte Carlo para el cálculo del p-valor (GEV)
  n_sim <- 10000 # Número de simulaciones 
  n <- length(pmax)
  ks_gev_sim <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    sim_data <- quagev(runif(n), gev_param)
    sim_lmoms <- samlmu(sim_data)
    sim_params <- pelgev(sim_lmoms)
    ks_gev_sim[i] <- ks.test(sim_data, function(x) cdfgev(x, sim_params))$statistic
  }
  
  ks_gev_pvalue <- mean(ks_gev_sim >= ks_gev_observed)
  
  # --- Ajuste Log-Pearson Tipo III ---
  
  # Aplicar transformación logarítmica a los datos
  log_pmax <- log(pmax)
  # Calcular L-momentos muestrales
  l_mom_lp3 <- samlmu(log_pmax)
  # Estimar parámetros de la distribución Pearson tipo III a partir de los 
  # L-momentos muestrales
  lp3_param <- pelpe3(l_mom_lp3)
  # Calcular niveles de retorno 
  return_levels_log <- quape3(prob_no_exced, lp3_param)
  # Transformar los niveles de retorno al espacio original
  return_levels_lp3 <- exp(return_levels_log)
      
  # Prueba KS para LP3 usando simulaciones Monte Carlo
  # Calcular el estadístico K-S observado
  ks_lp3_observed <- ks.test(log_pmax, function(x) cdfpe3(x, lp3_param))$statistic
      
  # Simulaciones Monte Carlo para el cálculo del p-valor (LP3)
  ks_lp3_sim <- numeric(n_sim)
  for (i in 1:n_sim) {
    sim_log_data <- quape3(runif(n), lp3_param)
    sim_lmoms <- samlmu(sim_log_data)
    sim_params <- pelpe3(sim_lmoms)
    ks_lp3_sim[i] <- ks.test(sim_log_data, function(x) cdfpe3(x, sim_params))$statistic
  }
      
  ks_lp3_pvalue <- mean(ks_lp3_sim >= ks_lp3_observed)
  
  
  # Crear filas para la tabla (GEV)
  gev_row <- data.frame(
    Code = code,
    distribution = "GEV",
    T5 = return_levels_gev[1],
    T10 = return_levels_gev[2],
    T25 = return_levels_gev[3],
    T50 = return_levels_gev[4],
    T100 = return_levels_gev[5],
    T200 = return_levels_gev[6],
    T500 = return_levels_gev[7],
    KS_pvalor = ks_gev_pvalue
  )
  
  lp3_row <- data.frame(
    Code = "",
    distribution = "LP3",
    T5 = return_levels_lp3[1],
    T10 = return_levels_lp3[2],
    T25 = return_levels_lp3[3],
    T50 = return_levels_lp3[4],
    T100 = return_levels_lp3[5],
    T200 = return_levels_lp3[6],
    T500 = return_levels_lp3[7],
    KS_pvalor = ks_lp3_pvalue
  )
  
  # Combinar filas
  return(rbind(gev_row, lp3_row))
}

# 6. Procesar todos los archivos y recopilar resultados
for (file in csv_files) {
  tryCatch({
    result <- process_file(file)
    if (!is.null(result)) {
      results <- rbind(results, result)
    }
  }, error = function(e) {
    cat("\nError while processing", basename(file), ":", e$message, "\n")
  })
}

# 7. Generar la tabla en formato LaTeX
if (nrow(results) == 0) {
  cat("Results has not been generated.\n")
} else {
  latex_table <- paste(
    "\\scriptsize\n",
    "\\setlength{\\tabcolsep}{4pt}\n",
    "\\renewcommand{\\arraystretch}{1.1}\n",
    "\\begin{longtable}{|c|c|c|c|c|c|c|c|c|c|}\n",
    "\n",
    "\\hline\n",
    "\\multirow{2}{*}{\\centering\\textbf{\\footnotesize Indicativo}} & \\multirow{2}{*}{\\centering\\textbf{\\footnotesize Distribución}} & \\multicolumn{7}{c|}{\\centering\\textbf{\\footnotesize Precipitaciones 24 h (mm)}} & \\multirow{2}{*}{\\centering\\textbf{\\footnotesize \\( p \\)-valor (K-S)}} \\\\\n",
    "\\cline{3-9}\n",
    "& & \\textbf{\\shortstack{T=5 \\\\ años}} & \\textbf{\\shortstack{T=10 \\\\ años}} & \\textbf{\\shortstack{T=25 \\\\ años}} & \\textbf{\\shortstack{T=50 \\\\ años}} & \\textbf{\\shortstack{T=100 \\\\ años}} & \\textbf{\\shortstack{T=200 \\\\ años}} & \\textbf{\\shortstack{T=500 \\\\ años}} & \\\\\n",
    "\\hline\n",
    "\\endfirsthead\n",
    "\n",
    "\\hline\n",
    "\\multirow{2}{*}{\\centering\\textbf{\\footnotesize Indicativo}} & \\multirow{2}{*}{\\centering\\textbf{\\footnotesize Distribución}} & \\multicolumn{7}{c|}{\\centering\\textbf{\\footnotesize Precipitaciones 24 h (mm)}} & \\multirow{2}{*}{\\centering\\textbf{\\footnotesize \\( p \\)-valor (K-S)}} \\\\\n",
    "\\cline{3-9}\n",
    "& & \\textbf{\\shortstack{T=5 \\\\ años}} & \\textbf{\\shortstack{T=10 \\\\ años}} & \\textbf{\\shortstack{T=25 \\\\ años}} & \\textbf{\\shortstack{T=50 \\\\ años}} & \\textbf{\\shortstack{T=100 \\\\ años}} & \\textbf{\\shortstack{T=200 \\\\ años}} & \\textbf{\\shortstack{T=500 \\\\ años}} & \\\\\n",
    "\\hline\n",
    "\\endhead\n",
    "\n",
    "\\multicolumn{10}{r}{} \\\\\n",
    "\\endfoot\n",
    "\n",
    "\\endlastfoot\n",
    sep = "")
  
  # Añadir filas a la tabla
  for (i in seq(1, nrow(results), by = 2)) {
    gev_row <- results[i, ]
    lp3_row <- results[i + 1, ]
    
    # Formatear los valores de KS_pvalor para manejar NA
    ks_pvalor_gev <- if (is.na(gev_row$KS_pvalor)) "-" else sprintf("%.3f", gev_row$KS_pvalor)
    ks_pvalor_lp3 <- if (is.na(lp3_row$KS_pvalor)) "-" else sprintf("%.3f", lp3_row$KS_pvalor)
    
    latex_table <- paste(latex_table,
                         sprintf("\\multirow{2}{*}{\\centering %s} & %s & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f & %s \\\\\n",
                                 gev_row$Code, gev_row$distribution, gev_row$T5, gev_row$T10, gev_row$T25, gev_row$T50, gev_row$T100, gev_row$T200, gev_row$T500, ks_pvalor_gev),
                         "\\cline{2-10}\n",
                         sprintf(" & %s & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f & %s \\\\\n",
                                 lp3_row$distribution, lp3_row$T5, lp3_row$T10, lp3_row$T25, lp3_row$T50, lp3_row$T100, lp3_row$T200, lp3_row$T500, ks_pvalor_lp3),
                         ifelse(i + 1 < nrow(results), "\\hline\n", ""),
                         sep = "")
  }
  
  # Cerrar la tabla
  latex_table <- paste(latex_table,
                       "\\hline\n",
                       "\\caption{Precipitaciones máximas diarias estimadas para distintos periodos de retorno utilizando las distribuciones GEV y LP3.}\n",
                       "\\label{tab:precipitaciones_estimadas}\n",
                       "\\end{longtable}\n",
                       sep = "")
  
  # 8. Guardar la tabla en un archivo .tex
  write(latex_table, file = "file_path/pmax_table.tex")
  
  cat("LaTeX table generated and saved in 'file.tex'\n")
}