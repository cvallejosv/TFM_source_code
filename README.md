# Análisis Estadístico de Precipitaciones Máximas Diarias en la Comunidad Valenciana

Este repositorio contiene el código R utilizado para el desarrollo del Trabajo Final de Máster. A continuación, se ofrece una breve descripción de los archivos incluidos. Para más detalles sobre la funcionalidad y uso de cada script, se recomienda consultar la documentación incorporada al inicio de cada archivo.

#### `8416_pmax_analysis.R`

Este script genera dos gráficos para analizar las precipitaciones máximas diarias anuales registradas en la estación meteorológica de Valencia (8416):

- **Gráfico (a):** Serie temporal que muestra la evolución de las precipitaciones máximas diarias a lo largo de los años, incluyendo una línea de referencia en 150 mm.
- **Gráfico (b):** Histograma de frecuencias relativas de las precipitaciones máximas diarias.



#### `8416_pp_qq_plots.R`

Este script ajusta las distribuciones de Valores Extremos Generalizada (GEV) y log-Pearson Tipo III (LP3) a los datos de precipitaciones máximas diarias anuales de la estación meteorológica de Valencia (8416). Para la estimación de los parámetros de las distribuciones se utiliza el método de los L-momentos.

Se generan los siguientes gráficos para evaluar visualmente la calidad del ajuste:

- **Gráfico Q-Q:** Cuantiles empíricos vs. cuantiles teóricos.
- **Gráfico P-P:** Probabilidades empíricas vs. probabilidades teóricas.



#### `create_maps.R`

Este script genera dos gráficos que representan las precipitaciones máximas diarias estimadas en la Comunidad Valenciana para períodos de retorno de 200 y 500 años. Cada gráfico incluye dos mapas de dicha región dispuestos en paralelo, uno por cada distribución de probabilidad empleada: la Distribución de Valores Extremos Generalizada (GEV) y la log-Pearson Tipo III (LP3).



#### `create_return_levels_table.R`

Este script calcula las precipitaciones máximas diarias probables para diversos períodos de retorno (5, 10, 25, 50, 100, 200 y 500 años) en una serie de estaciones meteorológicas de la Comunidad Valenciana. Para ello, se utiliza el ajuste de los datos empíricos a la distribución de Valores Extremos Generalizada (GEV) y la distribución log-Pearson Tipo III (LP3). Para la estimación de los parámetros de las distribuciones se utiliza el método de los L-momentos. Además, se evalúa la bondad del ajuste de los datos empíricos a ambas distribuciones mediante la prueba de Kolmogórov-Smirnov (K-S), empleando simulaciones Monte Carlo. Los resultados se presentan en una tabla generada en formato LaTeX.



#### `ecdf_gev.R`

Este script genera un gráfico que compara la función de distribución empírica de una muestra de 100 observaciones simuladas a partir de una distribución GEV con parámetros predefinidos con la función de distribución teórica de la misma distribución.



#### `gev_density.R`

Este script genera un gráfico comparativo de las funciones de densidad de la distribución de Valores Extremos Generalizada (GEV) para tres valores distintos del parámetro de forma ξ.



#### `gev_pp_qq_plots.R`

Este script genera dos gráficos que permiten comparar las probabilidades acumuladas y cuantiles teóricos de una distribución GEV con parámetros μ = 50, σ = 15 y ξ = 0,2 frente a las probabilidades y cuantiles empíricas obtenidos a partir de una muestra simulada de 100 observaciones generadas bajo la misma distribución.



#### `lp3_density.R`

Este script genera dos gráficos que comparan las funciones de densidad de la distribución Log-Pearson Tipo III (LP3) bajo el supuesto de ξ = 0, considerando diferentes combinaciones de los parámetros α (forma) y β (escala).



#### `data`

Contiene los archivos de datos necesarios para ejecutar algunos de los scripts descritos anteriormente.  La información utilizada para generar dichos archivos fue proporcionada por la Agencia Estatal de Meteorología (AEMET).
