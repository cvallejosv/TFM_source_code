# Análisis Estadístico de Precipitaciones Máximas Diarias en la Comunidad Valenciana

Este repositorio contiene el código R utilizado para desarrollar el Trabajo Final de Máster. A continuación, se presenta una breve descripción de los archivos incluidos:

### `8416_pmax_analysis.R`

Este script genera dos gráficos para analizar las precipitaciones máximas diarias anuales registradas en la estación meteorológica de Valencia (8416):

- **Gráfico (a):** Serie temporal que muestra la evolución de las precipitaciones máximas diarias a lo largo de los años, incluyendo una línea de referencia en 150 mm.
- **Gráfico (b):** Histograma de frecuencias relativas de las precipitaciones máximas diarias, representado en orientación horizontal y sin relleno.

---

### `8416_pp_qq_plots.R`

Este script ajusta las distribuciones de Valores Extremos Generalizada (GEV) y log-Pearson Tipo III (LP3) a los datos de precipitaciones máximas diarias anuales de la estación meteorológica de Valencia (8416), utilizando L-momentos muestrales.

Se generan los siguientes gráficos para evaluar visualmente la calidad del ajuste:

- **Gráfico Q-Q:** Cuantiles empíricos vs. cuantiles teóricos.
- **Gráfico P-P:** Probabilidades empíricas vs. probabilidades teóricas.

---
