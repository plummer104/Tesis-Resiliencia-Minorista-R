# -------------------------------
# Mapa del Efecto Espacial - Alta calidad
# -------------------------------
library(ggplot2)
library(viridis)

# Asumimos que 'mapa_espacial' es un data frame que contiene las columnas:
#   X, Y: coordenadas (en metros)
#   efecto_espacial: el valor del efecto espacial proyectado

mapa_efecto <- ggplot(mapa_espacial, aes(x = X, y = Y, color = efecto_espacial)) +
  geom_point(size = 1.5) +
  scale_color_viridis_c(
    option = "B",
    name = "Efecto\nEspacial",
    limits = c(min(mapa_espacial$efecto_espacial, na.rm = TRUE),
               max(mapa_espacial$efecto_espacial, na.rm = TRUE)),
    guide = guide_colorbar(
      barwidth = 10,
      barheight = 0.5,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Mapa del Efecto Espacial - Modelo AMZ (INLA)",
    x = "Coordenada X (m)",
    y = "Coordenada Y (m)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  ) +
  coord_fixed()

# Mostrar el mapa en pantalla
print(mapa_efecto)

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

output_path <- normalizePath(here::here("3_Outputs", "3_Figures"), winslash = "/", mustWork = FALSE)

if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_path)) stop("No se pudo crear output_path: ", output_path)

ggsave(
  filename = file.path(output_path, "efecto_espacial_publicacion.png"),
  plot = mapa_efecto,
  width = 12,
  height = 8,
  dpi = 600
)

ggsave(
  filename = file.path(output_path, "efecto_espacial_publicacion.pdf"),
  plot = mapa_efecto,
  width = 12,
  height = 8
)


# --------------------------------------------------------------------
# Generar objetos a partir de inla.posterior para análisis gráfico
# --------------------------------------------------------------------

# 1. Extraer el índice de las observaciones del stack utilizando inla.stack.index.
#    Esto asegura que se seleccionen las posiciones correctas del stack para el modelo.
idx_post <- inla.stack.index(stack, tag = "modelo_AMZ")$data

# 2. Especificar el número de muestras (draws) a extraer del posterior.
n_draws <- 1000

# 3. Obtener n_draws muestras de la distribución posterior del modelo INLA.
posterior_samples <- inla.posterior.sample(n_draws, modelo_inla)

# 4. Inicializar una matriz para almacenar las probabilidades predichas para cada 
#    observación (según el índice) en cada draw.
n_obs <- length(idx_post)
ps_mat <- matrix(NA, nrow = n_obs, ncol = n_draws)

# 5. Para cada draw, extraer los valores latentes correspondientes a las observaciones
#    definidas por idx_post y calcular la probabilidad (usando la función logística).
for(i in 1:n_draws) {
  # Extraer el vector de valores latentes para las observaciones de interés
  latent_values <- posterior_samples[[i]]$latent[idx_post]
  
  # Transformar a probabilidades usando la función logística: f(η) = 1/(1+exp(-η))
  ps_mat[, i] <- 1 / (1 + exp(-latent_values))
}

# 6. Resumir las probabilidades predichas para cada observación a partir de todas las muestras.
#    Aquí se utiliza la media a través de las columnas (draws),
#    lo que resulta en un vector de probabilidades predichas.
prob_pred_att_obs <- rowMeans(ps_mat)

response_vector <- datos_analisis_escalado$AMZ

# Verificación rápida:
cat("Número de observaciones (n_obs):", n_obs, "\n")
cat("Resumen de las probabilidades predichas (prob_pred_att_obs):\n")
print(summary(prob_pred_att_obs))

# -------------------------------
# Bloque: Generación y Exportación de la Curva ROC (alta resolución)
# -------------------------------

# Cargar las librerías necesarias (si aún no se han cargado)
library(ggplot2)
library(viridis)
library(sf)
library(showtext)   # Opcional: para fuentes de mejor presentación
library(pROC)

# Se asume que el vector 'response_vector' contiene los valores reales de AMZ 
# y 'prob_pred_att_obs' ya contiene la media de las probabilidades derivadas de inla.posterior.
if(length(prob_pred_att_obs) != length(response_vector)) {
  stop("La longitud de 'prob_pred_att_obs' (", length(prob_pred_att_obs), 
       ") no coincide con la longitud de 'AMZ' (", length(response_vector), "). 
       Revisa que el orden y el número de observaciones sean consistentes.")
}

# Calcular la curva ROC utilizando pROC
roc_obj <- roc(response = response_vector, predictor = prob_pred_att_obs)
auc_val <- round(auc(roc_obj), 3)

# Preparar data frame para ggplot2
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# Generar gráfico ROC
roc_plot <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "#2c7bb6", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  coord_equal() +
  labs(title = paste("Curva ROC — AUC:", auc_val),
       x = "1 - Especificidad",
       y = "Sensibilidad") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))

# Mostrar la curva ROC en la ventana gráfica
print(roc_plot)

# Definir ruta de salida
if (!exists("output_path")) {
  output_path <- normalizePath(file.path(getwd(), "3_Outputs", "3_Figures"), winslash = "/", mustWork = FALSE)
}

# Crear carpeta si falta
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_path)) stop("No se pudo crear output_path: ", output_path)

# Exportar curva ROC
ggsave(
  filename = file.path(output_path, "ROC_Curve_HighRes.png"),
  plot = roc_plot,
  width = 7, height = 7, dpi = 600
)
ggsave(
  filename = file.path(output_path, "ROC_Curve_HighRes.pdf"),
  plot = roc_plot,
  width = 7, height = 7
)

# Calcular el punto de corte óptimo (Youden)
opt_threshold <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

# Imprimir en consola los valores del punto de corte, convirtiéndolos a numérico para evitar el error.
cat("\n--- Punto de corte óptimo (Youden) ---\n")
cat("Umbral:", round(as.numeric(opt_threshold["threshold"]), 3), "\n")
cat("Sensibilidad:", round(as.numeric(opt_threshold["sensitivity"]), 3), "\n")
cat("Especificidad:", round(as.numeric(opt_threshold["specificity"]), 3), "\n")





# -------------------------------
# 3.3. Gráficos de efectos marginales (GAM) — versión para publicación
# -------------------------------

library(scales)
library(cowplot)  # Para combinar gráficos con plot_grid

# Definir las variables a utilizar (según el script principal)
variables_manual <- c("S1902_C02_001E", "S2403_C01_003E", "S2504_C03_018E",
                      "S2503_C06_019E", "S2402_C01_018E", "S2506_C01_047E")

# Etiquetas legibles (se pueden personalizar; aquí se usan los mismos nombres)
var_labels <- c(
  "S1902_C02_001E" = "S1902_C02_001E",
  "S2403_C01_003E" = "S2403_C01_003E",
  "S2504_C03_018E" = "S2504_C03_018E",
  "S2503_C06_019E" = "S2503_C06_019E",
  "S2402_C01_018E" = "S2402_C01_018E",
  "S2506_C01_047E" = "S2506_C01_047E"
)

# Función para crear gráficos GAM con estética limpia para publicación
create_gam_plot_pub <- function(data, var, label) {
  # Crear fórmula dinámica
  formula_gam <- as.formula(paste("AMZ ~ s(", var, ", bs = 'tp')"))
  modelo_gam <- gam(formula_gam, data = data, family = binomial)
  
  # Crear rejilla de valores para la variable
  pred_grid <- data.frame(seq(from = min(data[[var]], na.rm = TRUE),
                              to   = max(data[[var]], na.rm = TRUE),
                              length.out = 100))
  colnames(pred_grid) <- var
  
  # Obtener predicciones en la escala del enlace y calcular errores estándar
  pred <- predict(modelo_gam, newdata = pred_grid, se.fit = TRUE, type = "link")
  
  # Transformar a probabilidades usando la función logística
  pred_grid$fit <- plogis(pred$fit)
  pred_grid$lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
  pred_grid$upr <- plogis(pred$fit + 1.96 * pred$se.fit)
  
  # Generar gráfico
  p <- ggplot(pred_grid, aes_string(x = var)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#d1e5f0", alpha = 0.5) +
    geom_line(aes(y = fit), color = "#2c7bb6", size = 1.1) +
    labs(
      x = label,
      y = "Pr(AMZ = 1)"
    ) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    theme_minimal(base_size = 15) +
    theme(
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11)
    )
  return(p)
}

# Crear lista de gráficos
plots_publication <- mapply(create_gam_plot_pub,
                            var = variables_manual,
                            label = var_labels[variables_manual],
                            MoreArgs = list(data = datos_analisis_escalado),
                            SIMPLIFY = FALSE)

# Combinar los gráficos (2 columnas)
combined_plot <- plot_grid(plotlist = plots_publication, ncol = 2)

# Exportar a PNG con alta resolución
ggsave(
  filename = file.path(output_path, "Marginal_Effects_Full_Publication.png"),
  plot = combined_plot,
  width = 12,
  height = 14,
  dpi = 300
)

ggsave(
  filename = file.path(output_path, "Marginal_Effects_Full_Publication.pdf"),
  plot = combined_plot,
  width = 12,
  height = 14
)


# ---------------------------------------------------------------
# 5. ESTADÍSTICOS DE LOS EFECTOS MARGINALES (GAM)
# ---------------------------------------------------------------

cat("\n=== ESTADÍSTICOS DE LOS EFECTOS MARGINALES (GAM) ===\n")

# Función para extraer estadísticos de los modelos GAM
gam_results <- lapply(variables_manual, function(var) {
  formula_gam <- as.formula(paste("AMZ ~ s(", var, ", bs = 'tp')"))
  modelo_gam <- gam(formula_gam, data = datos_analisis_escalado, family = binomial)
  summ <- summary(modelo_gam)
  
  if (!is.null(summ$s.table)) {
    s_table <- as.data.frame(summ$s.table)
    # Convertir nombres de columnas a minúsculas
    colnames(s_table) <- tolower(colnames(s_table))
    # Si la columna con el estadístico se llama "f" en lugar de "chi.sq", renombrarla
    if ("f" %in% colnames(s_table) && !("chi.sq" %in% colnames(s_table))) {
      colnames(s_table)[colnames(s_table) == "f"] <- "chi.sq"
    }
    # Si el nombre del p-valor aparece como "p.value" en lugar de "p-value", se corrige
    if ("p.value" %in% colnames(s_table) && !("p-value" %in% colnames(s_table))) {
      colnames(s_table)[colnames(s_table) == "p.value"] <- "p-value"
    }
    s_table$variable <- var
    return(s_table)
  } else {
    warning(paste("No se encontraron términos suaves para la variable:", var))
    return(NULL)
  }
})

# Filtrar resultados nulos y combinar
gam_results <- Filter(Negate(is.null), gam_results)
gam_stats_df <- do.call(rbind, gam_results)

# Definir los nombres de columnas requeridos
required_cols <- c("variable", "edf", "ref.df", "chi.sq", "p-value")
if (!all(required_cols %in% colnames(gam_stats_df))) {
  cat("Nombres disponibles en s.table:\n")
  print(colnames(gam_stats_df))
  stop("⚠️ Las columnas requeridas no están disponibles en el resultado GAM.")
}

# Filtrar únicamente las columnas de interés
gam_stats_df <- gam_stats_df[, required_cols]

# Mostrar en consola los estadísticos
print(gam_stats_df)

# Exportar la tabla a CSV para la publicación
write.csv(gam_stats_df, file.path(output_path, "Marginal_Effects_Statistics_GAM.csv"), row.names = FALSE)





# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(viridis)
library(showtext)

# Activar showtext para mejorar las tipografías en gráficos (opcional)
showtext_auto()

# Preparar el data frame para el gráfico:
# Se asume que 'datos_analisis_escalado' ya contiene:
# - La variable 'propensity_score': puntajes de propensión predichos desde el modelo INLA.
# - La variable 'AMZ': indicador de tratamiento (0=control, 1=tratado).
datos_common <- datos_analisis_escalado %>%
  mutate(tratamiento = factor(AMZ, levels = c(0, 1), labels = c("Control", "Tratado")))

# Crear el gráfico de common support usando densidades con ajustes para publicación
common_support_plot <- ggplot(datos_common, aes(x = propensity_score, fill = tratamiento)) +
  geom_density(alpha = 0.6, adjust = 1.2) +  # suaviza la curva
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.8) +
  labs(
    title = "Common Support: Distribución de puntajes de propensión",
    subtitle = "Modelo INLA Logit Espacial con Componente Espacial en ATT",
    x = "Puntuación de Propensión (Modelo INLA)",
    y = "Densidad Kernel",
    fill = "Grupo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  # Truncar visualmente el eje y para evitar el pico excesivamente alto
  scale_y_continuous(limits = c(0, 10)) +
  coord_cartesian(ylim = c(0, 10))

# Visualizar el gráfico
print(common_support_plot)

# Exportar el gráfico en alta resolución (600 dpi, 8 x 6 pulgadas) para publicación
ggsave(
  filename = file.path(output_path, "common_support_INLA.png"),
  plot = common_support_plot,
  dpi = 600,
  width = 8,
  height = 6
)



# --------------------------------------------------
# Estadísticos ampliados del Common Support
# --------------------------------------------------
library(dplyr)
library(broom)   # Para tidy() de las pruebas estadísticas

cat("\n=== Estadísticos Ampliados de Common Support ===\n\n")

# 1. Tabla de percentiles (deciles) por grupo
deciles_common <- datos_common %>%
  group_by(tratamiento) %>%
  summarise_at(vars(propensity_score),
               list(
                 D0  = ~quantile(., 0.00),
                 D10 = ~quantile(., 0.10),
                 D20 = ~quantile(., 0.20),
                 D30 = ~quantile(., 0.30),
                 D40 = ~quantile(., 0.40),
                 D50 = ~quantile(., 0.50),
                 D60 = ~quantile(., 0.60),
                 D70 = ~quantile(., 0.70),
                 D80 = ~quantile(., 0.80),
                 D90 = ~quantile(., 0.90),
                 D100= ~quantile(., 1.00)
               ))

cat(">> Deciles de la puntuación de propensión por grupo:\n")
print(deciles_common)

# 2. Estadístico de Kolmogorov-Smirnov para comparar distribuciones
ks_res <- ks.test(
  x = datos_common$propensity_score[datos_common$tratamiento == "Control"],
  y = datos_common$propensity_score[datos_common$tratamiento == "Tratado"]
)
cat("\n>> Prueba KS (Control vs Tratado):\n")
print(tidy(ks_res))

# 3. Diferencia de medias y ratio de varianzas
mean_ctrl <- mean(datos_common$propensity_score[datos_common$tratamiento=="Control"])
mean_trt  <- mean(datos_common$propensity_score[datos_common$tratamiento=="Tratado"])
var_ctrl  <- var(datos_common$propensity_score[datos_common$tratamiento=="Control"])
var_trt   <- var(datos_common$propensity_score[datos_common$tratamiento=="Tratado"])
diff_means <- mean_trt - mean_ctrl
var_ratio  <- var_trt / var_ctrl

cat("\n>> Estadísticos comparativos:\n")
cat("   Media Control   =", round(mean_ctrl, 4), "\n")
cat("   Media Tratado   =", round(mean_trt, 4), "\n")
cat("   Diferencia      =", round(diff_means, 4), "\n")
cat("   Varianza Control=", round(var_ctrl, 4), "\n")
cat("   Varianza Tratado=", round(var_trt, 4), "\n")
cat("   Ratio Varianzas =", round(var_ratio, 4), "\n")

# 4. Cohen's d para el tamaño del efecto
pooled_sd <- sqrt(((nrow(filter(datos_common, tratamiento=="Control")) - 1) * var_ctrl +
                     (nrow(filter(datos_common, tratamiento=="Tratado")) - 1) * var_trt) /
                    (nrow(datos_common) - 2))
cohen_d <- diff_means / pooled_sd
cat("\n>> Tamaño del efecto (Cohen's d) =", round(cohen_d, 3), "\n")

# 5. Recuento dentro del rango de solapamiento
# Definir el Common Support
ps_ctrl <- datos_common$propensity_score[datos_common$tratamiento == "Control"]
ps_trt  <- datos_common$propensity_score[datos_common$tratamiento == "Tratado"]
lower_overlap <- max(min(ps_ctrl, na.rm = TRUE), min(ps_trt, na.rm = TRUE))
upper_overlap <- min(max(ps_ctrl, na.rm = TRUE), max(ps_trt, na.rm = TRUE))
cat(sprintf("Common support va de %.3f hasta %.3f\n", lower_overlap, upper_overlap))

# Cálculo de conteo dentro del rango
in_overlap <- datos_common %>%
  mutate(inCS = propensity_score >= lower_overlap & propensity_score <= upper_overlap) %>%
  group_by(tratamiento) %>%
  summarise(
    Dentro_CS = sum(inCS, na.rm = TRUE),
    Total     = n(),
    Pct_CS    = round(100 * Dentro_CS / Total, 1)
  )
print(in_overlap)




# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(viridis)
library(showtext)

# Activar showtext para mejorar las tipografías en gráficos (opcional)
showtext_auto()

# Preparar el data frame para el gráfico:
# Se asume que 'datos_analisis_escalado' ya contiene:
# - La variable 'propensity_score': puntajes de propensión predichos desde el modelo INLA.
# - La variable 'AMZ': indicador de tratamiento (0 = Control, 1 = Tratado).
datos_common <- datos_analisis_escalado %>%
  mutate(tratamiento = factor(AMZ, levels = c(0, 1), labels = c("Control", "Tratado")))

# Crear el gráfico de common support con ajustes para publicación:
# Se visualizan las densidades de las puntuaciones de propensión para los dos grupos,
# enfatizando la zona de solapamiento y recortando el eje Y para atenuar el pico extremo.
common_support_plot <- ggplot(datos_common, aes(x = propensity_score, fill = tratamiento)) +
  geom_density(alpha = 0.6, adjust = 1.2) +  # Suaviza la curva de densidad
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  labs(
    title = "",
    subtitle = "",
    x = "Puntuación de Propensión",
    y = "Densidad",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  # Recortar visualmente el eje Y para evitar que el pico extremo lo oculte:
  coord_cartesian(ylim = c(0, 10))

# Visualizar el gráfico en la consola
print(common_support_plot)

# Exportar el gráfico en alta resolución (600 dpi, 8 x 6 pulgadas) para publicación
invisible(NULL)

# Definir ruta de salida
if (!exists("output_path")) {
  output_path <- normalizePath(file.path(getwd(), "3_Outputs", "3_Figures"), winslash = "/", mustWork = FALSE)
}

# Crear carpeta si falta
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_path)) stop("No se pudo crear output_path: ", output_path)

# Exportar common support
ggsave(
  filename = file.path(output_path, "common_support_INLA.png"),
  plot = common_support_plot,
  dpi = 600,
  width = 8,
  height = 6
)
ggsave(
  filename = file.path(output_path, "common_support_INLA.pdf"),
  plot = common_support_plot,
  width = 8,
  height = 6
)


# ---------------------------------------------------------------
#  Mapa de probabilidades predichas (basado en la inla.posterior)
# ---------------------------------------------------------------

## 1) Paquetes
suppressPackageStartupMessages({
  library(INLA)      # para inla.posterior.sample
  library(sf)        # para leer el shapefile
  library(ggplot2)   # para graficar
  library(viridis)   # escala de color
  library(showtext)  # tipografías más limpias
})
showtext_auto()

## 2) Carpeta de salida
if (!exists("output_path") || !dir.exists(output_path)) {
  output_path <- normalizePath(file.path(getwd(), "3_Outputs", "3_Figures"), winslash = "/", mustWork = FALSE)
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_path)) stop("No se pudo crear output_path: ", output_path)
}

## 3) Shapefile de las unidades espaciales
shapefile_path <- here::here("1_Data", "shapefiles", "DummyCondado_Project_ExportFeatures.shp")
if (!file.exists(shapefile_path)) stop("No se encontró shapefile: ", shapefile_path)

shp <- st_read(shapefile_path, quiet = TRUE)


## 4) VECTOR DE PROBABILIDADES -------------------------------------------------
#  ─────────────────────────────────────────────────────────────────────────────
#  a) Si ya existen las probabilidades calculadas antes (por ej. `prob_pred_att_obs`),
#     simplemente las usamos.
#  b) Si no existen, las calculamos aquí a partir de la inla.posterior.

if (exists("prob_pred_att_obs")) {
  pred_vec <- prob_pred_att_obs
  
} else {
  
  message("⚠️  No encontré 'prob_pred_att_obs'; recalculando a partir de la inla.posterior…")
  
  # ‣ Índices de las observaciones en el stack (se asume objeto 'stack' y etiqueta 'modelo_AMZ')
  if (!exists("idx_post")) {
    if (!exists("stack"))
      stop("No existe 'stack' en el entorno; no puedo recuperar los índices de observación.")
    idx_post <- inla.stack.index(stack, tag = "modelo_AMZ")$data
  }
  
  # ‣ Número de draws (ajusta si quieres más/menos)
  n_draws <- 1000
  
  # ‣ Muestras de la posterior
  posterior_samples <- inla.posterior.sample(n_draws, modelo_inla)
  
  # ‣ Matriz para almacenar las probabilidades de cada draw
  n_obs <- length(idx_post)
  ps_mat <- matrix(NA_real_, nrow = n_obs, ncol = n_draws)
  
  for (d in seq_len(n_draws)) {
    eta <- posterior_samples[[d]]$latent[idx_post]
    ps_mat[, d] <- 1 / (1 + exp(-eta))   # función logística
  }
  
  # ‣ Media posterior por observación
  pred_vec <- rowMeans(ps_mat)
}

## 5) Comprobación de longitudes
if (nrow(shp) != length(pred_vec)) {
  stop("❌ nrow(shp) = ", nrow(shp),
       " ≠ length(pred_vec) = ", length(pred_vec),
       ". Revisa que shapefile y datos estén en el mismo orden y tamaño.")
}

## 6) Añadir la columna con probabilidades
shp$pred_prob <- pred_vec

## 7) Construir el mapa
mapa_prob <- ggplot(shp) +
  geom_sf(aes(fill = pred_prob), colour = "white", linewidth = 0.1) +
  scale_fill_viridis(
    option = "plasma", limits = c(0, 1), na.value = "grey90",
    name = "Probabilidad\nPredicha",
    guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                           title.position = "top", title.hjust = 0.5)
  ) +
  labs(title = "Distribución espacial de probabilidades predichas") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank()
  )

print(mapa_prob)

## 8) Guardar PNG y PDF
ggsave(
  filename = file.path(output_path, "Predicted_Probability_Map_Posterior.png"),
  plot = mapa_prob,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(output_path, "Predicted_Probability_Map_Posterior.pdf"),
  plot = mapa_prob,
  width = 10,
  height = 8
)

message("Mapa exportado en: ", output_path)

## 9) Exportar tabla de probabilidades predichas a Excel (con GEOID y NAME)
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)

df_probs <- data.frame(
  GEOID     = shp$GEOID,
  NAME      = shp$NAME,
  pred_prob = pred_vec,
  check.names = FALSE
)

write.xlsx(
  df_probs,
  file     = file.path(output_path, "Predicted_Probabilities_By_County.xlsx"),
  rowNames = FALSE
)

message("Tabla exportada en: ", output_path)

## 10) Exportar efecto espacial por condado a Excel (con GEOID y NAME)
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)

df_effect <- data.frame(
  GEOID           = shp$GEOID,
  NAME            = shp$NAME,
  efecto_espacial = mapa_espacial$efecto_espacial,
  check.names     = FALSE
)

write.xlsx(
  df_effect,
  file     = file.path(output_path, "Spatial_Effect_By_County.xlsx"),
  rowNames = FALSE
)

message("Efecto exportado en: ", output_path)
