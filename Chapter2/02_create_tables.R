# ==============================================================================
# 10.6. Verificación del Emparejamiento y Preparación para el Diagnóstico de Balance Pre y Post-Matching
# ==============================================================================

# ------------------------------------------------------------------------------  
# A. Comprobación de la Existencia del Objeto de Emparejamiento en el Entorno
# ------------------------------------------------------------------------------  
if (exists("match_obj") && inherits(match_obj, "matchit")) {
  cat("✅ Emparejamiento detectado: SÍ ESTÁ HECHO\n")
} else {
  cat("❌ Emparejamiento NO detectado: ejecutar matchit() primero\n")
  stop("Por favor, ejecuta el bloque de emparejamiento con matchit() primero.")
}

# ------------------------------------------------------------------------------  
# # B. Definición de las Covariables para el Modelo INLA, Excluyendo la Variable 'Distance'
# (Estas son las variables que se usaron en el modelo INLA para construir el PS)
# ------------------------------------------------------------------------------  
covariables_modelo <- c("S1902_C02_001E", "S2403_C01_003E", "S2504_C03_018E",
                        "S2503_C06_019E", "S2402_C01_018E", "S2506_C01_047E")

# ------------------------------------------------------------------------------  
#  C. Preparación de los Datos para la Evaluación del Balance Pre y Post-Matching
# Se crea un data frame uniendo 'df_base' (que contiene la variable "treated")
# con las covariables originales extraídas de 'datos_analisis_escalado'.
# ------------------------------------------------------------------------------  
data_balance <- merge(df_base, 
                      datos_analisis_escalado[, c("GEOIDFQ", covariables_modelo)], 
                      by = "GEOIDFQ")

# ------------------------------------------------------------------------------  
# D. Cálculo del Balance de Covariables Antes y Después del Emparejamiento
# Se fuerza a evaluar solo las covariables originales usando el argumento 'addl'
# y se especifica la variable de tratamiento mediante 'treat'.
# Se incluye el parámetro drop.distance para excluir "distance" de la salida.
# ------------------------------------------------------------------------------  
library(cobalt)

balance <- bal.tab(match_obj,
                   data = data_balance,
                   treat = "treated",
                   addl = data_balance[, covariables_modelo],  # Solo las covariables originales
                   un = TRUE,                # Reporta métricas pre y post matching
                   m.threshold = 0.1,        # Umbral para SMD: balance si |SMD| < 0.1
                   v.threshold = 2,          # Umbral para razón de varianzas: balance si está entre 0.5 y 2
                   ks.threshold = 0.05,      # Umbral para test KS: balance si p > 0.05
                   estimand = "ATT",
                   stats = NULL,             # Usar comportamiento por defecto para stats
                   int = FALSE,              # Forzar que 'int' sea FALSE
                   poly = 1,                 # Fijar poly a 1
                   drop.distance = TRUE)     # Excluir la variable "distance" de la salida

# Si la fila "distance" sigue apareciendo en la tabla, se elimina manualmente.
if ("distance" %in% rownames(balance$Balance.table)) {
  balance$Balance.table <- balance$Balance.table[!rownames(balance$Balance.table) %in% "distance", ]
}

# ------------------------------------------------------------------------------  
# E. Creación del Love Plot para Visualizar el Balance de Covariables Pre y Post-Matching
# ------------------------------------------------------------------------------  
love.plot(balance, 
          stats = c("mean.diffs", "variance.ratios", "ks.statistics"), 
          threshold = 0.1, 
          abs = TRUE,
          var.order = "unadjusted",
          drop.distance = TRUE,  # Intenta excluir "distance" en el gráfico
          title = "Love Plot: Balance de Covariables Pre y Post Matching (Sin 'distance')")

# ------------------------------------------------------------------------------  
# F. Implementación de Función para Imprimir la Tabla de Balance Excluyendo la Variable 'Distance'
# ------------------------------------------------------------------------------  
print_no_distance <- function(bal_obj) {
  # Usamos la tabla de balance ya modificada si está disponible
  if (!is.null(bal_obj$Balance.table)) {
    cat("Balance Measures (excluyendo 'distance'):\n")
    print(bal_obj$Balance.table)
  } else if (!is.null(bal_obj$Balance)) {
    bal_table <- bal_obj$Balance
    bal_table <- bal_table[rownames(bal_table) != "distance", ]
    cat("Balance Measures (excluyendo 'distance'):\n")
    print(bal_table)
  } else {
    cat("No se encontró la tabla de balance en el objeto.\n")
  }
  
  # Mostrar los tamaños muestrales si están disponibles
  if (!is.null(bal_obj$Pairs)) {
    cat("\nSample sizes:\n")
    print(bal_obj$Pairs)
  }
}

# Usar la función personalizada
cat("\n=== Impresión personalizada del balance (sin 'distance') ===\n")
print_no_distance(balance)



# Extraer la tabla resumen de los efectos fijos del modelo INLA
summary_fixed <- modelo_inla$summary.fixed

# Construir el data frame con las columnas solicitadas
resultados <- data.frame(
  Variable = rownames(summary_fixed),
  OR = exp(summary_fixed[, "mean"]),
  `95% IC` = paste0("[", round(exp(summary_fixed[, "0.025quant"]), 3), ", ", round(exp(summary_fixed[, "0.975quant"]), 3), "]"),
  `Media (log-OR)` = round(summary_fixed[, "mean"], 4),
  SD = round(summary_fixed[, "sd"], 4),
  check.names = FALSE
)

# Mostrar los resultados
print(resultados)





# Cargar la librería pROC para calcular el AUC si aún no está cargada
library(pROC)

# Supongamos que ya se ha definido 'idx' para extraer las predicciones:
# idx <- inla.stack.index(stack, tag = "modelo_AMZ")$data

# Extraer las métricas de ajuste del modelo INLA:
dic_val    <- modelo_inla$dic$dic                # DIC
dic_sat    <- modelo_inla$dic$dic.sat            # DIC saturado
waic_val   <- modelo_inla$waic$waic              # WAIC
logLik_val <- modelo_inla$mlik[1]               # Log-likelihood marginal

# Calcular el AUC utilizando la función roc de pROC. Se evalúa sobre el vector de la respuesta real
# y los valores predichos (por ejemplo, la media de las predicciones obtenidas con inla.stack)
roc_obj  <- roc(datos_analisis_escalado$AMZ, modelo_inla$summary.fitted.values[idx, "mean"])
auc_val  <- auc(roc_obj)

# Crear un data frame con las métricas deseadas
tabla_ajuste <- data.frame(
  Métrica          = c("DIC", "DIC (Saturado)", "WAIC", "Log-Likelihood", "AUC (ROC)"),
  Valor            = c(dic_val, dic_sat, waic_val, logLik_val, auc_val)
)

# Visualizar la tabla de métricas de ajuste
print(tabla_ajuste)

###############################################################################
# 10.7. Cálculo del SMD y Número de Pares Emparejados por Caliper
###############################################################################
# Usamos una sola muestra (posterior_sample_single) para ilustrar
# SMD en el score de propensión y # de pares, por caliper
posterior_sample_single <- inla.posterior.sample(1, modelo_inla)
n_obs <- nrow(df_base)

ps_draw_single <- extract_ps_samples(posterior_sample_single, n_obs, "^APredictor:")[, 1]
df_draw_single <- df_base %>% mutate(ps_draw = ps_draw_single)

resultados_balance <- data.frame(
  Escenario  = character(),
  Caliper    = numeric(),
  N_Treated  = numeric(),
  N_Control  = numeric(),
  SMD_ps     = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(calipers)) {
  caliper_i <- calipers[i]
  
  match_obj <- matchit(
    formula  = treated ~ 1,
    data     = df_draw_single,
    method   = "nearest",
    distance = df_draw_single$ps_draw,
    caliper  = caliper_i,
    replace  = FALSE
  )
  
  df_matched <- match.data(match_obj)
  counts <- table(df_matched$treated)
  
  n_treated <- ifelse("1" %in% names(counts), counts["1"], 0)
  n_control <- ifelse("0" %in% names(counts), counts["0"], 0)
  
  mean_treated <- mean(df_matched$ps_draw[df_matched$treated == 1], na.rm = TRUE)
  mean_control <- mean(df_matched$ps_draw[df_matched$treated == 0], na.rm = TRUE)
  sd_treated   <- sd(df_matched$ps_draw[df_matched$treated == 1], na.rm = TRUE)
  sd_control   <- sd(df_matched$ps_draw[df_matched$treated == 0], na.rm = TRUE)
  
  pooled_sd <- sqrt(((n_treated - 1) * sd_treated^2 + (n_control - 1) * sd_control^2) /
                      (n_treated + n_control - 2))
  smd_ps <- abs(mean_treated - mean_control) / pooled_sd
  
  resultados_balance <- rbind(
    resultados_balance,
    data.frame(
      Escenario  = paste0("Escenario_", i),
      Caliper    = caliper_i,
      N_Treated  = as.numeric(n_treated),
      N_Control  = as.numeric(n_control),
      SMD_ps     = smd_ps,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n============================================\n")
cat("BALANCE DE PS (SMD) Y PARES EMPAREJADOS\n")
cat("============================================\n")
print(resultados_balance)

###############################################################################
# 10.8. Análisis de Balance de PS (SMD) y Pares Emparejados para el Modelo Espacial Bayesiano
###############################################################################
# Este bloque calcula el balance (SMD) y el número de pares emparejados
# usando una única muestra del posterior del modelo (que ya incorpora efecto_espacial).

# Extraer una muestra del posterior para ilustrar el balance del PS en el modelo espacial
posterior_sample_single_spatial <- inla.posterior.sample(1, modelo_inla)
n_obs <- nrow(df_base)

# Extraer los propensity scores simulados a partir de la muestra seleccionada
ps_draw_single_spatial <- extract_ps_samples(posterior_sample_single_spatial, n_obs, "^APredictor:")[, 1]

# Agregar la columna con los propensity scores a la base para matching
df_draw_single_spatial <- df_base %>% mutate(ps_draw = ps_draw_single_spatial)

# Inicializar el data frame para almacenar los resultados del balance
resultados_balance_spatial <- data.frame(
  Escenario  = character(),
  Caliper    = numeric(),
  N_Treated  = numeric(),
  N_Control  = numeric(),
  SMD_ps     = numeric(),
  stringsAsFactors = FALSE
)

# Se asume que 'calipers' ya está definido (por ejemplo, calipers <- c(0.1))
for (i in seq_along(calipers)) {
  caliper_i <- calipers[i]
  
  # Realizar matching utilizando 'matchit' con el score simulado
  match_obj_spatial <- matchit(
    formula  = treated ~ 1,
    data     = df_draw_single_spatial,
    method   = "nearest",
    distance = df_draw_single_spatial$ps_draw,
    caliper  = caliper_i,
    replace  = FALSE
  )
  
  # Obtener la base emparejada
  df_matched_spatial <- match.data(match_obj_spatial)
  
  # Calcular el número de tratados y controles
  counts <- table(df_matched_spatial$treated)
  n_treated <- if ("1" %in% names(counts)) counts["1"] else 0
  n_control <- if ("0" %in% names(counts)) counts["0"] else 0
  
  # Calcular media y desviación estándar de los propensity scores en cada grupo
  mean_treated <- mean(df_matched_spatial$ps_draw[df_matched_spatial$treated == 1], na.rm = TRUE)
  mean_control <- mean(df_matched_spatial$ps_draw[df_matched_spatial$treated == 0], na.rm = TRUE)
  sd_treated   <- sd(df_matched_spatial$ps_draw[df_matched_spatial$treated == 1], na.rm = TRUE)
  sd_control   <- sd(df_matched_spatial$ps_draw[df_matched_spatial$treated == 0], na.rm = TRUE)
  
  # Calcular la desviación estándar ponderada (pooled SD)
  pooled_sd <- sqrt(((n_treated - 1) * sd_treated^2 + (n_control - 1) * sd_control^2) / 
                      (n_treated + n_control - 2))
  
  # Calcular el Standardized Mean Difference (SMD)
  smd_ps <- abs(mean_treated - mean_control) / pooled_sd
  
  # Acumular los resultados
  resultados_balance_spatial <- rbind(
    resultados_balance_spatial,
    data.frame(
      Escenario  = paste0("Escenario_", i),
      Caliper    = caliper_i,
      N_Treated  = as.numeric(n_treated),
      N_Control  = as.numeric(n_control),
      SMD_ps     = smd_ps,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n============================================\n")
cat("BALANCE DE PS (SMD) Y PARES EMPAREJADOS - MODELO CON COMPONENTE ESPACIAL\n")
cat("============================================\n")
print(resultados_balance_spatial)




# 10.9. Evaluación del Balance de PS (SMD) y Pares Emparejados para el Modelo Clásico con Propensity Scores Simulados
# Suponemos que ya se ha generado la matriz 'ps_draws_classic' de dimensión:
#   [n_obs x n_draws] 
# y que ya existe el data frame 'df_base' con las variables GEOIDFQ y el indicador 
# 'treated' (donde treated = as.numeric(AMZ)).

# Seleccionar una única muestra (por ejemplo, la primera columna de los PS simulados)
ps_draw_single_classic <- ps_draws_classic[, 1]

# Agregar esta muestra a la base de matching
df_draw_single_classic <- df_base %>% mutate(ps_draw = ps_draw_single_classic)

# Inicializar el data frame para almacenar los resultados del balance
resultados_balance_classic_spatial <- data.frame(
  Escenario  = character(),
  Caliper    = numeric(),
  N_Treated  = numeric(),
  N_Control  = numeric(),
  SMD_ps     = numeric(),
  stringsAsFactors = FALSE
)

# Se utiliza el mismo vector de calipers definido (por ejemplo, calipers <- c(0.1))
for (i in seq_along(calipers)) {
  caliper_i <- calipers[i]
  
  # Realizar matching basado en el propensity score del draw seleccionado
  match_obj_classic <- matchit(
    formula  = treated ~ 1,
    data     = df_draw_single_classic,
    method   = "nearest",
    distance = df_draw_single_classic$ps_draw,
    caliper  = caliper_i,
    replace  = FALSE
  )
  
  # Extraer los datos emparejados
  df_matched_classic <- match.data(match_obj_classic)
  
  # Calcular la cantidad de observaciones emparejadas en cada grupo
  counts <- table(df_matched_classic$treated)
  n_treated <- if ("1" %in% names(counts)) counts["1"] else 0
  n_control <- if ("0" %in% names(counts)) counts["0"] else 0
  
  # Calcular medias y desviaciones estándar de los PS en cada grupo
  mean_treated <- mean(df_matched_classic$ps_draw[df_matched_classic$treated == 1], na.rm = TRUE)
  mean_control <- mean(df_matched_classic$ps_draw[df_matched_classic$treated == 0], na.rm = TRUE)
  sd_treated   <- sd(df_matched_classic$ps_draw[df_matched_classic$treated == 1], na.rm = TRUE)
  sd_control   <- sd(df_matched_classic$ps_draw[df_matched_classic$treated == 0], na.rm = TRUE)
  
  # Calcular la desviación estándar combinada (pooled SD)
  pooled_sd <- sqrt(((n_treated - 1) * sd_treated^2 + (n_control - 1) * sd_control^2) / 
                      (n_treated + n_control - 2))
  
  # Calcular el Standardized Mean Difference (SMD)
  smd_ps <- abs(mean_treated - mean_control) / pooled_sd
  
  # Acumular los resultados en el data frame
  resultados_balance_classic_spatial <- rbind(
    resultados_balance_classic_spatial,
    data.frame(
      Escenario  = paste0("Escenario_", i),
      Caliper    = caliper_i,
      N_Treated  = as.numeric(n_treated),
      N_Control  = as.numeric(n_control),
      SMD_ps     = smd_ps,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n============================================\n")
cat("BALANCE DE PS (SMD) Y PARES EMPAREJADOS\n")
cat("MODELO CLÁSICO (LOGIT) CON PS SIMULADOS\n")
cat("============================================\n")
print(resultados_balance_classic_spatial)



# BLOQUE ADICIONAL: BALANCE DE PS (SMD) Y PARES EMPAREJADOS
# PARA EL MODELO CLÁSICO SIN COMPONENTE ESPACIAL (LOGIT)
# Se supone que ya se han generado:
#   - df_base_classic: data.frame con columnas GEOIDFQ y treated (treated = as.numeric(AMZ))
#   - ps_draws_classic: matriz de draws de los propensity scores (dimensión: [n_obs x n_draws])
#   - calipers: vector de caliper a emplear (por ejemplo, calipers <- c(0.1))

# Seleccionar una única simulación (draw) de los PS obtenidos del logit clásico
ps_draw_single_classic <- ps_draws_classic[, 1]

# Agregar esta muestra de PS a la base de matching para el modelo clásico
df_draw_single_classic <- df_base_classic %>% mutate(ps_draw = ps_draw_single_classic)

# Inicializar el contenedor para almacenar los resultados del balance
resultados_balance_classic_nospatial <- data.frame(
  Escenario  = character(),
  Caliper    = numeric(),
  N_Treated  = numeric(),
  N_Control  = numeric(),
  SMD_ps     = numeric(),
  stringsAsFactors = FALSE
)

# Para cada valor de caliper (por ejemplo, 0.1)
for (i in seq_along(calipers)) {
  caliper_i <- calipers[i]
  
  # Realizar matching usando matchit (nearest neighbor sin reemplazo)
  match_obj_classic <- matchit(
    formula  = treated ~ 1,
    data     = df_draw_single_classic,
    method   = "nearest",
    distance = df_draw_single_classic$ps_draw,
    caliper  = caliper_i,
    replace  = FALSE
  )
  
  # Extraer la base emparejada
  df_matched_classic <- match.data(match_obj_classic)
  
  # Calcular la cantidad de unidades emparejadas en cada grupo
  counts <- table(df_matched_classic$treated)
  n_treated <- if ("1" %in% names(counts)) counts["1"] else 0
  n_control <- if ("0" %in% names(counts)) counts["0"] else 0
  
  # Calcular las medias y desviaciones estándar de los PS por grupo
  mean_treated <- mean(df_matched_classic$ps_draw[df_matched_classic$treated == 1], na.rm = TRUE)
  mean_control <- mean(df_matched_classic$ps_draw[df_matched_classic$treated == 0], na.rm = TRUE)
  sd_treated   <- sd(df_matched_classic$ps_draw[df_matched_classic$treated == 1], na.rm = TRUE)
  sd_control   <- sd(df_matched_classic$ps_draw[df_matched_classic$treated == 0], na.rm = TRUE)
  
  # Calcular la desviación estándar combinada (pooled SD)
  pooled_sd <- sqrt(((n_treated - 1) * sd_treated^2 + (n_control - 1) * sd_control^2) /
                      (n_treated + n_control - 2))
  
  # Calcular el Standardized Mean Difference (SMD)
  smd_ps <- abs(mean_treated - mean_control) / pooled_sd
  
  # Almacenar los resultados en el data.frame
  resultados_balance_classic_nospatial <- rbind(
    resultados_balance_classic_nospatial,
    data.frame(
      Escenario  = paste0("Escenario_", i),
      Caliper    = caliper_i,
      N_Treated  = as.numeric(n_treated),
      N_Control  = as.numeric(n_control),
      SMD_ps     = smd_ps,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n============================================\n")
cat("BALANCE DE PS (SMD) Y PARES EMPAREJADOS\n")
cat("MODELO CLÁSICO SIN COMPONENTE ESPACIAL (LOGIT)\n")
cat("============================================\n")
print(resultados_balance_classic_nospatial)



###############################################################################
# PARTE FINAL DEL SCRIPT CORREGIDO
# (Reemplaza el bloque anterior que hacía merges y usaba nombres duplicados)
###############################################################################

# A) Imprimir elementos finales de diagnóstico y resultados
# ----------------------------------------------------------

# Imprime la tabla sin 'distance' (ya que se definió una función print_no_distance)
print_no_distance(balance)

# Imprime el resumen de efectos fijos del INLA (tabla de OR, etc.)
print(resultados)

# Imprime la tabla de métricas de ajuste (DIC, WAIC, AUC, etc.)
print(tabla_ajuste)

# Imprime cada data frame de balance
cat("\n=== Balance de PS para cada variante ===\n")
cat("\n--- (1) Modelo Espacial Bayesiano (resultados_balance) ---\n")
print(resultados_balance)

cat("\n--- (2) Modelo Espacial Bayesiano con Efecto Espacial (resultados_balance_spatial) ---\n")
print(resultados_balance_spatial)

cat("\n--- (3) Modelo Clásico (Logit) CON Componente Espacial (resultados_balance_classic_spatial) ---\n")
print(resultados_balance_classic_spatial)

cat("\n--- (4) Modelo Clásico (Logit) SIN Componente Espacial (resultados_balance_classic_nospatial) ---\n")
print(resultados_balance_classic_nospatial)


# Imprime cada data frame de ATT (sensibilidad)
cat("\n=== Resultados de ATT (sensibilidad) para cada variante ===\n")
cat("\n--- (1) Modelo Espacial Bayesiano (resultado_sensibilidad) ---\n")
print(resultado_sensibilidad)

cat("\n--- (2) Modelo Espacial Bayesiano con Efecto Espacial (resultado_sensibilidad_spatial) ---\n")
print(resultado_sensibilidad_spatial)

cat("\n--- (3) Modelo Clásico (Logit) CON Componente Espacial (resultado_sensibilidad_classic) ---\n")
print(resultado_sensibilidad_classic)

cat("\n--- (4) Modelo Clásico (Logit) SIN Componente Espacial (resultado_sensibilidad_classic_nospatial) ---\n")
print(resultado_sensibilidad_classic_nospatial)


###############################################################################
# B) Merge Final para la Presentación de los Resultados de ATT
###############################################################################
# Se realizan cuatro merges distintos, cada uno asociado a un enfoque específico:
#
#   - ATT_modelo1: Basado en el modelo espacial bayesiano.
#                  (resultados_balance) vs (resultado_sensibilidad)
#
#   - ATT_modelo2: Basado en el modelo espacial bayesiano con efecto espacial.
#                  (resultados_balance_spatial) vs (resultado_sensibilidad_spatial)
#
#   - ATT_modelo3: Basado en el modelo clásico (LOGIT) CON componente espacial.
#                  (resultados_balance_classic_spatial) vs (resultado_sensibilidad_classic)
#
#   - ATT_modelo4: Basado en el modelo clásico (LOGIT) SIN componente espacial.
#                  (resultados_balance_classic_nospatial) vs (resultado_sensibilidad_classic_nospatial)
###############################################################################

ATT_modelo1 <- merge(
  resultados_balance,
  resultado_sensibilidad,
  by = "Escenario"
)
cat("\n--- ATT_modelo1: Modelo Espacial Bayesiano ---\n")
print(ATT_modelo1)

ATT_modelo2 <- merge(
  resultados_balance_spatial,
  resultado_sensibilidad_spatial,
  by = "Escenario"
)
cat("\n--- ATT_modelo2: Modelo Espacial Bayesiano con Efecto Espacial ---\n")
print(ATT_modelo2)

ATT_modelo3 <- merge(
  resultados_balance_classic_spatial,
  resultado_sensibilidad_classic,
  by = "Escenario"
)
cat("\n--- ATT_modelo3: Modelo Clásico (Logit) CON Componente Espacial ---\n")
print(ATT_modelo3)

ATT_modelo4 <- merge(
  resultados_balance_classic_nospatial,
  resultado_sensibilidad_classic_nospatial,
  by = "Escenario"
)
cat("\n--- ATT_modelo4: Modelo Clásico (Logit) SIN Componente Espacial ---\n")
print(ATT_modelo4)

###############################################################################
# FIN DEL BLOQUE CORREGIDO
###############################################################################





library(dplyr)

# ---------------------------
# BLOQUE FINAL: Preparación y Exportación de la Tabla Consolidada de ATT
# ---------------------------

# Asegurarse de que los cuatro objetos ATT_modelo1, ATT_modelo2, ATT_modelo3 y ATT_modelo4 estén en el entorno.
# Consolidar los 4 modelos en un solo objeto (apilarlos fila a fila)
final_ATT_model <- rbind(ATT_modelo1, ATT_modelo2, ATT_modelo3, ATT_modelo4)

# Renombrar y eliminar columnas
final_ATT_model_mod <- final_ATT_model %>%
  dplyr::rename(
    Modelo = Escenario,
    Caliper = `Caliper.x`,
    Pares = `N_Treated`,
    ATT = `ATT_total_mean`
  ) %>%
  dplyr::select(-`N_Control`, -`N_Draws`, -`Caliper.y`)

# Asignar a cada fila el nombre del objeto de origen.
# (La salida esperada es: fila 1 = "ATT_modelo1", fila 2 = "ATT_modelo2", fila 3 = "ATT_modelo3", fila 4 = "ATT_modelo4")
final_ATT_model_mod$Modelo <- c("ATT_modelo1", "ATT_modelo2", "ATT_modelo3", "ATT_modelo4")

# Paso 2: Consolidar intervalos de confianza para el ATT Global
final_ATT_model_mod <- final_ATT_model_mod %>%
  mutate(`95% CrI ATT` = paste0("[", round(ATT_total_lower, 3), ", ", round(ATT_total_upper, 3), "]")) %>%
  dplyr::select(-ATT_total_lower, -ATT_total_upper) %>%
  rename(`ATT Global` = ATT)

# Paso 3: Consolidar intervalos de confianza para los ATT de condados vulnerables
final_ATT_model_mod <- final_ATT_model_mod %>%
  dplyr::rename(`ATT condados vulnerables` = `ATT_vuln_mean`) %>%
  mutate(`95% CrI condados vulnerables` = paste0("[", round(ATT_vuln_lower, 3), ", ", round(ATT_vuln_upper, 3), "]")) %>%
  dplyr::select(-ATT_vuln_lower, -ATT_vuln_upper)

# Paso 4: Consolidar intervalos de confianza para los ATT de condados resilientes
final_ATT_model_mod <- final_ATT_model_mod %>%
  dplyr::rename(`ATT condados resilientes` = `ATT_resil_mean`) %>%
  mutate(`95% CrI condados resilientes` = paste0("[", round(ATT_resil_lower, 3), ", ", round(ATT_resil_upper, 3), "]")) %>%
  dplyr::select(-ATT_resil_lower, -ATT_resil_upper)

# Paso 5: Reordenar las columnas para la presentación final
final_ATT_model_mod <- final_ATT_model_mod %>%
  dplyr::select(
    Modelo, Caliper, Pares, SMD_ps,
    `ATT Global`, `95% CrI ATT`,
    `ATT condados vulnerables`, `95% CrI condados vulnerables`,
    `ATT condados resilientes`, `95% CrI condados resilientes`
  )

# Mostrar en consola la tabla final consolidada
cat("\n--- Tabla Final Consolidada de ATT ---\n")
print(final_ATT_model_mod)

# ---------------------------
# Exportación a Excel usando openxlsx
# ---------------------------
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library(openxlsx)
}

# Definir la ruta de exportación (usa '/' como separador)
ruta_excel <- "C:/Users/plumm/OneDrive - UAM/Documentos/0 Paper Markov_PC/3. Condados/Paper Condados espacial/Gráficos, Figuras, Tablas, etc/final_ATT_model_mod.xlsx"

# Exportar el objeto final_ATT_model_mod a un archivo Excel sin incluir los nombres de fila
write.xlsx(final_ATT_model_mod, file = ruta_excel, rowNames = FALSE)

cat("\n✅ La tabla final consolidada se ha exportado a Excel en la siguiente ruta:\n", ruta_excel, "\n")





# =============================================================================
# BLOQUE: Cálculo de residuos incorporando explícitamente la variabilidad de la inla.posterior
# del Modelo Bayesiano Espacial Binario con INLA
# =============================================================================

# Verificar que los objetos necesarios existen: modelo_inla, datos_analisis_escalado y el índice de las observaciones
if (!exists("modelo_inla") || !exists("datos_analisis_escalado")) {
  stop("Falta el objeto 'modelo_inla' o 'datos_analisis_escalado' en el entorno.")
}

# Se asume que el stack fue creado y se tiene el índice de observaciones (para la respuesta 'AMZ')
if (!exists("idx")) {
  idx <- inla.stack.index(stack, tag = "modelo_AMZ")$data
  if(is.null(idx) || length(idx)==0){
    stop("No se pudo obtener el índice de observaciones a partir del stack.")
  }
}

# Número de draws para incorporar la variabilidad de la posterior
n_draws_res <- 1000

# Extraer n_draws_res muestras de la inla.posterior
posterior_draws <- inla.posterior.sample(n_draws_res, modelo_inla)

# Número de observaciones (según el índice idx)
n_obs <- length(idx)

# Inicializar una matriz para almacenar los fitted values (probabilidades predichas) para cada draw
fitted_draws_inla <- matrix(NA, nrow = n_obs, ncol = n_draws_res)

# Para cada draw, extraer el vector de valores latentes correspondientes a las observaciones y transformarlos
for(d in 1:n_draws_res) {
  # Cada muestra del posterior es un objeto con componente 'latent'; se seleccionan los índices de respuesta
  eta_draw <- posterior_draws[[d]]$latent[idx]
  # Transformar el predictor lineal a la escala de probabilidad: p = 1 / (1 + exp(-eta))
  fitted_draws_inla[, d] <- 1 / (1 + exp(-eta_draw))
}

# Extraer los valores observados de la variable de respuesta 'AMZ' (en el mismo orden que idx)
observed_vals <- datos_analisis_escalado$AMZ[idx]

# Calcular la matriz de residuos RAW:
# Para cada draw se obtiene: residuo = observado - fitted_value
residuals_draws_inla <- matrix(observed_vals, nrow = n_obs, ncol = n_draws_res, byrow = FALSE) - fitted_draws_inla

# Calcular la matriz de residuos Pearson para cada draw:
# (observado - fitted) / sqrt(fitted*(1-fitted))
pearson_residuals_draws_inla <- (matrix(observed_vals, nrow = n_obs, ncol = n_draws_res, byrow = FALSE) - fitted_draws_inla) /
  sqrt(fitted_draws_inla * (1 - fitted_draws_inla))

# Resumir los residuos por observación (por ejemplo, media y desviación estándar sobre los draws)
mean_residual_inla <- rowMeans(residuals_draws_inla)
sd_residual_inla   <- apply(residuals_draws_inla, 1, sd)

mean_pearson_inla  <- rowMeans(pearson_residuals_draws_inla)
sd_pearson_inla    <- apply(pearson_residuals_draws_inla, 1, sd)

# Mostrar algunos resúmenes generales (por observación)
cat("Resumen de la media de residuos RAW (por observación):\n")
print(summary(mean_residual_inla))

cat("\nResumen de la desviación estándar de residuos RAW (entre draws, por observación):\n")
print(summary(sd_residual_inla))

cat("\nResumen de la media de residuos Pearson (por observación):\n")
print(summary(mean_pearson_inla))

cat("\nResumen de la desviación estándar de residuos Pearson (entre draws, por observación):\n")
print(summary(sd_pearson_inla))

# Opcional: Resumen general promediado en todas las observaciones
overall_mean_raw <- mean(mean_residual_inla)
overall_sd_raw   <- mean(sd_residual_inla)
overall_mean_pearson <- mean(mean_pearson_inla)
overall_sd_pearson   <- mean(sd_pearson_inla)

cat("\nResumen general de residuos RAW: media =", overall_mean_raw, ", SD =", overall_sd_raw, "\n")
cat("Resumen general de residuos Pearson: media =", overall_mean_pearson, ", SD =", overall_sd_pearson, "\n")

# Graficar la distribución de la media de residuos (por observación) como ejemplo
par(mfrow = c(1,2))
boxplot(mean_residual_inla, main = "Boxplot de la media de residuos RAW",
        ylab = "Media de residuos RAW (por obs.)", col = "lightblue")
boxplot(mean_pearson_inla, main = "Boxplot de la media de residuos Pearson",
        ylab = "Media de residuos Pearson (por obs.)", col = "lightgreen")
par(mfrow = c(1,1))

# =============================================================================
# Comentario adicional sobre los residuos en el contexto del ATT con Matching:
# =============================================================================
# El procedimiento ATT basado en matching utiliza los propensity scores generados a partir
# de la inla.posterior. Dado que el matching no ajusta un modelo de regresión en la segunda etapa,
# no se generan residuos directamente asociados al ATT. Por ello, para evaluar la estructura (p. ej.,
# análisis de Moran) se pueden utilizar los residuos del modelo logit base. En este bloque se ha
# incorporado explícitamente la variabilidad de la inla.posterior en los residuos calculados.




# =============================================================================
# BLOQUE: Cálculo de residuos del modelo de propensity score - Logit clásico
# incorporando la incertidumbre de la simulación de los coeficientes (mvrnorm)
# =============================================================================

# Verificar que el modelo clásico y la base de datos existen
if (!exists("modelo_classico") || !exists("datos_analisis_escalado")) {
  stop("Falta el objeto 'modelo_classico' o 'datos_analisis_escalado' en el entorno.")
}

# Cargar MASS (si no estuviese ya cargado)
if(!"MASS" %in% (.packages())){
  library(MASS)
}

# Número de draws a simular
n_draws_classic <- 1000

# Obtener la matriz de diseño a partir del modelo clásico
X_classic <- model.matrix(modelo_classico)

# Simular draws de los coeficientes utilizando mvrnorm
beta_draws_classic_sim <- mvrnorm(n = n_draws_classic, 
                                  mu = coef(modelo_classico), 
                                  Sigma = vcov(modelo_classico))

# Número de observaciones (debe coincidir con el número de filas de X_classic)
n_obs_classic <- nrow(X_classic)

# Inicializar una matriz para almacenar los fitted values (propensities) para cada draw
fitted_draws_classic_sim <- matrix(NA, nrow = n_obs_classic, ncol = n_draws_classic)

# Para cada draw, calcular el predictor lineal y transformarlo a probabilidad
for (d in 1:n_draws_classic) {
  eta_draw <- X_classic %*% beta_draws_classic_sim[d, ]
  fitted_draws_classic_sim[, d] <- 1 / (1 + exp(-eta_draw))
}

# Extraer los valores observados para AMZ (la respuesta binaria 0/1) de la base de datos
observed_classic <- datos_analisis_escalado$AMZ

# Calcular la matriz de residuos RAW para cada draw:
# Residuo = observado - fitted
residuals_draws_classic <- matrix(observed_classic, nrow = n_obs_classic, ncol = n_draws_classic, byrow = FALSE) - fitted_draws_classic_sim

# Calcular la matriz de residuos Pearson para cada draw:
# Residuo Pearson = (observado - fitted) / sqrt(fitted*(1 - fitted))
pearson_residuals_draws_classic <- (matrix(observed_classic, nrow = n_obs_classic, ncol = n_draws_classic, byrow = FALSE) - fitted_draws_classic_sim) /
  sqrt(fitted_draws_classic_sim * (1 - fitted_draws_classic_sim))

# Resumir los residuos por observación (a lo largo de los draws)
mean_residual_classic <- rowMeans(residuals_draws_classic)
sd_residual_classic   <- apply(residuals_draws_classic, 1, sd)
mean_pearson_classic  <- rowMeans(pearson_residuals_draws_classic)
sd_pearson_classic    <- apply(pearson_residuals_draws_classic, 1, sd)

# Mostrar resúmenes generales (por observación)
cat("Resumen de la media de residuos RAW (modelo clásico):\n")
print(summary(mean_residual_classic))
cat("\nResumen de la desviación estándar de residuos RAW (modelo clásico):\n")
print(summary(sd_residual_classic))

cat("\nResumen de la media de residuos Pearson (modelo clásico):\n")
print(summary(mean_pearson_classic))
cat("\nResumen de la desviación estándar de residuos Pearson (modelo clásico):\n")
print(summary(sd_pearson_classic))

# Resumen global promedio (sobre todas las observaciones)
overall_mean_raw_classic <- mean(mean_residual_classic)
overall_sd_raw_classic   <- mean(sd_residual_classic)
overall_mean_pearson_classic <- mean(mean_pearson_classic)
overall_sd_pearson_classic   <- mean(sd_pearson_classic)

cat("\nResumen global de residuos RAW (modelo clásico): media =", overall_mean_raw_classic, 
    ", SD =", overall_sd_raw_classic, "\n")
cat("Resumen global de residuos Pearson (modelo clásico): media =", overall_mean_pearson_classic, 
    ", SD =", overall_sd_pearson_classic, "\n")

# Graficar la distribución de la media de residuos (por observación) como ejemplo
par(mfrow = c(1,2))
boxplot(mean_residual_classic, main = "Boxplot de la media de residuos RAW (clásico)",
        ylab = "Media de residuos RAW", col = "lightpink")
boxplot(mean_pearson_classic, main = "Boxplot de la media de residuos Pearson (clásico)",
        ylab = "Media de residuos Pearson", col = "lightcyan")
par(mfrow = c(1,1))

# =============================================================================
# Comentario:
# =============================================================================
# Se han simulado 1000 draws de los coeficientes del logit clásico para calcular
# la variabilidad en los fitted values (propensity scores) a partir del modelo clásico.
# Con ello se calculan, para cada observación, los residuos raw y Pearson,
# lo que permite incorporar la incertidumbre en la primera etapa.
#
# Dado que ambos procedimientos de ATT basados en logit clásico (con y sin componente espacial)
# utilizan el mismo modelo de propensity scores, se espera que estos residuos sean
# idénticos para ambos, permitiendo su uso posterior (por ejemplo, en un análisis de Moran
# de los residuos).






# -----------------------------------------------------------
# Test global de Moran para residuos de los modelos
# -----------------------------------------------------------

# Cargar spdep (ya debería estar cargado, pero se asegura)
if (!require(spdep)) install.packages("spdep", dependencies = TRUE)
library(spdep)

cat("\n============================================\n")
cat("TEST GLOBAL DE MORAN\n")
cat("============================================\n")

# 1.a. Para el modelo espacial (residuos RAW)
cat("\n--- Modelo Espacial (RAW) ---\n")
global_moran_inla_raw <- moran.test(mean_residual_inla, listw = W_dummy, zero.policy = TRUE)
print(global_moran_inla_raw)

# 1.b. Para el modelo espacial (residuos Pearson)
cat("\n--- Modelo Espacial (Pearson) ---\n")
# Seleccionar los índices sin NA en el vector de residuos Pearson
idx_inla <- which(!is.na(mean_pearson_inla))
# Crear un vector lógico para subindexar la lista de pesos:
logical_idx_inla <- rep(FALSE, length(W_dummy$neighbours))
logical_idx_inla[idx_inla] <- TRUE
# Subconjunto de la lista de pesos para los índices válidos
W_dummy_inla <- subset(W_dummy, subset = logical_idx_inla)
# Aplicar el test usando únicamente los residuos correspondientes
global_moran_inla_pearson <- moran.test(mean_pearson_inla[idx_inla], listw = W_dummy_inla, zero.policy = TRUE)
print(global_moran_inla_pearson)

# 1.c. Para el modelo clásico (residuos RAW)
cat("\n--- Modelo Clásico (RAW) ---\n")
global_moran_classic_raw <- moran.test(mean_residual_classic, listw = W_dummy, zero.policy = TRUE)
print(global_moran_classic_raw)

# 1.d. Para el modelo clásico (residuos Pearson)
cat("\n--- Modelo Clásico (Pearson) ---\n")
# Seleccionar los índices sin NA para el vector de residuos Pearson del modelo clásico
idx_classic <- which(!is.na(mean_pearson_classic))
logical_idx_classic <- rep(FALSE, length(W_dummy$neighbours))
logical_idx_classic[idx_classic] <- TRUE
W_dummy_classic <- subset(W_dummy, subset = logical_idx_classic)
global_moran_classic_pearson <- moran.test(mean_pearson_classic[idx_classic], listw = W_dummy_classic, zero.policy = TRUE)
print(global_moran_classic_pearson)


# ------------------------------------------------------------
# Análisis detallado de efectos marginales (solo consola)
# ------------------------------------------------------------

# Instala 'gratia' si es necesario
if (!requireNamespace("gratia", quietly = TRUE)) {
  install.packages("gratia", repos = "https://cloud.r-project.org")
}

library(mgcv)
library(gratia)
library(dplyr)

vars <- c(
  "S1902_C02_001E", "S2403_C01_003E", "S2504_C03_018E",
  "S2503_C06_019E", "S2402_C01_018E", "S2506_C01_047E"
)

for (v in vars) {
  cat("\n====================================================\n")
  cat("Variable explicativa:", v, "\n")
  cat("====================================================\n\n")
  
  # 1) Ajuste GAM
  f  <- as.formula(paste0("AMZ ~ s(", v, ", bs = 'tp')"))
  m  <- gam(f, data = datos_analisis_escalado, family = binomial(link = "logit"))
  sm <- summary(m)
  
  # 2) Estadísticos del modelo
  edf_val  <- sm$s.table[1, "edf"]
  r2adj    <- sm$r.sq
  dev_expl <- sm$dev.expl * 100
  ubre     <- sm$sp.criterion
  nobs     <- sm$n
  
  cat(sprintf(
    "• EDF (suave): %.3f | R² ajustado: %.3f | Deviance explicada: %.1f%%\n",
    edf_val, r2adj, dev_expl
  ))
  cat(sprintf(
    "• UBRE: %.4f | Observaciones: %d\n\n",
    ubre, nobs
  ))
  
  # 3) Grid de predicción
  grid <- tibble(
    !!v := seq(
      min(datos_analisis_escalado[[v]], na.rm = TRUE),
      max(datos_analisis_escalado[[v]], na.rm = TRUE),
      length.out = 200
    )
  )
  pr <- predict(m, newdata = grid, type = "link", se.fit = TRUE)
  grid <- grid %>%
    mutate(
      eta = pr$fit,
      se  = pr$se.fit,
      fit = plogis(eta),
      lwr = plogis(eta - 1.96 * se),
      upr = plogis(eta + 1.96 * se)
    )
  
  # 4) Resumen de la curva predicha
  fit_vals <- grid$fit
  cat(sprintf(
    "• Fit (Pr) — Min: %.3f | 10%%: %.3f | 50%%: %.3f | 90%%: %.3f | Max: %.3f\n",
    min(fit_vals), quantile(fit_vals, .1), median(fit_vals),
    quantile(fit_vals, .9), max(fit_vals)
  ))
  cat("• IC95 fit en puntos clave:\n")
  key_idx <- c(1, 50, 100, 150, 200)
  for (i in key_idx) {
    cat(sprintf(
      "    - %s = %.3f → [%.3f, %.3f]\n",
      v, grid[[v]][i], grid$lwr[i], grid$upr[i]
    ))
  }
  cat("\n")
  
  # 5) Derivadas y umbrales (uso 'select' con partial_match)
  der <- derivatives(
    object        = m,
    select        = v,
    type          = "central",
    n             = 200,
    partial_match = TRUE
  )
  
  # Detectar nombres de columna para derivada y su error estándar
  der_col <- grep("derivative", names(der), value = TRUE)[1]
  se_col  <- grep("^(se|std)", names(der), value = TRUE)[1]
  
  if (is.na(der_col) || is.na(se_col)) {
    cat("• Atención: no se encontraron derivadas o su error estándar para esta variable.\n")
  } else {
    der_vals <- der[[der_col]]
    se_vals  <- der[[se_col]]
    
    cat(sprintf(
      "• Derivada (dPr/d%s) — Min: %.4f | Mediana: %.4f | Máx: %.4f | SD: %.4f\n",
      v, min(der_vals), median(der_vals),
      max(der_vals), sd(der_vals)
    ))
    
    # Umbrales significativos
    sig_pos <- der_vals - 1.96 * se_vals > 0
    sig_neg <- der_vals + 1.96 * se_vals < 0
    thr <- der %>%
      filter(sig_pos | sig_neg) %>%
      summarize(
        desde = round(min(data), 4),
        hasta = round(max(data), 4),
        npts  = n()
      )
    
    if (nrow(thr) > 0 && !any(is.na(thr))) {
      cat(sprintf(
        "• Cambio significativo (derivada 95%% CI ≠ 0) desde %.4f hasta %.4f (n = %d puntos)\n",
        thr$desde, thr$hasta, thr$npts
      ))
    } else {
      cat("• No hay regiones con cambio estadísticamente significativo.\n")
    }
  }
  
  # 6) Tabla de términos suaves
  cat("\nTabla 's.table':\n")
  print(sm$s.table)
  cat("\n----------------------------------------------------\n\n")
}


