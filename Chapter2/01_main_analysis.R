# ==============================================================================
# Título: Efecto de la presencia logística de Amazon sobre los salarios minoristas en Estados Unidos 2023 a nivel condado
#
# Autores:
#   1. Juan Pablo Rodríguez Paredes* (Estudiante de doctorado)
#   2. Coro Chasco Yrigoyen (Dirección de tesis)
#   3. Beatriz Sánchez Reyes (Dirección de tesis)
#
# Afiliaciones:
#   1. Departamento de Economía Aplicada, Universidad Autónoma de Madrid, España
#      Email: jpablo.rodriguez@estudiante.uam.es
#   2. Departamento de Economía Aplicada, Universidad Autónoma de Madrid, España
#      Email: coro.chasco@uam.es
#   3. Departamento de Economía Aplicada, Universidad Autónoma de Madrid, España
#      Email: beatriz.sanchez@uam.es
#
# Fecha de la última actualización: 2025-04-22
# Versión: 1.0
# Descripción: Script para analizar el impacto de la infraestructura logística de Amazon en los salarios del sector minorista a nivel de condado en EE.UU. durante 2023.
# Reproducibilidad: Sí
# ==============================================================================

# 1. Configuración del Entorno de Trabajo
# Establecer semilla aleatoria para replicabilidad
set.seed(1234)

# Liberar memoria
gc()

# 2. Vector con los paquetes requeridos
# 2. Vector con los paquetes requeridos
required_packages <- c(
  "brms",       # Modelos bayesianos con Stan
  "BART",       # Matching con BART
  "rstanarm",   # Modelos bayesianos (stan_glm)
  "MatchIt",    # Emparejamientos clásicos
  "sf",         # Datos espaciales
  "glmnet",     # Regresión penalizada
  "dplyr",      # Manipulación de datos
  "cobalt",     # Diagnóstico de balance
  "readr",      # Lectura de CSV
  "survey",     # Datos de encuesta
  "gstat",      # Kriging y variogramas
  "ggplot2",    # Visualización
  "tidyr",      # Transformación de datos
  "knitr",      # Salida en Markdown
  "viridis",    # Paletas de color
  "showtext",   # Tipografías en gráficos
  "gridExtra",  # Manipulación avanzada de gráficos
  "xtable",     # Tablas LaTeX
  "pROC",       # Curvas ROC
  "mgcv",       # GAMs
  "cowplot",    # Combinación de gráficos
  "gratia"      # GAMs diagnostics (Añadido desde el inicio para evitar errores)
)

# 2.1 Definir librería local del proyecto
r_mm <- paste0(R.Version()$major, ".", sub("\\..*$", "", R.Version()$minor))
lib_custom <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "R", "win-library",
  r_mm,
  "Project_Libs"
)
if (!dir.exists(lib_custom)) dir.create(lib_custom, recursive = TRUE)

# 2.2 Priorizar librería local del proyecto
.libPaths(c(lib_custom, .libPaths()))

# 2.3 Verificar versión instalada de dplyr
get_pkg_version <- function(pkg, libs = .libPaths()) {
  for (lib in libs) {
    v <- tryCatch(utils::packageVersion(pkg, lib.loc = lib), error = function(e) NULL)
    if (!is.null(v)) return(v)
  }
  return(NULL)
}
dplyr_ver <- get_pkg_version("dplyr")
need_update_dplyr <- is.null(dplyr_ver) || (utils::compareVersion(as.character(dplyr_ver), "1.2.0") < 0)


# 2.4 Instalar dplyr en librería local
if (need_update_dplyr) {
  if ("package:dplyr" %in% search()) detach("package:dplyr", unload = TRUE)
  if ("dplyr" %in% loadedNamespaces()) try(unloadNamespace("dplyr"), silent = TRUE)
  
  install.packages(
    "dplyr",
    lib = lib_custom,
    repos = "https://cloud.r-project.org",
    dependencies = NA
  )
}


# 2.5 Instalar paquetes faltantes requeridos local
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    lib = lib_custom,
    repos = "https://cloud.r-project.org",
    dependencies = NA
  )
}


# 2.6 Cargar paquetes del análisis
invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))


# --- Fix INLA + fmesher (Windows/R 4.5.x) ---
options(repos = c(
  CRAN = "https://cloud.r-project.org",
  INLA = "https://inla.r-inla-download.org/R/stable"
))

need <- c("fmesher", "INLA")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

library(fmesher)  # validación explícita
library(INLA)


# 🔧 Instalar el paquete 'sn' si es requerido por inla.posterior.sample()
if (!requireNamespace("sn", quietly = TRUE)) {
  install.packages("sn")
}

# 🎨 Activar showtext para que las fuentes se vean bien en gráficos
showtext::showtext_auto()

# ==============================================================================
# 3. Importación de Datos
# ==============================================================================

load("C:/Users/Rodriguez/OneDrive - UAM/Documentos/0 Paper Markov_PC/3. Condados/Paper Condados espacial/R/ACS_depurada.RData")

# ==============================================================================
# 4. Construcción y validación de la malla espacial (INLA)
# ==============================================================================

crs_info <- st_crs(shapefile_malla)
print(crs_info)

crs_description <- crs_info$wkt
if (grepl("USA_Contiguous_Albers_Equal_Area_Conic", crs_description, ignore.case = TRUE)) {
  cat("El CRS es un sistema planar (Albers Equal Area). No requiere reproyección.\n")
} else {
  cat("El CRS no es un sistema planar estándar; revisa el WKT o reproyecta.\n")
}

shapefile_malla <- st_centroid(shapefile_malla)
coordinates <- st_coordinates(shapefile_malla)

crs_units <- crs_info$units

if (crs_units == "m") {
  cat("\n✅ Creando malla para condados (unidades en metros)...\n")
  mesh <- inla.mesh.2d(
    loc = coordinates,
    max.edge = c(50000, 100000),
    cutoff = 5000,
    offset = c(50000, 100000)
  )
  if (mesh$n > 15000) {
    warning("¡Malla demasiado densa para condados! Considera aumentar cutoff.")
  }
} else {
  stop("❌ CRS debe estar en metros (Albers). Por favor, reproyecta.")
}

cat("\n🔍 Resumen de la malla (condados):\n")
cat("- Nodos:", mesh$n, "\n")
cat("- Triángulos:", nrow(mesh$graph$tv), "\n")

# (Opcional) Calcular área promedio de los triángulos si existe 'mesh$triangles'
if (!is.null(mesh$triangles$area)) {
  area_prom <- mean(mesh$triangles$area)/1e6
  cat("- Área promedio de triángulos:", round(area_prom, 2), "km²\n")
}

# Graficar la malla
png("malla_condados.png", width = 1200, height = 800)
plot(mesh, main = "Malla INLA para Análisis a Nivel de Condado")
points(coordinates, col = "red", pch = 16, cex = 0.6)
dev.off()

cat("\n✅ Malla INLA creada con éxito.\n")

# ==============================================================================
# 5. Modelo bayesiano espacial para propensity scores (INLA)
# ==============================================================================

# 5.1 Chequeo de naturaleza binaria de AMZ
cat("🔍 Verificando que 'AMZ' es binaria (0/1):\n")
print(table(datos_analisis_escalado$AMZ))
if (!all(unique(datos_analisis_escalado$AMZ) %in% c(0, 1))) {
  stop("❌ La variable AMZ no es binaria. Corrige antes de proceder.")
}

# 5.2 Definición del componente espacial SPDE
# Configurar el modelo SPDE con parámetros para la escala de condado (en metros)
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 2,
  prior.range = c(100000, 0.5),   # Prior: Pr(r < 100 km) = 0.5
  prior.sigma = c(1, 0.1)         # Prior: Pr(sigma > 1) = 0.1
)

# Matriz de diseño espacial: asocia los centroides a los nodos de la malla
A.est <- inla.spde.make.A(mesh = mesh, loc = coordinates)

# Índice para efectos espaciales basado en la malla
s.index <- inla.spde.make.index("espacial", spde$n.spde)

# 5.3 Creación del stack de datos para INLA
covariables <- c("S1902_C02_001E", "S2403_C01_003E", "S2504_C03_018E",
                 "S2503_C06_019E", "S2402_C01_018E", "S2506_C01_047E")

stack <- inla.stack(
  data = list(y = datos_analisis_escalado$AMZ),  # Respuesta
  A = list(A.est, 1),  # A.est para efecto espacial y 1 para efectos fijos
  effects = list(
    espacial = s.index,
    data.frame(intercept = 1, datos_analisis_escalado[, covariables])
  ),
  tag = "modelo_AMZ"
)

# 5.4 Especificación de la fórmula bayesiana
formula <- y ~ 0 + intercept +
  S1902_C02_001E + S2403_C01_003E + S2504_C03_018E +
  S2503_C06_019E + S2402_C01_018E + S2506_C01_047E +
  f(espacial, model = spde)

# 5.5 Estimación del modelo en INLA
cat("⏳ Ajustando modelo logístico bayesiano espacial...\n")
modelo_inla <- inla(
  formula,
  data = inla.stack.data(stack),
  family = "binomial",
  control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  control.fixed = list(mean = 0, prec = 0.01)
)

cat("✅ Modelo ajustado correctamente.\n")

# 5.6 Resumen del ajuste
cat("\n=== Resumen del Modelo Espacial Bayesiano ===\n")
print(summary(modelo_inla))

# 5.7 Proyección y visualización del efecto espacial
# Para ello se proyecta el efecto espacial (calculado a nivel de la malla)
# a las ubicaciones de las observaciones mediante la matriz de proyección A.est
efecto_espacial_obs <- as.vector(A.est %*% modelo_inla$summary.random$espacial$mean)

# Crear data frame con las coordenadas de los centroides y el efecto espacial proyectado
mapa_espacial <- data.frame(coordinates, efecto_espacial = efecto_espacial_obs)

# Graficar el efecto espacial
library(ggplot2)
ggplot(mapa_espacial, aes(X, Y, color = efecto_espacial)) +
  geom_point(size = 1.5) +
  scale_color_viridis_c(option = "B") +
  labs(
    title = "🌍 Efecto Espacial - Modelo AMZ (INLA)",
    x = "Coordenada X (m)", y = "Coordenada Y (m)",
    color = "Efecto espacial"
  ) +
  theme_minimal()

# 5.8 Cálculo de propensity scores bayesianos
# Extraer los índices correspondientes a las observaciones del stack
idx <- inla.stack.index(stack, tag = "modelo_AMZ")$data

# Asignar los propensity scores (probabilidades predichas) al data frame de análisis escalado
datos_analisis_escalado$propensity_score <- modelo_inla$summary.fitted.values[idx, "mean"]

# Verificar el resumen de los propensity scores
cat("Resumen de los Propensity Scores:\n")
print(summary(datos_analisis_escalado$propensity_score))

# Graficar la distribución de los propensity scores
library(ggplot2)
ggplot(datos_analisis_escalado, aes(x = propensity_score)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Distribución de Propensity Scores",
    x = "Propensity Score",
    y = "Densidad"
  ) +
  theme_minimal()

# ==============================================================================
# 6. Fusión de datos externos y creación de moderadores
# ==============================================================================
library(readr)
library(dplyr)
library(survey)
library(gstat)
if (!require("cobalt")) {
  install.packages("cobalt")
  library(cobalt)
}

# 6.1 Cargar datos exógenos y unir con base de análisis
qwi_data <- read_csv(
  "C:/Users/Rodriguez/OneDrive - UAM/Documentos/0 Paper Markov_PC/5. PUMAs/PUMA_eco/Excel/QWI/qwi_958444104b164a8ab67704728ee1fb1f.csv",
  col_types = cols(geography = col_character())
)

head(datos_analisis_escalado$GEOIDFQ, 30)
head(qwi_data$geography, 30)

# Crear la columna fips
datos_analisis_escalado$fips <- substr(datos_analisis_escalado$GEOIDFQ, 10, 15)

# Unir con qwi_data
datos_unidos <- left_join(datos_analisis_escalado, qwi_data, by = c("fips" = "geography"))
datos_unidos <- datos_unidos[, c("fips", setdiff(names(datos_unidos), "fips"))]

# 6.2 Incorporar datos CRE y unir por 'fips'
ruta_csv <- "C:/Users/Rodriguez/OneDrive - UAM/Documentos/0 Paper Markov_PC/5. PUMAs/PUMA_eco/Excel/QWI/CRE_23_County.csv"
datos_cre <- read.csv(ruta_csv)

head(datos_cre)
head(datos_cre$GEO_ID, 20)
head(datos_unidos[, c("fips", "GEOIDFQ")], 20)

datos_cre$fips <- substr(datos_cre$GEO_ID, 10, 14)
datos_cre <- datos_cre[, c("fips", setdiff(names(datos_cre), "fips"))]

datos_merged <- merge(datos_unidos, datos_cre, by = "fips", all.x = TRUE)

head(datos_merged[, c("EarnBeg", "EarnS", "PRED3_PE", "PRED0_PE")])
if (!"GEOIDFQ" %in% names(datos_merged)) stop("La columna 'GEOIDFQ' no está en datos_merged.")
if (!"GEOIDFQ" %in% names(shapefile_malla)) stop("La columna 'GEOIDFQ' no está en shapefile_malla.")

# 6.3 Combinación con shapefile
datos_combinados <- left_join(
  shapefile_malla,
  datos_merged[, c("GEOIDFQ", "EarnS", "EarnBeg", "PRED0_PE")],
  by = "GEOIDFQ"
)
# Asegurar que PRED0_PE sea numérico (si no lo es)
if (!is.numeric(datos_combinados$PRED0_PE)) {
  datos_combinados$PRED0_PE <- as.numeric(as.character(datos_combinados$PRED0_PE))
}

str(datos_combinados)

cat("NA en EarnS:", sum(is.na(datos_combinados$EarnS)), "\n")
cat("NA en EarnBeg:", sum(is.na(datos_combinados$EarnBeg)), "\n")

# 6.4 Interpolación espacial de variables incompletas (kriging)
if (!inherits(datos_combinados, "sf")) {
  datos_combinados <- st_as_sf(datos_combinados)
}

# Interpolar EarnS
train_data_S <- datos_combinados[!is.na(datos_combinados$EarnS), ]
pred_locs_S  <- datos_combinados[ is.na(datos_combinados$EarnS), ]
vario_S      <- variogram(EarnS ~ 1, train_data_S)
fit_vario_S  <- fit.variogram(vario_S, model = vgm("Exp"))
krige_result_S <- krige(EarnS ~ 1, locations = train_data_S, newdata = pred_locs_S, model = fit_vario_S)
datos_combinados$EarnS[is.na(datos_combinados$EarnS)] <- krige_result_S$var1.pred

# Interpolar EarnBeg (aunque no se use en el ATT)
train_data_B <- datos_combinados[!is.na(datos_combinados$EarnBeg), ]
pred_locs_B  <- datos_combinados[ is.na(datos_combinados$EarnBeg), ]
vario_B      <- variogram(EarnBeg ~ 1, train_data_B)
fit_vario_B  <- fit.variogram(vario_B, model = vgm("Sph"))
krige_result_B <- krige(EarnBeg ~ 1, locations = train_data_B, newdata = pred_locs_B, model = fit_vario_B)
datos_combinados$EarnBeg[is.na(datos_combinados$EarnBeg)] <- krige_result_B$var1.pred

cat("NA en EarnS post-interpol:", sum(is.na(datos_combinados$EarnS)), "\n")
cat("NA en EarnBeg post-interpol:", sum(is.na(datos_combinados$EarnBeg)), "\n")

plot(vario_S, fit_vario_S, main = "Variograma EarnS (Exp)")
plot(vario_B, fit_vario_B, main = "Variograma EarnBeg (Esférico)")

summary(datos_combinados$EarnS)
summary(datos_combinados$EarnBeg)

###############################################################################
# 7. Modelo ATT bayesiano (sin componente espacial)
# Tipo de estimador: ATT (Average Treatment effect on the Treated)
# Método: nearest‑neighbor matching con caliper = 0.1 y 1000 draws de INLA
# Se calcula el coeficiente “treated” sobre log(EarnS) y se estiman efectos
# para los subgrupos de resiliencia (PRED0_PE: “Baja” vs “Alta”).
###############################################################################

# Crear un data.frame con las variables de interés (sin geometría)
datos_seleccionados <- st_drop_geometry(datos_combinados[, c("GEOIDFQ", "EarnS", "AMZ")])
datos_seleccionados <- merge(
  datos_seleccionados,
  datos_merged[, c("GEOIDFQ", "PRED0_PE")],
  by = "GEOIDFQ",
  all.x = TRUE
)
datos_seleccionados <- merge(
  datos_seleccionados,
  datos_analisis_escalado[, c("GEOIDFQ", "propensity_score")],
  by = "GEOIDFQ",
  all.x = TRUE
)
# 7.1 Preparación de datos para matching
df_base <- datos_analisis_escalado %>%
  mutate(treated = as.numeric(AMZ)) %>%
  dplyr::select(GEOIDFQ, treated)

# 7.2 Muestras bayesianas de propensity scores
extract_ps_samples <- function(posterior_samples, n_obs, pattern = "^APredictor:") {
  latent_names <- rownames(posterior_samples[[1]]$latent)
  predictor_names <- grep(pattern, latent_names, value = TRUE)
  predictor_ids <- as.integer(sub(pattern, "", predictor_names))
  ordered_idx   <- order(predictor_ids)
  idx_eta_all   <- grep(pattern, latent_names)
  idx_eta_sorted <- idx_eta_all[ordered_idx]
  
  idx_obs <- idx_eta_sorted[1:n_obs]
  
  ps_samples <- sapply(posterior_samples, function(s) {
    eta_i <- s$latent[idx_obs]
    1 / (1 + exp(-eta_i))
  })
  
  return(ps_samples)  # [n_obs x n_draws]
}

# 7.3 Configuración de caliper y número de draws
calipers   <- c(0.1)
draws_list <- c(1000)

# 7.4 Estructura para almacenar resultados
resultado_sensibilidad <- data.frame(
  Escenario        = character(),
  Caliper          = numeric(),
  N_Draws          = numeric(),
  ATT_total_mean   = numeric(),
  ATT_total_lower  = numeric(),
  ATT_total_upper  = numeric(),
  ATT_vuln_mean    = numeric(),
  ATT_vuln_lower   = numeric(),
  ATT_vuln_upper   = numeric(),
  ATT_resil_mean   = numeric(),
  ATT_resil_lower  = numeric(),
  ATT_resil_upper  = numeric(),
  stringsAsFactors = FALSE
)

# 7.5 Ejecución del matching y estimación ATT
for (i in seq_along(calipers)) {
  caliper_bayes <- calipers[i]
  n_draws       <- draws_list[i]
  
  cat("\n-----------------------------------------------------------\n")
  cat("Escenario", i, ": caliper =", caliper_bayes, ", draws =", n_draws, "\n")
  cat("-----------------------------------------------------------\n")
  
  # A) Tomar muestras del posterior
  posterior_samples <- inla.posterior.sample(n_draws, modelo_inla)
  
  # B) Extraer propensity scores simulados
  n_obs   <- nrow(df_base)
  ps_draws <- extract_ps_samples(posterior_samples, n_obs, pattern = "^APredictor:")
  
  # C) Contenedores para ATT
  att_draws_total <- numeric(n_draws)
  att_draws_vuln  <- numeric(n_draws)
  att_draws_resil <- numeric(n_draws)
  
  # D) Para cada muestra
  for (m in seq_len(n_draws)) {
    df_draw <- df_base %>% mutate(ps_draw = ps_draws[, m])
    
    match_obj <- matchit(
      formula  = treated ~ 1,
      data     = df_draw,
      method   = "nearest",
      distance = df_draw$ps_draw,
      caliper  = caliper_bayes,
      replace  = FALSE
    )
    
    df_matched <- match.data(match_obj)
    
    # Unir con EarnS y PRED0_PE
    df_matched <- merge(
      df_matched,
      datos_seleccionados[, c("GEOIDFQ", "EarnS", "PRED0_PE")],
      by = "GEOIDFQ",
      all.x = TRUE
    )
    
    # log(EarnS)
    df_matched$logEarnS <- log(df_matched$EarnS)
    
    # Subgrupos
    cuartiles_res <- quantile(df_matched$PRED0_PE, probs = c(0.25, 0.75), na.rm = TRUE)
    df_matched$grupo_resiliencia <- NA_character_
    df_matched$grupo_resiliencia[df_matched$PRED0_PE < cuartiles_res[1]] <- "Baja"
    df_matched$grupo_resiliencia[df_matched$PRED0_PE > cuartiles_res[2]] <- "Alta"
    
    design_emp <- svydesign(ids = ~1, weights = ~weights, data = df_matched)
    
    # ATT total
    mod_total <- svyglm(logEarnS ~ treated, design = design_emp)
    att_draws_total[m] <- coef(mod_total)["treated"]
    
    # ATT vulnerable
    design_baja <- subset(design_emp, grupo_resiliencia == "Baja")
    if (nrow(design_baja$variables) > 0) {
      mod_baja <- svyglm(logEarnS ~ treated, design = design_baja)
      att_draws_vuln[m] <- coef(mod_baja)["treated"]
    } else {
      att_draws_vuln[m] <- NA
    }
    
    # ATT resiliente
    design_alta <- subset(design_emp, grupo_resiliencia == "Alta")
    if (nrow(design_alta$variables) > 0) {
      mod_alta <- svyglm(logEarnS ~ treated, design = design_alta)
      att_draws_resil[m] <- coef(mod_alta)["treated"]
    } else {
      att_draws_resil[m] <- NA
    }
  }
  
  # E) Estadísticas resumen
  att_total_mean  <- mean(att_draws_total, na.rm = TRUE)
  att_total_lower <- quantile(att_draws_total, probs = 0.025, na.rm = TRUE)
  att_total_upper <- quantile(att_draws_total, probs = 0.975, na.rm = TRUE)
  
  att_vuln_mean   <- mean(att_draws_vuln, na.rm = TRUE)
  att_vuln_lower  <- quantile(att_draws_vuln, probs = 0.025, na.rm = TRUE)
  att_vuln_upper  <- quantile(att_draws_vuln, probs = 0.975, na.rm = TRUE)
  
  att_resil_mean  <- mean(att_draws_resil, na.rm = TRUE)
  att_resil_lower <- quantile(att_draws_resil, probs = 0.025, na.rm = TRUE)
  att_resil_upper <- quantile(att_draws_resil, probs = 0.975, na.rm = TRUE)
  
  resultado_sensibilidad <- rbind(
    resultado_sensibilidad,
    data.frame(
      Escenario        = paste0("Escenario_", i),
      Caliper          = caliper_bayes,
      N_Draws          = n_draws,
      ATT_total_mean   = att_total_mean,
      ATT_total_lower  = att_total_lower,
      ATT_total_upper  = att_total_upper,
      ATT_vuln_mean    = att_vuln_mean,
      ATT_vuln_lower   = att_vuln_lower,
      ATT_vuln_upper   = att_vuln_upper,
      ATT_resil_mean   = att_resil_mean,
      ATT_resil_lower  = att_resil_lower,
      ATT_resil_upper  = att_resil_upper,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n=====================================================\n")
cat("RESUMEN FINAL - ANÁLISIS DE SENSIBILIDAD (BAYESIANO)\n")
cat("=====================================================\n")
print(resultado_sensibilidad)


###############################################################################
# 7.6 Diagnóstico de balance (SMD y pares emparejados)
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
# 8. Modelo ATT bayesiano con efecto espacial observado
# Tipo de estimador: ATT (Average Treatment effect on the Treated)
# Método: nearest‑neighbor matching con caliper = 0.1 y 1000 draws de INLA
# El modelo incluye como covariable el efecto espacial observado (efecto_espacial).
# Se estima el efecto “treated” sobre log(EarnS) y sus variantes por resiliencia.
###############################################################################

# 8.1 Integración del componente espacial
mapa_efecto <- data.frame(
  GEOIDFQ = shapefile_malla$GEOIDFQ,
  efecto_espacial = efecto_espacial_obs
)
# Merge del efecto espacial a datos_analisis_escalado
datos_analisis_escalado <- merge(
  datos_analisis_escalado,
  mapa_efecto,
  by = "GEOIDFQ",
  all.x = TRUE
)

# 8.2 Preparación de datos para matching espacial
df_base <- datos_analisis_escalado %>%
  mutate(treated = as.numeric(AMZ)) %>%
  dplyr::select(GEOIDFQ, treated)

# 8.3 Función de muestreo PSD bayesiano (reutilizada)
extract_ps_samples <- function(posterior_samples, n_obs, pattern = "^APredictor:") {
  latent_names <- rownames(posterior_samples[[1]]$latent)
  predictor_names <- grep(pattern, latent_names, value = TRUE)
  predictor_ids <- as.integer(sub(pattern, "", predictor_names))
  ordered_idx   <- order(predictor_ids)
  idx_eta_all   <- grep(pattern, latent_names)
  idx_eta_sorted <- idx_eta_all[ordered_idx]
  
  idx_obs <- idx_eta_sorted[1:n_obs]
  
  ps_samples <- sapply(posterior_samples, function(s) {
    eta_i <- s$latent[idx_obs]
    1 / (1 + exp(-eta_i))
  })
  
  return(ps_samples)  # [n_obs x n_draws]
}

# 8.4 Parámetros de simulación espacial
calipers   <- c(0.1)
draws_list <- c(1000)

# 8.5 Estructura resultados espaciales
resultado_sensibilidad_spatial <- data.frame(
  Escenario        = character(),
  Caliper          = numeric(),
  N_Draws          = numeric(),
  ATT_total_mean   = numeric(),
  ATT_total_lower  = numeric(),
  ATT_total_upper  = numeric(),
  ATT_vuln_mean    = numeric(),
  ATT_vuln_lower   = numeric(),
  ATT_vuln_upper   = numeric(),
  ATT_resil_mean   = numeric(),
  ATT_resil_lower  = numeric(),
  ATT_resil_upper  = numeric(),
  stringsAsFactors = FALSE
)

# 8.6 Matching espacial y cálculo de ATT
for (i in seq_along(calipers)) {
  caliper_bayes <- calipers[i]
  n_draws       <- draws_list[i]
  
  cat("\n-----------------------------------------------------------\n")
  cat("Escenario", i, ": caliper =", caliper_bayes, ", draws =", n_draws, "\n")
  cat("-----------------------------------------------------------\n")
  
  # A) Extraer muestras del posterior
  posterior_samples <- inla.posterior.sample(n_draws, modelo_inla)
  
  # B) Extraer propensity scores simulados
  n_obs   <- nrow(df_base)
  ps_draws <- extract_ps_samples(posterior_samples, n_obs, pattern = "^APredictor:")
  
  # C) Contenedores por muestra
  att_draws_total <- numeric(n_draws)
  att_draws_vuln  <- numeric(n_draws)
  att_draws_resil <- numeric(n_draws)
  
  # D) Matching, merge y estimación por muestra
  for (m in seq_len(n_draws)) {
    df_draw <- df_base %>% mutate(ps_draw = ps_draws[, m])
    
    # Realizar matching utilizando la muestra de PS simulada
    match_obj <- matchit(
      formula  = treated ~ 1,   # Nota: el matching sigue sin incluir covariables; se basa en ps_draw
      data     = df_draw,
      method   = "nearest",
      distance = df_draw$ps_draw,
      caliper  = caliper_bayes,
      replace  = FALSE
    )
    
    df_matched <- match.data(match_obj)
    
    # Unir las variables de resultado, moderador y el efecto espacial
    # Se asume que en 'datos_seleccionados' se encuentran GEOIDFQ, EarnS y PRED0_PE
    df_matched <- merge(
      df_matched,
      datos_seleccionados[, c("GEOIDFQ", "EarnS", "PRED0_PE")],
      by = "GEOIDFQ",
      all.x = TRUE
    )
    # Unir la variable efecto_espacial desde la base principal
    df_matched <- merge(
      df_matched,
      datos_analisis_escalado[, c("GEOIDFQ", "efecto_espacial")],
      by = "GEOIDFQ",
      all.x = TRUE
    )
    
    # Calcular log(EarnS)
    df_matched$logEarnS <- log(df_matched$EarnS)
    
    # Definir subgrupos según el moderador PRED0_PE
    cuartiles_res <- quantile(df_matched$PRED0_PE, probs = c(0.25, 0.75), na.rm = TRUE)
    df_matched$grupo_resiliencia <- NA_character_
    df_matched$grupo_resiliencia[df_matched$PRED0_PE < cuartiles_res[1]] <- "Baja"
    df_matched$grupo_resiliencia[df_matched$PRED0_PE > cuartiles_res[2]] <- "Alta"
    
    # Crear diseño de survey (se usa un identificador simple; los pesos vienen del matching si se hubieran generado)
    design_emp <- svydesign(ids = ~1, weights = ~weights, data = df_matched)
    
    # E) Estimación ATT total con efecto espacial
    mod_total <- svyglm(logEarnS ~ treated + efecto_espacial, design = design_emp)
    att_draws_total[m] <- coef(mod_total)["treated"]
    
    # F) Subgrupo "Baja" resiliencia
    design_baja <- subset(design_emp, grupo_resiliencia == "Baja")
    if (nrow(design_baja$variables) > 0) {
      mod_baja <- svyglm(logEarnS ~ treated + efecto_espacial, design = design_baja)
      att_draws_vuln[m] <- coef(mod_baja)["treated"]
    } else {
      att_draws_vuln[m] <- NA
    }
    
    # G) Subgrupo "Alta" resiliencia
    design_alta <- subset(design_emp, grupo_resiliencia == "Alta")
    if (nrow(design_alta$variables) > 0) {
      mod_alta <- svyglm(logEarnS ~ treated + efecto_espacial, design = design_alta)
      att_draws_resil[m] <- coef(mod_alta)["treated"]
    } else {
      att_draws_resil[m] <- NA
    }
  }
  
  # H) Resumen de resultados
  att_total_mean  <- mean(att_draws_total, na.rm = TRUE)
  att_total_lower <- quantile(att_draws_total, probs = 0.025, na.rm = TRUE)
  att_total_upper <- quantile(att_draws_total, probs = 0.975, na.rm = TRUE)
  
  att_vuln_mean   <- mean(att_draws_vuln, na.rm = TRUE)
  att_vuln_lower  <- quantile(att_draws_vuln, probs = 0.025, na.rm = TRUE)
  att_vuln_upper  <- quantile(att_draws_vuln, probs = 0.975, na.rm = TRUE)
  
  att_resil_mean  <- mean(att_draws_resil, na.rm = TRUE)
  att_resil_lower <- quantile(att_draws_resil, probs = 0.025, na.rm = TRUE)
  att_resil_upper <- quantile(att_draws_resil, probs = 0.975, na.rm = TRUE)
  
  resultado_sensibilidad_spatial <- rbind(
    resultado_sensibilidad_spatial,
    data.frame(
      Escenario        = paste0("Escenario_", i),
      Caliper          = caliper_bayes,
      N_Draws          = n_draws,
      ATT_total_mean   = att_total_mean,
      ATT_total_lower  = att_total_lower,
      ATT_total_upper  = att_total_upper,
      ATT_vuln_mean    = att_vuln_mean,
      ATT_vuln_lower   = att_vuln_lower,
      ATT_vuln_upper   = att_vuln_upper,
      ATT_resil_mean   = att_resil_mean,
      ATT_resil_lower  = att_resil_lower,
      ATT_resil_upper  = att_resil_upper,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n=====================================================\n")
cat("RESUMEN FINAL - ATT CON COMPONENTE ESPACIAL (BAYESIANO)\n")
cat("=====================================================\n")
print(resultado_sensibilidad_spatial)




###############################################################################
# 9. Modelo ATT clásico con efecto espacial observado
# Tipo de estimador: ATT (Average Treatment effect on the Treated)
# Método: nearest‑neighbor matching con caliper = 0.1 y 1000 draws simulados
# Los propensity scores se estiman desde un modelo logit clásico (sin espacial).
# El modelo de resultado incluye el efecto espacial observado.
###############################################################################

library(MASS)  # para mvrnorm

# 9.1 Modelado clásico de propensity scores
modelo_classico <- glm(AMZ ~ S1902_C02_001E + S2403_C01_003E + S2504_C03_018E +
                         S2503_C06_019E + S2402_C01_018E + S2506_C01_047E,
                       data = datos_analisis_escalado, family = binomial)

# Obtener la matriz de diseño
X <- model.matrix(modelo_classico)

# 9.2 Muestreo de parámetros del logit clásico
set.seed(1234)  # para replicabilidad
n_draws <- 1000  # número de muestras a simular
beta_draws <- mvrnorm(n_draws, mu = coef(modelo_classico), Sigma = vcov(modelo_classico))

# Para cada observación, calcular draws de las probabilidades (propensity scores)
n_obs <- nrow(datos_analisis_escalado)
ps_draws_classic <- matrix(NA, nrow = n_obs, ncol = n_draws)
for(j in 1:n_draws){
  eta_draw <- X %*% beta_draws[j, ]
  ps_draws_classic[, j] <- 1 / (1 + exp(-eta_draw))
}

# Agregar la probabilidad estimada (punto estimado) por el modelo clásico al dataset
datos_analisis_escalado$propensity_score_classic <- predict(modelo_classico, type = "response")

# 9.3 Preparación de datos para matching clásico
df_base <- datos_analisis_escalado %>%
  mutate(treated = as.numeric(AMZ)) %>%
  dplyr::select(GEOIDFQ, treated)

# 9.4 Estructura resultados clásico
resultado_sensibilidad_classic <- data.frame(
  Escenario        = character(),
  Caliper          = numeric(),
  N_Draws          = numeric(),
  ATT_total_mean   = numeric(),
  ATT_total_lower  = numeric(),
  ATT_total_upper  = numeric(),
  ATT_vuln_mean    = numeric(),
  ATT_vuln_lower   = numeric(),
  ATT_vuln_upper   = numeric(),
  ATT_resil_mean   = numeric(),
  ATT_resil_lower  = numeric(),
  ATT_resil_upper  = numeric(),
  stringsAsFactors = FALSE
)

# 9.5 Configuración de caliper clásico
calipers <- c(0.1)

# 9.6 Matching clásico y estimación de ATT
for (i in seq_along(calipers)) {
  caliper_val <- calipers[i]
  cat("\n-----------------------------------------------------------\n")
  cat("Escenario (Clásico) ", i, ": caliper =", caliper_val, ", draws =", n_draws, "\n")
  cat("-----------------------------------------------------------\n")
  
  # Inicializar contenedores para las estimaciones de ATT en cada draw
  att_draws_total <- numeric(n_draws)
  att_draws_vuln  <- numeric(n_draws)
  att_draws_resil <- numeric(n_draws)
  
  for(m in 1:n_draws) {
    # Extraer los propensity scores del draw m
    df_draw <- df_base %>% mutate(ps_draw = ps_draws_classic[, m])
    
    # Realizar matching basado en ps_draw
    match_obj <- matchit(
      formula = treated ~ 1,  # Matching sin covariables extra; se basa en ps_draw
      data = df_draw,
      method = "nearest",
      distance = df_draw$ps_draw,
      caliper = caliper_val,
      replace = FALSE
    )
    df_matched <- match.data(match_obj)
    
    # Unir el outcome y el moderador (PRED0_PE) desde "datos_seleccionados"
    df_matched <- merge(
      df_matched,
      datos_seleccionados[, c("GEOIDFQ", "EarnS", "PRED0_PE")],
      by = "GEOIDFQ",
      all.x = TRUE
    )
    # Unir el efecto espacial (ya presente en datos_analisis_escalado)
    df_matched <- merge(
      df_matched,
      datos_analisis_escalado[, c("GEOIDFQ", "efecto_espacial")],
      by = "GEOIDFQ",
      all.x = TRUE
    )
    
    # Calcular log(EarnS) para el outcome
    df_matched$logEarnS <- log(df_matched$EarnS)
    
    # Definir subgrupos de resiliencia (por ejemplo, "Baja" y "Alta") a partir de PRED0_PE
    cuartiles_res <- quantile(df_matched$PRED0_PE, probs = c(0.25, 0.75), na.rm = TRUE)
    df_matched$grupo_resiliencia <- NA_character_
    df_matched$grupo_resiliencia[df_matched$PRED0_PE < cuartiles_res[1]] <- "Baja"
    df_matched$grupo_resiliencia[df_matched$PRED0_PE > cuartiles_res[2]] <- "Alta"
    
    # Crear diseño de survey para la estimación ponderada (usando los pesos del matching)
    design_emp <- svydesign(ids = ~1, weights = ~weights, data = df_matched)
    
    # Modelo de resultados que incluye tratamiento y efecto espacial:
    mod_total <- svyglm(logEarnS ~ treated + efecto_espacial, design = design_emp)
    att_draws_total[m] <- coef(mod_total)["treated"]
    
    # En subgrupo "Baja"
    design_baja <- subset(design_emp, grupo_resiliencia == "Baja")
    if(nrow(design_baja$variables) > 0) {
      mod_baja <- svyglm(logEarnS ~ treated + efecto_espacial, design = design_baja)
      att_draws_vuln[m] <- coef(mod_baja)["treated"]
    } else {
      att_draws_vuln[m] <- NA
    }
    
    # En subgrupo "Alta"
    design_alta <- subset(design_emp, grupo_resiliencia == "Alta")
    if(nrow(design_alta$variables) > 0) {
      mod_alta <- svyglm(logEarnS ~ treated + efecto_espacial, design = design_alta)
      att_draws_resil[m] <- coef(mod_alta)["treated"]
    } else {
      att_draws_resil[m] <- NA
    }
  }
  
  # 9.7 Estadísticas resumen
  att_total_mean  <- mean(att_draws_total, na.rm = TRUE)
  att_total_lower <- quantile(att_draws_total, probs = 0.025, na.rm = TRUE)
  att_total_upper <- quantile(att_draws_total, probs = 0.975, na.rm = TRUE)
  
  att_vuln_mean   <- mean(att_draws_vuln, na.rm = TRUE)
  att_vuln_lower  <- quantile(att_draws_vuln, probs = 0.025, na.rm = TRUE)
  att_vuln_upper  <- quantile(att_draws_vuln, probs = 0.975, na.rm = TRUE)
  
  att_resil_mean  <- mean(att_draws_resil, na.rm = TRUE)
  att_resil_lower <- quantile(att_draws_resil, probs = 0.025, na.rm = TRUE)
  att_resil_upper <- quantile(att_draws_resil, probs = 0.975, na.rm = TRUE)
  
  resultado_sensibilidad_classic <- rbind(
    resultado_sensibilidad_classic,
    data.frame(
      Escenario        = paste0("Escenario_", i),
      Caliper          = caliper_val,
      N_Draws          = n_draws,
      ATT_total_mean   = att_total_mean,
      ATT_total_lower  = att_total_lower,
      ATT_total_upper  = att_total_upper,
      ATT_vuln_mean    = att_vuln_mean,
      ATT_vuln_lower   = att_vuln_lower,
      ATT_vuln_upper   = att_vuln_upper,
      ATT_resil_mean   = att_resil_mean,
      ATT_resil_lower  = att_resil_lower,
      ATT_resil_upper  = att_resil_upper,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n=====================================================\n")
cat("RESUMEN FINAL - ATT CON COMPONENTE ESPACIAL (CLÁSICO)\n")
cat("=====================================================\n")
print(resultado_sensibilidad_classic)

###############################################################################
# 10. Modelo ATT clásico (sin efecto espacial)
# Tipo de estimador: ATT (Average Treatment effect on the Treated)
# Método: nearest‑neighbor matching con caliper = 0.1 y 1000 draws simulados
# Los propensity scores se estiman desde un modelo logit clásico tradicional.
# El modelo de resultado no incluye el efecto espacial.
###############################################################################

# 10.1 Preparación de base para matching final
df_base_classic <- datos_analisis_escalado %>%
  mutate(treated = as.numeric(AMZ)) %>%
  dplyr::select(GEOIDFQ, treated)

# 10.2 Estructura resultados final
resultado_sensibilidad_classic_nospatial <- data.frame(
  Escenario        = character(),
  Caliper          = numeric(),
  N_Draws          = numeric(),
  ATT_total_mean   = numeric(),
  ATT_total_lower  = numeric(),
  ATT_total_upper  = numeric(),
  ATT_vuln_mean    = numeric(),
  ATT_vuln_lower   = numeric(),
  ATT_vuln_upper   = numeric(),
  ATT_resil_mean   = numeric(),
  ATT_resil_lower  = numeric(),
  ATT_resil_upper  = numeric(),
  stringsAsFactors = FALSE
)

# 10.3 Configuración de parámetros finales
calipers <- c(0.1)
n_draws <- 1000

# 10.4 Matching y cálculo de ATT
for (i in seq_along(calipers)) {
  caliper_val <- calipers[i]
  cat("\n-----------------------------------------------------------\n")
  cat("Escenario Clásico Sin Componente Espacial ", i, ": caliper =", caliper_val, ", draws =", n_draws, "\n")
  cat("-----------------------------------------------------------\n")
  
  # Inicializar contenedores para las estimaciones de ATT en cada draw
  att_draws_total <- numeric(n_draws)
  att_draws_vuln  <- numeric(n_draws)
  att_draws_resil <- numeric(n_draws)
  
  # Para cada draw simulada del PS obtenido del logit clásico
  for (m in seq_len(n_draws)) {
    # Agregar la columna de propensity score correspondiente al draw m
    df_draw <- df_base_classic %>% mutate(ps_draw = ps_draws_classic[, m])
    
    # Realizar matching basado en la columna ps_draw
    match_obj <- matchit(
      formula = treated ~ 1,    # Matching sin covariables; la distancia es ps_draw
      data = df_draw,
      method = "nearest",
      distance = df_draw$ps_draw,
      caliper = caliper_val,
      replace = FALSE
    )
    df_matched <- match.data(match_obj)
    
    # Unir el outcome (EarnS) y el moderador (PRED0_PE) desde 'datos_seleccionados'
    df_matched <- merge(
      df_matched,
      datos_seleccionados[, c("GEOIDFQ", "EarnS", "PRED0_PE")],
      by = "GEOIDFQ",
      all.x = TRUE
    )
    
    # Calcular el logaritmo de EarnS
    df_matched$logEarnS <- log(df_matched$EarnS)
    
    # Definir subgrupos en base a PRED0_PE: "Baja" y "Alta"
    cuartiles_res <- quantile(df_matched$PRED0_PE, probs = c(0.25, 0.75), na.rm = TRUE)
    df_matched$grupo_resiliencia <- NA_character_
    df_matched$grupo_resiliencia[df_matched$PRED0_PE < cuartiles_res[1]] <- "Baja"
    df_matched$grupo_resiliencia[df_matched$PRED0_PE > cuartiles_res[2]] <- "Alta"
    
    # Crear el diseño de survey a partir de los pesos generados por el matching
    design_emp <- svydesign(ids = ~1, weights = ~weights, data = df_matched)
    
    # Estimar el modelo de resultados: logEarnS ~ treated
    mod_total <- svyglm(logEarnS ~ treated, design = design_emp)
    att_draws_total[m] <- coef(mod_total)["treated"]
    
    # Estimar ATT en subgrupo "Baja"
    design_baja <- subset(design_emp, grupo_resiliencia == "Baja")
    if (nrow(design_baja$variables) > 0) {
      mod_baja <- svyglm(logEarnS ~ treated, design = design_baja)
      att_draws_vuln[m] <- coef(mod_baja)["treated"]
    } else {
      att_draws_vuln[m] <- NA
    }
    
    # Estimar ATT en subgrupo "Alta"
    design_alta <- subset(design_emp, grupo_resiliencia == "Alta")
    if (nrow(design_alta$variables) > 0) {
      mod_alta <- svyglm(logEarnS ~ treated, design = design_alta)
      att_draws_resil[m] <- coef(mod_alta)["treated"]
    } else {
      att_draws_resil[m] <- NA
    }
  }
  
  # 10.5 Consolidación de resultados
  att_total_mean  <- mean(att_draws_total, na.rm = TRUE)
  att_total_lower <- quantile(att_draws_total, probs = 0.025, na.rm = TRUE)
  att_total_upper <- quantile(att_draws_total, probs = 0.975, na.rm = TRUE)
  
  att_vuln_mean   <- mean(att_draws_vuln, na.rm = TRUE)
  att_vuln_lower  <- quantile(att_draws_vuln, probs = 0.025, na.rm = TRUE)
  att_vuln_upper  <- quantile(att_draws_vuln, probs = 0.975, na.rm = TRUE)
  
  att_resil_mean  <- mean(att_draws_resil, na.rm = TRUE)
  att_resil_lower <- quantile(att_draws_resil, probs = 0.025, na.rm = TRUE)
  att_resil_upper <- quantile(att_draws_resil, probs = 0.975, na.rm = TRUE)
  
  resultado_sensibilidad_classic_nospatial <- rbind(
    resultado_sensibilidad_classic_nospatial,
    data.frame(
      Escenario        = paste0("Escenario_", i),
      Caliper          = caliper_val,
      N_Draws          = n_draws,
      ATT_total_mean   = att_total_mean,
      ATT_total_lower  = att_total_lower,
      ATT_total_upper  = att_total_upper,
      ATT_vuln_mean    = att_vuln_mean,
      ATT_vuln_lower   = att_vuln_lower,
      ATT_vuln_upper   = att_vuln_upper,
      ATT_resil_mean   = att_resil_mean,
      ATT_resil_lower  = att_resil_lower,
      ATT_resil_upper  = att_resil_upper,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n=====================================================\n")
cat("RESUMEN FINAL - ATT CLÁSICO SIN COMPONENTE ESPACIAL (LOGIT CLÁSICO)\n")
cat("=====================================================\n")
print(resultado_sensibilidad_classic_nospatial)

# =============================================================================
# BLOQUE: Generación de la Matriz de Pesos Espaciales (Estilo Binario)
#        Usando un Shapefile con Polígonos ("Dummy condado")
# =============================================================================

# 1) Instalar y cargar librerías necesarias (solo si no lo has hecho antes)
if (!require(sf)) install.packages("sf", dependencies = TRUE)
if (!require(spdep)) install.packages("spdep", dependencies = TRUE)
if (!require(igraph)) install.packages("igraph", dependencies = TRUE)

library(sf)       # Para leer y manejar shapefiles con geometrías poligonales
library(spdep)    # Para crear la lista de vecinos y la matriz de pesos
library(igraph)   # Para verificar la conectividad en la matriz resultante

# 2) Cargar el shapefile poligonal  
#    Se asume que "DummyCondado_Project_ExportFeatures.shp" contiene polígonos
shapefile_path <- "C:/Users/Rodriguez/OneDrive - UAM/Documentos/0 Paper Markov_PC/5. PUMAs/PUMA_eco/R-STATA/Dummy condado/DummyCondado_Project_ExportFeatures.shp"
shapefile_corrigido <- st_read(shapefile_path)

# 3) Verificar que la geometría sea POLYGON o MULTIPOLYGON
geom_types <- unique(st_geometry_type(shapefile_corrigido))
cat("\nTipo(s) de geometría detectada:", as.character(geom_types), "\n")
# Se espera ver "POLYGON" o "MULTIPOLYGON".

# 4) Verificar el CRS 
cat("\nCRS del shapefile:\n")
print(st_crs(shapefile_corrigido))

# 5) Generar la lista de vecinos basada en la regla "Queen"
neighbors <- poly2nb(shapefile_corrigido, queen = TRUE)


# 6) Convertir la lista de vecinos a una lista de pesos espaciales binarios (estilo "B")
W_dummy <- nb2listw(neighbors, style = "B", zero.policy = TRUE)

# 7) Convertir la lista de pesos a una matriz de adyacencia binaria
W_matrix <- listw2mat(W_dummy)

# 8) Validar la matriz de pesos:
cat("\n¿La matriz es cuadrada?: ", nrow(W_matrix) == ncol(W_matrix), "\n")
cat("¿La diagonal es cero?: ", all(diag(W_matrix) == 0), "\n")
cat("¿La matriz es simétrica?: ", all.equal(W_matrix, t(W_matrix), check.attributes = FALSE), "\n")

# 9) Verificar la conectividad usando igraph  
#    Se utiliza mode = "max" para suprimir la advertencia (ya que la matriz es simétrica)
g <- graph_from_adjacency_matrix(W_matrix, mode = "max", diag = FALSE)
cat("¿El grafo resultante es conexo?: ", is_connected(g), "\n")

if (is_connected(g)) {
  cat("✅ ¡La matriz de pesos espaciales es válida para análisis econométricos!\n")
} else {
  cat("⚠️ La matriz de pesos espaciales no es conexa, lo cual es crítico para el análisis.\n")
}

# 10) Mostrar la matriz de pesos resultante (W_matrix)
cat("\n--- MATRIZ DE PESOS (estilo binario) ---\n")
W_matrix

# -----------------------------------------------
# 11. Modelo ATT espacial (PS BYM2 + outcome BYM2)
# -----------------------------------------------

# 11.1 Carga de librerías y dependencias
library(INLA)
library(MatchIt)
library(spdep)
library(dplyr)

# 11.2 Construcción del grafo de adyacencia (BYM2)
spdep::nb2INLA("adj.graph", neighbors)
g_adj <- INLA::inla.read.graph("adj.graph")

# 11.3 Asignación de identificadores de región
region_map <- data.frame(
  GEOIDFQ   = shapefile_corrigido$GEOIDFQ,
  region_id = seq_len(nrow(shapefile_corrigido))
)

# 11.4 Preparación de datos para el modelo de PS espacial
ps_data <- datos_analisis_escalado %>%
  inner_join(region_map, by = "GEOIDFQ") %>%
  mutate(treated = as.integer(AMZ == 1)) %>%
  filter(
    !is.na(treated),
    !is.na(S1902_C02_001E), !is.na(S2403_C01_003E),
    !is.na(S2504_C03_018E), !is.na(S2503_C06_019E),
    !is.na(S2402_C01_018E), !is.na(S2506_C01_047E)
  )

# 11.5 Ajuste del modelo de propensity scores espacial (BYM2)
ps_formula <- treated ~
  S1902_C02_001E + S2403_C01_003E + S2504_C03_018E +
  S2503_C06_019E + S2402_C01_018E + S2506_C01_047E +
  f(
    region_id,
    model       = "bym2",
    graph       = g_adj,
    scale.model = TRUE,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi  = list(prior = "pc",      param = c(0.5, 0.5))
    )
  )

ps_inla <- inla(
  ps_formula,
  data              = ps_data,
  family            = "binomial",
  control.family    = list(link = "logit"),
  control.predictor = list(compute = TRUE, link = TRUE),
  control.compute   = list(config = TRUE, dic = TRUE, waic = TRUE),
  control.inla      = list(strategy = "simplified.laplace", int.strategy = "ccd"),
  verbose           = FALSE
)

# 11.6 Extracción de propensity scores espaciales
ps_data$ps_spatial <- plogis(ps_inla$summary.linear.predictor$mean)

# 11.7 Emparejamiento usando PS espacial
match_obj <- matchit(
  treated  ~ 1,
  data      = ps_data,
  method    = "nearest",
  distance  = ps_data$ps_spatial,
  caliper   = 0.1,
  replace   = FALSE
)
matched <- match.data(match_obj)

# 11.8 Preparación de datos de resultado y definición de logEarnS
tmp <- matched %>%
  dplyr::select(GEOIDFQ, treated, ps_spatial, weights) %>%
  inner_join(
    datos_seleccionados[, c("GEOIDFQ","EarnS")],
    by = "GEOIDFQ"
  )

outcome_data <- tmp %>%
  mutate(
    logEarnS  = log(EarnS),
    region_id = region_map$region_id[match(GEOIDFQ, region_map$GEOIDFQ)]
  )

# 11.9 Función para estimar efectos y sus intervalos HPD
fit_subgrupo <- function(data_sub){
  if (nrow(data_sub) == 0) return(c(mean=NA, lower=NA, upper=NA))
  mod <- inla(
    logEarnS ~ treated +
      f(region_id, model="bym2", graph=g_adj, scale.model=TRUE,
        hyper=list(
          prec=list(prior="pc.prec", param=c(1,0.01)),
          phi =list(prior="pc",      param=c(0.5,0.5))
        )
      ),
    data            = data_sub,
    family          = "gaussian",
    control.family  = list(link="identity"),
    control.compute = list(config=TRUE),
    control.inla    = list(strategy="simplified.laplace", int.strategy="ccd"),
    verbose         = FALSE
  )
  s <- mod$summary.fixed["treated", ]
  c(
    mean  = as.numeric(s["mean"]),
    lower = as.numeric(s["0.025quant"]),
    upper = as.numeric(s["0.975quant"])
  )
}

# 11.12 Cálculo de ATT global y por resiliencia

# Primero, hacer merge para incluir PRED0_PE en outcome_data
outcome_data <- outcome_data %>%
  left_join(datos_seleccionados[, c("GEOIDFQ", "PRED0_PE")], by = "GEOIDFQ")

# Luego, calcular ATT global
hpd_global <- fit_subgrupo(outcome_data)

# Estratificar por resiliencia
cuartiles_res <- quantile(outcome_data$PRED0_PE, probs = c(0.25, 0.75), na.rm = TRUE)

# Crear los subgrupos
baja_data <- filter(outcome_data, PRED0_PE <= cuartiles_res[1])
alta_data <- filter(outcome_data, PRED0_PE >= cuartiles_res[2])

# Estimar ATT en cada subgrupo
hpd_baja <- fit_subgrupo(baja_data)
hpd_alta <- fit_subgrupo(alta_data)

# Consolidar resultados
resultado_100pc_espacial <- data.frame(
  Grupo       = c("Global", "Vulnerables", "Resilientes"),
  ATT_mean    = c(hpd_global["mean"],  hpd_baja["mean"],  hpd_alta["mean"]),
  `95%_lower` = c(hpd_global["lower"], hpd_baja["lower"], hpd_alta["lower"]),
  `95%_upper` = c(hpd_global["upper"], hpd_baja["upper"], hpd_alta["upper"])
)

print(resultado_100pc_espacial)

# 11.12 Diagnóstico de balance post-matching
if (!require("cobalt")) install.packages("cobalt"); library(cobalt)

# Balance de covariables utilizando solo la variable de tratamiento
bal <- bal.tab(match_obj, treat = "treated", un = TRUE)

# Número de tratados emparejados
n_treated_matched <- sum(match.data(match_obj)$treated == 1)

# SMD promedio (después del emparejamiento)
smd_post <- bal$Balance$Diff.Adj
smd_promedio <- mean(abs(smd_post), na.rm = TRUE)

# Imprimir los resultados
cat("✅ Número de tratados emparejados:", n_treated_matched, "\n")
cat("📏 SMD promedio post-matching:", round(smd_promedio, 4), "\n")




mod_outcome <- inla(
  logEarnS ~ treated +
    f(region_id, model = "bym2", graph = g_adj, scale.model = TRUE,
      hyper = list(
        prec = list(prior = "pc.prec", param = c(1, 0.01)),
        phi  = list(prior = "pc",      param = c(0.5, 0.5))
      )
    ),
  data            = outcome_data,
  family          = "gaussian",
  control.family  = list(link = "identity"),
  control.compute = list(config = TRUE),
  control.inla    = list(strategy = "simplified.laplace", int.strategy = "ccd"),
  verbose         = FALSE
)

# 11.13 Diagnóstico de autocorrelación espacial y varianza BYM2

# Asegúrate de haber ejecutado previamente el modelo 'mod_outcome'
# y que 'g_adj', 'outcome_data' y 'region_id' están definidos

# A) Extraer el efecto espacial posterior (componente BYM2)
# Nota: En BYM2, el efecto f(region_id, model = "bym2") se descompone en:
# - efecto estructurado (spatial structured)
# - efecto no estructurado (iid)

# INLA entrega la mezcla de ambos en una única f(region_id)
# Extraemos los valores esperados del efecto BYM2

bym2_effect <- mod_outcome$summary.random$region_id$mean

# B) Visualización rápida del efecto espacial
# Puedes inspeccionar si hay patrones espaciales dominantes:
hist(bym2_effect, main = "Distribución del efecto espacial (BYM2)", xlab = "Efecto estimado")

# C) Medir autocorrelación espacial directamente sobre el componente BYM2
# Usamos el test de Moran, pero aplicado al componente espacial puro del modelo
# Este diagnóstico es más coherente que aplicarlo sobre residuos

# Creamos lista de pesos alineada con los region_id (ya ordenada)
keep_logical <- seq_along(neighbors) %in% outcome_data$region_id
neighbors_tmp <- neighbors
names(neighbors_tmp) <- NULL
neighbors_sub <- spdep::subset.nb(neighbors_tmp, subset = keep_logical)
names(neighbors_sub) <- as.character(outcome_data$region_id)
listw_sub <- spdep::nb2listw(neighbors_sub, style = "W", zero.policy = TRUE)

# Reordenar efecto BYM2 según region_id
id_order <- match(as.character(outcome_data$region_id), names(neighbors_sub))
bym2_ordered <- bym2_effect[id_order]

# Test de Moran sobre el componente espacial estimado
moran_bym2 <- spdep::moran.test(bym2_ordered, listw_sub, zero.policy = TRUE)
print(moran_bym2)

# D) Diagnóstico adicional: plot del efecto espacial en el mapa (si deseas)
# library(sf)
# shapefile_corrigido$bym2 <- NA
# shapefile_corrigido$bym2[outcome_data$region_id] <- bym2_effect
# plot(shapefile_corrigido["bym2"], main = "Efecto espacial estimado (BYM2)")

# E) Alternativa bayesiana: cuantificar la varianza explicada por la componente espacial

# Calculamos proporción de varianza espacial (phi)
phi_post <- mod_outcome$summary.hyperpar["Phi for region_id", "mean"]
cat("📊 Proporción de varianza espacial explicada (phi):", round(phi_post, 3), "\n")

# phi ~ 1: casi todo es espacialmente estructurado
# phi ~ 0: casi todo es ruido/no estructurado