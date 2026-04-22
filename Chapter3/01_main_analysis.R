# ==============================================================================
# Título: [Título]
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
# Fecha: [Fecha]
# Versión: [Versión]
# Descripción:
#   Este script reproduce el análisis empírico del estudio. La metodología
#   incluye el procesamiento de microdatos, la construcción de variables de
#   aglomeración, la estimación de un modelo de salarios con variables
#   instrumentales y el cálculo de un índice de resiliencia individual.
#
# Reproducibilidad: Sí.
# ==============================================================================

# ==============================================================================
# BLOQUE 1: Configuración del Entorno de Trabajo
# ==============================================================================

# 1.1 Silenciar avisos
suppressWarnings({
  try({
    if (exists("readCitationFile", where = getNamespace("utils"), inherits = FALSE)) {
      unlockBinding("readCitationFile", getNamespace("utils"))
      assign("readCitationFile", function(file, lib.loc = NULL) NULL, envir = getNamespace("utils"))
    }
  }, silent = TRUE)
})

# 1.2 Inicialización del entorno de trabajo
# 1.2.1 Limpiar memoria
rm(list = ls())
gc()

# 1.2.2 Fijar semilla
set.seed(2024)

# 1.3 Carga de los paquetes
# 1.3.1 Definición de dependencias
paquetes_requeridos <- c(
  "here",             # Rutas de archivo
  "tidyverse",        # Manipulación de datos
  "ipumsr",           # Lectura de microdatos IPUMS
  "sf",               # Datos espaciales
  "units",            # Unidades de medida
  "sp",               # Datos espaciales 
  "spdep",            # Dependencia espacial
  "fixest",           # Modelos de panel / IV
  "modelsummary",     # Tablas de resultados
  "readxl",           # Lectura de archivos Excel
  "janitor",          # Depuración de nombres de variables
  "arrow",            # Lectura/escritura de archivos Parquet
  "knitr",            # Tablas en markdown (kable)
  "mgcv"              # Modelos aditivos generalizados
)

# 1.3.2 Instalar paquetes
invisible(lapply(paquetes_requeridos, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  }
  library(pkg, character.only = TRUE, quietly = TRUE)
}))

# 1.4 Definición de las rutas de los archivos
# 1.4.1 Definición de rutas de entrada
ruta_datos_ipums      <- here::here("1_Data", "ipums")
ruta_excel_crosswalks <- here::here("1_Data", "excel")
ruta_shapefiles       <- here::here("1_Data", "shapefiles")

# 1.4.2 Definición de rutas de salida
ruta_datos_procesados <- here::here("3_Outputs", "1_Data_Processed")
ruta_modelos          <- here::here("3_Outputs", "2_Models")

# 1.4.3 Crear carpetas
dir.create(ruta_datos_procesados, showWarnings = FALSE, recursive = TRUE)
dir.create(ruta_modelos, showWarnings = FALSE, recursive = TRUE)

# 1.5 Parámetros globales
# 1.5.1 Marco temporal
periodo_pre_shock        <- c(2017, 2018, 2019)
año_del_shock            <- 2020

# 1.5.2 Definición del periodo post-shock
periodo_post_shock       <- c(2022L, 2023L)
# Año base para el análisis
año_analisis_resiliencia <- 2023L
# Panel analítico total
años_panel_analitico     <- c(periodo_pre_shock, periodo_post_shock)


# 1.5.3 Parámetros del modelo teórico
# Elasticidad de sustitución
parametro_sigma <- 6.25 
# Costo de transporte
parametro_tau   <- 0.24

# ==============================================================================
# BLOQUE 2: Preparación y Procesamiento de Datos
# ==============================================================================

# 2.1 Carga de microdatos brutos
# 2.1.1 Verificar DDI
ruta_ddi <- file.path(ruta_datos_ipums, "usa_00078.xml")
if (!file.exists(ruta_ddi)) {
  stop("Archivo DDI no encontrado: ", ruta_ddi)
}

# 2.1.2 Lectura de archivo IPUMS
datos_micro_nacional_brutos <- ipumsr::read_ipums_micro(ddi = ruta_ddi, verbose = FALSE)

# 2.2 Selección de variables de interés
variables_esenciales <- c(
  "YEAR", "SERIAL", "HHWT", "STATEFIP", "PUMA", "PERWT",
  "EMPSTAT", "IND", "IND1990",
  "WKSWORK2", "UHRSWORK",
  "AGE", "SEX", "RACE", "MARST", "EDUC",
  "INCWAGE_CPIU_2010", "INCEARN_CPIU_2010", "HHINCOME_CPIU_2010"
)

datos_micro_nacional_brutos <- datos_micro_nacional_brutos %>%
  dplyr::select(all_of(variables_esenciales)) %>%
  dplyr::rename(
    INCWAGE  = INCWAGE_CPIU_2010,
    INCEARN  = INCEARN_CPIU_2010,
    HHINCOME = HHINCOME_CPIU_2010
  ) %>%
  dplyr::filter(YEAR %in% años_panel_analitico)

# 2.2.1 Liberación de memoria
gc() 

# 2.3 Armonización geográfica a PUMAs 2020
# 2.3.1 Carga de tablas de correspondencia
ruta_crosswalks_corregida <- here::here("1_Data", "crosswalk")
ruta_cw_xls <- file.path(ruta_crosswalks_corregida, "PUMA2010_PUMA2020_crosswalk.xls")
if (!file.exists(ruta_cw_xls)) stop("Archivo crosswalk .xls no encontrado en: ", ruta_cw_xls)

# 2.3.2 Cálculo del factor de asignación poblacional
cw_df <- readxl::read_excel(ruta_cw_xls, sheet = "PUMA2010_PUMA2020") %>%
  janitor::clean_names() %>%
  dplyr::select(
    state10, puma10, geoid10, state20, puma20, geoid20,
    part_pop20, puma10_pop20, p_puma10_pop20
  ) %>%
  dplyr::mutate(
    across(c(geoid10, geoid20), as.character),
    allocation_factor = part_pop20 / puma10_pop20
  )

ruta_tract_csv <- file.path(ruta_crosswalks_corregida, "2020_Census_Tract_to_2020_PUMA.csv")
if (!file.exists(ruta_tract_csv)) stop("Archivo de Tracts a PUMA .csv no encontrado en: ", ruta_tract_csv)
tract_to_puma_df <- readr::read_csv(ruta_tract_csv, col_types = readr::cols(.default = "c"))

verification <- cw_df %>%
  dplyr::summarise(max_abs_diff = max(abs(allocation_factor - (p_puma10_pop20 / 100)), na.rm = TRUE))
if (verification$max_abs_diff > 1e-4) {
  warning("Verificación de crosswalk fallida: pPUMA10_Pop20 no coincide con Part_Pop20 / PUMA10_Pop20.")
}

# 2.4 Aplicación de la armonización temporal
# 2.4.1 Creación de identificador geográfico (GEOID10)
datos_con_geoid10 <- datos_micro_nacional_brutos %>%
  dplyr::mutate(GEOID10 = sprintf("%02d%05d", STATEFIP, PUMA))

# 2.4.2 Separación de series pre y post-2022
datos_pre_2022 <- datos_con_geoid10 %>% dplyr::filter(YEAR < 2022)
datos_post_2022 <- datos_con_geoid10 %>% dplyr::filter(YEAR >= 2022)

# 2.4.3 Asignación proporcional de pesos pre-2022
cw_tabla_aplicada <- dplyr::left_join(datos_pre_2022, cw_df, by = c("GEOID10" = "geoid10"), relationship = "many-to-many")
cw_no_match <- cw_tabla_aplicada %>% dplyr::filter(is.na(geoid20))

datos_harmonizados_pre_2022 <- cw_tabla_aplicada %>%
  dplyr::filter(!is.na(geoid20)) %>%
  dplyr::mutate(
    PUMA2020_GEOID7 = geoid20,
    PERWT_HARM = PERWT * allocation_factor,
    HHWT_HARM = HHWT * allocation_factor
  )

# 2.4.4 Integración de la serie temporal completa
datos_passthrough_post_2022 <- datos_post_2022 %>%
  dplyr::mutate(
    PUMA2020_GEOID7 = GEOID10,
    PERWT_HARM = PERWT,
    HHWT_HARM = HHWT
  )

columnas_comunes <- intersect(names(datos_harmonizados_pre_2022), names(datos_passthrough_post_2022))
datos_harmonizados_puma2020 <- dplyr::bind_rows(
  datos_harmonizados_pre_2022 %>% dplyr::select(all_of(columnas_comunes)),
  datos_passthrough_post_2022 %>% dplyr::select(all_of(columnas_comunes))
)

# 2.5 Validación y guardado de datos armonizados
# 2.5.1 Verificación de consistencia de pesos
pesos_originales <- datos_pre_2022 %>%
  dplyr::group_by(YEAR, STATEFIP) %>%
  dplyr::summarise(orig_perwt = sum(PERWT, na.rm = TRUE), .groups = 'drop')

pesos_harmonizados <- datos_harmonizados_pre_2022 %>%
  dplyr::group_by(YEAR, STATEFIP) %>%
  dplyr::summarise(harm_perwt = sum(PERWT_HARM, na.rm = TRUE), .groups = 'drop')

cw_resumen_validacion <- dplyr::full_join(pesos_originales, pesos_harmonizados, by = c("YEAR", "STATEFIP")) %>%
  dplyr::mutate(
    delta_perwt_rel = ifelse(orig_perwt == 0, 0, (harm_perwt - orig_perwt) / orig_perwt),
    validation_pass = abs(delta_perwt_rel) < 1e-6 
  )

cw_resumen_validacion <- cw_resumen_validacion %>%
  dplyr::mutate(
    validation_pass = abs(delta_perwt_rel) < 1e-5 # Tolerancia ajustada
  )

total_pass <- all(cw_resumen_validacion$validation_pass, na.rm = TRUE)
max_rel_error <- max(abs(cw_resumen_validacion$delta_perwt_rel), na.rm = TRUE)
map_summary <- cw_df %>%
  dplyr::group_by(geoid10) %>%
  dplyr::summarise(n_destino = n(), .groups = 'drop')
one_to_many_count <- sum(map_summary$n_destino > 1)

# 2.5.2 Guardado de datos armonizados en Parquet
ruta_salida_harmonizada <- file.path(ruta_datos_procesados, "datos_harmonizados_puma2020.parquet")
arrow::write_parquet(datos_harmonizados_puma2020, ruta_salida_harmonizada)

# 2.5.3 Liberación de memoria intermedia
rm(list = c("datos_con_geoid10", "datos_pre_2022", "datos_post_2022", 
            "datos_harmonizados_pre_2022", "tract_to_puma_df", "datos_passthrough_post_2022", 
            "pesos_originales", "pesos_harmonizados", "cw_df", "verification", 
            "map_summary", "ruta_cw_xls", "ruta_tract_csv", "ruta_crosswalks_corregida",
            "variables_esenciales", "columnas_comunes", "one_to_many_count",
            "max_rel_error", "total_pass", "cw_tabla_aplicada", "cw_no_match",
            "cw_resumen_validacion", "datos_micro_nacional_brutos"))
gc()

ruta_datos_harmonizados <- file.path(ruta_datos_procesados, "datos_harmonizados_puma2020.parquet")
if (!file.exists(ruta_datos_harmonizados)) {
  stop("ERROR CRÍTICO: No se encontraron los datos armonizados en: ", ruta_datos_harmonizados, ". Ejecute los bloques anteriores.")
}

# 2.6 Construcción del panel analítico
# 2.6.1 Carga de datos armonizados
datos_harmonizados <- arrow::read_parquet(ruta_datos_harmonizados)

# 2.6.2 Transformación del panel
datos_micro_nacional_full <- datos_harmonizados %>%

# 2.6.3 Depuración de la muestra
  dplyr::filter(
    YEAR %in% años_panel_analitico,
    !is.na(PERWT_HARM),
    PERWT_HARM > 0
  ) %>%

# 2.6.4 Tratamiento de valores extremos
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(
    umbral_incwage = quantile(INCWAGE, 0.999, na.rm = TRUE, type = 7),
    umbral_incearn = quantile(INCEARN, 0.999, na.rm = TRUE, type = 7),
    umbral_hhinc   = quantile(HHINCOME, 0.999, na.rm = TRUE, type = 7),
    INCWAGE_wins  = pmin(INCWAGE, umbral_incwage, na.rm = TRUE),
    INCEARN_wins  = pmin(INCEARN, umbral_incearn, na.rm = TRUE),
    HHINCOME_wins = pmin(HHINCOME, umbral_hhinc, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(GEOID = PUMA2020_GEOID7) %>%
  dplyr::relocate(GEOID, .before = 1) %>%
  dplyr::select(-dplyr::starts_with("umbral_"))

# 2.6.5 Gestión de memoria
rm(datos_harmonizados)
gc()

# 2.7 Procesamiento de datos geográficos
# 2.7.1 Carga de geometrías espaciales (PUMA 2020)
ruta_shp_puma <- file.path(ruta_shapefiles, "ipums_puma_2020", "ipums_puma_2020.shp")
if (!file.exists(ruta_shp_puma)) {
  stop("Shapefile de PUMAs no encontrado en: ", ruta_shp_puma)
}

# 2.7.2 Cálculo de atributos geográficos
geometrias_puma_nacional_brutas <- sf::st_read(ruta_shp_puma, quiet = TRUE)
geometrias_puma_nacional <- geometrias_puma_nacional_brutas %>%
  mutate(
    GEOID = as.character(GEOID),
    area_km2 = units::set_units(sf::st_area(sf::st_transform(geometry, crs = 5070)), "km^2"),
    centroide = sf::st_centroid(geometry)
  )

if (any(!sf::st_is_valid(geometrias_puma_nacional))) {
  geometrias_puma_nacional <- sf::st_make_valid(geometrias_puma_nacional)
}

# 2.8 Guardado de objetos de análisis
# 2.8.1 Liberación de memoria
rm(geometrias_puma_nacional_brutas)
gc()

# 2.8.2 Guardado de objetos RDS para bloques futuros
saveRDS(geometrias_puma_nacional, file.path(ruta_datos_procesados, "geometrias_puma_nacional.rds"))
saveRDS(datos_micro_nacional_full, file.path(ruta_datos_procesados, "datos_micro_nacional_full.rds"))

# ==============================================================================
# BLOQUE 3: Construcción de variables de aglomeración
# ==============================================================================

# 3.1 Verificación de objetos de entrada
if (!exists("datos_micro_nacional_full")) {
  stop("Objeto 'datos_micro_nacional_full' no encontrado.")
}
if (!exists("geometrias_puma_nacional")) {
  stop("Objeto 'geometrias_puma_nacional' no encontrado.")
}

# 3.2 Carga de referencias geográficas urbanas
ruta_crosswalk <- file.path(ruta_excel_crosswalks, "large_place_PUMA2020_crosswalk.xlsx")
ruta_coords_csv <- file.path(ruta_excel_crosswalks, "uscities.csv")
if (!file.exists(ruta_crosswalk)) stop("Archivo crosswalk no encontrado: ", ruta_crosswalk)
if (!file.exists(ruta_coords_csv)) stop("Archivo de coordenadas de ciudades no encontrado: ", ruta_coords_csv)
ciudades_coords_df <- readr::read_csv(ruta_coords_csv, show_col_types = FALSE)
crosswalk_df_bruto <- readxl::read_excel(ruta_crosswalk)

# 3.3 Definición de centros económicos
# 3.3.1 Identificación de la ciudad principal
main_towns_df <- crosswalk_df_bruto %>%
  janitor::clean_names() %>%
  group_by(puma_code) %>%
  slice_max(order_by = percent_puma_population, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(GEOID = puma_code, place_name, state_name) %>%
  mutate(GEOID = as.character(GEOID))

# 3.3.2 Asignación del punto central
puntos_centrales_final_sf <- geometrias_puma_nacional %>%
  left_join(main_towns_df, by = "GEOID") %>%
  left_join(ciudades_coords_df %>% dplyr::select(city, state_name, lat, lng), 
            by = c("place_name" = "city", "state_name")) %>%
  mutate(
    geometry_ciudad = sf::st_as_sfc(
      ifelse(!is.na(lat) & !is.na(lng), paste("POINT(", lng, lat, ")"), "POINT EMPTY"), 
      crs = 4326
    ) %>% sf::st_transform(sf::st_crs(geometrias_puma_nacional)),
    punto_central_final = if_else(!sf::st_is_empty(geometry_ciudad), geometry_ciudad, centroide)
  ) %>%
  sf::st_set_geometry("punto_central_final") %>%
  dplyr::select(GEOID, area_km2)

rm(ciudades_coords_df, crosswalk_df_bruto, main_towns_df); gc()

# 3.4 Cálculo de la matriz de distancias
# 3.4.1 Matriz de distancias inter-PUMA
matriz_distancias_km <- sf::st_distance(puntos_centrales_final_sf) %>%
  units::set_units("km") %>%
  units::drop_units()
rownames(matriz_distancias_km) <- puntos_centrales_final_sf$GEOID
colnames(matriz_distancias_km) <- puntos_centrales_final_sf$GEOID

# 3.4.2 Distancia intra-PUMA
distancia_intra_puma <- (2/3) * sqrt(units::drop_units(puntos_centrales_final_sf$area_km2) / pi)
diag(matriz_distancias_km) <- distancia_intra_puma

# 3.5 Cálculo de índices de aglomeración
# 3.5.1 Exponente de coste de transporte
trade_cost_exponent <- parametro_tau * (parametro_sigma - 1)

pumas_referencia <- geometrias_puma_nacional %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(GEOID, area_km2)

# 3.5.2 Cálculo de variables anuales
datos_aglomeracion_nacional_final <- datos_micro_nacional_full %>%
  group_by(YEAR) %>%
  nest() %>%
  mutate(
    aglomeracion_df = map2(data, YEAR, function(microdatos_año, ano_actual) {
      
      # Agregación anual por PUMA
      empleo_puma <- microdatos_año %>%
        group_by(GEOID) %>%
        summarise(
          total_empleados = sum(PERWT_HARM[EMPSTAT == 1], na.rm = TRUE),
          .groups = "drop"
        )
      
      # Agregación de masa económica
      masa_puma <- microdatos_año %>%
        filter(
          !is.na(INCEARN_wins),
          INCEARN_wins > 0,
          !is.na(PERWT_HARM),
          PERWT_HARM > 0
        ) %>%
        group_by(GEOID) %>%
        summarise(
          # Ajuste: se utilizan INCEARN y pesos individuales (PERWT_HARM)
          masa_economica = sum(INCEARN_wins * PERWT_HARM, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Unión de datos anuales
      datos_anuales <- pumas_referencia %>%
        left_join(empleo_puma, by = "GEOID") %>%
        left_join(masa_puma,  by = "GEOID") %>%
        mutate(
          total_empleados = tidyr::replace_na(total_empleados, 0),
          masa_economica  = tidyr::replace_na(masa_economica, 0)
        )
      
      orden_geoide <- rownames(matriz_distancias_km)
      vector_masa_economica <- datos_anuales$masa_economica[match(orden_geoide, datos_anuales$GEOID)]
      vector_masa_economica[is.na(vector_masa_economica)] <- 0
      
      dist_mat_pos <- pmax(matriz_distancias_km, 1e-6)
      matriz_impedancia <- dist_mat_pos ^ (-trade_cost_exponent)
      potencial_mercado <- as.numeric(matriz_impedancia %*% vector_masa_economica)
      
      datos_anuales %>%
        mutate(
          potencial_mercado    = potencial_mercado,
          densidad_empleo      = total_empleados / units::drop_units(area_km2),
          ln_densidad_empleo   = log(densidad_empleo + 1),
          ln_potencial_mercado = log(potencial_mercado + 1)
        ) %>%
        dplyr::select(GEOID, ln_densidad_empleo, ln_potencial_mercado)
    })
  ) %>%
  dplyr::select(YEAR, aglomeracion_df) %>%
  unnest(cols = c(aglomeracion_df)) %>%
  rename(año = YEAR) %>%
  filter(is.finite(ln_densidad_empleo) & is.finite(ln_potencial_mercado))

# 3.6 Guardado del panel de aglomeración
saveRDS(datos_aglomeracion_nacional_final, file.path(ruta_datos_procesados, "datos_aglomeracion_nacional_final.rds"))
rm(puntos_centrales_final_sf, matriz_distancias_km, distancia_intra_puma, pumas_referencia, trade_cost_exponent)
gc()

# ==============================================================================
# BLOQUE 4: Construcción de Variables Instrumentales
# ==============================================================================

# 4.1 Verificación de la integridad de los objetos de entrada
stopifnot(
  exists("datos_aglomeracion_nacional_final"), is.data.frame(datos_aglomeracion_nacional_final),
  exists("geometrias_puma_nacional"), inherits(geometrias_puma_nacional, "sf"),
  exists("datos_micro_nacional_full"), is.data.frame(datos_micro_nacional_full)
)
stopifnot(
  nrow(datos_aglomeracion_nacional_final) > 0, "GEOID" %in% names(datos_aglomeracion_nacional_final),
  nrow(geometrias_puma_nacional) > 0, "GEOID" %in% names(geometrias_puma_nacional),
  nrow(datos_micro_nacional_full) > 0, "GEOID" %in% names(datos_micro_nacional_full)
)

# 4.2 Construcción del Instrumento de Autovectores Espaciales (ESF)
# 4.2.1 Construcción de la matriz de ponderación espacial (W)
lista_vecinos_nacional <- spdep::poly2nb(geometrias_puma_nacional, queen = TRUE)
W_mat <- spdep::nb2mat(lista_vecinos_nacional, style = "B", zero.policy = TRUE)

W_sym <- W_mat
n <- nrow(W_sym)

# 4.2.2 Construcción de la matriz de covariables exógenas (X)
datos_micro_para_X <- datos_micro_nacional_full %>%
  dplyr::filter(YEAR %in% periodo_pre_shock,
                !is.na(PERWT_HARM),
                PERWT_HARM > 0,
                EMPSTAT == 1L) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(
    AGE_mean = weighted.mean(AGE, PERWT_HARM, na.rm = TRUE),
    SEX_frac2 = weighted.mean(as.numeric(SEX == 2L), PERWT_HARM, na.rm = TRUE),
    EDUC_mean = weighted.mean(EDUC, PERWT_HARM, na.rm = TRUE),
    .groups = "drop"
  )

pumas_ordenados <- geometrias_puma_nacional %>%
  sf::st_drop_geometry() %>%
  dplyr::select(GEOID)

X_df <- pumas_ordenados %>%
  dplyr::left_join(datos_micro_para_X, by = "GEOID") %>%
  dplyr::mutate(
    AGE_mean = tidyr::replace_na(AGE_mean, mean(AGE_mean, na.rm = TRUE)),
    SEX_frac2 = tidyr::replace_na(SEX_frac2, mean(SEX_frac2, na.rm = TRUE)),
    EDUC_mean = tidyr::replace_na(EDUC_mean, mean(EDUC_mean, na.rm = TRUE))
  )

X <- model.matrix(~ AGE_mean + SEX_frac2 + EDUC_mean, data = X_df)
XtX_inv <- solve(t(X) %*% X)
P <- X %*% XtX_inv %*% t(X)

# 4.2.3 Cálculo de la matriz de proyección ortogonal (M)
M <- diag(n) - P

# 4.2.4 Descomposición espectral de la matriz MCM
A <- M %*% W_sym %*% M
eig <- eigen(A, symmetric = TRUE)

# 4.2.5 Selección de autovectores basada en el coeficiente de Moran
denom_W <- sum(W_sym)
lambda <- eig$values
mc_all <- as.numeric(lambda) * n / denom_W

pos_idx <- which(mc_all > 0)
ord <- order(mc_all[pos_idx], decreasing = TRUE)
E_pos <- eig$vectors[, pos_idx, drop = FALSE][, ord, drop = FALSE]
mc_pos <- mc_all[pos_idx][ord]
E_pos <- scale(E_pos)

# 4.2.6 Estandarización y asignación de identificadores geográficos
ev_names <- paste0("EV_", seq_len(ncol(E_pos)))
autovectores_nacionales_df <- tibble(
  GEOID = pumas_ordenados$GEOID,
  !!!setNames(as.data.frame(E_pos), ev_names)
)

# 4.2.7 Tabla de información de Moran por autovector
moran_info_df <- tibble(
  ev = ev_names,
  moran_mc = mc_pos,
  moran_ratio = mc_pos / max(mc_pos, na.rm = TRUE)
)

# 4.3 Definición de la función para la construcción de instrumentos sintéticos
construir_iv_sintetico <- function(datos_anuales, var_endogena,
                                   df_evs, moran_df,
                                   mc_ratio_threshold = 0.25) {
  stopifnot(all(c("GEOID") %in% names(df_evs)), var_endogena %in% names(datos_anuales))
  
  # Selección de autovectores candidatos
  cand_evs <- moran_df %>%
    dplyr::filter(is.finite(moran_mc),
                  moran_mc > 0,
                  moran_ratio >= mc_ratio_threshold) %>%
    dplyr::pull(ev)
  
  # Tratamiento en caso de candidatos escasos
  if (length(cand_evs) < 5) {
    cand_evs <- moran_df %>%
      dplyr::filter(moran_mc > 0) %>%
      dplyr::slice_max(order_by = moran_mc, n = 50, with_ties = FALSE) %>%
      dplyr::pull(ev)
  }
  
  # Alineación y filtrado de datos
  datos_join <- datos_anuales %>%
    dplyr::left_join(df_evs, by = "GEOID")
  
  x <- datos_join[[var_endogena]]
  Z <- as.matrix(datos_join[, cand_evs, drop = FALSE])
  
  ok <- is.finite(x) & apply(Z, 1, function(r) all(is.finite(r)))
  x <- x[ok]
  Z <- Z[ok, , drop = FALSE]
  GEOID_ok <- datos_join$GEOID[ok]
  
  # Protección: si no queda ningún EV o ninguna observación válida, devuelve NAs
  if (length(GEOID_ok) == 0L || ncol(Z) == 0L) {
    return(
      tibble(
        GEOID = datos_anuales$GEOID,
        Z_sintetico_w  = NA_real_,
        Z_sintetico_uw = NA_real_
      )
    )
  }
  
  # Construcción de matriz de covariables exógenas
  X_exog_full <- X_df # Objeto creado previamente con AGE_mean, SEX_frac2 y EDUC_mean por GEOID
  X_exog_año <- X_exog_full %>%
    dplyr::filter(GEOID %in% GEOID_ok) %>%
    dplyr::arrange(match(GEOID, GEOID_ok))
  
  X_exog_mat <- model.matrix(~ AGE_mean + SEX_frac2 + EDUC_mean, data = X_exog_año)
  
  ev_stats <- lapply(seq_len(ncol(Z)), function(j) {
    ev <- Z[, j]
    tryCatch({
      if (all(is.finite(ev))) {
        m <- lm(x ~ ev)
        beta <- coef(m)[2]
        pval <- summary(m)$coefficients[2, 4]
        c(beta = beta, pval = pval)
      } else {
        c(beta = NA_real_, pval = NA_real_)
      }
    }, error = function(e) c(beta = NA_real_, pval = NA_real_))
  })
  ev_stats_mat <- do.call(rbind, ev_stats)
  
  # Asegura los nombres de las columnas
  
  colnames(ev_stats_mat) <- c("beta", "pval")
  betas <- ev_stats_mat[, "beta"]
  
  # Selección de autovectores por significancia
  pvals <- ev_stats_mat[, "pval"]
  
  sel <- which(pvals < 0.1 & is.finite(pvals))
  if (length(sel) > 0L) {
    Z_sig <- Z[, sel, drop = FALSE]
    betas_sig <- betas[sel]
  } else {
    Z_sig <- Z
    betas_sig <- betas
  }
  
  Z_sig_df <- as.data.frame(Z_sig)
  colnames(Z_sig_df) <- paste0("EV_sel_", seq_len(ncol(Z_sig_df)))
  
  # Regresión de primera etapa (IV ponderado)
  df_first_stage <- cbind(
    data.frame(x = x),
    as.data.frame(X_exog_mat[, -1, drop = FALSE]), # sin la constante (la añade lm)
    Z_sig_df
  )
  
  formula_first_stage <- as.formula(
    paste0("x ~ .")
  )
  
  mod_first <- lm(formula_first_stage, data = df_first_stage)
  x_hat <- fitted(mod_first)
  
  # Construye IV ponderado (W) como los valores ajustados y estandarizados
  Z_sintetico_w <- as.numeric(scale(x_hat))
  
  # Construye IV alternativo no ponderado (UW): endógena ~ EVs seleccionados
  df_esf <- cbind(
    data.frame(x = x),
    Z_sig_df
  )
  
  # Regresión de primera etapa para el IV no ponderado
  
  
  mod_esf <- lm(x ~ ., data = df_esf)
  x_hat_uw <- fitted(mod_esf)
  Z_sintetico_uw <- as.numeric(scale(x_hat_uw))
  
  
  tibble(
    GEOID = GEOID_ok,
    Z_sintetico_w = Z_sintetico_w,
    Z_sintetico_uw = Z_sintetico_uw
  )
}

# 4.4 Generación de instrumentos sintéticos (aplicación al panel)
# 4.4.1 Instrumentos para el potencial de mercado
ivs_sinteticos_potencial <- datos_aglomeracion_nacional_final %>%
  dplyr::group_by(año) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    ivs_potencial = purrr::map(
      data,
      ~ construir_iv_sintetico(
        .x,
        "ln_potencial_mercado",
        autovectores_nacionales_df,
        moran_info_df
      )
    )
  ) %>%
  dplyr::select(año, ivs_potencial) %>%
  tidyr::unnest(cols = c(ivs_potencial)) %>%
  dplyr::rename(
    Z_sintetico_w_potencial  = Z_sintetico_w,
    Z_sintetico_uw_potencial = Z_sintetico_uw
  )

# 4.4.2 Instrumentos para la densidad de empleo
ivs_sinteticos_densidad <- datos_aglomeracion_nacional_final %>%
  dplyr::group_by(año) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    ivs_densidad = purrr::map(
      data,
      ~ construir_iv_sintetico(
        .x,
        "ln_densidad_empleo",
        autovectores_nacionales_df,
        moran_info_df
      )
    )
  ) %>%
  dplyr::select(año, ivs_densidad) %>%
  tidyr::unnest(cols = c(ivs_densidad)) %>%
  dplyr::rename(
    Z_sintetico_w_densidad  = Z_sintetico_w,
    Z_sintetico_uw_densidad = Z_sintetico_uw
  )

ivs_sinteticos_panel <- ivs_sinteticos_potencial %>%
  dplyr::full_join(
    ivs_sinteticos_densidad,
    by = c("año", "GEOID")
  )

# 4.5 Implementación de la estrategia IV-GL (Graph Laplacian)
# 4.5.1 Definición del orden geográfico de referencia
geo_order <- geometrias_puma_nacional %>%
  sf::st_drop_geometry() %>%
  dplyr::select(GEOID)

# 4.5.2 Cálculo del laplaciano del grafo
W_tmp <- W_sym
diag(W_tmp) <- 0
deg_vec <- rowSums(W_tmp)
L_mat <- diag(deg_vec) - W_tmp

eig_GL <- eigen(L_mat, symmetric = TRUE)
V_GL  <- eig_GL$vectors
lambda_GL <- eig_GL$values

# 4.5.3 Ordenación espectral por frecuencia espacial
ord_GL <- order(lambda_GL, decreasing = FALSE)
V_GL   <- V_GL[, ord_GL, drop = FALSE]
lambda_GL <- lambda_GL[ord_GL]

# 4.5.4 Identificación de autovectores con valor propio cero (constantes) y no nulos
lambda_tol <- 1e-10
zero_idx <- which(lambda_GL <= lambda_tol)
nonzero_idx <- which(lambda_GL > lambda_tol)

# 4.5.5 Función de descomposición Woodward (IV-GL)
k_woodward <- min(35L, length(nonzero_idx))

if (k_woodward < 1L) {
  stop("ERROR CRÍTICO: No hay autovectores no nulos suficientes para construir la base de Woodward.")
}

# Función para calcular descomposición IV-GL (Woodward et al. con Graph Laplacian)
calcular_woodward_tps <- function(df_anual, k_basis = k_woodward) {
  # Vector completo de PUMAs en el mismo orden que geo_order / V_GL
  a_mp_full  <- rep(0, nrow(geo_order))
  a_den_full <- rep(0, nrow(geo_order))
  
  idx <- match(df_anual$GEOID, geo_order$GEOID)
  idx_valid <- which(!is.na(idx))
  
  a_mp_full[idx[idx_valid]]  <- df_anual$ln_potencial_mercado[idx_valid]
  a_den_full[idx[idx_valid]] <- df_anual$ln_densidad_empleo[idx_valid]
  
  # k_use no puede exceder el número de autovectores no nulos disponibles
  k_use <- min(k_basis, length(nonzero_idx))
  
  low_idx  <- nonzero_idx[seq_len(k_use)]
  high_idx <- setdiff(seq_len(ncol(V_GL)), low_idx)
  
  V_low  <- V_GL[, low_idx, drop = FALSE]   # baja frecuencia (AC, sin autovector constante)
  V_high <- V_GL[, high_idx, drop = FALSE]  # alta frecuencia (AUC, incluye autovector constante y resto)
  
  P_low  <- V_low  %*% t(V_low)
  P_high <- V_high %*% t(V_high)
  
  AC_mp_full   <- as.numeric(P_low  %*% a_mp_full)
  AUC_mp_full  <- as.numeric(P_high %*% a_mp_full)
  AC_den_full  <- as.numeric(P_low  %*% a_den_full)
  AUC_den_full <- as.numeric(P_high %*% a_den_full)
  
  base_df <- tibble::tibble(
    GEOID = geo_order$GEOID,
    # AUC (alta frecuencia) -> instrumento Z
    Z_woodward_resid_potencial = AUC_mp_full,
    Z_woodward_resid_densidad  = AUC_den_full,
    # AC (baja frecuencia) -> componente confundente C
    C_woodward_pred_potencial  = AC_mp_full,
    C_woodward_pred_densidad   = AC_den_full
  )
  
  base_df %>%
    dplyr::inner_join(df_anual %>% dplyr::select(GEOID), by = "GEOID")
}

# 4.5.6 Aplicación de la descomposición al panel anual
ivs_woodward_panel <- datos_aglomeracion_nacional_final %>%
  dplyr::group_by(año) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    woodward_data = purrr::map(
      data,
      ~calcular_woodward_tps(.x, k_basis = k_woodward)
    )
  ) %>%
  dplyr::select(año, woodward_data) %>%
  tidyr::unnest(cols = c(woodward_data))

# 4.6 Integración del panel de instrumentos final
variables_aglomeracion_e_iv_nacional <- datos_aglomeracion_nacional_final %>%
  # Fusión de instrumentos sintéticos (ESF)
  dplyr::left_join(ivs_sinteticos_panel, by = c("GEOID", "año")) %>%
  # Fusión de instrumentos Woodward (IV-GL)
  dplyr::left_join(ivs_woodward_panel,  by = c("GEOID", "año"))

# 4.6.1 Validación de la integridad dimensional

if (nrow(variables_aglomeracion_e_iv_nacional) != nrow(datos_aglomeracion_nacional_final)) {
  warning("ALERTA: El número de filas ha cambiado tras la fusión de instrumentos.")
}

# 4.7 Almacenamiento de resultados
saveRDS(variables_aglomeracion_e_iv_nacional,
        file.path(ruta_datos_procesados, "variables_aglomeracion_e_iv_nacional.rds"))


if (!exists("ruta_datos_procesados")) {
  suppressPackageStartupMessages(library(here))
  ruta_datos_procesados <- here::here("3_Outputs", "1_Data_Processed")
}

# 4.8 Gestión de memoria y limpieza
rm(
  lista_vecinos_nacional, W_mat, W_sym, n,
  datos_micro_para_X, pumas_ordenados, X_df, X, XtX_inv, P, M, A, eig,
  denom_W, lambda, mc_all, pos_idx, ord, E_pos, mc_pos, ev_names,
  autovectores_nacionales_df, moran_info_df,
  ivs_sinteticos_potencial, ivs_sinteticos_densidad,
  ivs_sinteticos_panel
  
)
gc()

# ==============================================================================
# BLOQUE 5: Ensamblaje de datos del modelo de salarios
# ==============================================================================

# 5.1 Verificación de objetos de entrada
if (!exists("datos_micro_nacional_full") || !is.data.frame(datos_micro_nacional_full)) {
  stop("ERROR CRÍTICO (B5-5.1): Falta 'datos_micro_nacional_full'. Ejecute los bloques previos.")
}
if (!exists("variables_aglomeracion_e_iv_nacional") || !is.data.frame(variables_aglomeracion_e_iv_nacional)) {
  stop("ERROR CRÍTICO (B5-5.1): Falta 'variables_aglomeracion_e_iv_nacional'. Ejecute los bloques previos.")
}

# 5.1.1 Validaciones de columnas clave para asegurar la integridad de los datos
cols_micro_requeridas <- c(
  "GEOID", "YEAR", "INCWAGE_wins",
  "AGE", "SEX", "RACE", "MARST", "EDUC", "WKSWORK2", "IND1990",
  "PERWT", "PERWT_HARM"
)
faltan_micro <- setdiff(cols_micro_requeridas, names(datos_micro_nacional_full))
if (length(faltan_micro) > 0) {
  stop("ERROR CRÍTICO (B5-5.1): Faltan columnas en 'datos_micro_nacional_full': ", paste(faltan_micro, collapse=", "))
}

cols_iv_requeridas <- c("GEOID", "año", "ln_potencial_mercado", "ln_densidad_empleo")
# Verificación básica + variables Woodward
faltan_iv <- setdiff(cols_iv_requeridas, names(variables_aglomeracion_e_iv_nacional))
if (length(faltan_iv) > 0) {
  stop("ERROR CRÍTICO (B5-5.1): Faltan columnas base en 'variables_aglomeracion_e_iv_nacional': ", paste(faltan_iv, collapse=", "))
}
# Verificación específica Woodward (si falló el bloque 4)
if (!"Z_woodward_resid_potencial" %in% names(variables_aglomeracion_e_iv_nacional)) {
  stop("ERROR CRÍTICO: No se encontraron las variables Woodward (IV-GL). Revise el Bloque 4.")
}

# 5.2 Ensamblaje de la muestra pre-shock
# 5.2.1 Filtrado de microdatos
microdatos_pre_shock <- datos_micro_nacional_full %>%
  dplyr::filter(
    YEAR %in% periodo_pre_shock,
    !is.na(PERWT_HARM),
    PERWT_HARM > 0,
    EMPSTAT == 1L,
    !is.na(WKSWORK2),
    WKSWORK2 != 0,
    INCWAGE_wins > 0
  )

# 5.2.2 Unión con variables de aglomeración e instrumentales
vars_pre_shock <- variables_aglomeracion_e_iv_nacional %>%
  dplyr::filter(año %in% periodo_pre_shock)

datos_modelo_pre_shock <- dplyr::left_join(
  microdatos_pre_shock,
  vars_pre_shock,
  by = c("GEOID", "YEAR" = "año")
)
rm(microdatos_pre_shock, vars_pre_shock); gc()

# 5.3 Definición de la estructura sectorial (IND1990)
# 5.3.1 Códigos de industria
codigos_retail_IND1990 <- c(580, 581, 582, 591, 601, 610, 611, 623, 630, 631, 633, 640, 642, 650, 651, 652, 660, 661, 662, 681, 682, 691)
codigos_resto_economia_IND1990 <- c(40:571, 590, 612, 620, 621, 622, 641, 672, 700:999)

# 5.3.2 Creación de nombres para las variables indicadoras
nombres_retail_individuales <- paste0("ind_retail_", codigos_retail_IND1990)

nombres_sectores_dummies <- c("ind_resto_economia", nombres_retail_individuales)

# 5.4 Creación de variables para el modelo
# 5.4.1 Indicadores sectoriales
crear_dummies_sectoriales_IND1990 <- function(df) {
  stopifnot("IND1990" %in% names(df))
  # Definición del tipo de dato
  
  df <- df %>% mutate(IND1990 = as.integer(IND1990))
  # Indicador del resto de la economía
  
  df <- df %>%
    mutate(ind_resto_economia = as.integer(IND1990 %in% codigos_resto_economia_IND1990))
  # Indicadores individuales de comercio minorista
  dummies_list <- purrr::map(codigos_retail_IND1990, ~ as.integer(df$IND1990 == .x))
  names(dummies_list) <- paste0("ind_retail_", codigos_retail_IND1990)
  dplyr::bind_cols(df, as_tibble(dummies_list))
}

if (!exists("crear_dummies_sectoriales_modificado")) {
  crear_dummies_sectoriales_modificado <- crear_dummies_sectoriales_IND1990
}

datos_modelo_pre_shock <- datos_modelo_pre_shock %>%
  mutate(
    # Recodificación de valores ausentes
    WKSWORK2 = na_if(as.character(WKSWORK2), "0"),
    MARST    = na_if(as.character(MARST),    "9"),
    EDUC     = na_if(EDUC,                   99)
  ) %>%
  mutate(
    ln_incwage      = log(INCWAGE_wins),
    AGE2            = AGE^2,
    SEX_factor      = factor(SEX),
    MARST_factor    = relevel(factor(MARST),    ref = "1"),
    RACE_factor     = relevel(factor(RACE),     ref = "1"),
    EDUC_factor     = relevel(factor(EDUC),     ref = "0"),
    WKSWORK2_factor = relevel(factor(WKSWORK2), ref = "1"),
    # Factor de año
    MULTYEAR_factor = relevel(factor(YEAR), ref = as.character(min(periodo_pre_shock))),
    GEOID_factor    = factor(GEOID)
  ) %>%
  # Aplicación de la función para crear dummies sectoriales
  crear_dummies_sectoriales_IND1990()

# 5.5 Diagnóstico de valores ausentes
variables_para_diagnostico <- c(
  "ln_incwage", "AGE", "AGE2", "SEX_factor", "MARST_factor", "RACE_factor",
  "EDUC_factor", "WKSWORK2_factor", "MULTYEAR_factor", "GEOID_factor",
  nombres_sectores_dummies,
  "ln_potencial_mercado", "ln_densidad_empleo"
)
faltan_diag <- setdiff(variables_para_diagnostico, names(datos_modelo_pre_shock))
if (length(faltan_diag) > 0) {
  stop("ERROR (B5-5.5): Variables esperadas no presentes para el diagnóstico: ",
       paste(faltan_diag, collapse = ", "))
}
na_counts <- colSums(is.na(datos_modelo_pre_shock[, variables_para_diagnostico]))

if (any(na_counts > 0)) {
  print(na_counts[na_counts > 0])
} else {
  cat("Diagnóstico: Datos íntegros.\n")
}

# 5.6 Guardado del entorno para el Bloque 6
if (!exists("ruta_datos_procesados")) {
  suppressPackageStartupMessages(library(here))
  ruta_datos_procesados <- here::here("3_Outputs", "1_Data_Processed")
}

if (!exists("ruta_modelos")) {
  suppressPackageStartupMessages(library(here))
  ruta_modelos <- here::here("3_Outputs", "2_Models")
}

ruta_sesion_b6 <- here::here("3_Outputs", "1_Data_Processed", "sesion_para_bloque_6.RData")

save(
  datos_modelo_pre_shock,
  nombres_sectores_dummies,
  ruta_modelos,
  ruta_datos_procesados,
  file = ruta_sesion_b6
)

# 5.7 Liberación de memoria
gc()

# ==============================================================================
# BLOQUE 6: Estimación del modelo de salarios
# ==============================================================================

# 6.1 Carga del entorno de trabajo
# 6.1.1 Limpieza de memoria
rm(list = ls())
gc()

# 6.1.2 Carga de la sesión de datos preparada en el Bloque 5
ruta_sesion_b6 <- here::here("3_Outputs", "1_Data_Processed", "sesion_para_bloque_6.RData")
if (!file.exists(ruta_sesion_b6)) {
  stop("ERROR CRÍTICO: No se encontró el archivo de sesión 'sesion_para_bloque_6.RData'. Ejecute el Bloque 5.")
}
load(ruta_sesion_b6)

# 6.2 Definición de las especificaciones del modelo
# 6.2.1 Configuración del entorno de estimación
setFixest_nthreads(0)

# 6.2.2 Definición de componentes de las fórmulas
variable_dependiente <- "ln_incwage"
controles_principales_base <- c("AGE", "AGE2", "SEX_factor", "MARST_factor", "RACE_factor", "EDUC_factor", "WKSWORK2_factor", "MULTYEAR_factor")
efectos_fijos <- "GEOID_factor"
variables_endogenas <- "ln_potencial_mercado + ln_densidad_empleo"

# 6.2.3 Configuración del Modelo 1 (variables instrumentales Woodward IV-GL)
instrumentos_m1 <- "Z_woodward_resid_potencial + Z_woodward_resid_densidad"
controles_woodward <- "C_woodward_pred_potencial + C_woodward_pred_densidad"

# Modelos 2 y 3 (IV sintéticos): ponderado (W) y no ponderado (UW)
instrumentos_m2 <- "Z_sintetico_w_potencial + Z_sintetico_w_densidad"
instrumentos_m3 <- "Z_sintetico_uw_potencial + Z_sintetico_uw_densidad"

# 6.3 Preparación de los datos para la estimación
# 6.3.1 Selección de la muestra base
vars_base_no_na <- unique(c(
  variable_dependiente,
  controles_principales_base,
  nombres_sectores_dummies,
  efectos_fijos,
  str_split_1(variables_endogenas, " \\+ "),
  "PERWT_HARM"
))

# Selección de variables (adición de columnas instrumentales y variables de control)
vars_todas <- unique(c(
  vars_base_no_na,
  str_split_1(instrumentos_m1, " \\+ "),
  str_split_1(instrumentos_m2, " \\+ "),
  str_split_1(instrumentos_m3, " \\+ "),
  str_split_1(controles_woodward, " \\+ ")
))

obs_iniciales <- nrow(datos_modelo_pre_shock)

datos_base <- datos_modelo_pre_shock %>%
  dplyr::select(any_of(vars_todas)) %>%
  dplyr::filter(!is.na(PERWT_HARM) & PERWT_HARM > 0) %>%
  tidyr::drop_na(any_of(setdiff(vars_base_no_na, "PERWT_HARM"))) %>% 
  droplevels()

rm(datos_modelo_pre_shock); gc()

# 6.3.2 Limpieza de variables de control
varianzas_dummies <- sapply(datos_base[nombres_sectores_dummies], var, na.rm = TRUE)
dummies_constantes <- names(varianzas_dummies[varianzas_dummies == 0])
nombres_sectores_dummies_final <- setdiff(nombres_sectores_dummies, dummies_constantes)
if (length(dummies_constantes) > 0) {
  warning("Dummies constantes eliminadas: ", paste(dummies_constantes, collapse=", "), call. = FALSE)
}

# 6.3.3 Definición de fórmulas de estimación
controles_principales <- paste(c(controles_principales_base, nombres_sectores_dummies_final), collapse = " + ")

# Modelo 1: IV-GL con FE de PUMA
formula_m1_iv <- as.formula(paste0(
  variable_dependiente, " ~ ", controles_principales, " + ", controles_woodward,
  " | ", efectos_fijos,
  " | ", variables_endogenas, " ~ ", instrumentos_m1
))

# Modelo 2: IV Sintético Ponderado
formula_m2_iv <- as.formula(paste0(
  variable_dependiente, " ~ ", controles_principales,
  " | ", efectos_fijos,
  " | ", variables_endogenas, " ~ ", instrumentos_m2
))

# Modelo 3: IV Sintético No Ponderado
formula_m3_iv <- as.formula(paste0(
  variable_dependiente, " ~ ", controles_principales,
  " | ", efectos_fijos,
  " | ", variables_endogenas, " ~ ", instrumentos_m3
))

# 6.3.4 Función auxiliar para la creación de conjuntos de datos específicos
mk_datos_modelo <- function(.datos, instr_string, control_string = NULL) {
  cols_check <- str_split_1(instr_string, " \\+ ")
  if (!is.null(control_string)) {
    cols_check <- c(cols_check, str_split_1(control_string, " \\+ "))
  }
  .datos %>% tidyr::drop_na(any_of(cols_check))
}

# 6.4 Estimación de los modelos de variables instrumentales
# 6.4.1 Estimación secuencial con gestión eficiente de memoria
# Estimación del Modelo 1 (variables instrumentales Woodward IV-GL)
datos_m1 <- mk_datos_modelo(datos_base, instrumentos_m1, controles_woodward)

# 6.4.2 Definición de métricas de diagnóstico
pat_instr <- list(
  m1 = "^Z_woodward_resid_",
  m2 = "^Z_sintetico_w_",
  m3 = "^Z_sintetico_uw_"
)

get_iv_metrics <- function(model) {
  tibble(
    Hausman_p = suppressWarnings(tryCatch(fixest::fitstat(model, type = "wh.p", simplify = TRUE), error = function(e) NA_real_))
  )
}

metricas_list <- list()
save(
  list = ls(),
  file = file.path(ruta_modelos, "entorno_completo_bloque6_antes_modelo_1.RData")
)

modelo_1 <- feols(
  formula_m1_iv,
  data    = datos_m1,
  weights = ~PERWT_HARM,
  vcov    = ~GEOID_factor,
  lean    = TRUE
)
metricas_list[["Modelo 1 (IV-GL Woodward)"]] <- get_iv_metrics(modelo_1)
saveRDS(modelo_1, file.path(ruta_modelos, "modelo_1_iv_woodward.rds"))
rm(modelo_1); gc()

# Liberar la muestra específica del Modelo 1 antes de construir la del Modelo 2
if ("datos_m1" %in% ls()) {
  rm(datos_m1)
  gc()
}

# 6.4.3 Estimación del Modelo 2 (IV Sintético Ponderado)
datos_m2 <- mk_datos_modelo(datos_base, instrumentos_m2)

save(
  list = ls(),
  file = file.path(ruta_modelos, "entorno_completo_bloque6_antes_modelo_2.RData")
)

modelo_2 <- feols(
  formula_m2_iv,
  data    = datos_m2,
  weights = ~PERWT_HARM,
  vcov    = ~GEOID_factor,
  lean    = TRUE
)
metricas_list[["Modelo 2 (Sintético W)"]] <- get_iv_metrics(modelo_2)
saveRDS(modelo_2, file.path(ruta_modelos, "modelo_2_iv_sintetico_w.rds"))
rm(modelo_2); gc()

if ("datos_m2" %in% ls()) {
  rm(datos_m2)
  gc()
}

# 6.4.4 Estimación del Modelo 3 (IV Sintético No Ponderado)
datos_m3 <- mk_datos_modelo(datos_base, instrumentos_m3)

save(
  list = ls(),
  file = file.path(ruta_modelos, "entorno_completo_bloque6_antes_modelo_3.RData")
)

modelo_3 <- feols(
  formula_m3_iv,
  data    = datos_m3,
  weights = ~PERWT_HARM,
  vcov    = ~GEOID_factor,
  lean    = TRUE
)

metricas_list[["Modelo 3 (Sintético UW)"]] <- get_iv_metrics(modelo_3)
saveRDS(modelo_3, file.path(ruta_modelos, "modelo_3_iv_sintetico_uw.rds"))
rm(modelo_3); gc()

# Limpieza final de datos de estimación
objetos_pesados_final <- intersect(ls(), c("datos_base", "datos_m1", "datos_m2", "datos_m3"))
if (length(objetos_pesados_final) > 0) {
  rm(list = objetos_pesados_final)
}
gc()

# 6.5 Selección del modelo principal
# 6.5.1 Recopilación de métricas de diagnóstico
metricas_df <- dplyr::bind_rows(metricas_list, .id = "Modelo")

# 6.6 Presentación de resultados y limpieza
# 6.6.1 Tabla de diagnóstico (prueba de Hausman)
print(knitr::kable(metricas_df, format = "pipe", digits = 6, caption = "Diagnóstico: Test de Endogeneidad de Hausman"))

# 6.6.2 Definición y guardado del modelo de referencia
mejor_modelo_nombre <- "Modelo 2 (Sintético W)"
mejor_modelo_info <- metricas_df %>%
  dplyr::filter(Modelo == mejor_modelo_nombre)

if (nrow(mejor_modelo_info) == 0L) {
  stop("ERROR CRÍTICO: No se encontraron métricas para el 'Modelo 2 (Sintético W)'.")
}

cat("\nModelo principal de referencia seleccionado:", mejor_modelo_nombre, "\n")
cat("P-valor Hausman (para información):", mejor_modelo_info$Hausman_p, "\n")

mapa_archivos <- c(
  "Modelo 1 (IV-GL Woodward)" = "modelo_1_iv_woodward.rds",
  "Modelo 2 (Sintético W)"    = "modelo_2_iv_sintetico_w.rds",
  "Modelo 3 (Sintético UW)"   = "modelo_3_iv_sintetico_uw.rds"
)

nombre_archivo_ganador <- mapa_archivos[[mejor_modelo_nombre]]
aviso_seleccion <- paste0(mejor_modelo_nombre, " seleccionado como modelo de referencia.")

# Cargar en memoria el modelo principal de referencia en versión ligera (opción lean)
modelo_caballo_de_batalla_feols <- readRDS(file.path(ruta_modelos, nombre_archivo_ganador))

# Guardar información para bloques subsiguientes
info_modelo_caballo_de_batalla <- list(
  modelo_seleccionado = as.character(mejor_modelo_nombre),
  archivo_modelo_lean = as.character(nombre_archivo_ganador)
)

saveRDS(
  info_modelo_caballo_de_batalla,
  file = file.path(ruta_modelos, "info_modelo_caballo_de_batalla.rds")
)

saveRDS(
  modelo_caballo_de_batalla_feols,
  file = file.path(ruta_modelos, "modelo_caballo_de_batalla_lean.rds")
)

cat("\n--- Decisión Final ---\n", aviso_seleccion, "\n")
cat("\n*** El objeto 'modelo_caballo_de_batalla_feols' ha sido creado, y la información del modelo principal se ha guardado en disco. ***\n")

# 6.6.3 Tabla comparativa de resultados
local({
  lista_de_modelos <- list(
    "Modelo 1 (IV-GL Woodward)" = readRDS(file.path(ruta_modelos, "modelo_1_iv_woodward.rds")),
    "Modelo 2 (Sintético W)"    = readRDS(file.path(ruta_modelos, "modelo_2_iv_sintetico_w.rds")),
    "Modelo 3 (Sintético UW)"   = readRDS(file.path(ruta_modelos, "modelo_3_iv_sintetico_uw.rds"))
  )
  
  gof_map_final <- list(
    list("raw" = "nobs", "clean" = "Observaciones", "fmt" = 0),
    list("raw" = "r2.within", "clean" = "R2 Within", "fmt" = 4),
    list("raw" = "FE: GEOID_factor", "clean" = "FE: PUMA (GEOID)", "fmt" = "Sí")
  )
  print(modelsummary(
    lista_de_modelos, output = "markdown", stars = c('*' = .1, '**' = .05, '***' = .01),
    title = "Tabla 1 (Réplica Final 2017-2019): Estimación del Modelo de Salarios Pre-Shock",
    gof_map = gof_map_final, fmt = 5
  ))
})

# 6.6.4 Liberación final de memoria
rm(metricas_list, metricas_df)
gc()

# ==============================================================================
# BLOQUE 7: Cálculo del índice de resiliencia
# ==============================================================================

# 7.1 Configuración del Entorno de Trabajo
suppressPackageStartupMessages({
  library(here); library(tidyverse); library(fixest); library(arrow)
})

# 7.1.1 Definición de rutas y parámetros temporales
ruta_datos_procesados     <- here::here("3_Outputs", "1_Data_Processed")
ruta_modelos              <- here::here("3_Outputs", "2_Models")
periodo_pre_shock         <- c(2017, 2018, 2019)
periodo_post_shock        <- c(2022L, 2023L)
periodo_post_shock_resil  <- periodo_post_shock
año_analisis_resiliencia  <- max(periodo_post_shock_resil)

# 7.2 Carga del Modelo de Referencia
cat("Verificación del modelo de referencia.\n")
path_caballo_de_batalla_full <- file.path(ruta_modelos, "modelo_caballo_de_batalla_full.rds")
path_info_caballo            <- file.path(ruta_modelos, "info_modelo_caballo_de_batalla.rds")

# 7.2.1 Carga de la sesión de datos del Bloque 6
ruta_sesion_b6 <- file.path(ruta_datos_procesados, "sesion_para_bloque_6.RData")
if (!file.exists(ruta_sesion_b6)) {
  stop("Error: 'sesion_para_bloque_6.RData' no encontrado. Ejecute el Bloque 5.")
}
load(ruta_sesion_b6)

# 7.2.2 Reconstrucción de la especificación econométrica
variable_dependiente <- "ln_incwage"
controles_principales_base <- c(
  "AGE", "AGE2", "SEX_factor", "MARST_factor",
  "RACE_factor", "EDUC_factor", "WKSWORK2_factor",
  "MULTYEAR_factor"
)
efectos_fijos <- "GEOID_factor"
variables_endogenas <- "ln_potencial_mercado + ln_densidad_empleo"

# Definición de instrumentos y controles espaciales
controles_woodward <- "C_woodward_pred_potencial + C_woodward_pred_densidad"
instrumentos_m1 <- "Z_woodward_resid_potencial + Z_woodward_resid_densidad"
instrumentos_m2 <- "Z_sintetico_w_potencial + Z_sintetico_w_densidad"
instrumentos_m3 <- "Z_sintetico_uw_potencial + Z_sintetico_uw_densidad"

controles_principales <- paste(
  c(controles_principales_base, nombres_sectores_dummies),
  collapse = " + "
)

# Definición de fórmulas de estimación
formula_m1_iv <- as.formula(paste0(
  variable_dependiente, " ~ ", controles_principales, " + ", controles_woodward,
  " | ", efectos_fijos,
  " | ", variables_endogenas, " ~ ", instrumentos_m1
))

formula_m2_iv <- as.formula(paste0(
  variable_dependiente, " ~ ", controles_principales,
  " | ", efectos_fijos,
  " | ", variables_endogenas, " ~ ", instrumentos_m2
))

formula_m3_iv <- as.formula(paste0(
  variable_dependiente, " ~ ", controles_principales,
  " | ", efectos_fijos,
  " | ", variables_endogenas, " ~ ", instrumentos_m3
))

# 7.2.3 Selección y carga del objeto del modelo
if (file.exists(path_info_caballo)) {
  info_modelo_caballo <- readRDS(path_info_caballo)
  mejor_modelo_nombre <- info_modelo_caballo$modelo_seleccionado
  cat(" -> Información del modelo principal encontrada en disco:", mejor_modelo_nombre, "\n")
} else {
  mejor_modelo_nombre <- "Modelo 2 (Sintético W)"
  cat(" -> ADVERTENCIA: no se encontró 'info_modelo_caballo_de_batalla.rds'. ",
      "Se utilizará por defecto: ", mejor_modelo_nombre, "\n", sep = "")
}

if (identical(mejor_modelo_nombre, "Modelo 1 (IV-GL Woodward)")) {
  formula_caballo <- formula_m1_iv
} else if (identical(mejor_modelo_nombre, "Modelo 3 (Sintético UW)")) {
  formula_caballo <- formula_m3_iv
} else {
  formula_caballo <- formula_m2_iv
}


# Preparación de datos para la estimación
variables_necesarias <- unique(c(all.vars(formula_caballo), "PERWT_HARM"))
datos_estimacion <- datos_modelo_pre_shock %>%
  dplyr::select(any_of(variables_necesarias)) %>%
  tidyr::drop_na()

# Carga o reestimación del modelo de referencia
if (file.exists(path_caballo_de_batalla_full)) {
  cat(" -> Modelo completo encontrado. Cargando desde el disco...\n")
  modelo_caballo_de_batalla <- readRDS(path_caballo_de_batalla_full)
} else {
  cat(" -> Modelo completo no encontrado. Estimando desde cero con la especificación seleccionada...\n")
  modelo_caballo_de_batalla <- feols(
    formula_caballo,
    data    = datos_estimacion,
    weights = ~PERWT_HARM,
    vcov    = ~GEOID_factor,
    lean    = FALSE
  )
  cat(" -> Modelo generado. Guardando en disco para uso futuro...\n")
  saveRDS(modelo_caballo_de_batalla, file = path_caballo_de_batalla_full)
}

cat(" -> Modelo 'caballo de batalla' listo en memoria.\n")

# Carga de niveles de factor para consistencia en la predicción
load(file.path(ruta_datos_procesados, "sesion_para_bloque_6.RData"))
factor_cols   <- names(Filter(is.factor, datos_modelo_pre_shock))
factor_levels <- lapply(datos_modelo_pre_shock[factor_cols], levels)

# 7.3 Proyección contrafactual de variables de aglomeración
variables_aglomeracion_e_iv_nacional <- readRDS(
  file.path(ruta_datos_procesados, "variables_aglomeracion_e_iv_nacional.rds")
)

tasas_crecimiento_pre <- variables_aglomeracion_e_iv_nacional %>%
  dplyr::filter(año %in% periodo_pre_shock) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(
    last_ln_potencial  = dplyr::last(ln_potencial_mercado, order_by = año),
    first_ln_potencial = dplyr::first(ln_potencial_mercado, order_by = año),
    last_ln_densidad   = dplyr::last(ln_densidad_empleo,   order_by = año),
    first_ln_densidad  = dplyr::first(ln_densidad_empleo,  order_by = año),
    n = dplyr::n_distinct(año),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    growth_rate_mp  = (last_ln_potencial - first_ln_potencial) / pmax(n - 1, 1),
    growth_rate_den = (last_ln_densidad  - first_ln_densidad ) / pmax(n - 1, 1),
    growth_rate_mp  = ifelse(is.finite(growth_rate_mp),  growth_rate_mp,  0),
    growth_rate_den = ifelse(is.finite(growth_rate_den), growth_rate_den, 0)
  )

variables_aglomeracion_cf <- purrr::map_dfr(
  periodo_post_shock_resil,
  function(anio_obj) {
    delta <- anio_obj - max(periodo_pre_shock)
    tasas_crecimiento_pre %>%
      dplyr::transmute(
        GEOID,
        año = anio_obj,
        ln_potencial_mercado_cf = last_ln_potencial + delta * growth_rate_mp,
        ln_densidad_empleo_cf   = last_ln_densidad  + delta * growth_rate_den
      )
  }
)

# 7.4 Construcción del panel de predicción post-shock
# 7.4.1 Filtrado y preparación de microdatos observados
datos_micro_nacional_full <- readRDS(
  file.path(ruta_datos_procesados, "datos_micro_nacional_full.rds")
)

microdatos_post <- datos_micro_nacional_full %>%
  dplyr::filter(
    YEAR %in% periodo_post_shock_resil,
    !is.na(PERWT_HARM),
    PERWT_HARM > 0,
    EMPSTAT == 1L,
    !is.na(WKSWORK2),
    WKSWORK2 != 0,
    INCWAGE_wins > 0
  )

# 7.4.2 Generación de variables indicadoras sectoriales
codigos_retail_IND1990         <- c(580, 581, 582, 591, 601, 610, 611, 623, 630, 631, 633, 640, 642, 650, 651, 652, 660, 661, 662, 681, 682, 691)
codigos_resto_economia_IND1990 <- c(40:571, 590, 612, 620, 621, 622, 641, 672, 700:999)
crear_dummies_sectoriales_IND1990 <- function(df) {
  stopifnot("IND1990" %in% names(df))
  df <- df %>% dplyr::mutate(IND1990 = as.integer(IND1990))
  df <- df %>% dplyr::mutate(ind_resto_economia = as.integer(IND1990 %in% codigos_resto_economia_IND1990))
  dummies_list <- purrr::map(codigos_retail_IND1990, ~ as.integer(df$IND1990 == .x))
  names(dummies_list) <- paste0("ind_retail_", codigos_retail_IND1990)
  dplyr::bind_cols(df, tibble::as_tibble(dummies_list))
}
microdatos_post <- crear_dummies_sectoriales_IND1990(microdatos_post)

# 7.4.3 Integración del panel de predicción final
variables_aglomeracion_obs_post <- variables_aglomeracion_e_iv_nacional %>%
  dplyr::filter(año %in% periodo_post_shock_resil) %>%
  dplyr::select(
    GEOID,
    año,
    ln_potencial_mercado,
    ln_densidad_empleo,
    dplyr::starts_with("Z_"),
    dplyr::starts_with("C_woodward")
  ) %>%
  dplyr::rename_with(
    ~ paste0(.x, "_obs"),
    dplyr::starts_with("C_woodward")
  )

# 7.4.4 Definición de las matrices de ponderación espacial
if (!exists("V_GL") || !exists("geo_order")) {
  geometrias_puma_nacional <- readRDS(file.path(ruta_datos_procesados, "geometrias_puma_nacional.rds"))
  
  lista_vecinos_nacional <- spdep::poly2nb(geometrias_puma_nacional, queen = TRUE)
  W_mat <- spdep::nb2mat(lista_vecinos_nacional, style = "B", zero.policy = TRUE)
  W_sym <- W_mat
  
  geo_order <- geometrias_puma_nacional %>%
    sf::st_drop_geometry() %>%
    dplyr::select(GEOID)
  
  W_tmp <- W_sym
  diag(W_tmp) <- 0
  deg_vec <- rowSums(W_tmp)
  L_mat <- diag(deg_vec) - W_tmp
  
  eig_GL <- eigen(L_mat, symmetric = TRUE)
  V_GL  <- eig_GL$vectors
  lambda_GL <- eig_GL$values
  
  ord_GL <- order(lambda_GL, decreasing = FALSE)
  V_GL   <- V_GL[, ord_GL, drop = FALSE]
  lambda_GL <- lambda_GL[ord_GL]
  
  # Identificación de autovectores con valor propio cero (constantes) y no nulos
  lambda_tol <- 1e-10
  zero_idx <- which(lambda_GL <= lambda_tol)
  nonzero_idx <- which(lambda_GL > lambda_tol)
  
  rm(lista_vecinos_nacional, W_mat, W_sym, W_tmp, L_mat, eig_GL)
}

if (!exists("k_woodward")) {
  k_woodward <- min(35L, length(nonzero_idx))
  if (k_woodward < 1L) {
    stop("ERROR CRÍTICO: No hay autovectores no nulos suficientes para construir la base de Woodward (bloque 7).")
  }
}

if (!exists("calcular_woodward_tps")) {
  calcular_woodward_tps <- function(df_anual, k_basis = k_woodward) {
    a_mp_full  <- rep(0, nrow(geo_order))
    a_den_full <- rep(0, nrow(geo_order))
    
    idx <- match(df_anual$GEOID, geo_order$GEOID)
    idx_valid <- which(!is.na(idx))
    
    a_mp_full[idx[idx_valid]]  <- df_anual$ln_potencial_mercado[idx_valid]
    a_den_full[idx[idx_valid]] <- df_anual$ln_densidad_empleo[idx_valid]
    
    k_use <- min(k_basis, length(nonzero_idx))
    
    low_idx  <- nonzero_idx[seq_len(k_use)]
    high_idx <- setdiff(seq_len(ncol(V_GL)), low_idx)
    
    V_low  <- V_GL[, low_idx, drop = FALSE]
    V_high <- V_GL[, high_idx, drop = FALSE]
    
    P_low  <- V_low  %*% t(V_low)
    P_high <- V_high %*% t(V_high)
    
    AC_mp_full   <- as.numeric(P_low  %*% a_mp_full)
    AUC_mp_full  <- as.numeric(P_high %*% a_mp_full)
    AC_den_full  <- as.numeric(P_low  %*% a_den_full)
    AUC_den_full <- as.numeric(P_high %*% a_den_full)
    
    base_df <- tibble::tibble(
      GEOID = geo_order$GEOID,
      Z_woodward_resid_potencial = AUC_mp_full,
      Z_woodward_resid_densidad  = AUC_den_full,
      C_woodward_pred_potencial  = AC_mp_full,
      C_woodward_pred_densidad   = AC_den_full
    )
    
    base_df %>%
      dplyr::inner_join(df_anual %>% dplyr::select(GEOID), by = "GEOID")
  }
}

# 7.4.5 Generación de controles espaciales contrafactuales
datos_para_woodward_cf <- variables_aglomeracion_cf %>%
  dplyr::rename(
    ln_potencial_mercado_original = ln_potencial_mercado_cf,
    ln_densidad_empleo_original   = ln_densidad_empleo_cf
  ) %>%
  dplyr::mutate(
    ln_potencial_mercado = ln_potencial_mercado_original,
    ln_densidad_empleo   = ln_densidad_empleo_original
  )

# Cálculo del escenario sin shock
ivs_woodward_cf_panel <- datos_para_woodward_cf %>%
  dplyr::group_by(año) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    woodward_data = purrr::map(
      data,
      ~ calcular_woodward_tps(.x, k_basis = k_woodward)
    )
  ) %>%
  dplyr::select(año, woodward_data) %>%
  tidyr::unnest(cols = c(woodward_data)) %>%
  dplyr::rename(
    Z_woodward_resid_potencial_cf = Z_woodward_resid_potencial,
    Z_woodward_resid_densidad_cf  = Z_woodward_resid_densidad
  )

datos_prediccion_panel <- microdatos_post %>%
  # 1) Variables Observadas
  dplyr::left_join(variables_aglomeracion_obs_post, by = c("GEOID", "YEAR" = "año")) %>%
  # 2) Variables Contrafactuales
  dplyr::left_join(variables_aglomeracion_cf,       by = c("GEOID", "YEAR" = "año")) %>%
  # 3) Controles Woodward Contrafactuales
  dplyr::left_join(ivs_woodward_cf_panel,           by = c("GEOID", "YEAR" = "año")) %>%
  # 4) Armonización de factores con el conjunto de entrenamiento
  dplyr::mutate(
    AGE2            = AGE^2,
    SEX_factor      = factor(SEX,   levels = factor_levels$SEX_factor),
    MARST_factor    = factor(MARST, levels = factor_levels$MARST_factor),
    RACE_factor     = factor(RACE,  levels = factor_levels$RACE_factor),
    EDUC_factor     = factor(EDUC,  levels = factor_levels$EDUC_factor),
    WKSWORK2_factor = factor(WKSWORK2, levels = factor_levels$WKSWORK2_factor),
    GEOID_factor    = factor(GEOID, levels = factor_levels$GEOID_factor),
    MULTYEAR_factor = factor(
      as.character(max(periodo_pre_shock)),
      levels = factor_levels$MULTYEAR_factor
    )
  )

# Verificación de valores ausentes
if (any(is.na(datos_prediccion_panel$C_woodward_pred_potencial)) ||
    any(is.na(datos_prediccion_panel$C_woodward_pred_densidad))) {
  stop("ERROR: Generación de C_woodward contrafactual fallida (NAs detectados).")
}

# 7.4.6 Depuración final de valores perdidos
variables_no_na_requeridas <- c(
  "AGE", "AGE2",
  "SEX_factor", "MARST_factor", "RACE_factor", "EDUC_factor",
  "WKSWORK2_factor", "GEOID_factor", "MULTYEAR_factor",
  "ln_potencial_mercado", "ln_densidad_empleo",
  "ln_potencial_mercado_cf", "ln_densidad_empleo_cf",
  "INCWAGE_wins", "PERWT_HARM"
)

datos_prediccion_panel <- datos_prediccion_panel %>%
  tidyr::drop_na(dplyr::all_of(variables_no_na_requeridas))

# 7.5 Cálculo del Salario Contrafactual y del Índice de Resiliencia
# 7.5.1 Predicción del salario contrafactual (logaritmo)
newdata_contrafactual <- datos_prediccion_panel %>%
  dplyr::mutate(
    ln_potencial_mercado = ln_potencial_mercado_cf,
    ln_densidad_empleo   = ln_densidad_empleo_cf
  )

salario_contrafactual_pred_log <- predict(
  modelo_caballo_de_batalla,
  newdata = newdata_contrafactual
)

# 7.5.2 Corrección de sesgo por retransformación
residuos_modelo_original <- residuals(modelo_caballo_de_batalla)
pesos_modelo_original    <- datos_estimacion$PERWT_HARM

# Cálculo de la media ponderada de los residuos exponenciados
factor_smearing <- weighted.mean(exp(residuos_modelo_original), w = pesos_modelo_original, na.rm = TRUE)

# Transformación a niveles ajustada por sesgo
salario_contrafactual_pred <- exp(salario_contrafactual_pred_log) * factor_smearing

# 7.5.3 Cálculo del índice y tratamiento de valores extremos
datos_resiliencia_panel <- datos_prediccion_panel %>%
  dplyr::mutate(
    salario_contrafactual_pred = salario_contrafactual_pred,
    # Índice proporcional de resiliencia (definición básica)
    RP_prop = (INCWAGE_wins - salario_contrafactual_pred) / INCWAGE_wins
  )

# Definición de límites para la winsorización (1% - 99%)
limite_teorico_inf <- -1
limite_teorico_sup <- 1

qs <- stats::quantile(
  datos_resiliencia_panel$RP_prop,
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  type = 7
)

limite_inf_final <- max(limite_teorico_inf, qs[[1]])
limite_sup_final <- min(limite_teorico_sup, qs[[2]])

datos_resiliencia_panel <- datos_resiliencia_panel %>%
  dplyr::mutate(
    RP_prop_wins = pmin(pmax(RP_prop, limite_inf_final), limite_sup_final),
    Rp_wins      = RP_prop_wins # Alias para consistencia con el Bloque 8

  )

# 7.6 Guardado del panel final
ruta_panel_resiliencia <- file.path(ruta_datos_procesados, "panel_resiliencia_postshock_2022_2023.parquet")
arrow::write_parquet(datos_resiliencia_panel, ruta_panel_resiliencia)

# 7.7 Liberación de memoria
rm(list=c("tasas_crecimiento_pre", "variables_aglomeracion_cf", "datos_micro_nacional_full", 
          "microdatos_post", "variables_aglomeracion_e_iv_nacional", "variables_aglomeracion_obs_post",
          "datos_prediccion_panel", "newdata_contrafactual", "salario_contrafactual_pred", 
          "datos_resiliencia_panel", "factor_cols", "factor_levels", "qs",
          "codigos_retail_IND1990", "codigos_resto_economia_IND1990", "crear_dummies_sectoriales_IND1990",
          "modelo_caballo_de_batalla", "datos_modelo_pre_shock", "nombres_sectores_dummies",
          "datos_estimacion", "residuos_modelo_original", "pesos_modelo_original", "factor_smearing"))

# ==============================================================================
# BLOQUE 8: Estimación del Modelo de Resiliencia
# ==============================================================================

# 8.1 Preparación del entorno
rm(list = ls()); gc()
suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(fixest)
  library(modelsummary)
  library(arrow)
})

ruta_datos_procesados <- here::here("3_Outputs", "1_Data_Processed")
ruta_modelos          <- here::here("3_Outputs", "2_Models")
setFixest_nthreads(0)


# 8.2 Carga y preparación de la muestra
datos_modelo_resiliencia <- arrow::read_parquet(
  file.path(ruta_datos_procesados, "panel_resiliencia_postshock_2022_2023.parquet"),
  as_data_frame = TRUE
)

# 8.2.1 Verificación de variables de aglomeración
if ("ln_potencial_mercado_obs" %in% names(datos_modelo_resiliencia) &&
    "ln_densidad_empleo_obs"   %in% names(datos_modelo_resiliencia)) {
  # Verificación de la estructura de las variables
  
  
} else if ("ln_potencial_mercado" %in% names(datos_modelo_resiliencia) &&
           "ln_densidad_empleo"   %in% names(datos_modelo_resiliencia)) {
  datos_modelo_resiliencia <- datos_modelo_resiliencia %>%
    dplyr::rename(
      ln_potencial_mercado_obs = ln_potencial_mercado,
      ln_densidad_empleo_obs   = ln_densidad_empleo
    )
} else {
  stop("No se encontraron variables de aglomeración (ln_potencial_mercado y ln_densidad_empleo). Revise el panel de resiliencia.")
}

# 8.2.2 Restricción de la muestra post-shock
datos_modelo_resiliencia <- datos_modelo_resiliencia %>%
  dplyr::filter(YEAR %in% c(2022L, 2023L)) %>%
  dplyr::mutate(
    YEAR           = as.integer(YEAR),
    AGE2           = AGE^2,
    SEX_factor     = factor(SEX),
    MARST_factor   = factor(MARST),
    RACE_factor    = factor(RACE),
    EDUC_factor    = factor(EDUC),
    WKSWORK2_factor= factor(WKSWORK2),
    GEOID_factor   = factor(GEOID),
    # Factor temporal anual (YEAR)
    
    
    
    YEAR_factor    = factor(YEAR)
  )

# 8.2.3 Validación del efecto del shock espacial
cols_requeridas_check <- c(
  "C_woodward_pred_potencial", "C_woodward_pred_densidad",
  "C_woodward_pred_potencial_obs", "C_woodward_pred_densidad_obs"
)

missing_cols <- setdiff(cols_requeridas_check, names(datos_modelo_resiliencia))
if (length(missing_cols) > 0) {
  stop("ERROR CRÍTICO: Faltan variables C_woodward diferenciadas (obs vs cf). Faltan: ",
       paste(missing_cols, collapse = ", "))
}

test_cambio <- datos_modelo_resiliencia %>%
  dplyr::mutate(
    diff_C_potencial = C_woodward_pred_potencial_obs - C_woodward_pred_potencial
  )

x_ks <- test_cambio$C_woodward_pred_potencial_obs
y_ks <- test_cambio$C_woodward_pred_potencial
ok_ks <- is.finite(x_ks) & is.finite(y_ks)

if (!any(ok_ks)) {
  warning("ALERTA: No hay datos válidos para el KS-test de C_woodward (potencial). Se omite este diagnóstico.")
} else {
  ks_resultado <- tryCatch(
    stats::ks.test(
      x_ks[ok_ks],
      y_ks[ok_ks]
    ),
    error = function(e) {
      warning("ALERTA: No se pudo calcular el KS-test de C_woodward (potencial): ",
              conditionMessage(e))
      return(NULL)
    }
  )
  
  cat("\n[Diagnóstico] Shock espacial (C_woodward, potencial):\n")
  cat("Diferencia media (obs - cf): ",
      mean(test_cambio$diff_C_potencial, na.rm = TRUE), "\n")
  
  if (!is.null(ks_resultado)) {
    cat("Test Kolmogorov-Smirnov p-valor: ", ks_resultado$p.value, "\n")
    if (ks_resultado$p.value < 0.05) {
      cat("Conclusión: el shock modificó significativamente la estructura espacial (p < 0.05).\n")
    } else {
      warning("ALERTA: no hay evidencia estadística fuerte de cambio estructural espacial (KS).")
    }
  }
}

# 8.3 Creación de variables indicadoras
codigos_retail_IND1990        <- c(580, 581, 582, 591, 601, 610, 611, 623, 630,
                                   631, 633, 640, 642, 650, 651, 652, 660, 661,
                                   662, 681, 682, 691)
codigos_resto_economia_IND1990 <- c(40:571, 590, 612, 620, 621, 622, 641, 672, 700:999)
nombres_sectores_dummies       <- c("ind_resto_economia",
                                    paste0("ind_retail_", codigos_retail_IND1990))

if ("IND1990" %in% names(datos_modelo_resiliencia)) {
  df_con_dummies <- datos_modelo_resiliencia %>%
    dplyr::mutate(IND1990 = as.integer(IND1990))
  
  df_con_dummies$ind_resto_economia <-
    as.integer(df_con_dummies$IND1990 %in% codigos_resto_economia_IND1990)
  
  for (code in codigos_retail_IND1990) {
    df_con_dummies[[paste0("ind_retail_", code)]] <-
      as.integer(df_con_dummies$IND1990 == code)
  }
  
  datos_modelo_resiliencia <- df_con_dummies
  rm(df_con_dummies)
} else {
  nombres_sectores_dummies <- character(0)
}

# 8.4 Construcción de la base de datos final
variables_base_res <- c(
  "Rp_wins",
  "ln_potencial_mercado_obs",
  "ln_densidad_empleo_obs",
  "AGE", "AGE2",
  "SEX_factor", "MARST_factor", "RACE_factor", "EDUC_factor",
  "WKSWORK2_factor",
  # Control de año (2022–2023)
  "YEAR_factor",
  "GEOID_factor",
  "PERWT_HARM",
  nombres_sectores_dummies
)

# Definición de instrumentos disponibles
variables_instrumentos_res <- c(
  # Instrumentos sintéticos (ESF)
  "Z_sintetico_w_potencial",    "Z_sintetico_w_densidad",
  "Z_sintetico_uw_potencial",   "Z_sintetico_uw_densidad",
  # Instrumentos Woodward (IV-GL)
  "Z_woodward_resid_potencial", "Z_woodward_resid_densidad"
)

# Controles espaciales de gran escala OBSERVADOS (sufijo _obs)
variables_controles_woodward_res <- c(
  "C_woodward_pred_potencial_obs", "C_woodward_pred_densidad_obs"
)

datos_base_res <- datos_modelo_resiliencia %>%
  dplyr::select(any_of(unique(c(
    variables_base_res,
    variables_instrumentos_res,
    variables_controles_woodward_res
  )))) %>%
  dplyr::filter(!is.na(PERWT_HARM) & PERWT_HARM > 0) %>%
  tidyr::drop_na(any_of(setdiff(variables_base_res, "PERWT_HARM"))) %>%
  droplevels()

# 8.4.1 Eliminación de dummies sin variabilidad
if (length(nombres_sectores_dummies) > 0) {
  var_dums_res  <- sapply(datos_base_res[nombres_sectores_dummies], var, na.rm = TRUE)
  dums_const_res <- names(var_dums_res[var_dums_res == 0])
  nombres_sectores_dummies_final <- setdiff(nombres_sectores_dummies, dums_const_res)
} else {
  nombres_sectores_dummies_final <- character(0)
}

# 8.5 Definición de las especificaciones IV
controles_principales_cs <- paste(
  c("AGE", "AGE2",
    "SEX_factor", "MARST_factor", "RACE_factor", "EDUC_factor",
    "WKSWORK2_factor",
    nombres_sectores_dummies_final,
    "YEAR_factor"),
  collapse = " + "
)

# Controles adicionales de gran escala (AC observada, C_woodward*_obs) para el Modelo 1
controles_principales_cs_ext <- paste(
  controles_principales_cs,
  paste(variables_controles_woodward_res, collapse = " + "),
  sep = " + "
)

# 8.5.1 Especificación de los efectos fijos
# Modelo 1: IV-GL con efectos fijos
formula_m1_iv_cs <- as.formula(paste0(
  "Rp_wins ~ ", controles_principales_cs_ext,
  " | GEOID_factor | ",
  "ln_potencial_mercado_obs + ln_densidad_empleo_obs ~ ",
  "Z_woodward_resid_potencial + Z_woodward_resid_densidad"
))


# Modelo 2: IV sintético ponderado (W)
formula_m2_iv_cs <- as.formula(paste0(
  "Rp_wins ~ ", controles_principales_cs,
  " | GEOID_factor | ",
  "ln_potencial_mercado_obs + ln_densidad_empleo_obs ~ ",
  "Z_sintetico_w_potencial + Z_sintetico_w_densidad"
))

# Modelo 3: IV sintético no ponderado (UW)
formula_m3_iv_cs <- as.formula(paste0(
  "Rp_wins ~ ", controles_principales_cs,
  " | GEOID_factor | ",
  "ln_potencial_mercado_obs + ln_densidad_empleo_obs ~ ",
  "Z_sintetico_uw_potencial + Z_sintetico_uw_densidad"
))


# 8.5.2 Filtrado de datos según la disponibilidad de instrumentos
mk_res_datos <- function(.datos, instr_vec) {
  cols_req <- c(instr_vec, variables_controles_woodward_res)
  .datos %>% tidyr::drop_na(any_of(cols_req))
}

# Modelo 1: IV-GL Woodward
datos_res_m1 <- mk_res_datos(
  datos_base_res,
  c("Z_woodward_resid_potencial", "Z_woodward_resid_densidad")
)

# Modelo 2: IV sintético ponderado (W)
datos_res_m2 <- mk_res_datos(
  datos_base_res,
  c("Z_sintetico_w_potencial", "Z_sintetico_w_densidad")
)

# Modelo 3: IV sintético no ponderado (UW)
datos_res_m3 <- mk_res_datos(
  datos_base_res,
  c("Z_sintetico_uw_potencial", "Z_sintetico_uw_densidad")
)

# 8.6 Estimación de los modelos
# Modelo 1: IV-GL Woodward
modelo_res_1 <- feols(
  formula_m1_iv_cs,
  data    = datos_res_m1,
  weights = ~PERWT_HARM,
  vcov    = ~GEOID_factor,   # Errores estándar agrupados por PUMA
  lean    = TRUE
)

# Modelo 2: IV sintético ponderado (W)
modelo_res_2 <- feols(
  formula_m2_iv_cs,
  data    = datos_res_m2,
  weights = ~PERWT_HARM,
  vcov    = ~GEOID_factor,
  lean    = TRUE
)

# Modelo 3: IV sintético no ponderado (UW)
modelo_res_3 <- feols(
  formula_m3_iv_cs,
  data    = datos_res_m3,
  weights = ~PERWT_HARM,
  vcov    = ~GEOID_factor,
  lean    = TRUE
)

lista_modelos_res <- list(
  "Modelo 1 (IV-GL Woodward, 2022-2023)" = modelo_res_1,
  "Modelo 2 (Sintético W, 2022-2023)"    = modelo_res_2,
  "Modelo 3 (Sintético UW, 2022-2023)"   = modelo_res_3
)

# 8.7 Presentación de resultados
gof_map_cs_2023 <- list(
  list(raw = "nobs", clean = "Obs.",   fmt = 0),
  list(raw = "r2",   clean = "R2 (IV)", fmt = 4),
  list(raw = "ivf.ln_potencial_mercado_obs",
       clean = "F-test (Potencial)", fmt = 2),
  list(raw = "ivf.ln_densidad_empleo_obs",
       clean = "F-test (Densidad)",  fmt = 2)
)

print(modelsummary(
  lista_modelos_res,
  output      = "markdown",
  stars       = c('*' = .1, '**' = .05, '***' = .01),
  title = "Determinantes de la Resiliencia Salarial (Periodos post-shock 2022-2023)",
  gof_map     = gof_map_cs_2023,
  coef_rename = c(
    "fit_ln_potencial_mercado_obs" = "ln(Potencial Mercado)",
    "fit_ln_densidad_empleo_obs"   = "ln(Densidad Empleo)"
  ),
  fmt         = 5
))

# 8.7.1 Diagnósticos de validez de las estimaciones
diagnosticos_iv_resiliencia <- purrr::map_dfr(
  names(lista_modelos_res),
  function(nombre) {
    mod <- lista_modelos_res[[nombre]]
    
    # F-stat y p-valor de la primera etapa (para cada variable endógena)
    fstat <- fixest::fitstat(mod, type = "ivf", simplify = TRUE)
    f_stat_vec <- as.numeric(fstat$stat)
    f_p_vec    <- as.numeric(fstat$p)
    
    # Resumen escalar: usamos el F mínimo (más restrictivo) y el p-valor máximo
    f_stat <- min(f_stat_vec, na.rm = TRUE)
    f_p    <- max(f_p_vec,  na.rm = TRUE)
    
    # p-valor de Sargan (si el modelo está sobre-identificado)
    sarg_p <- tryCatch(
      as.numeric(fixest::fitstat(mod, type = "sargan.p", simplify = TRUE)),
      error = function(e) NA_real_
    )
    
    # p-valor de Hausman de endogeneidad
    haus_p <- tryCatch(
      as.numeric(fixest::fitstat(mod, type = "wh.p", simplify = TRUE)),
      error = function(e) NA_real_
    )
    
    tibble::tibble(
      Modelo               = nombre,
      `F-stat 1a etapa`    = f_stat,
      `P-val F-stat`       = f_p,
      `Instrumento fuerte` = ifelse(f_stat > 10, "Sí", "No"),
      `Sargan p-val`       = sarg_p,
      `Hausman p-val`      = haus_p
    )
  }
)

print(knitr::kable(
  diagnosticos_iv_resiliencia,
  digits  = 3,
  caption = "Diagnósticos IV del modelo de resiliencia"
))


# 8.8 Almacenamiento de resultados
saveRDS(
  lista_modelos_res,
  file = file.path(ruta_modelos, "lista_modelos_res.rds")
)
