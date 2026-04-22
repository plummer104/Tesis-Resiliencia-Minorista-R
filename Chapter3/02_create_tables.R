# ==============================================================================
# Título: Generación de Tablas para Publicación
# Reproducibilidad: Sí
# Descripción: Este script produce las Tablas 1, 2 y 3 del análisis,
#              utilizando los modelos y datos procesados por el "script principal".
#              Cada tabla se exporta como un archivo Excel independiente.
# ==============================================================================

# 0.1 Carga de paquetes para tablas
paquetes_tablas <- c("knitr", "openxlsx", "xml2", "magrittr", "modelsummary", "fixest", "broom")
invisible(lapply(paquetes_tablas, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  }
  library(pkg, character.only = TRUE, quietly = TRUE)
}))


# 0.2 Rutas y carga de metadatos (DDI)
if (!exists("ruta_modelos")) {
  ruta_modelos <- here::here("3_Outputs", "2_Models")
}

if (!exists("ruta_datos_procesados")) {
  ruta_datos_procesados <- here::here("3_Outputs", "1_Data_Processed")
}

ruta_ddi <- here::here("1_Data", "ipums", "usa_00078.xml")
if (!file.exists(ruta_ddi)) {
  stop("No se encontró el DDI de IPUMS en: ", ruta_ddi, ". Verifique la ruta.")
}
ddi <- ipumsr::read_ipums_ddi(ruta_ddi)


# 0.3 Creación del directorio de salida para las tablas
ruta_carpeta_tablas <- here::here("3_Outputs", "4_Tables")
dir.create(ruta_carpeta_tablas, showWarnings = FALSE, recursive = TRUE)
cat("Entorno para tablas configurado.\n\n")

# --- Bloque 1: Funciones de Apoyo para Etiquetado de Variables ---

# 1.1 Función para obtener etiquetas de variables desde el DDI
get_label <- function(var, code) {
  labs <- ipumsr::ipums_val_labels(ddi, var = var)
  if (is.null(labs) || length(labs) == 0) return(NA_character_)
  
  if (is.data.frame(labs) && all(c("val", "lbl") %in% names(labs))) {
    map <- setNames(as.character(labs$lbl), as.character(labs$val))
    return(unname(map[as.character(code)]))
  }
  
  if (is.atomic(labs)) {
    if (!is.null(names(labs))) {
      idx <- which(unname(labs) == as.integer(code))
      if (length(idx) > 0) return(names(labs)[idx[1]])
      return(unname(labs[as.character(code)]))
    }
  }
  NA_character_
}

# 1.2 Función para construir el mapeo de códigos de industria a dummies

# 1.2.1 Asegurar definición local de códigos sectoriales IND1990 (por si no vienen del script principal)
if (!exists("codigos_retail_IND1990") || !exists("codigos_resto_economia_IND1990")) {
  codigos_retail_IND1990 <- c(
    580, 581, 582, 591, 601, 610, 611, 623, 630, 631, 633,
    640, 642, 650, 651, 652, 660, 661, 662, 681, 682, 691
  )
  codigos_resto_economia_IND1990 <- c(
    40:571, 590, 612, 620, 621, 622, 641, 672, 700:999
  )
}

construir_mapeo_ind <- function() {
  if (!exists("codigos_resto_economia_IND1990") || !exists("codigos_retail_IND1990")) {
    stop("Los objetos 'codigos_resto_economia_IND1990' y 'codigos_retail_IND1990' no se encontraron.")
  }
  map_resto <- tibble::tibble(
    dummy = "ind_resto_economia",
    code  = as.integer(codigos_resto_economia_IND1990)
  )
  map_retail <- tibble::tibble(
    dummy = paste0("ind_retail_", codigos_retail_IND1990),
    code  = as.integer(codigos_retail_IND1990)
  )
  dplyr::bind_rows(map_resto, map_retail) %>%
    dplyr::filter(!is.na(code) & !is.na(dummy))
}

mapeo_ind <- construir_mapeo_ind()


# 1.3 Función para mapear nombres de dummies de industria a etiquetas descriptivas
dummy_ind_to_label <- function(coef_names) {
  if (!is.data.frame(mapeo_ind) || nrow(mapeo_ind) == 0) return(character(0))
  
  d_present <- unique(intersect(mapeo_ind$dummy, coef_names))
  if (length(d_present) == 0) return(character(0))
  
  lab_map <- setNames(character(length(d_present)), d_present)
  for (d in d_present) {
    codes_d <- sort(unique(mapeo_ind$code[mapeo_ind$dummy == d]))
    labs_d <- unique(na.omit(purrr::map_chr(codes_d, ~ get_label("IND1990", .x))))
    if (length(labs_d) > 0) {
      lab_map[d] <- if (d == "ind_resto_economia") "Industry: Rest of Economy" else paste0("Industry: ", paste(labs_d, collapse = " + "))
    }
  }
  lab_map
}

# 1.4 Función para construir el diccionario completo de renombrado de coeficientes
build_coef_rename_map <- function(model_list) {
  coef_names <- unique(unlist(lapply(model_list, function(m) names(coef(m)))))
  rename_map <- c()
  
  for (var_base in c("MARST", "RACE", "EDUC", "WKSWORK2")) {
    var_factor <- paste0(var_base, "_factor")
    idx <- stringr::str_detect(coef_names, paste0("^", var_factor))
    if (any(idx)) {
      codes <- stringr::str_remove(coef_names[idx], paste0("^", var_factor))
      labels <- purrr::map_chr(codes, ~ get_label(var_base, .x))
      labels[is.na(labels)] <- codes[is.na(labels)]
      rename_map <- c(rename_map, setNames(labels, coef_names[idx]))
    }
  }
  
  if (any(stringr::str_detect(coef_names, "^SEX_factor2"))) {
    rename_map["SEX_factor2"] <- get_label("SEX", 2)
  }
  
  idx <- stringr::str_detect(coef_names, "^MULTYEAR_factor\\d{4}$")
  if (any(idx)) {
    years <- stringr::str_remove(coef_names[idx], "^MULTYEAR_factor")
    rename_map <- c(rename_map, setNames(years, coef_names[idx]))
  }
  
  rename_map <- c(rename_map, dummy_ind_to_label(coef_names))
  
  pretty_terms <- c(
    "AGE"                          = "Age",
    "AGE2"                         = "Age²",
    "ln_potencial_mercado"         = "ln(Market Potential)",
    "ln_densidad_empleo"           = "ln(Employment Density)",
    "ln_potencial_mercado_obs"     = "ln(Market Potential)",
    "ln_densidad_empleo_obs"       = "ln(Employment Density)",
    "fit_ln_potencial_mercado"     = "ln(Market Potential)",
    "fit_ln_densidad_empleo"       = "ln(Employment Density)",
    "fit_ln_potencial_mercado_obs" = "ln(Market Potential)",
    "fit_ln_densidad_empleo_obs"   = "ln(Employment Density)",
    "(Intercept)"                  = "Constant"
  )
  
  present_terms <- intersect(names(pretty_terms), coef_names)
  if (length(present_terms) > 0) {
    new_terms <- pretty_terms[present_terms]
    rename_map <- c(rename_map, new_terms[!names(new_terms) %in% names(rename_map)])
  }
  
  return(rename_map)
}

# --- Bloque 1.5: Funciones de Apoyo para F-tests ---

# 1.5.1 Función para realizar los F-tests (pruebas de Wald)
perform_joint_test <- function(model, pattern) {
  coef_names <- names(coef(model))
  target_coefs <- coef_names[stringr::str_detect(coef_names, pattern)]
  
  if (length(target_coefs) < 1) {
    return(NA_character_)
  }
  
  tryCatch({
    test_result <- fixest::wald(model, keep = target_coefs, print = FALSE)
    
    f_stat <- test_result$stat
    p_val <- test_result$p
    
    stars <- dplyr::case_when(
      p_val < 0.01 ~ "***",
      p_val < 0.05 ~ "**",
      p_val < 0.10 ~ "*",
      TRUE         ~ ""
    )
    
    paste0(format(round(f_stat, 2), nsmall = 2), stars)
    
  }, error = function(e) {
    warning(paste("No se pudo calcular el test de Wald para el patrón:", pattern))
    return(NA_character_)
  })
}

# 1.5.2 Método personalizado para que modelsummary extraiga los F-tests
glance_custom.fixest <- function(x, ...) {
  # Obtener los estadísticos por defecto usando el paquete `broom`
  s <- broom::glance(x)
  
  # Definir los patrones para cada grupo de variables.
  f_tests_patterns <- list(
    f_marital   = "^MARST_factor",
    f_age       = "^AGE",
    f_race      = "^RACE_factor",
    f_education = "^EDUC_factor",
    f_industry  = "^ind_",
    f_weeks     = "^WKSWORK2_factor",
    f_puma_vars = "^fit_ln_(potencial_mercado|densidad_empleo)"
  )
  
  f_results <- purrr::map(f_tests_patterns, ~perform_joint_test(x, .x))
  f_tests_df <- dplyr::as_tibble(f_results)
  resultado_final <- dplyr::bind_cols(s, f_tests_df)
  
  return(resultado_final)
}

# --- Bloque 2: Carga de Modelos y Preparación de Tablas ---

# 2.1 Carga de modelos de salarios
m1s <- readRDS(file.path(ruta_modelos, "modelo_1_iv_woodward.rds"))
m2s <- readRDS(file.path(ruta_modelos, "modelo_2_iv_sintetico_w.rds"))
m3s <- readRDS(file.path(ruta_modelos, "modelo_3_iv_sintetico_uw.rds"))
lista_salarios <- list(
  "Modelo 1 (IV-GL Woodward)" = m1s,
  "Modelo 2 (Sintético W)"    = m2s,
  "Modelo 3 (Sintético UW)"   = m3s
)


# 2.2 Uso de modelos de resiliencia (cargados desde el entorno o desde archivo .rds)
if (!exists("lista_modelos_res")) {
  ruta_lista_res <- file.path(ruta_modelos, "lista_modelos_res.rds")
  if (!file.exists(ruta_lista_res)) {
    stop(
      "No se encontró el objeto 'lista_modelos_res' en memoria ni el archivo: ",
      ruta_lista_res,
      ". Ejecute el script principal completo para generarlo y guardarlo, ",
      "o verifique la ruta/nombre del archivo RDS."
    )
  }
  lista_modelos_res <- readRDS(ruta_lista_res)
}
lista_resiliencia <- lista_modelos_res


# 2.3 Parámetros de presentación para modelsummary
coef_rename_salarios <- build_coef_rename_map(lista_salarios)
coef_rename_res      <- build_coef_rename_map(lista_resiliencia)

gof_map_t1_salarios <- list(
  list("raw" = "nobs", "clean" = "Observaciones", "fmt" = 0),
  list("raw" = "r2.within", "clean" = "R2 Within", "fmt" = 4),
  list("raw" = "FE: GEOID_factor", "clean" = "FE: PUMA (GEOID)", "fmt" = "Sí")
)
gof_map_t2_resiliencia <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R2", "fmt" = 4),
  list("raw" = "adj.r.squared", "clean" = "R2 Adjusted", "fmt" = 4),
  list("raw" = "f_marital", "clean" = "F-test Marital Status", "fmt" = "char"),
  list("raw" = "f_age", "clean" = "F-test Age", "fmt" = "char"),
  list("raw" = "f_race", "clean" = "F-test Race", "fmt" = "char"),
  list("raw" = "f_education", "clean" = "F-test Education", "fmt" = "char"),
  list("raw" = "f_industry", "clean" = "F-test Industry", "fmt" = "char"),
  list("raw" = "f_weeks", "clean" = "F-test Weeks Worked", "fmt" = "char"),
  list("raw" = "f_puma_vars", "clean" = "F-test PUMA Market Potential & Employment Density", "fmt" = "char")
)

# --- Bloque 3: Generación de Tablas de Resultados ---

# 3.1 Generación de Tabla 1: Modelo de Salarios
cat("\n--- Tabla 1: Modelo de Salarios ---\n")
tabla1_salarios <- modelsummary(
  lista_salarios, output = "markdown", stars = c("*" = .1, "**" = .05, "***" = .01),
  title = "Tabla 1: Estimación del Modelo de Salarios Pre-Shock (2017-2019)",
  gof_map = gof_map_t1_salarios, coef_rename = coef_rename_salarios, fmt = 5
)
print(tabla1_salarios)

# 3.2 Generación de Tabla 2: Modelo de Resiliencia
cat("\n--- Tabla 2: Modelo de Resiliencia ---\n")
tabla2_resiliencia <- modelsummary(
  lista_resiliencia, output = "markdown", stars = c("*" = .1, "**" = .05, "***" = .01),
  title = "Tabla 2: Determinantes de la Resiliencia Salarial (Periodos post-shock 2022-2023)",
  notes = "Variable dependiente: Rp_wins. Errores estándar clúster a nivel de PUMA.",
  gof_map = gof_map_t2_resiliencia, coef_rename = coef_rename_res, fmt = 5
)
print(tabla2_resiliencia)


# 3.3 Generación de Tabla 3: Descripción de Variables
cat("\n--- Tabla 3: Descripción de Variables ---\n")

# 3.3.1 Identificar variables usadas en los modelos
vars_regresores_raw <- unique(unlist(lapply(c(lista_salarios, lista_resiliencia), function(m) all.vars(formula(m)))))
vars_fe_raw <- unique(unlist(lapply(c(lista_salarios, lista_resiliencia), function(m) all.vars(m$fml_all$fixef))))
vars_base_raw <- unique(c(vars_regresores_raw, vars_fe_raw))
vars_base <- vars_base_raw[!grepl("^Z_", vars_base_raw)]
vars_base <- unique(stringr::str_remove_all(vars_base, "_factor$|_obs$|_wins$"))
vars_base <- gsub("MULTYEAR", "YEAR", vars_base, fixed = TRUE)

# 3.3.2 Construir la tabla de descripción
vars_ipums_presentes <- intersect(
  toupper(c("AGE", "SEX", "MARST", "RACE", "EDUC", "WKSWORK2", "YEAR", "GEOID", "IND1990", "INCWAGE")),
  toupper(vars_base)
)

ipums_filas <- tibble::tibble(codigo = vars_ipums_presentes) %>%
  dplyr::mutate(
    descripcion = purrr::map_chr(codigo, ~{
      tryCatch(
        ipumsr::ipums_var_desc(ddi, var = .x),
        error = function(e) NA_character_
      )
    }),
    nivel = ifelse(codigo == "GEOID", "PUMA (Efectos fijos)", "Individual"),
    descripcion = dplyr::case_when(
      codigo == "GEOID" ~ "Identificador de Área de Micromuestra de Uso Público (PUMA).",
      codigo == "ln_incwage" ~ "Logaritmo natural del ingreso salarial.",
      is.na(descripcion) ~ paste("Descripción no disponible para", codigo),
      TRUE ~ stringr::str_squish(descripcion)
    )
  )

vars_aglomeracion <- c("ln_potencial_mercado", "ln_densidad_empleo")
aglomeracion_filas <- tibble::tibble(
  codigo = vars_aglomeracion[vars_aglomeracion %in% vars_base],
  descripcion = dplyr::case_when(
    codigo == "ln_potencial_mercado" ~ "Log del potencial de mercado (PUMA).",
    codigo == "ln_densidad_empleo"   ~ "Log de la densidad de empleo (PUMA)."
  ),
  nivel = "PUMA"
)

resultado_filas <- tibble::tibble(
  codigo = "Rp",
  descripcion = "Índice de resiliencia salarial individual (Rp_wins), winsorizado.",
  nivel = "Individual (Variable dependiente)"
)

tabla3_variables <- dplyr::bind_rows(resultado_filas, ipums_filas, aglomeracion_filas) %>%
  dplyr::select(`Código Variable` = codigo, `Descripción` = descripcion, `Nivel` = nivel) %>%
  dplyr::distinct() %>%
  dplyr::arrange(match(Nivel, c("Individual (Variable dependiente)", "Individual", "PUMA", "PUMA (Efectos fijos)")), `Código Variable`)

print(knitr::kable(tabla3_variables, format = "pipe"))

# --- Bloque 4: Exportación de Resultados a Archivos Individuales ---
cat("\n--- Exportando cada tabla a un archivo Excel individual ---\n")

# 4.1 Generar versiones de data.frame de las tablas de regresión
tabla1_df <- modelsummary(lista_salarios, output = "data.frame", stars = TRUE,
                          gof_map = gof_map_t1_salarios, coef_rename = coef_rename_salarios, fmt = 5)
tabla2_df <- modelsummary(lista_resiliencia, output = "data.frame", stars = TRUE,
                          gof_map = gof_map_t2_resiliencia, coef_rename = coef_rename_res, fmt = 5)

# 4.2 Definir rutas de salida para cada tabla
ruta_tabla_1 <- file.path(ruta_carpeta_tablas, "Tabla_1_Modelo_Salarios.xlsx")
ruta_tabla_2 <- file.path(ruta_carpeta_tablas, "Tabla_2_Modelo_Resiliencia.xlsx")
ruta_tabla_3 <- file.path(ruta_carpeta_tablas, "Tabla_3_Descripcion_Variables.xlsx")

# 4.3 Exportar Tabla 1
openxlsx::write.xlsx(
  tabla1_df,
  file = ruta_tabla_1,
  asTable = TRUE,
  overwrite = TRUE
)
cat(paste0(" -> Tabla 1 (Salarios) exportada a: '", basename(ruta_tabla_1), "'\n"))

# 4.4 Exportar Tabla 2
openxlsx::write.xlsx(
  tabla2_df,
  file = ruta_tabla_2,
  asTable = TRUE,
  overwrite = TRUE
)
cat(paste0(" -> Tabla 2 (Resiliencia) exportada a: '", basename(ruta_tabla_2), "'\n"))

# 4.5 Exportar Tabla 3
openxlsx::write.xlsx(
  tabla3_variables,
  file = ruta_tabla_3,
  asTable = TRUE,
  overwrite = TRUE
)
cat(paste0(" -> Tabla 3 (Variables) exportada a: '", basename(ruta_tabla_3), "'\n"))

cat(paste0("\n✅ Proceso de exportación completado. Se generaron 3 archivos en la carpeta: '", ruta_carpeta_tablas, "'.\n"))
cat("--- Proceso de generación de tablas finalizado. ---\n")