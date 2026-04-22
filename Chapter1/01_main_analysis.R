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
#   aglomeración, la estimación de un modelo de salarios.
#
# Reproducibilidad: Sí.
# ==============================================================================

# Paso 1: Configuración de entorno y librerías R.

# 1.1: Carga librerías para gestión de datos.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ipumsr, data.table, janitor, sf, fixest, units, here)

# 1.2: Definición de rutas relativas del proyecto.
# Uso de paquetería here para rutas relativas.
ruta_ddi <- here::here("1_Data", "ipums", "usa_00098.xml")
ruta_shp <- here::here("1_Data", "shapefiles", "ArcGis", "IVNDiD.shp")
ruta_j2j <- here::here("1_Data", "excel", "j2j_500e41e2909a5c9752726849a6a0eb9a.csv")

# Validación de existencia de archivos de entrada.
if (!file.exists(ruta_ddi)) stop("Error: Falta archivo DDI.")
if (!file.exists(ruta_shp)) stop("Error: Falta archivo Shapefile.")
if (!file.exists(ruta_j2j)) stop("Error: Falta archivo J2J.")

# 1.3: Importación de datos.

# Carga de metadatos del censo (formato DDI XML).
ddi <- read_ipums_ddi(ruta_ddi)

# Carga de microdatos del censo (formato IPUMS).
data <- read_ipums_micro(ddi)
DT <- as.data.table(data)
rm(data); invisible(gc())

# Carga de geometría espacial y distancias físicas.
states_sf <- sf::st_read(ruta_shp, quiet = TRUE)
states_sf <- sf::st_make_valid(states_sf)
states_sf <- sf::st_transform(states_sf, 4326)

# Carga de matriz de flujos laborales origen-dest.
j2j_raw <- read_csv(ruta_j2j, show_col_types = FALSE)
j2j_raw <- janitor::clean_names(j2j_raw)

# Paso 2: Parametrización y limpieza de variables.

# 2.1: Definición de parámetros y llaves de cruce.

# Definición del año de corte para el análisis.
cut_year <- 2010L

# Filtrado del periodo de análisis de la serie.
YEAR_START <- 2000L
YEAR_END   <- 2024L
DT <- DT[YEAR >= YEAR_START & YEAR <= YEAR_END]

# Verificación de integridad en la serie temporal.
print(DT[, .N, by = YEAR][order(YEAR)])

if (max(DT$YEAR, na.rm = TRUE) < YEAR_END) {
  warning(paste0("Advertencia: El extract IPUMS no llega al año ", YEAR_END, ". Revise el archivo .xml"))
}

# Estandarización de llave geográfica STATEFIP.
DT[, STATEFIP := sprintf("%02d", as.integer(STATEFIP))]

# Estandarización de llave del mapa para el cruce.
fips_col <- grep("STATEFP|FIPS|GEOID", names(states_sf), value = TRUE)[1]

# Estandariza llave mapa.
states_sf$STATEFIP <- sprintf("%02d", as.integer(states_sf[[fips_col]]))

# Validación del vínculo espacial de bases unidas.
common_fips <- intersect(DT$STATEFIP, states_sf$STATEFIP)
if (length(common_fips) == 0) stop("Error: Fallo vinculo STATEFIP.")

# Filtrado de mapa coincidente con datos de flujo.
states_sf <- states_sf[states_sf$STATEFIP %in% common_fips, ]

# 2.2: Selección de variables definitivas modelo.

# a) Variables de gravedad (primera etapa).
vars_grav <- c(
  "flow", "D_i", "D_j",
  "Log_EmpDensity_o", "Log_MarketPotential_o",
  "Share_College_TOTAL_o",
  "AGE_o", "AGE_Sq_o", "TRANTIME_o",
  "Share_HomeOwner_o", "VALUEH_o",
  "Log_EmpDensity_d", "Log_MarketPotential_d",
  "Share_College_TOTAL_d",
  "AGE_d", "AGE_Sq_d", "TRANTIME_d",
  "Share_HomeOwner_d", "VALUEH_d"
)

# b) Variables salarios (segunda etapa).
vars_wage <- c(
  "LWage", "fit_S_it", "D_it",
  "Share_Male_RETAIL", "Share_White_RETAIL",
  "AGE_RETAIL", "AGE_Sq_RETAIL",
  "Share_PreCollege_RETAIL", "Share_FullYear_RETAIL"
)

vars_micro_needed <- c(
  "YEAR", "SERIAL", "STATEFIP", "PERNUM", 
  "HHWT", "PERWT", "CBSERIAL", "SAMPLE",   # Identificadores y Trazabilidad
  "CLUSTER", "STRATA",                     # Diseño muestral
  "INCWAGE_CPIU_2010", "INCEARN_CPIU_2010", "HHINCOME_CPIU_2010", 
  "EDUC", "AGE", "TRANTIME", "SEX", "EMPSTAT",
  "OWNERSHP", "VALUEH", "CPI99", "RACE", "MARST", "WKSWORK2", 
  "OCC2010",                               # Variable ocupacional armonizada
  "IND1990"                                # Industria (base 1990) para filtro parametrizable
)

DT <- DT[, ..vars_micro_needed]
invisible(gc())


# 2.3: Configuración de muestras y datos atípicos.

# 2.3.1: Depuración de datos.

# a) Conversión a numérico y limpieza de missings.
cols_num <- c("INCWAGE_CPIU_2010", "INCEARN_CPIU_2010", "HHINCOME_CPIU_2010", "VALUEH", "TRANTIME", "CPI99")
DT[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]

# INCWAGE (6 dígitos): 999999 (N/A), 999998 (Missing).
DT[INCWAGE_CPIU_2010 >= 999998, INCWAGE_CPIU_2010 := NA]

# INCEARN: Umbral seguro de 7 dígitos por consistencia.
DT[INCEARN_CPIU_2010 >= 9999998, INCEARN_CPIU_2010 := NA]

# HHINCOME (7 dígitos PDF): 9999999 (N/A Value).
DT[HHINCOME_CPIU_2010 >= 9999998, HHINCOME_CPIU_2010 := NA]

# VALUEH (7 dígitos): 9999999 (N/A), 9999998 (Missing).
DT[VALUEH >= 9999998, VALUEH := NA]

# Deflactación VALUEH: Nominal -> USD 1999 -> USD 2010.
# Fuente IPUMS: 1999 dollars = Nominal * CPI99 (donde CPI99 tiene 3 decimales).
# Fuente Tabla 2 IPUMS: Factor de conversión 1999 -> 2010 es 1.309.
factor_1999_2010 <- 1.309

# Aplicación del ajuste: (Nominal * CPI99_real) * Factor_2010
DT[!is.na(VALUEH) & !is.na(CPI99), VALUEH := (VALUEH * (CPI99 / 1000)) * factor_1999_2010]

# TRANTIME: 000 (N/A), 888 (Suppressed).
DT[TRANTIME %in% c(0, 888), TRANTIME := NA]

# b) Ajuste de factores de peso (escala centesimal).
DT[, HHWT := HHWT / 100]
DT[, PERWT := PERWT / 100]
DT <- DT[HHWT > 0 & PERWT > 0]


# Validación visual de máximos tras limpieza de datos.
print(DT[, .(
  max_incwage = if (all(is.na(INCWAGE_CPIU_2010))) NA_real_ else max(INCWAGE_CPIU_2010, na.rm = TRUE),
  max_hhinc   = if (all(is.na(HHINCOME_CPIU_2010))) NA_real_ else max(HHINCOME_CPIU_2010, na.rm = TRUE),
  max_incearn = if (all(is.na(INCEARN_CPIU_2010))) NA_real_ else max(INCEARN_CPIU_2010, na.rm = TRUE)
)])

# Corrección de variable EDUC (99 es Missing Val).
DT[EDUC == 99, EDUC := NA]

# 2.3.2: Generación de banderas binarias control.
DT[, Male_bin := as.integer(SEX == 1)] # Solo para control Retail
DT[, White_bin := as.integer(RACE == 1)]
DT[, Married_bin := as.integer(MARST == 1)]
DT[, HomeOwner_bin := as.integer(OWNERSHP == 1)]
DT[, FullYear_bin := as.integer(WKSWORK2 == 6)]

# Definición de dummies educativas.
DT[, HS_Diploma_bin := as.integer(EDUC >= 6)]
DT[, PreCollege_bin := as.integer(EDUC <= 6)]
DT[, College_bin := as.integer(EDUC >= 7)]

# 2.3.3: Definición muestra Retail.
# Código 4760: Retail Salespersons
DT[, IS_RETAIL := (EMPSTAT == 1 & OCC2010 == 4760)]

# 2.4: Construcción variables de aglomeración.

# 2.4.1: Métricas espaciales base (Proyecciones).
states_sf <- sf::st_transform(states_sf, 9311) 
states_sf$area_km2 <- as.numeric(sf::st_area(states_sf)) / 1e6
states_sf$centroid <- sf::st_centroid(states_sf)

# 2.4.2: Cálculo matriz de distancias bilaterales.
states_sf <- sf::st_transform(states_sf, 4326)
states_sf$centroid <- sf::st_transform(states_sf$centroid, 4326)
dist_mat_km <- as.matrix(units::set_units(sf::st_distance(states_sf$centroid), "km"))
rownames(dist_mat_km) <- states_sf$STATEFIP
colnames(dist_mat_km) <- states_sf$STATEFIP

# 2.4.3: Cálculo de distancia interna (Radio).
internal_dists <- (2/3) * sqrt(states_sf$area_km2 / pi)
names(internal_dists) <- states_sf$STATEFIP

# 2.4.4: Función Potencial de Mercado.
calc_mp <- function(mass, ids, d_mat) {
  idx <- match(ids, rownames(d_mat))
  d_sub <- d_mat[idx, idx]
  w <- 1 / d_sub
  diag(w) <- 1 / internal_dists[ids] 
  w[!is.finite(w)] <- 0
  return(as.numeric(w %*% mass))
}

# 2.4.5: Generación de variables definitivas.
agg_agglom <- DT[, .(
  Mass_Income = sum(pmax(INCEARN_CPIU_2010, 0) * PERWT, na.rm = TRUE),
  Emp_Count   = sum(PERWT * (EMPSTAT == 1), na.rm = TRUE)
), by = .(STATEFIP, YEAR)]

# Fusión con datos de área para cálculo densidad.
area_dt <- data.table(STATEFIP = states_sf$STATEFIP, area_km2 = states_sf$area_km2)
agg_agglom <- merge(agg_agglom, area_dt, by = "STATEFIP", all.x = TRUE)

# Cálculo de Densidad de Empleo (Emp_Density).
agg_agglom[, Emp_Density := Emp_Count / area_km2]
agg_agglom[, Log_EmpDensity := log(Emp_Density + 1)] 

# Cálculo de Potencial de Mercado.
years_vec <- unique(agg_agglom$YEAR)
mp_list <- list()
for(y in years_vec) {
  sub_dt <- agg_agglom[YEAR == y]
  if(nrow(sub_dt) > 0){
    mp_val <- calc_mp(sub_dt$Mass_Income, sub_dt$STATEFIP, dist_mat_km)
    mp_list[[as.character(y)]] <- data.table(STATEFIP = sub_dt$STATEFIP, YEAR = y, MarketPotential = mp_val)
  }
}
agg_agglom <- merge(agg_agglom, rbindlist(mp_list), by = c("STATEFIP", "YEAR"))
agg_agglom[, Log_MarketPotential := log(MarketPotential + 1)]

# Limpieza de memoria.
rm(area_dt, mp_list, years_vec)
invisible(gc())

# 2.5: Construcción de base de datos final.

# 2.5.1: Agregación de variables a nivel individual.
agg_total_p <- DT[, .(
  Avg_IncWage_TOTAL = weighted.mean(as.numeric(INCWAGE_CPIU_2010), PERWT, na.rm = TRUE),
  Share_College_TOTAL = weighted.mean(as.numeric(College_bin), PERWT, na.rm = TRUE),
  AGE = weighted.mean(as.numeric(AGE), PERWT, na.rm = TRUE),
  TRANTIME = weighted.mean(as.numeric(TRANTIME), PERWT, na.rm = TRUE)
), by = .(STATEFIP, YEAR)]

# 2.5.2: Agregación de variables a nivel de hogar.
# Filtra cabeza de familia y asegura tipos numéricos puros.
agg_total_hh <- DT[PERNUM == 1, .(
  Share_HomeOwner = weighted.mean(as.numeric(HomeOwner_bin), HHWT, na.rm = TRUE),
  VALUEH = weighted.mean(as.numeric(VALUEH), HHWT, na.rm = TRUE)
), by = .(STATEFIP, YEAR)]

# 2.5.3: Agregación de variables sector retail.
# Filtra muestra retail y asegura tipos de datos.
ind1990_objetivo <- c(
  580L, 581L, 582L, 601L, 610L, 611L, 623L,
  630L, 640L, 650L, 652L,
  660L, 661L, 662L, 681L
)  # EDITA AQUÍ (ej.: 650L o c(650L, 651L))

agg_retail <- DT[IS_RETAIL == TRUE & IND1990 %in% ind1990_objetivo, .(
  Avg_IncWage_RETAIL = weighted.mean(as.numeric(INCWAGE_CPIU_2010), PERWT, na.rm = TRUE),
  Share_Male_RETAIL = weighted.mean(as.numeric(Male_bin), PERWT, na.rm = TRUE),
  Share_White_RETAIL = weighted.mean(as.numeric(White_bin), PERWT, na.rm = TRUE),
  AGE_RETAIL = weighted.mean(as.numeric(AGE), PERWT, na.rm = TRUE),
  AGE_Sq_RETAIL = weighted.mean(as.numeric(AGE^2), PERWT, na.rm = TRUE),
  Share_PreCollege_RETAIL = weighted.mean(as.numeric(PreCollege_bin), PERWT, na.rm = TRUE),
  Share_FullYear_RETAIL = weighted.mean(as.numeric(FullYear_bin), PERWT, na.rm = TRUE)
), by = .(STATEFIP, YEAR)]



# 2.5.4: Consolidación del panel de datos final.
# Une tablas agregadas a la base de aglomeración.
Base_Datos_Final <- agg_agglom |>
  merge(agg_total_p, by = c("STATEFIP", "YEAR"), all.x = TRUE) |>
  merge(agg_total_hh, by = c("STATEFIP", "YEAR"), all.x = TRUE) |>
  merge(agg_retail, by = c("STATEFIP", "YEAR"), all.x = TRUE)

# 2.5.5: Genera rezagos (t-2) controles gravedad.
setorder(Base_Datos_Final, STATEFIP, YEAR)
cols_grav_lag <- c("Log_EmpDensity", "Log_MarketPotential", "Share_HomeOwner", "VALUEH", "Share_College_TOTAL")

# Asegura que el rezago sea cronológico por estado.
Base_Datos_Final[, (paste0(cols_grav_lag, "_lag2")) := shift(.SD, 2L, type = "lag"), 
                 .SDcols = cols_grav_lag, by = STATEFIP]

# Limpia filas donde el rezago no es posible.
Base_Datos_Final <- Base_Datos_Final[!is.na(Log_EmpDensity_lag2)]

# Limpieza de objetos intermedios de memoria.
rm(agg_total_p, agg_total_hh, agg_retail, agg_agglom)
invisible(gc())

# Paso 3: Modelo de gravedad (PPML) - Etapa 1.

# 3.1: Configuración de insumos modelo gravedad.
# 3.1.1: Define variables control (t-2 exógenas).
vars_grav_base <- c(
  "Log_EmpDensity_lag2", "Log_MarketPotential_lag2",
  "Share_HomeOwner_lag2", "VALUEH_lag2",
  "Share_College_TOTAL_lag2", "TRANTIME"
)

# Subset de controles
ctrl_subset <- Base_Datos_Final[, c("STATEFIP", "YEAR", vars_grav_base), with = FALSE]

# 3.2: Construcción de matriz de flujos balanceada.

# Definición de tratamiento (D_i, D_j) por zona.
treat_key <- data.table(STATEFIP = states_sf$STATEFIP, AMZ = as.integer(as.character(states_sf$AMZ)))

# Preparación de datos J2J (Flujos laborales).
j2j_od <- j2j_raw %>%
  dplyr::filter(periodicity == "Q", geography_orig != geography) %>%
  dplyr::mutate(
    fips_i = sprintf("%02d", as.integer(stringr::str_extract(geography_orig, "\\d+"))),
    fips_j = sprintf("%02d", as.integer(stringr::str_extract(geography, "\\d+"))),
    year = as.integer(year)
  ) %>%
  dplyr::group_by(fips_i, fips_j, year) %>%
  dplyr::summarise(flow = sum(j2j, na.rm = TRUE), .groups = "drop")

# Crear panel rectangular (fips_i * fips_j * año).
years_model <- intersect(ctrl_subset$YEAR, unique(j2j_od$year))
fips_univ   <- unique(ctrl_subset$STATEFIP)

j2j_ppml <- tidyr::expand_grid(fips_i = fips_univ, fips_j = fips_univ, year = years_model) %>%
  dplyr::filter(fips_i != fips_j) %>%
  dplyr::left_join(j2j_od, by = c("fips_i", "fips_j", "year")) %>%
  tidyr::replace_na(list(flow = 0))

# Unión de variables de Tratamiento y Controles.
j2j_ppml <- j2j_ppml %>%
  dplyr::left_join(treat_key, by = c("fips_i" = "STATEFIP")) %>% dplyr::rename(AMZ_i = AMZ) %>%
  dplyr::left_join(treat_key, by = c("fips_j" = "STATEFIP")) %>% dplyr::rename(AMZ_j = AMZ) %>%
  dplyr::mutate(
    post = dplyr::if_else(year > cut_year, 1L, 0L),
    D_i  = tidyr::replace_na(AMZ_i * post, 0),
    D_j  = tidyr::replace_na(AMZ_j * post, 0),
    D_jt = tidyr::replace_na(AMZ_j * post, 0) 
  ) %>%
  dplyr::left_join(dplyr::rename_with(ctrl_subset, ~paste0(., "_o"), -c(STATEFIP, YEAR)), by = c("fips_i"="STATEFIP", "year"="YEAR")) %>%
  dplyr::left_join(dplyr::rename_with(ctrl_subset, ~paste0(., "_d"), -c(STATEFIP, YEAR)), by = c("fips_j"="STATEFIP", "year"="YEAR"))

# 3.2.5: Filtra observaciones missing en controles.
vars_to_check <- c(paste0(vars_grav_base, "_o"), paste0(vars_grav_base, "_d"))
n_before <- nrow(j2j_ppml)
j2j_ppml <- j2j_ppml %>% tidyr::drop_na(dplyr::all_of(vars_to_check))
n_after <- nrow(j2j_ppml)

if ((n_before - n_after) > 0) {
  message(paste("Advertencia: Se eliminaron", n_before - n_after, "observaciones por NAs en controles."))
}

# 3.3: Estima modelo Poisson de pseudo máxima verosimilitud.
fml_ppml <- stats::as.formula(paste("flow ~ D_i + D_j +", paste(vars_to_check, collapse = " + "), "| fips_i^fips_j + year"))

message("Estimando Modelo Gravedad (PPML)...")
model_exog <- fixest::fepois(fml_ppml, data = j2j_ppml, 
                             vcov = ~ fips_i^year + fips_j^year, 
                             combine.quick = FALSE)

# 3.4: Construcción de instrumentos de etapa uno.
j2j_ppml$flow_hat_exog <- stats::predict(model_exog, newdata = j2j_ppml, type = "response")

exposure_metrics <- j2j_ppml %>%
  dplyr::group_by(fips_i, year) %>%
  dplyr::mutate(
    sum_flow = sum(flow, na.rm = TRUE),
    sum_hat  = sum(flow_hat_exog, na.rm = TRUE),
    w_obs    = dplyr::if_else(sum_flow > 0, flow / sum_flow, 0),
    w_hat    = dplyr::if_else(sum_hat > 0, flow_hat_exog / sum_hat, 0)
  ) %>%
  dplyr::summarise(
    S_it = sum(w_obs * D_jt, na.rm = TRUE),
    Z_it = sum(w_hat * D_jt, na.rm = TRUE),
    .groups = "drop"
  )

# 3.5: Diagnóstico y estadísticos del modelo.
message("--- Resumen Modelo Gravedad (PPML) ---")
fixest::etable(model_exog,
               signif.code = NA, 
               fitstat = c("n", "pr2", "aic", "bic", "ll"),
               headers = "Modelo Gravedad (PPML)")

# 3.5.1: Preserva datos base para bootstrap por clúster.
j2j_ppml_base <- data.table::copy(as.data.table(j2j_ppml))
data.table::setkey(j2j_ppml_base, fips_i)

# 3.5.2: Elimina objetos pesados modelo gravedad.  
rm(j2j_ppml, model_exog, j2j_od, ctrl_subset, treat_key)
invisible(gc())

# Paso 4: Segunda etapa construcción panel final.  

# 4.1: Reconstruye indicador tratamiento estatal. 
treat_key <- data.table(fips = states_sf$STATEFIP, AMZ = as.integer(as.character(states_sf$AMZ)))


# 4.2: Normaliza identificador temporal en panel. 
Base_Datos_Final[, year := as.integer(YEAR)]

# 4.3: Integra exposición al panel.
Panel_Final <- Base_Datos_Final %>%
  dplyr::left_join(exposure_metrics, by = c("STATEFIP" = "fips_i", "year" = "year"))

# 4.4: Incorporación variable tratamiento directo.
Panel_Final <- Panel_Final %>%
  dplyr::left_join(treat_key, by = c("STATEFIP" = "fips"))

# 4.5: Genera variables interacción tratamiento.  
Panel_Final <- Panel_Final %>%
  dplyr::mutate(
    post = as.integer(year > cut_year),
    D_it = as.integer(AMZ * post)
  )

# 4.6: Calcula logaritmo salario sector retail.   
Panel_Final <- Panel_Final %>%
  dplyr::mutate(LWage = log(Avg_IncWage_RETAIL + 1))

# 4.7: Retiene solo observaciones valores finitos. 
Panel_Final <- Panel_Final %>%
  dplyr::filter(is.finite(LWage), is.finite(S_it), is.finite(Z_it))

# 4.8: Libera objetos temporales segunda etapa.   
rm(exposure_metrics, treat_key, Base_Datos_Final)

# 4.9: Crea plantilla panel para bootstrap inferencia.
Panel_Template <- as.data.table(Panel_Final)
Panel_Template[, c("S_it", "Z_it") := NULL]
data.table::setkey(Panel_Template, STATEFIP)

# 4.10: Almacena panel completo en disco RDS.     
# Configuración del esquema global de carpetas de salida.
# Se crean automáticamente si no existen (recursive = TRUE).
dir.create(here::here("3_Outputs", "1_Data_Processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("3_Outputs", "2_Models"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("3_Outputs", "3_Figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("3_Outputs", "4_Tables"), showWarnings = FALSE, recursive = TRUE)

saveRDS(as.data.table(Panel_Final), here::here("3_Outputs", "1_Data_Processed", "Panel_Final.rds"))
saveRDS(Panel_Template, here::here("3_Outputs", "1_Data_Processed", "Panel_Template.rds"))


# Paso 5: Tercera etapa modelo salarios ENDID.    

# 5.1: Definición de controles del modelo.
ctrls_iv <- c("Share_Male_RETAIL", "Share_White_RETAIL", "AGE_RETAIL", 
              "AGE_Sq_RETAIL", "Share_PreCollege_RETAIL", "Share_FullYear_RETAIL")

# 5.2: Definición de la fórmula del modelo (IV).
fml_iv <- stats::as.formula(paste("LWage ~ D_it +", 
                                  paste(ctrls_iv, collapse = " + "), 
                                  "| STATEFIP + YEAR | S_it ~ Z_it"))

# 5.3: Estima regresión variables instrumentales. 
m_endid <- fixest::feols(fml_iv, data = Panel_Final, vcov = ~ STATEFIP + YEAR)
message("--- Resumen Modelo Salarios (ENDID) ---")
fixest::etable(m_endid,
               signif.code = NA, # NA es el comando correcto para borrar estrellas
               fitstat = c("n", "r2", "wr2", "ivf"),
               headers = "Modelo Salarios (ENDID)")

# 5.4: Calcula estadístico F primera etapa IV.    
f_stage1 <- fixest::fitstat(m_endid, "ivf")

message("--- Test F del modelo (ENDID) ---")
print(fixest::fitstat(m_endid, "f"))

message("--- Test F de 1ra etapa (ENDID) ---")
print(fixest::fitstat(m_endid, "ivf"))

# 5.5: Ejecuta test endogeneidad control function.
message("--- Test Hausman de endogeneidad (Control Function) ---")
m_fs_hausman <- fixest::feols(
  stats::as.formula(paste("S_it ~ Z_it + D_it +", paste(ctrls_iv, collapse = " + "), "| STATEFIP + YEAR")),
  data = Panel_Final,
  vcov = ~ STATEFIP + YEAR
)

Panel_Final$fs_resid_hausman <- stats::residuals(m_fs_hausman)

m_cf_hausman <- fixest::feols(
  stats::as.formula(paste("LWage ~ D_it + S_it +", paste(ctrls_iv, collapse = " + "),
                          "+ fs_resid_hausman | STATEFIP + YEAR")),
  data = Panel_Final,
  vcov = ~ STATEFIP + YEAR
)

ct_haus <- fixest::coeftable(m_cf_hausman)
haus_row <- ct_haus["fs_resid_hausman", , drop = FALSE]
print(data.frame(
  Term = "fs_resid_hausman",
  Estimate = as.numeric(haus_row[1, "Estimate"]),
  Std_Error = as.numeric(haus_row[1, "Std. Error"]),
  t_value = as.numeric(haus_row[1, "t value"]),
  p_value = as.numeric(haus_row[1, "Pr(>|t|)"])
))

Panel_Final$fs_resid_hausman <- NULL
rm(m_fs_hausman, m_cf_hausman, ct_haus, haus_row)


# 5.6: Implementa inferencia bootstrap tres etapas.
set.seed(123)
B_reps <- 300L
estados_univ <- intersect(unique(Panel_Template$STATEFIP), unique(j2j_ppml_base$fips_i))

boot_results <- matrix(NA_real_, nrow = B_reps, ncol = length(coef(m_endid)))
colnames(boot_results) <- names(coef(m_endid))

n_success <- 0L

message("Iniciando Bootstrap dinámico (Etapa 1 + 2 + 3)...")

for (b in 1:B_reps) {
  # 1. Resampling de estados (con reemplazo)
  draw_fips <- sample(estados_univ, length(estados_univ), replace = TRUE)
  
  # Pesos por estado = frecuencia en el draw (W)
  w_dt <- as.data.table(table(draw_fips))
  data.table::setnames(w_dt, c("STATEFIP", "w_origin"))
  w_dt[, w_origin := as.numeric(w_origin)]
  
  # 2. Etapa 1: Flujos para estados remuestreados.
  j2j_b <- merge(j2j_ppml_base, w_dt, by.x = "fips_i", by.y = "STATEFIP", all = FALSE)
  
  # 3. Etapa 3: Panel para estados remuestreados.
  df_iv_b <- merge(Panel_Template, w_dt, by = "STATEFIP", all = FALSE)
  
  ok_b <- tryCatch({
    # A) Gravedad (PPML).
    m_grav_b <- fixest::fepois(
      fml_ppml,
      data = j2j_b,
      weights = ~ w_origin,
      vcov = "iid",
      warn = FALSE,
      notes = FALSE,
      combine.quick = FALSE
    )
    
    # B) Predicción y construcción de S_it / Z_it.
    j2j_b[, flow_hat_b := stats::predict(m_grav_b, newdata = j2j_b, type = "response")]
    
    metrics_b <- j2j_b[is.finite(flow_hat_b), .(
      w_obs_b = if (sum(flow, na.rm = TRUE) > 0) flow / sum(flow, na.rm = TRUE) else 0,
      w_hat_b = if (sum(flow_hat_b, na.rm = TRUE) > 0) flow_hat_b / sum(flow_hat_b, na.rm = TRUE) else 0,
      D_jt_b  = D_jt
    ), by = .(fips_i, year)][, .(
      S_it = sum(w_obs_b * D_jt_b, na.rm = TRUE),
      Z_it = sum(w_hat_b * D_jt_b, na.rm = TRUE)
    ), by = .(fips_i, year)]
    
    # C) Merge y limpieza antes de estimar IV.
    df_iv_b <- merge(df_iv_b, metrics_b,
                     by.x = c("STATEFIP", "year"),
                     by.y = c("fips_i", "year"),
                     all.x = FALSE)
    
    df_iv_b <- df_iv_b[is.finite(LWage) & is.finite(D_it) & is.finite(S_it) & is.finite(Z_it)]
    
    if (nrow(df_iv_b) < 50) return(FALSE)
    
    m_iv_b <- fixest::feols(
      fml_iv,
      data = df_iv_b,
      weights = ~ w_origin,
      warn = FALSE,
      notes = FALSE
    )
    
    coef_b <- coef(m_iv_b)
    nm_b <- intersect(names(coef_b), colnames(boot_results))
    boot_results[b, nm_b] <- coef_b[nm_b]
    
    TRUE
  }, error = function(e) {
    if (b == 1) message(paste("Error en iteración 1:", e$message))
    FALSE
  })
  
  if (isTRUE(ok_b)) n_success <- n_success + 1L
  
  if (b %% 50 == 0) message(paste("Iteración:", b, "| Exitosas:", n_success))
}

if (n_success == 0L) stop("Error Crítico: El bootstrap falló en todas las iteraciones. Verifique mensaje de Error iteración 1 arriba.")


# 5.7: Reporta coeficientes con errores bootstrap.
se_boot_endid <- apply(boot_results, 2, sd, na.rm = TRUE)
t_stats_boot  <- coef(m_endid) / se_boot_endid
p_vals_boot   <- 2 * (1 - pnorm(abs(t_stats_boot)))

res_table <- data.frame(
  Term = names(coef(m_endid)),
  Estimate = coef(m_endid),
  Std_Error_Boot = se_boot_endid,
  t_stat = t_stats_boot,
  p_val = p_vals_boot
)
print("--- Resultados Finales ENDID (Inferencia Bootstrap Correcta) ---")
print(res_table)


# 5.8: Ejecución de prueba placebo de tiempo.
df_temp <- Panel_Final %>%
  mutate(
    post_placebo = as.integer(year > 2006),
    D_it = as.integer(AMZ * post_placebo)
  )

m_time <- fixest::feols(fml_iv, data = df_temp)


# 5.9: Ejecución de prueba placebo de red.
df_rand <- Panel_Final %>% mutate(S_it = sample(S_it))
m_net <- fixest::feols(fml_iv, data = df_rand)

# Paso 6: Verificación de tendencias paralelas.
message("--- Ejecutando Test PTA ---")

# 6.1: Definición del año de corte del evento.
if (!exists("cut_year")) cut_year <- 2010L

# 6.2: Recupera controles salarios si ausentes.
if (!exists("ctrls_iv")) {
  ctrls_iv <- c("Share_Male_RETAIL", "Share_White_RETAIL", "AGE_RETAIL",
                "AGE_Sq_RETAIL", "Share_PreCollege_RETAIL", "Share_FullYear_RETAIL")
}

# 6.3: Define rutas archivo panel almacenado RDS.
ruta_panel_final_rds <- here::here("3_Outputs", "1_Data_Processed", "Panel_Final.rds")
ruta_panel_template_rds <- here::here("3_Outputs", "1_Data_Processed", "Panel_Template.rds")

# 6.4: Carga panel desde memoria o disco disponible.
if (exists("Panel_Final")) {
  panel_for_pta <- as.data.table(Panel_Final)
} else if (exists("Panel_Template")) {
  panel_for_pta <- as.data.table(Panel_Template)
} else if (file.exists(ruta_panel_final_rds)) {
  panel_for_pta <- as.data.table(readRDS(ruta_panel_final_rds))
} else if (file.exists(ruta_panel_template_rds)) {
  panel_for_pta <- as.data.table(readRDS(ruta_panel_template_rds))
} else {
  stop("Error: Falta panel PTA. Ejecute hasta 4.9.")
}

# 6.5: Normalización de variables temporales.
if (!"YEAR" %in% names(panel_for_pta) && "year" %in% names(panel_for_pta)) panel_for_pta[, YEAR := as.integer(year)]
if (!"year" %in% names(panel_for_pta) && "YEAR" %in% names(panel_for_pta)) panel_for_pta[, year := as.integer(YEAR)]
panel_for_pta[, YEAR := as.integer(YEAR)]
panel_for_pta[, year := as.integer(year)]

# 6.6: Validación de existencia de variables.
vars_pta_need <- c("LWage", "AMZ", "STATEFIP", "YEAR", "year", ctrls_iv)
vars_pta_need <- unique(vars_pta_need)
miss_pta <- setdiff(vars_pta_need, names(panel_for_pta))
if (length(miss_pta) > 0L) stop(paste0("Error: Faltan PTA: ", paste(miss_pta, collapse = ", ")))

# 6.7: Filtrado de periodo pre-tratamiento.
df_pta <- panel_for_pta[year <= cut_year]
if (nrow(df_pta) == 0L) stop("Error: df_pta vacío.")

# 6.8: Creación de variable de tiempo de evento.
df_pta[, event_time := as.integer(year - (cut_year + 1L))]

# 6.9: Estimación del modelo de estudio de eventos.
fml_pta <- stats::as.formula(paste("LWage ~ i(event_time, AMZ, ref = -1) +",
                                   paste(ctrls_iv, collapse = " + "),
                                   "| STATEFIP + YEAR"))
m_pta <- fixest::feols(fml_pta, data = df_pta, vcov = ~STATEFIP + YEAR)

message("--- Wald PTA ---")
print(fixest::wald(m_pta, keep = "event_time"))

# 6.10: Genera gráfico coeficientes pre-tendencias.
fixest::iplot(m_pta, main = "ENDID: Pre-trends check")

# Paso 7: Síntesis comparativa modelos finales.

# 7.1: Carga panel principal si no está en memoria.
if (!exists("Panel_Final")) {
  ruta_panel_final_rds <- here::here("3_Outputs", "1_Data_Processed", "Panel_Final.rds")
  if (!file.exists(ruta_panel_final_rds)) stop("Error: Falta Panel_Final.rds en 3_Outputs/1_Data_Processed.")
  Panel_Final <- as.data.table(readRDS(ruta_panel_final_rds))
}

# 7.2: Recupera lista controles si no disponible.
if (!exists("ctrls_iv")) {
  ctrls_iv <- c("Share_Male_RETAIL", "Share_White_RETAIL", "AGE_RETAIL",
                "AGE_Sq_RETAIL", "Share_PreCollege_RETAIL", "Share_FullYear_RETAIL")
}

# 7.3: Estimación del modelo ENDID (Instrumental).
if (!exists("fml_iv")) {
  fml_iv <- stats::as.formula(paste("LWage ~ D_it +",
                                    paste(ctrls_iv, collapse = " + "),
                                    "| STATEFIP + YEAR | S_it ~ Z_it"))
}
if (!exists("m_endid")) {
  m_endid <- fixest::feols(fml_iv, data = Panel_Final, vcov = ~ STATEFIP + YEAR)
}

# 7.4: Estimación del modelo NDID (Network DID).
fml_ndid <- stats::as.formula(paste("LWage ~ D_it + S_it +",
                                    paste(ctrls_iv, collapse = " + "),
                                    "| STATEFIP + YEAR"))
m_ndid <- fixest::feols(fml_ndid, data = Panel_Final, vcov = ~ STATEFIP + YEAR)

# 7.5: Estimación del modelo DID (Estándar).
fml_did <- stats::as.formula(paste("LWage ~ D_it +",
                                   paste(ctrls_iv, collapse = " + "),
                                   "| STATEFIP + YEAR"))
m_did <- fixest::feols(fml_did, data = Panel_Final, vcov = ~ STATEFIP + YEAR)

# 7.6: Tabla síntesis (ENDID vs NDID vs DID).
message("--- Síntesis Modelos (Tabla Comparativa) ---")
fixest::etable(m_endid, m_ndid, m_did,
               signif.code = NA, 
               fitstat = c("n", "r2", "wr2", "ivf"), 
               headers = c("ENDID (IV)", "NDID", "DID"),
               keep = c("D_it", "fit_S_it", "S_it"),
               digits = 4)

# 7.7: Persistencia de resultados en disco.
dir.create(here::here("3_Outputs", "2_Models"), showWarnings = FALSE, recursive = TRUE)
saveRDS(list(m_endid = m_endid, m_ndid = m_ndid, m_did = m_did),
        here::here("3_Outputs", "2_Models", "modelos_salario.rds"))