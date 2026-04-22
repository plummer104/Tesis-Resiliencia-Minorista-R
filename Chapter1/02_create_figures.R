# Resumen flujos interestatales minoristas totales.
# Trimestres completos del año analizado.
# Promedio anual 2000-2024 estimado globalmente.

# Carga paquetes requeridos para análisis.
suppressPackageStartupMessages({
  library(dplyr)
  library(janitor)
  library(tidyr)
  library(sf)
  library(stringr)
  library(data.table)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(here)
  library(spdep)
})

# Define rutas internas de salida.
out_dir <- here::here("3_Outputs", "3_Figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Carga objetos desde disco disponible.
ruta_panel <- here::here("3_Outputs", "1_Data_Processed", "Panel_Final.rds")
if (!exists("Panel_Final") && file.exists(ruta_panel)) Panel_Final <- readRDS(ruta_panel)

ruta_modelos <- here::here("3_Outputs", "2_Models", "modelos_salario.rds")
if (!exists("m_endid") && file.exists(ruta_modelos)) {
  tmp_models <- readRDS(ruta_modelos)
  if (is.list(tmp_models) && "m_endid" %in% names(tmp_models)) m_endid <- tmp_models$m_endid
  rm(tmp_models)
}

# Valida dependencias mínimas del entorno.
if (!exists("cut_year")) cut_year <- 2010L

if (!exists("states_sf")) stop("Error: Falta objeto states_sf (shapefile en memoria).")
stopifnot(inherits(states_sf, "sf"))

fips_col <- grep("STATEFP|STATEFIP|FIPS|GEOID", names(states_sf), value = TRUE)[1]
stopifnot(!is.na(fips_col))

states_sf$STATEFIP <- sprintf("%02d", as.integer(states_sf[[fips_col]]))

stopifnot(exists("j2j_raw"), exists("Panel_Final"), exists("m_endid"), exists("cut_year"))

# 1) Verifica objeto j2j_raw existente
j2j <- janitor::clean_names(tibble::as_tibble(j2j_raw))

# 2) Valida columnas básicas requeridas
req_cols <- c("periodicity", "geography_orig", "geography", "year", "quarter")
stopifnot(all(req_cols %in% names(j2j)))
flow_col <- intersect(c("j2j", "flow"), names(j2j))[1]
stopifnot(!is.na(flow_col))

# 3) Normaliza llaves del flujo
j2j <- j2j %>%
  mutate(flow = .data[[flow_col]]) %>%
  mutate(
    fips_i = sprintf("%02d", as.integer(stringr::str_extract(geography_orig, "\\d+"))),
    fips_j = sprintf("%02d", as.integer(stringr::str_extract(geography, "\\d+"))),
    year = as.integer(year),
    quarter = as.integer(quarter)
  )

# 4) Filtra periodo y geografía
j2j <- j2j %>%
  filter(
    periodicity == "Q",
    year >= 2000, year <= 2024,
    quarter %in% 1:4,
    !is.na(fips_i), !is.na(fips_j),
    fips_i != fips_j,
    fips_i != "00", fips_j != "00"
  )
stopifnot(nrow(j2j) > 0)

# 5) Mapea nombres estatales consistentes
if (exists("states_sf")) {
  sf_att <- states_sf %>%
    st_drop_geometry() %>%
    mutate(STATEFIP = sprintf("%02d", as.integer(STATEFIP)))
  valid_fips <- unique(sf_att$STATEFIP)
  j2j <- j2j %>% filter(fips_i %in% valid_fips, fips_j %in% valid_fips)
  stopifnot(nrow(j2j) > 0)
  cand <- c("STUSPS", "STATE_ABBR", "ABBR", "NAME", "STATE_NAME", "STATE", "state", "name")
  cand <- cand[cand %in% names(sf_att)]
  if (length(cand) > 0) {
    name_col <- cand[1]
    map_states <- sf_att %>% distinct(STATEFIP, state_name = .data[[name_col]])
  } else {
    map_states <- tibble::tibble(STATEFIP = valid_fips, state_name = valid_fips)
  }
} else {
  univ_fips <- sort(unique(c(j2j$fips_i, j2j$fips_j)))
  map_states <- tibble::tibble(STATEFIP = univ_fips, state_name = univ_fips)
}

# 6) Agrega promedios anuales estatales
out_avg <- j2j %>%
  group_by(year, fips_i) %>%
  summarise(outflow = sum(flow, na.rm = TRUE), .groups = "drop") %>%
  group_by(fips_i) %>%
  summarise(
    avg_outflow = mean(outflow, na.rm = TRUE),
    years = n_distinct(year),
    .groups = "drop"
  ) %>%
  rename(STATEFIP = fips_i) %>%
  left_join(map_states, by = "STATEFIP")

in_avg <- j2j %>%
  group_by(year, fips_j) %>%
  summarise(inflow = sum(flow, na.rm = TRUE), .groups = "drop") %>%
  group_by(fips_j) %>%
  summarise(
    avg_inflow = mean(inflow, na.rm = TRUE),
    years = n_distinct(year),
    .groups = "drop"
  ) %>%
  rename(STATEFIP = fips_j) %>%
  left_join(map_states, by = "STATEFIP")

# 7) Identifica principales emisores receptores
top5_export <- out_avg %>% arrange(desc(avg_outflow)) %>% slice_head(n = 5)
top5_import <- in_avg %>% arrange(desc(avg_inflow)) %>% slice_head(n = 5)

# 8) Resume flujo total bilateral
total_avg <- full_join(
  out_avg %>% select(STATEFIP, state_name, avg_outflow),
  in_avg %>% select(STATEFIP, state_name, avg_inflow),
  by = c("STATEFIP", "state_name")
) %>%
  mutate(
    avg_outflow = replace_na(avg_outflow, 0),
    avg_inflow = replace_na(avg_inflow, 0),
    avg_total_flow = avg_outflow + avg_inflow
  ) %>%
  arrange(desc(avg_total_flow)) %>%
  slice_head(n = 5)

# 9) Imprime resultados descriptivos agregados
print(top5_export)
print(top5_import)
print(total_avg)

# Bloque mapa nueva numeración.
# Visualización red y contigüidad espacial.
# Tratamiento AMZ define sombreado cartográfico.
# Periodo 2000-2024 para flujos anuales.

# Parámetros globales para visualización final
year_plot <- 2024L
years_flow <- 2000:2024
quarters_flow <- 1:4
top_n_flow <- 8L
out_file <- file.path(out_dir, sprintf("Map_Flow_Top5Out_Top5In_%d.png", year_plot))

# 0) Construye mapa base nodos
sf_map <- states_sf %>%
  st_make_valid() %>%
  mutate(STATEFIP = sprintf("%02d", as.integer(STATEFIP))) %>%
  st_transform(4326)

amz_map <- if ("AMZ" %in% names(sf_map)) as.integer(as.character(sf_map$AMZ)) else NA_integer_
sf_map$AMZ_map <- amz_map

att <- sf_map %>% st_drop_geometry()
cand_abbr <- c("STUSPS", "STATE_ABBR", "ABBR", "abbr", "NAME", "name", "STATE", "state")
abbr_col <- cand_abbr[cand_abbr %in% names(att)][1]
sf_map$ABBR <- if (!is.na(abbr_col)) att[[abbr_col]] else sf_map$STATEFIP

pts <- NULL

if ("centroid" %in% names(sf_map) && inherits(sf_map$centroid, "sfc")) {
  pts <- tryCatch({
    xy <- sf::st_coordinates(sf::st_transform(sf_map$centroid, 4326))
    as.data.frame(xy)
  }, error = function(e) NULL)
}

if (is.null(pts)) {
  pts <- sf::st_point_on_surface(sf_map) %>%
    sf::st_coordinates() %>%
    as.data.frame()
}

nodes <- data.table(
  STATEFIP = sf_map$STATEFIP,
  X = pts$X,
  Y = pts$Y,
  ABBR = sf_map$ABBR
)

# 1) Agrega flujos promedio anuales
j2j_tbl <- janitor::clean_names(tibble::as_tibble(j2j_raw))
flow_col2 <- intersect(c("j2j", "flow"), names(j2j_tbl))[1]
stopifnot(!is.na(flow_col2))

flows_y <- j2j_tbl %>%
  filter(
    periodicity == "Q",
    as.integer(year) %in% years_flow,
    as.integer(quarter) %in% quarters_flow
  ) %>%
  mutate(
    fips_i = sprintf("%02d", as.integer(stringr::str_extract(geography_orig, "\\d+"))),
    fips_j = sprintf("%02d", as.integer(stringr::str_extract(geography, "\\d+"))),
    year = as.integer(year)
  ) %>%
  filter(
    !is.na(fips_i), !is.na(fips_j),
    fips_i %in% nodes$STATEFIP,
    fips_j %in% nodes$STATEFIP,
    fips_i != fips_j
  ) %>%
  group_by(fips_i, fips_j, year) %>%
  summarise(flow_year = sum(.data[[flow_col2]], na.rm = TRUE), .groups = "drop")

yr_flow_min <- suppressWarnings(min(flows_y$year, na.rm = TRUE))
yr_flow_max <- suppressWarnings(max(flows_y$year, na.rm = TRUE))
stopifnot(is.finite(yr_flow_min), is.finite(yr_flow_max))


flows_avg <- flows_y %>%
  group_by(fips_i, fips_j) %>%
  summarise(avg_flow = mean(flow_year, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  group_by(fips_i) %>%
  mutate(
    w = avg_flow / sum(avg_flow, na.rm = TRUE),
    w = ifelse(is.finite(w), w, 0),
    w = replace_na(w, 0)
  ) %>%
  ungroup() %>%
  group_by(fips_j) %>%
  mutate(
    w_in = avg_flow / sum(avg_flow, na.rm = TRUE),
    w_in = ifelse(is.finite(w_in), w_in, 0),
    w_in = replace_na(w_in, 0)
  ) %>%
  ungroup()

out_avg2 <- flows_avg %>%
  group_by(fips_i) %>%
  summarise(avg_outflow = sum(avg_flow, na.rm = TRUE), .groups = "drop")

in_avg2 <- flows_avg %>%
  group_by(fips_j) %>%
  summarise(avg_inflow = sum(avg_flow, na.rm = TRUE), .groups = "drop") %>%
  rename(fips_i = fips_j)

top5_out <- out_avg2 %>%
  arrange(desc(avg_outflow)) %>%
  slice_head(n = 5) %>%
  pull(fips_i)

top5_in <- in_avg2 %>%
  arrange(desc(avg_inflow)) %>%
  slice_head(n = 5) %>%
  pull(fips_i)

origins <- unique(c(top5_out, top5_in))

# 2) Selecciona aristas relevantes origen
flow_plot_out <- flows_avg %>%
  filter(fips_i %in% origins) %>%
  group_by(fips_i) %>%
  slice_max(order_by = w, n = top_n_flow, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(flow_dir = "Flujos salientes")

flow_plot_in <- flows_avg %>%
  filter(fips_j %in% origins) %>%
  group_by(fips_j) %>%
  slice_max(order_by = w_in, n = top_n_flow, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(flow_dir = "Flujos entrantes")

edges <- bind_rows(flow_plot_out, flow_plot_in) %>%
  distinct(fips_i, fips_j, .keep_all = TRUE) %>%
  left_join(
    tibble::as_tibble(nodes)[, c("STATEFIP", "X", "Y", "ABBR")] %>%
      rename(fips_i = STATEFIP, Xi = X, Yi = Y, or_abbr = ABBR),
    by = "fips_i"
  ) %>%
  left_join(
    tibble::as_tibble(nodes)[, c("STATEFIP", "X", "Y", "ABBR")] %>%
      rename(fips_j = STATEFIP, Xj = X, Yj = Y, de_abbr = ABBR),
    by = "fips_j"
  )

# 3) Estima componente ϕ por AMZ
pf <- as.data.table(Panel_Final)
if (!"year" %in% names(pf) && "YEAR" %in% names(pf)) pf[, year := as.integer(YEAR)]
stopifnot("STATEFIP" %in% names(pf), "year" %in% names(pf), "S_it" %in% names(pf))
pf[, STATEFIP := sprintf("%02d", as.integer(STATEFIP))]

bS <- coef(m_endid)
bS_val <- if ("fit_S_it" %in% names(bS)) bS[["fit_S_it"]] else if ("S_it" %in% names(bS)) bS[["S_it"]] else NA_real_
stopifnot(is.finite(bS_val))

amz_pf_ok <- "AMZ" %in% names(pf)

pf_used <- pf[year %in% years_flow]
stopifnot(nrow(pf_used) > 0)

yr_pf_min <- suppressWarnings(min(pf_used$year, na.rm = TRUE))
yr_pf_max <- suppressWarnings(max(pf_used$year, na.rm = TRUE))
stopifnot(is.finite(yr_pf_min), is.finite(yr_pf_max))

pf_period <- pf_used[, .(
  S_it = mean(S_it, na.rm = TRUE),
  AMZ_pf = if (amz_pf_ok) suppressWarnings(max(AMZ, na.rm = TRUE)) else NA_real_
), by = STATEFIP]


pf_period[, AMZ_pf := as.integer(as.character(AMZ_pf))]
pf_period[, indirect_hat := bS_val * S_it]

sf_plot <- sf_map %>%
  left_join(as.data.frame(pf_period), by = "STATEFIP") %>%
  mutate(
    AMZ_map = as.integer(AMZ_map),
    AMZ_pf = as.integer(AMZ_pf),
    AMZ = dplyr::coalesce(AMZ_map, AMZ_pf, 0L),
    AMZ = ifelse(is.finite(AMZ), AMZ, 0L),
    AMZ = as.integer(AMZ)
  ) %>%
  select(-AMZ_map, -AMZ_pf)

lab_orig <- pf_period[STATEFIP %in% origins]
lab_orig <- merge(lab_orig, nodes, by = "STATEFIP", all.x = TRUE)
lab_orig$lab <- sprintf("%s\nϕ=%+.3f", lab_orig$ABBR, lab_orig$indirect_hat)

# 4) Renderiza mapa final integrado
p <- ggplot() +
  geom_sf(data = sf_plot, aes(fill = factor(AMZ)), color = "grey60", linewidth = 0.20) +
  
  geom_curve(
    data = edges,
    aes(x = Xi, y = Yi, xend = Xj, yend = Yj, color = flow_dir),
    curvature = 0.15,
    linewidth = 0.60,
    alpha = 0.75,
    arrow = grid::arrow(length = grid::unit(0.012, "npc"), type = "closed"),
    lineend = "round"
  ) +
  
  scale_color_manual(
    values = c("Flujos salientes" = "#0072B2", "Flujos entrantes" = "#D55E00"),
    guide = guide_legend(title = "Flujos de trabajadores minoristas", override.aes = list(linewidth = 0.80, alpha = 1))
  ) +
  
  geom_point(
    data = tibble::as_tibble(nodes) %>% filter(STATEFIP %in% origins),
    aes(x = X, y = Y),
    shape = 21,
    size = 4.0,
    fill = "black",
    color = "white",
    stroke = 1
  ) +
  geom_label_repel(
    data = lab_orig,
    aes(x = X, y = Y, label = lab),
    size = 3.1,
    fontface = "plain",
    family = "serif",
    box.padding = 0.30,
    point.padding = 0.20,
    label.size = 0,
    fill = scales::alpha("white", 0.85),
    seed = 1
  ) +
  
  scale_fill_manual(
    values = c("0" = "#F0F0F0", "1" = "#D9F0D3"),
    labels = c("0" = "Baja presencia de Amazon", "1" = "Alta presencia de Amazon"),
    name = "Presencia de Amazon"
  ) +
  
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0, color = "grey45"),
    plot.margin = margin(8, 8, 8, 8),
    legend.position = "right"
  ) +
  labs(
    title = "Red de transmisión por movilidad laboral minorista"
  )


ggsave(out_file, p, width = 13, height = 8.5, dpi = 600, bg = "white", device = ragg::agg_png)

# 5) Exporta figura
# Define rutas archivos de salida
ruta_tiff <- file.path(out_dir, "Figura1_Tendencias_Paralelas.tiff")
ruta_pdf  <- file.path(out_dir, "Figura1_Tendencias_Paralelas.pdf")

# Notifica inicio generación imágenes gráficas
message("Exportando figuras en alta resolución")

# Crea archivo tiff alta resolución
tiff(filename = ruta_tiff, 
     width = 10, height = 6.5, units = "in", 
     res = 600, 
     compression = "lzw", 
     family = "serif")

# Ajusta márgenes del área gráfica
par(mar = c(5, 5, 4, 2) + 0.1, mgp = c(3, 1, 0))

# Dibuja gráfico de tendencias paralelas
fixest::iplot(m_pta, 
              main = "Figura 1. Validación del supuesto de tendencias paralelas",
              xlab = "Años relativos al tratamiento",
              ylab = "Coeficientes estimados",
              pt.join = TRUE, 
              grid   = TRUE, 
              zero   = TRUE, 
              pt.pch = 19, 
              pt.col = "black", 
              ci.col = "black", 
              ci.lwd = 1.2, 
              ci.width = 0.05)

# Cierra dispositivo de salida tiff
dev.off()

# Crea archivo pdf calidad vectorial
pdf(file = ruta_pdf, 
    width = 10, height = 6.5, 
    family = "serif")

# Ajusta márgenes del área gráfica
par(mar = c(5, 5, 4, 2) + 0.1, mgp = c(3, 1, 0))

# Dibuja gráfico en formato vectorial
fixest::iplot(m_pta, 
              main = "Figura 1. Validación del supuesto de tendencias paralelas",
              xlab = "Años relativos al tratamiento",
              ylab = "Coeficientes estimados",
              pt.join = TRUE, 
              grid   = TRUE, 
              zero   = TRUE, 
              pt.pch = 19, 
              ci.lwd = 1.2)

# Cierra dispositivo de salida pdf
dev.off()

# Confirma creación de archivos finales
if (file.exists(ruta_tiff)) message("Imagen tiff guardada con éxito")
if (file.exists(ruta_pdf))  message("Imagen pdf guardada con éxito")