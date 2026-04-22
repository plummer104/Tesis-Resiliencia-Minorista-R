# ==============================================================================
# Título: Generación de Figuras para el Análisis de Resiliencia
# Reproducibilidad: Sí
# Descripción: Script para producir las Figuras 1, 2, 3 y 4 del análisis,
#              utilizando los datos procesados por el "script principal".
#              Marco Temporal: Pre-shock (2017–2019), Post-shock (2023).
# ==============================================================================

# --- Bloque 0: Configuración del Entorno de Gráficos ---

# 0.1 Cargar paquetes requeridos para la visualización
paquetes_graficos <- c(
  "here", "tidyverse", "sf", "arrow", "patchwork",
  "gridExtra", "gtable", "grid"
)

invisible(lapply(paquetes_graficos, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  }
  library(pkg, character.only = TRUE, quietly = TRUE)
}))

# 0.2 Definir rutas de entrada y salida
ruta_datos_procesados <- here::here("3_Outputs", "1_Data_Processed")
ruta_figuras          <- here::here("3_Outputs", "3_Figures")
dir.create(ruta_figuras, showWarnings = FALSE, recursive = TRUE)

# 0.3 Eliminar figuras previas en la carpeta de salida
archivos_previos <- list.files(
  path       = ruta_figuras,
  pattern    = "\\.png$",
  full.names = TRUE
)
if (length(archivos_previos) > 0L) {
  file.remove(archivos_previos)
}


cat("Entorno de gráficos configurado.\n\n")

# --- Bloque 1: Carga de Datos de Entrada ---
cat("--- Bloque 1: Cargando datos pre-procesados ---\n")

# 1.1 Panel de variables de aglomeración
ruta_datos_aglomeracion <- file.path(ruta_datos_procesados, "datos_aglomeracion_nacional_final.rds")
if (!file.exists(ruta_datos_aglomeracion)) {
  stop("Archivo 'datos_aglomeracion_nacional_final.rds' no encontrado. Ejecute el 'script principal' completo primero.")
}
datos_aglomeracion_nacional_final <- readRDS(ruta_datos_aglomeracion)
cat(" -> Panel de aglomeración cargado.\n")

# 1.2 Geometrías PUMA
ruta_geometrias <- file.path(ruta_datos_procesados, "geometrias_puma_nacional.rds")
if (!file.exists(ruta_geometrias)) {
  stop("Archivo 'geometrias_puma_nacional.rds' no encontrado. Ejecute el 'script principal' completo primero.")
}
geometrias_puma_nacional <- readRDS(ruta_geometrias)
cat(" -> Geometrías PUMA cargadas.\n")

# 1.3 Panel de resiliencia
ruta_panel_resiliencia <- file.path(ruta_datos_procesados, "panel_resiliencia_postshock_2022_2023.parquet")
if (!file.exists(ruta_panel_resiliencia)) {
  stop("Archivo 'panel_resiliencia_postshock_2022_2023.parquet' no encontrado. Ejecute el 'script principal' completo primero.")
}
panel_resiliencia <- arrow::read_parquet(ruta_panel_resiliencia)
cat(" -> Panel de resiliencia (2022-2023) cargado.\n\n")


# ==============================================================================
# Figura 1: Mapa de Potencial de Mercado, 2019
# ==============================================================================
cat("--- Generando Figura 1: Mapa de Potencial de Mercado (2019) ---\n")

# 2.1 Datos 2019
datos_mapa_2019 <- datos_aglomeracion_nacional_final %>% 
  filter(año == 2019)

geometrias_con_datos_2019 <- geometrias_puma_nacional %>%
  left_join(datos_mapa_2019, by = "GEOID") %>%
  filter(!is.na(ln_potencial_mercado))

# 2.2 Cuantiles
breaks_potencial <- quantile(
  geometrias_con_datos_2019$ln_potencial_mercado, 
  probs = c(0, 0.25, 0.50, 0.75, 1), 
  na.rm = TRUE
)

geometrias_con_datos_2019 <- geometrias_con_datos_2019 %>%
  mutate(
    potencial_cat = cut(
      ln_potencial_mercado,
      breaks = breaks_potencial,
      labels = c(
        paste0("< ", round(breaks_potencial[2], 2)),
        paste0(round(breaks_potencial[2], 2), " - ", round(breaks_potencial[3], 2)),
        paste0(round(breaks_potencial[3], 2), " - ", round(breaks_potencial[4], 2)),
        paste0("> ", round(breaks_potencial[4], 2))
      ),
      include.lowest = TRUE, 
      right = FALSE
    )
  )

# 2.3 Gráfico
mapa_potencial_mercado_2019 <- ggplot(data = geometrias_con_datos_2019) +
  geom_sf(aes(fill = potencial_cat), color = "gray50", linewidth = 0.1) +
  scale_fill_brewer(palette = "YlOrRd", name = "ln(Market Potential)") +
  labs(
    title   = "Figure 1: Market Potential per PUMA, 2019",
    caption = "Source: Own elaboration with data from IPUMS ACS."
  ) +
  theme_void() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = c(0.15, 0.2),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  )

# 2.4 Guardar
ruta_figura_1 <- file.path(ruta_figuras, "Fig1_mapa_potencial_mercado_2019.png")
if (file.exists(ruta_figura_1)) file.remove(ruta_figura_1)
ggsave(ruta_figura_1, mapa_potencial_mercado_2019, width = 12, height = 8, dpi = 300, bg = "white")
cat(paste("Figura 1 guardada en:", ruta_figura_1, "\n\n"))



# ==============================================================================
# Figura 2: Mapa de Densidad de Empleo, 2019
# ==============================================================================
cat("--- Generando Figura 2: Mapa de Densidad de Empleo (2019) ---\n")

# 3.1 Reutilizar datos 2019
geometrias_con_datos_2019_densidad <- geometrias_con_datos_2019 %>%
  filter(!is.na(ln_densidad_empleo))

# 3.2 Cuantiles
breaks_densidad <- quantile(
  geometrias_con_datos_2019_densidad$ln_densidad_empleo, 
  probs = c(0, 0.25, 0.50, 0.75, 1), 
  na.rm = TRUE
)

geometrias_con_datos_2019_densidad <- geometrias_con_datos_2019_densidad %>%
  mutate(
    densidad_cat = cut(
      ln_densidad_empleo,
      breaks = breaks_densidad,
      labels = c(
        paste0("< ", round(breaks_densidad[2], 2)),
        paste0(round(breaks_densidad[2], 2), " - ", round(breaks_densidad[3], 2)),
        paste0(round(breaks_densidad[3], 2), " - ", round(breaks_densidad[4], 2)),
        paste0("> ", round(breaks_densidad[4], 2))
      ),
      include.lowest = TRUE, 
      right = FALSE
    )
  )

# 3.3 Gráfico
mapa_densidad_empleo_2019 <- ggplot(data = geometrias_con_datos_2019_densidad) +
  geom_sf(aes(fill = densidad_cat), color = "gray50", linewidth = 0.1) +
  scale_fill_brewer(palette = "YlGnBu", name = "ln(Employment Density)") +
  labs(
    title   = "Figure 2: Employment Density per PUMA, 2019",
    caption = "Source: Own elaboration with data from IPUMS ACS."
  ) +
  theme_void() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.caption = element_text(hjust = 1, face = "italic"),
    legend.position = c(0.15, 0.2),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2)
  )

# 3.4 Guardar
ruta_figura_2 <- file.path(ruta_figuras, "Fig2_mapa_densidad_empleo_2019.png")
if (file.exists(ruta_figura_2)) file.remove(ruta_figura_2)
ggsave(ruta_figura_2, mapa_densidad_empleo_2019, width = 12, height = 8, dpi = 300, bg = "white")
cat(paste("Figura 2 guardada en:", ruta_figura_2, "\n\n"))



# ==============================================================================
# Figura 3: Mapa de Resiliencia Proporcional por PUMA (CA & NY), 2023
# ==============================================================================
cat("--- Generando Figura 3: Mapas de Resiliencia Proporcional (2023) ---\n")

# 4.1 Agregar resiliencia 2023
resiliencia_puma_2023 <- panel_resiliencia %>%
  filter(YEAR == 2023L) %>% 
  group_by(GEOID) %>%
  summarise(resiliencia_media = mean(Rp_wins, na.rm = TRUE), .groups = "drop")

# 4.2 Unir con geometrías
geometrias_con_resiliencia <- geometrias_puma_nacional %>%
  left_join(resiliencia_puma_2023, by = "GEOID") %>%
  filter(!is.na(resiliencia_media))

# 4.3 Cuantiles
breaks_resiliencia <- quantile(
  geometrias_con_resiliencia$resiliencia_media, 
  probs = c(0, 0.25, 0.50, 0.75, 1), 
  na.rm = TRUE
)

geometrias_con_resiliencia <- geometrias_con_resiliencia %>%
  mutate(
    resiliencia_cat = cut(
      resiliencia_media,
      breaks = breaks_resiliencia,
      labels = c(
        paste0("< ", round(breaks_resiliencia[2], 3)),
        paste0(round(breaks_resiliencia[2], 3), " - ", round(breaks_resiliencia[3], 3)),
        paste0(round(breaks_resiliencia[3], 3), " - ", round(breaks_resiliencia[4], 3)),
        paste0("> ", round(breaks_resiliencia[4], 3))
      ),
      include.lowest = TRUE, 
      right = FALSE
    )
  )

# 4.4 Filtrar estados: CA (06) y NY (36)
mapa_ca_data <- geometrias_con_resiliencia %>% 
  filter(str_starts(GEOID, "06"))
mapa_ny_data <- geometrias_con_resiliencia %>% 
  filter(str_starts(GEOID, "36"))

# 4.5 Mapas individuales
mapa_ca <- ggplot(data = mapa_ca_data) +
  geom_sf(aes(fill = resiliencia_cat), color = "gray50", linewidth = 0.1) +
  labs(title = "California") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

mapa_ny <- ggplot(data = mapa_ny_data) +
  geom_sf(aes(fill = resiliencia_cat), color = "gray50", linewidth = 0.1) +
  labs(title = "New York") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 4.6 Combinar con patchwork
figura_3_combinada <- (mapa_ca | mapa_ny) +
  plot_layout(guides = "collect") &
  scale_fill_brewer(palette = "RdYlBu", name = "Proportional Resilience") &
  plot_annotation(
    title    = "Figure 3: Proportional Resilience per PUMA, 2023",
    subtitle = "California and New York cases",
    caption  = "Source: Own elaboration with data from IPUMS ACS.",
    theme    = theme(
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption  = element_text(hjust = 1, face = "italic")
    )
  ) &
  theme(legend.position = "right")

# 4.7 Guardar
ruta_figura_3 <- file.path(ruta_figuras, "Fig3_mapa_resiliencia_CA_NY_2023.png")
if (file.exists(ruta_figura_3)) file.remove(ruta_figura_3)
ggsave(ruta_figura_3, figura_3_combinada, width = 14, height = 8, dpi = 300, bg = "white")
cat(paste("Figura 3 guardada en:", ruta_figura_3, "\n\n"))



# ==============================================================================
# Figura 4: Patrones de Resiliencia en Megarregiones (BosWash y Midwest)
# ==============================================================================
cat("--- Generando Figura 4: Zoom en Megarregiones (2023) ---\n")

# Función para mapa de zoom sin leyenda
crear_zoom_mapa <- function(data, estados_filtro, titulo) {
  
  data_zoom <- data %>% 
    filter(substr(GEOID, 1, 2) %in% estados_filtro)
  
  ggplot(data = data_zoom) +
    geom_sf(aes(fill = resiliencia_cat), color = "gray50", linewidth = 0.05) +
    scale_fill_brewer(palette = "RdYlBu", name = "Proportional Resilience") +
    labs(title = titulo) +
    theme_void() +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold", size = 13), 
      legend.position = "none"
    )
}

# 5.1 Megarregión Northeast Corridor (BosWash)
zoom_ne_final <- crear_zoom_mapa(
  geometrias_con_resiliencia,
  c("36","34","42","25","09","44","10","24"),
  "A. Northeast Corridor (BosWash)"
)

# 5.2 Megarregión Midwest / Great Lakes
zoom_mw_final <- crear_zoom_mapa(
  geometrias_con_resiliencia,
  c("17","18","26","39","55"),
  "B. Midwest (Midwest / Great Lakes)"
)

# 5.3 Función para extraer leyenda
get_legend <- function(p) {
  g <- ggplotGrob(p)
  guide_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  g$grobs[[guide_index]]
}

# 5.4 Plot “dummy” para leyenda
plot_leyenda <- ggplot(geometrias_con_resiliencia) +
  geom_sf(aes(fill = resiliencia_cat)) +
  scale_fill_brewer(palette = "RdYlBu", name = "Proportional Resilience") +
  theme_void() +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.title     = element_text(size = 14, face = "bold"), 
    legend.text      = element_text(size = 12)
  )

leyenda_grob_final <- get_legend(plot_leyenda)

# 5.5 Guardar figura 4
ruta_figura_4_final <- file.path(ruta_figuras, "Fig4_zoom_BosWash_Midwest.png")
if (file.exists(ruta_figura_4_final)) file.remove(ruta_figura_4_final)

png(filename = ruta_figura_4_final, width = 14, height = 7, units = "in", res = 300)

grid.arrange(
  arrangeGrob(zoom_ne_final, zoom_mw_final, ncol = 2),
  leyenda_grob_final,
  nrow    = 2,
  heights = c(10, 1),
  top = textGrob(
    "Figure 4: Resilience Patterns in Major US Megaregions (Post-Shock 2023)",
    gp = gpar(fontsize = 18, fontface = "bold")
  )
)

dev.off()

cat(paste("Figura 4 guardada en:", ruta_figura_4_final, "\n\n"))
cat("Proceso completado.\n")

