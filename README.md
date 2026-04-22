# Tesis-Resiliencia-Minorista-R
**Repositorio de código reproducible**  
**Tesis doctoral:** *Métodos para analizar la resiliencia del comercio minorista en el contexto de la transformación digital*

---

| Campo | Información |
|---|---|
| Autor | Juan Pablo Rodríguez Paredes |
| Programa | Doctorado en Economía y Empresa — Universidad Autónoma de Madrid |
| Directora | Dra. María del Coro Chasco Yrigoyen |
| Tutora | Dra. Beatriz Sánchez Reyes |
| Año | 2026 |
| Repositorio | https://github.com/jp-rodriguez-uam/Tesis-Resiliencia-Minorista-R |

---

## Tabla de contenidos

1. [Descripción general](#1-descripción-general)
2. [Estructura del repositorio](#2-estructura-del-repositorio)
3. [Requisitos del sistema y dependencias](#3-requisitos-del-sistema-y-dependencias)
4. [Guía de descarga de datos](#4-guía-de-descarga-de-datos)
5. [Orden de ejecución](#5-orden-de-ejecución)
6. [Metodología paso a paso](#6-metodología-paso-a-paso)
7. [Resultados esperados y outputs](#7-resultados-esperados-y-outputs)
8. [Nota sobre ACS_depurada.RData](#8-nota-sobre-acs_depuradadata)
9. [Limitaciones para la reproducibilidad](#9-limitaciones-para-la-reproducibilidad)
10. [Recomendaciones antes de publicar](#10-recomendaciones-antes-de-publicar)
11. [Nota de uso académico](#11-nota-de-uso-académico)
12. [Contacto](#12-contacto)

---

## 1. Descripción general

Este repositorio contiene los scripts en **R** utilizados para implementar los tres estudios empíricos de la tesis doctoral sobre la resiliencia del comercio minorista en el contexto de la transformación digital. El objetivo general es examinar cómo la digitalización del comercio minorista reordena los ingresos mediante mecanismos territoriales que desbordan las fronteras administrativas, combinando tres escalas de análisis: **estados**, **condados** y **trabajadores individuales / PUMA**.

Los tres bloques analíticos son:

### Chapter1 (Capítulo 2 de la tesis)
Análisis estatal (panel 2002–2024) mediante un modelo de **Diferencias en Diferencias con Red Endógena Instrumentada (ENDID)**. El estimador combina (i) un modelo gravitacional PPML que predice la conectividad interestatal a partir de flujos bilaterales J2J de comercio minorista, (ii) la exposición de red instrumentada con esos flujos predichos para separar vínculos estructurales de ajustes inducidos por tratamiento, y (iii) una ecuación salarial IV con efectos fijos estatales y temporales.

Los resultados principales indican una reducción salarial de aproximadamente **−4.6 %** en estados tratados y un efecto adicional de transmisión por red de **−1.1 % por cada 10 puntos porcentuales** de conectividad con estados tratados. **Dado que el estudio de eventos detecta diferencias pretratamiento entre grupos (Wald conjunto = 62.1, p < 0.001), estas magnitudes deben interpretarse como asociaciones condicionadas al diseño, los controles y los supuestos del modelo, no como efectos causales nítidamente aislados.**

### Chapter2 (Capítulo 3 de la tesis)
Análisis a nivel de **condado** (corte transversal 2023) mediante **emparejamiento espacial por puntaje de propensión (PSM)**. El puntaje se estima con un modelo logístico bayesiano con componente espacial continuo **INLA–SPDE** (análisis principal, AUC ≈ 0.95) y con una especificación jerárquica discreta **BYM2** (análisis de sensibilidad). Tras el emparejamiento por vecino más cercano sin reemplazo (188 pares, caliper = 0.1 SD), el ATT estimado con el modelo BYM2 asciende a **+2.3 % (IC 95 %: 0.2–4.4 %)**. Ese efecto se anula en condados de baja resiliencia y pierde significación estadística en los de alta resiliencia.

### Chapter3 (Capítulo 4 de la tesis)
Análisis a nivel **individual / PUMA** para estimar un índice de resiliencia salarial post-shock y sus determinantes mediante **variables instrumentales espaciales**. El índice se construye como la brecha proporcional entre el salario observado en 2022–2023 y el salario contrafactual predicho con la estructura salarial pre-shock de 2017–2019. La endogeneidad de las variables de aglomeración se aborda con tres diseños de instrumentos sintéticos espaciales: **IV-GL (Graph Laplacian)**, **IV sintético ponderado** e **IV sintético no ponderado**. Los microdatos proceden de IPUMS USA (ACS) y la muestra post-shock asciende a **2 982 271 observaciones**.

### Aviso de disponibilidad
El repositorio distribuye exclusivamente **código** y **documentación metodológica**. Los datos originales **no se incluyen** ni se redistribuyen. Los extractos de IPUMS USA están sujetos a restricciones contractuales de redistribución. Los productos públicos del U.S. Census Bureau (LEHD, TIGER/Line, CRE, tablas ACS) tampoco se incluyen para garantizar que cada replicador trabaje con la versión oficial exacta.

---

## 2. Estructura del repositorio

**Nota sobre la numeración.** La estructura salta de `1_Data/` a `3_Outputs/` porque las carpetas de scripts (`Chapter1/`, `Chapter2/`, `Chapter3/`) constituyen el bloque lógico intermedio. Esta convención coincide con las rutas codificadas en los scripts mediante `here::here()` y no debe modificarse salvo que también se actualicen las rutas dentro del código.

```text
.
├── Chapter1/                    # Cap. 2 tesis: ENDID estatal (2002–2024)
│   ├── 01_main_analysis.R
│   └── 02_create_figures.R
│
├── Chapter2/                    # Cap. 3 tesis: PSM espacial y BYM2 (condados, 2023)
│   ├── 01_main_analysis.R
│   ├── 02_create_tables.R
│   └── 03_create_figures.R
│
├── Chapter3/                    # Cap. 4 tesis: Resiliencia e IV espacial (PUMAs)
│   ├── 01_main_analysis.R
│   ├── 02_create_tables.R
│   └── 03_create_figures.R
│
├── 1_Data/                      # Directorio de insumos (NO incluido — ver Sección 4)
│   ├── ipums/
│   ├── excel/
│   │   └── Data/                # Tablas ACS 5-Year 2023 (ACSST5Y2023.Sxxxx-Data.csv)
│   ├── shapefiles/
│   │   ├── ArcGis/              # Shapefiles condales y estatales
│   │   └── ipums_puma_2020/     # Shapefile de PUMAs 2020
│   └── crosswalk/               # Archivos de correspondencia geográfica
│
├── 3_Outputs/                   # Directorio de salidas (generado al ejecutar)
│   ├── 1_Data_Processed/
│   ├── 2_Models/
│   ├── 3_Figures/
│   └── 4_Tables/
│
├── .gitignore
└── README.md
```

### Correspondencia carpetas–capítulos

| Carpeta | Capítulo en la tesis | Unidad de análisis |
|---|---|---|
| `Chapter1/` | Capítulo 2 | Estados (panel 2002–2024) |
| `Chapter2/` | Capítulo 3 | Condados (corte 2023) |
| `Chapter3/` | Capítulo 4 | Trabajadores individuales / PUMA |

### Función de los scripts

- `01_main_analysis.R` — script principal; carga datos, construye variables, estima modelos y guarda todos los objetos de resultados en disco.
- `02_create_tables.R` — genera tablas de resultados a partir de objetos en disco (y, en Chapter2, de objetos en memoria).
- `03_create_figures.R` / `02_create_figures.R` — genera figuras, mapas y salidas gráficas de alta resolución.

Para que Git rastree carpetas vacías, puede incluirse un archivo `.gitkeep` en cada subdirectorio de `1_Data/` y `3_Outputs/`.

---

## 3. Requisitos del sistema y dependencias

### Entorno recomendado

- **R:** 4.5.1 o superior.
- **Sistema operativo:** Windows, Linux o macOS, siempre que sean compatibles con la cadena de compilación necesaria para paquetes espaciales y bayesianos.
- **RAM:** mínimo **16 GB**; recomendados **32 GB** para Chapter2 (INLA) y Chapter3 (descomposición espectral y microdatos).
- **GDAL / GEOS / PROJ:** compatibles con `sf`.
- **Stan / RStan:** necesarios para parte del entorno bayesiano de Chapter2.
- **Arrow / Parquet:** necesarios para el flujo de Chapter3.

### Paquetes requeridos por capítulo

#### Chapter1
```r
pacman::p_load(
  tidyverse, ipumsr, data.table, janitor, sf, fixest,
  units, here, ggplot2, ggrepel, scales, spdep
)
```

#### Chapter2
```r
required_packages <- c(
  "brms", "BART", "rstanarm", "MatchIt", "sf", "glmnet",
  "dplyr", "cobalt", "readr", "survey", "gstat", "ggplot2",
  "tidyr", "knitr", "viridis", "showtext", "gridExtra",
  "xtable", "pROC", "mgcv", "cowplot", "gratia",
  "fmesher", "INLA", "sn", "spdep", "stringr",
  "data.table", "scales", "writexl", "igraph"
)
```

#### Chapter3
```r
paquetes_requeridos <- c(
  "here", "tidyverse", "ipumsr", "sf", "units", "sp",
  "spdep", "fixest", "modelsummary", "readxl", "janitor",
  "arrow", "knitr", "mgcv", "openxlsx", "xml2", "magrittr",
  "broom", "patchwork", "gridExtra", "gtable", "grid"
)
```

### Instalación de INLA y fmesher
```r
options(repos = c(
  CRAN = "https://cloud.r-project.org",
  INLA = "https://inla.r-inla-download.org/R/stable"
))
install.packages(c("fmesher", "INLA"))
```

---

## 4. Guía de descarga de datos

**Principio fundamental:** cada replicador debe obtener los insumos directamente desde las fuentes oficiales, en cumplimiento de los términos de uso y requisitos de cita de cada proveedor.

---

### 4.1 IPUMS USA — Chapter1 y Chapter3

**IPUMS USA** contiene los microdatos de la American Community Survey (ACS) y de otros censos del U.S. Census Bureau. Su redistribución a terceros está expresamente prohibida.

#### Pasos para obtener el extracto
1. Crear una cuenta en `usa.ipums.org`.
2. Iniciar sesión y seleccionar **Create an Extract**.
3. Elegir las muestras según el capítulo:
   - **Chapter1:** ACS 1-Year samples para los años **2000–2024**, restringiendo al universo y variables pertinentes del análisis estatal.
   - **Chapter3:** ACS 1-Year samples para los años **2017–2019** y **2022–2023**, con trabajadores ocupados (`EMPSTAT = 1`).
4. Seleccionar las variables necesarias según las tablas de descripción de variables de cada paper/capítulo.
5. Enviar el extracto.
6. Descargar:
   - archivo **DDI XML** (p. ej., `usa_00098.xml` o `usa_00078.xml`),
   - archivo de microdatos (`.dat.gz` o `.csv`).
7. Guardar ambos archivos en `1_Data/ipums/`.

**Nombres esperados por los scripts:** `usa_00098.xml` (Chapter1) y `usa_00078.xml` (Chapter3).

**Variables monetarias relevantes para Chapter3:** los scripts esperan las variables derivadas ya deflactadas por IPUMS: `INCWAGE_CPIU_2010`, `INCEARN_CPIU_2010` y `HHINCOME_CPIU_2010`. Deben solicitarse explícitamente al construir el extracto.

#### Uso en R con `ipumsr`
```r
library(ipumsr)
ddi  <- read_ipums_ddi("1_Data/ipums/usa_00078.xml")
data <- read_ipums_micro(ddi)
```

---

### 4.2 Tablas temáticas de la ACS 5-Year 2023 — Chapter2

**Estas tablas son el insumo original replicable de Chapter2.** Agregan estadísticas socioeconómicas a nivel de condado y son de acceso público en `data.census.gov`.

#### Pasos para descargar cada tabla
1. Acceder a `data.census.gov`.
2. Buscar el código de tabla (p. ej., `S1902`).
3. Seleccionar **ACS 5-Year Estimates Subject Tables → 2023 → All Counties within United States**.
4. Descargar en formato **CSV**.
5. El archivo tendrá un patrón como: `ACSST5Y2023.Sxxxx-Data.csv`.
6. Repetir para todas las tablas requeridas.
7. Guardar todos los CSV en `1_Data/excel/Data/`.

#### Convención de importación
El script `Chapter2/01_main_analysis.R`:
- detecta automáticamente todos los archivos que siguen el patrón `^ACSST5Y2023\.S\d{4}-Data(\.csv)?$`;
- los importa **sin encabezado** (`header = FALSE`);
- construye nombres de columna concatenando la fila 1 y la fila 2 con el separador `@`;
- renombra la primera columna como `GEO_ID`;
- hace el merge secuencial por `GEO_ID`.

#### Tablas ACS requeridas por Chapter2

| Código | Descripción temática | Variable(s) destacadas |
|---|---|---|
| S0101 | Edad y sexo | Estructura demográfica |
| S0801 | Medios de transporte al trabajo | Variables de movilidad |
| S0802 | Medios de transporte al trabajo (detalle) | Variables de movilidad |
| S1001 | Abuelos como cuidadores | Estructura familiar |
| S1101 | Hogares y familias | Estructura de hogares |
| S1201 | Estado civil | Estructura social |
| S1301 | Fertilidad | Demografía |
| S1401 | Matrícula escolar | Educación |
| S1501 | Nivel educativo | Educación |
| S1601 | Idioma hablado en el hogar | Diversidad lingüística |
| S1602 | Hablantes de inglés limitado | Diversidad lingüística |
| S1603 | Familias e individuos con LEP | Diversidad lingüística |
| S1701 | Pobreza | Condición socioeconómica |
| S1702 | Pobreza familiar | Condición socioeconómica |
| S1810 | Discapacidad | Vulnerabilidad |
| S1901 | Ingresos en los últimos 12 meses | Distribución del ingreso |
| S1902 | Ingresos medios | `S1902_C02_001E` |
| S1903 | Ingresos medios (mediana) | Distribución del ingreso |
| S2001 | Educación y empleo de veteranos | Veteranos |
| S2101 | Características de los veteranos | Veteranos |
| S2201 | Participación en programas de alimentación | Pobreza |
| S2302 | Participación económica de adultos mayores | Adultos mayores |
| S2303 | Ingresos de adultos mayores | Adultos mayores |
| S2401 | Industria/ocupación de nativos e inmigrantes | Mercado laboral |
| S2402 | Ocupación por sexo — servicios | `S2402_C01_018E` |
| S2403 | Sector de actividad — agricultura | `S2403_C01_003E` |
| S2404–S2419 | Empleo por grupo demográfico | Mercado laboral |
| S2501 | Características de vivienda — arrendatarios | Estructura habitacional |
| S2502 | Características de vivienda — propietarios | Estructura habitacional |
| S2503 | Costos financieros de vivienda | `S2503_C06_019E` |
| S2504 | Habitaciones por tipo de vivienda | `S2504_C03_018E` |
| S2506 | Carga hipotecaria | `S2506_C01_047E` |
| S2507 | Viviendas sin hipoteca | Covariables adicionales |
| S2701–S2704 | Cobertura de salud | Salud |
| S2801–S2802 | Uso de computadoras e internet | Acceso digital |
| S2901–S2902 | Uso del voto | Participación cívica |

#### Nota sobre la base intermedia de Chapter2

Después del procesamiento local de las tablas ACS, el script genera una **base intermedia** (`ACS_depurada.RData`) que se guarda en `3_Outputs/1_Data_Processed/`. Ese archivo es un **producto local derivado** de las tablas CSV, no el insumo original. El insumo original replicable son las tablas ACS descargadas directamente por el replicador desde `data.census.gov`. Véase la Sección 8 para el flujo completo de construcción.

**Nota sobre rutas:** la versión de desarrollo del script puede contener rutas absolutas heredadas del entorno local de trabajo. Antes de ejecutar en otro equipo, deben sustituirse por rutas relativas gestionadas con `here::here()`.

---

### 4.3 LEHD: Job-to-Job Flows — Chapter1

Los datos **Job-to-Job (J2J)** capturan los flujos bilaterales de trabajadores entre estados.

#### Pasos
1. Acceder a `lehd.ces.census.gov/data`.
2. Seleccionar **Job-to-Job Flows (J2J) → National → Sector: 44-45 (Retail Trade)**.
3. Descargar el archivo CSV de flujos bilaterales estado-origen × estado-destino × año para **2002–2024**.
4. Guardar en `1_Data/excel/`.
5. Renombrar como: `j2j_500e41e2909a5c9752726849a6a0eb9a.csv`
   o actualizar la ruta en `Chapter1/01_main_analysis.R`.

---

### 4.4 LEHD: Quarterly Workforce Indicators — Chapter2

Los **QWI** proporcionan el ingreso mensual promedio (`EarnS`) del sector minorista a nivel de condado.

#### Pasos
1. Acceder a `ledextract.ces.census.gov` o `lehd.ces.census.gov/data`.
2. Seleccionar:
   - geografía: todos los condados contiguos,
   - período: **Q1 2023**,
   - sector: **NAICS 44–45**,
   - indicador: **EarnS**,
   - educación: **E1 / E2** (some high school or less; high school or equivalent, no college).
3. Descargar el CSV.
4. Guardar en `1_Data/excel/` con el nombre: `qwi_958444104b164a8ab67704728ee1fb1f.csv`

---

### 4.5 Community Resilience Estimates — Chapter2

El **CRE** del Census Bureau mide el porcentaje de población sin componentes de vulnerabilidad acumulada. La variable usada es **`PRED0_PE`**.

#### Pasos
1. Acceder a `www.census.gov/programs-surveys/community-resilience-estimates.html`.
2. Descargar el archivo a nivel de condado correspondiente a **2023**: `CRE_23_County.csv`.
3. Guardar en `1_Data/excel/`.

---

### 4.6 Cartografía (shapefiles)

#### Shapefiles de condados — Chapter2
Chapter2 requiere **dos shapefiles condales con roles distintos**:

1. **`Dummy Condado.shp`**
   - se utiliza en la primera fase de importación y merge con la base ACS;
   - sirve de entrada al **LASSO**;
   - contiene la variable de tratamiento **AMZ** y la columna `Join_Count`.

2. **`DummyCondado_Project_ExportFeatures.shp`**
   - se utiliza para construir la matriz de pesos espaciales **Queen**, la malla **INLA–SPDE** y el merge secundario;
   - corresponde al shapefile proyectado en **USA Contiguous Albers Equal Area Conic** (unidades en metros).

Estos archivos **no son productos estándar públicos** porque contienen la variable de tratamiento AMZ de elaboración propia.

#### Shapefiles de PUMA 2020 — Chapter3
1. Acceder a **NHGIS** o al portal **TIGER/Line** del Census Bureau.
2. Descargar los shapefiles de **PUMA 2020**.
3. Guardar en `1_Data/shapefiles/ipums_puma_2020/`.
4. El script reproyecta a **EPSG:5070** para cálculo de áreas en km².

#### Shapefile estatal con variable de tratamiento — Chapter1
`IVNDiD.shp` contiene la clasificación de estados según presencia alta/baja de Amazon (AMZ). No es un shapefile estándar del Census Bureau.

---

### 4.7 Archivos de correspondencia geográfica — Chapter3

| Archivo | Fuente | Ruta local |
|---|---|---|
| `PUMA2010_PUMA2020_crosswalk.xls` | U.S. Census Bureau | `1_Data/crosswalk/` |
| `2020_Census_Tract_to_2020_PUMA.csv` | U.S. Census Bureau | `1_Data/crosswalk/` |

El script verifica que el `allocation_factor` coincida con `p_puma10_pop20 / 100` con tolerancia de `1e-4`, y valida conservación de pesos con tolerancia de `1e-5`.

---

### 4.8 Archivos auxiliares — Chapter3

| Archivo | Descripción | Ruta local |
|---|---|---|
| `large_place_PUMA2020_crosswalk.xlsx` | Correspondencia entre lugares grandes y PUMAs 2020 | `1_Data/excel/` |
| `uscities.csv` | Ciudades de EE. UU. con coordenadas | `1_Data/excel/` |

---

### 4.9 Resumen de insumos por capítulo

#### Chapter1 — Análisis estatal
| Archivo esperado | Ruta local | Fuente |
|---|---|---|
| `usa_00098.xml` | `1_Data/ipums/` | DDI — IPUMS USA |
| Microdatos IPUMS | `1_Data/ipums/` | ACS — IPUMS USA |
| `IVNDiD.shp` (+ auxiliares) | `1_Data/shapefiles/ArcGis/` | Elaboración propia |
| `j2j_500e41e2909a5c9752726849a6a0eb9a.csv` | `1_Data/excel/` | LEHD J2J |

#### Chapter2 — Análisis condal
| Insumo | Ruta local | Fuente |
|---|---|---|
| Tablas ACS 5-Year 2023 (`ACSST5Y2023.Sxxxx-Data.csv`) | `1_Data/excel/Data/` | data.census.gov |
| `qwi_958444104b164a8ab67704728ee1fb1f.csv` | `1_Data/excel/` | LEHD QWI |
| `CRE_23_County.csv` | `1_Data/excel/` | Census Bureau |
| `Dummy Condado.shp` (+ auxiliares) | `1_Data/shapefiles/ArcGis/` | Elaboración propia |
| `DummyCondado_Project_ExportFeatures.shp` (+ auxiliares) | `1_Data/shapefiles/ArcGis/` | Elaboración propia |

#### Chapter3 — Análisis individual / PUMA
| Archivo esperado | Ruta local | Fuente |
|---|---|---|
| `usa_00078.xml` | `1_Data/ipums/` | DDI — IPUMS USA |
| Microdatos IPUMS | `1_Data/ipums/` | ACS — IPUMS USA |
| `ipums_puma_2020.shp` (+ auxiliares) | `1_Data/shapefiles/ipums_puma_2020/` | NHGIS / TIGER/Line |
| `PUMA2010_PUMA2020_crosswalk.xls` | `1_Data/crosswalk/` | Census Bureau |
| `2020_Census_Tract_to_2020_PUMA.csv` | `1_Data/crosswalk/` | Census Bureau |
| `large_place_PUMA2020_crosswalk.xlsx` | `1_Data/excel/` | Census Bureau |
| `uscities.csv` | `1_Data/excel/` | SimpleMaps |

---

### 4.10 Referencias de cita obligatorias para replicadores

#### IPUMS USA
Ruggles, S., Flood, S., Sobek, M., Backman, D., Cooper, G., Drew, J. A. R., Richards, S., Rogers, R., Schroeder, J., & Williams, K. C. W. (2025). *IPUMS USA: Version 16.0* [dataset]. Minneapolis, MN: IPUMS. https://doi.org/10.18128/D010.V16.0

#### Otras citas obligatorias
- ACS 5-Year 2023 (Chapter2): tablas específicas, período de referencia y fecha de acceso.
- LEHD Job-to-Job Flows (Chapter1): U.S. Census Bureau. (2025). Job-to-Job Flows (Versión R2025Q1). https://lehd.ces.census.gov/data/j2j/
- LEHD Quarterly Workforce Indicators (Chapter2): U.S. Census Bureau. (2025). https://ledextract.ces.census.gov
- Community Resilience Estimates (Chapter2): U.S. Census Bureau. (2023). https://www.census.gov/programs-surveys/community-resilience-estimates.html
- TIGER/Line Shapefiles (Chapter1–3): U.S. Census Bureau. (2024). https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
- MWPVL International (Chapter2): MWPVL International. (2025). https://mwpvl.com/html/amazon_com.html

---

## 5. Orden de ejecución

Los tres capítulos son independientes entre sí, pero dentro de cada carpeta los scripts tienen distintos grados de autonomía.

| Capítulo | Script | Lee desde disco | Requiere objetos en memoria |
|---|---|---|---|
| Chapter1 | `01_main_analysis.R` | `1_Data/` | No |
| Chapter1 | `02_create_figures.R` | `Panel_Final.rds`, `modelos_salario.rds` | Sí (`states_sf`, `j2j_raw`) |
| Chapter2 | `01_main_analysis.R` | `1_Data/` (tablas ACS + shapefiles + QWI + CRE) | No |
| Chapter2 | `02_create_tables.R` | Parcialmente | Sí (ver nota Chapter2) |
| Chapter2 | `03_create_figures.R` | Parcialmente | Sí (ver nota Chapter2) |
| Chapter3 | `01_main_analysis.R` | `1_Data/` | No |
| Chapter3 | `02_create_tables.R` | `3_Outputs/2_Models/`, DDI XML | Parcial |
| Chapter3 | `03_create_figures.R` | `3_Outputs/` íntegramente | No |

### Chapter1
Ejecutar `01_main_analysis.R` y, a continuación, `02_create_figures.R` **en la misma sesión de R**. El script de figuras puede recargar `Panel_Final.rds` y `modelos_salario.rds`, pero depende además de los objetos `states_sf` y `j2j_raw`, que no se guardan en disco por el script principal y deben estar activos en la sesión.

### Chapter2
El flujo recomendado es:
1. descargar las tablas ACS 5-Year 2023 desde `data.census.gov`;
2. ejecutar `01_main_analysis.R` (construye la base intermedia desde los CSV y la guarda como `ACS_depurada.RData`);
3. ejecutar `02_create_tables.R`;
4. ejecutar `03_create_figures.R`.

Los scripts secundarios dependen de objetos intermedios (`match_obj`, `df_base`, `datos_analisis_escalado`, `modelo_inla`, `prob_pred_att_obs`, entre otros) que solo existen en la sesión activa. Excepción: `ACS_depurada.RData` puede recargarse desde disco al inicio de los scripts secundarios si fue correctamente guardado por el script principal (véase la Sección 8).

### Chapter3
Ejecutar en orden numérico: `01_main_analysis.R`, `02_create_tables.R`, `03_create_figures.R`. El script de figuras (`03_create_figures.R`) lee íntegramente desde `3_Outputs/` y requiere en particular el archivo `panel_resiliencia_postshock_2022_2023.parquet`, generado por el script principal. El script `01_main_analysis.R` guarda tres entornos de trabajo parciales antes de cada estimación pesada, lo que permite reanudar el flujo en caso de interrupción.

---

## 6. Metodología paso a paso

### 6.1 Chapter1 — Modelo ENDID estatal

1. **Configuración del entorno.** Carga de paquetes y definición de rutas relativas con `here`. Validación de archivos de entrada. Semilla: `set.seed(2024)`.

2. **Importación de datos.** Lectura de microdatos IPUMS con `ipumsr::read_ipums_micro`, carga del shapefile estatal y del archivo J2J.

3. **Limpieza y parametrización.** Filtrado al período 2000–2024. Estandarización de `STATEFIP`. Deflactación a dólares de 2010. Ajuste de pesos (`PERWT/100`, `HHWT/100`). Definición del tratamiento: `AMZ = 1` si el estado supera **15 000** empleados directos de Amazon al cierre del Q4 2021.

4. **Construcción del panel agregado.** Salario promedio ponderado y controles demográficos/laborales a nivel estado-año.

5. **Modelo de gravedad PPML.** Estimación de conectividad bilateral origen–destino–año con efectos fijos por díada y año. Pseudo R² ≈ 0.951. Los coeficientes del tratamiento en origen (−0.173***) y destino (−0.152***) confirman que la presencia de Amazon altera los flujos bilaterales.

6. **Construcción del instrumento `Z_it` y de la exposición `S_it`.** Separación entre exposición observada y exposición predicha a partir de flujos.

7. **Estimación del modelo ENDID.** Primera etapa para la exposición; segunda etapa para el salario logarítmico. Bootstrap por bloques con 300 repeticiones (remuestreo por estado). F de primera etapa = 4 006. Prueba de endogeneidad: coeficiente del residuo = 0.133, p = 0.017. Resultados: β_dir = −0.046**, β_net = −0.109*.

8. **Validación y robustez.** Estudio de eventos (Wald conjunto de pretratamiento: 62.1, p < 0.001); placebo temporal; permutación de la exposición de red; comparación con DID estándar y NDID.

9. **Interpretación.** Dado el Wald conjunto significativo en el estudio de eventos, las magnitudes del capítulo deben leerse como **asociaciones condicionadas** al diseño, los controles y los supuestos del modelo, no como efectos causales nítidamente identificados.

10. **Guardado.** `Panel_Final.rds`, `Panel_Template.rds`, `modelos_salario.rds`.

---

### 6.2 Chapter2 — PSM espacial y BYM2 condal

1. **Configuración del entorno.** Semilla: `set.seed(1234)`. Instalación/carga de INLA y fmesher. Activación de tipografías con `showtext`.

2. **Importación y depuración de las tablas ACS.** Esta fase, la más extensa del capítulo, parte directamente de los archivos `ACSST5Y2023.Sxxxx-Data.csv` descargados por el replicador y produce la base intermedia `ACS_depurada.RData` (véase Sección 8). Los pasos son los siguientes:
   - Detección automática de los CSV mediante el patrón `^ACSST5Y2023\.S\d{4}-Data(\.csv)?$`.
   - Importación individual sin encabezado (`header = FALSE`).
   - Construcción de nombres de columna: fila 1 `@` fila 2; primera columna → `GEO_ID`.
   - Merge secuencial por `GEO_ID`.
   - Eliminación de columnas "Margin", conservación de columnas "Estimate", `GEO_ID` y `NAME`.
   - Eliminación de columnas con valores no deseados (`"N"`, `"null"`, `"(X)"`, `"*"`, `"."`, `".."`, `"..."`).
   - Limpieza de nombres y conversión numérica.
   - Guardado del objeto previo como `ACSST5Y2023_ESTIMATE_LIMPIOv0`.

   **Nota:** la versión de desarrollo del script puede cargar `ACS_depurada.RData` directamente mediante rutas absolutas heredadas. Para reproducibilidad completa, debe sustituirse esa carga por el flujo de construcción desde los CSV descrito aquí.

3. **Primer shapefile y LASSO.** Merge con `Dummy Condado.shp`, creación de `AMZ`, eliminación de geometría y selección de covariables con LASSO.

4. **Segundo shapefile y rama espacial principal.** Importación de `DummyCondado_Project_ExportFeatures.shp`, construcción de matriz Queen y merge secundario.

5. **Relativización y estandarización.** División de variables entre la población total del condado (`S0101_C01_001E`), salvo excepciones explícitas; estandarización con `scale()`.

6. **Malla INLA–SPDE.** Verificación del CRS planar y construcción con:
   ```r
   max.edge = c(50000, 100000)   # metros (50 km y 100 km)
   cutoff   = 5000
   offset   = c(50000, 100000)
   ```
   La malla resultante contiene aproximadamente **12 716 nodos** y **25 212 triángulos**.

7. **Modelo de propensity score espacial (INLA–SPDE).** Logit bayesiano con campo Matérn continuo. AUC ≈ 0.95. DIC = 1 078.5, WAIC = 1 078.7. Prueba de Moran sobre residuos: I ≈ −0.001, p > 0.5.

8. **Especificación alternativa BYM2.** Modelo discreto jerárquico para análisis de sensibilidad.

9. **Emparejamiento espacial.** Vecino más cercano sin reemplazo con `caliper = 0.1 SD`. Resultados: 188–192 pares según especificación.

10. **Estimación del ATT.** Cinco escenarios (BS-PSM, BS-PSM+E, PSM, PSM+E, BYM2 íntegro). ATT total BYM2: +2.3 % (IC 95 %: 0.2–4.4 %). El efecto se anula en condados con baja capacidad adaptativa y pierde significación en los de alta resiliencia.

---

### 6.3 Chapter3 — Resiliencia individual e IV espacial

1. **Configuración del entorno.** Semilla: `set.seed(2024)`. Parámetros globales del período pre-shock y post-shock.

2. **Preparación de microdatos.** Lectura de `usa_00078.xml` y de microdatos IPUMS. Armonización geográfica PUMA 2010 → 2020. Validación de conservación de pesos. Guardado: `datos_harmonizados_puma2020.parquet`.

3. **Construcción del panel analítico.** Filtrado, winsorización y carga de geometrías PUMA.

4. **Variables de aglomeración.** Potencial de mercado y densidad del empleo a nivel PUMA-año.

5. **Instrumentos sintéticos espaciales.** IV sintético ponderado, IV sintético no ponderado e IV-GL (Graph Laplacian).

6. **Modelo salarial pre-shock.** Ecuación tipo Mincer ampliada con efectos fijos de PUMA y año. Modelo preferido: IV sintético ponderado (Modelo 2), seleccionado mediante prueba Wu–Hausman (p = 0.014). Elasticidad del potencial de mercado ≈ 0.74.

7. **Predicción contrafactual.** Extrapolación de la trayectoria pre-shock para obtener el salario contrafactual 2022–2023.

8. **Índice de resiliencia.** `RP = (w_it − ŵ_it) / w_it`, acotado y winsorizado. Guardado del panel final: `panel_resiliencia_postshock_2022_2023.parquet`.

9. **Modelo de determinantes de resiliencia.** Estimaciones IV con instrumentos sintéticos espaciales. Muestra post-shock: 2 982 271 observaciones.

10. **Guardado.** Salidas procesadas, modelos, tablas, figuras y panel final.

---

## 7. Resultados esperados y outputs

**Advertencia.** Los archivos intermedios (`.rds`, `.parquet`, `.RData`) pueden contener información derivada de fuentes con restricciones de redistribución. Revisar cuidadosamente antes de publicar.

### Chapter1
| Archivo | Carpeta | Descripción |
|---|---|---|
| `Panel_Final.rds` | `1_Data_Processed/` | Panel estatal con exposición de red, tratamiento y controles |
| `Panel_Template.rds` | `1_Data_Processed/` | Plantilla del panel para bootstrap |
| `modelos_salario.rds` | `2_Models/` | Objetos de los modelos ENDID, NDID y DID |
| Figuras de red y tendencias | `3_Figures/` | Mapas, estudio de eventos y validaciones |

### Chapter2
| Archivo | Carpeta | Descripción |
|---|---|---|
| `ACS_depurada.RData` | `1_Data_Processed/` | Base ACS condal depurada, relativizada y estandarizada (ver §8) |
| `malla_condados.png` | `3_Figures/` | Validación visual de la malla INLA (12 716 nodos, 25 212 triángulos) |
| Curvas ROC, Love Plots, mapas | `3_Figures/` | Figuras del paper de Chapter2 |
| Tablas de balance y ATT | `4_Tables/` | Resultados del emparejamiento y del efecto promedio del tratamiento |

### Chapter3
| Archivo | Carpeta | Descripción |
|---|---|---|
| `datos_harmonizados_puma2020.parquet` | `1_Data_Processed/` | Microdatos armonizados a PUMA 2020 |
| `geometrias_puma_nacional.rds` | `1_Data_Processed/` | Geometrías PUMA con áreas y centroides |
| `datos_micro_nacional_full.rds` | `1_Data_Processed/` | Panel de microdatos procesados |
| `datos_aglomeracion_nacional_final.rds` | `1_Data_Processed/` | Variables de aglomeración por PUMA-año |
| `variables_aglomeracion_e_iv_nacional.rds` | `1_Data_Processed/` | Panel de aglomeración e instrumentos sintéticos |
| `sesion_para_bloque_6.RData` | `1_Data_Processed/` | Entorno de trabajo para la fase contrafactual |
| `panel_resiliencia_postshock_2022_2023.parquet` | `1_Data_Processed/` | Panel final de resiliencia post-shock (requerido por `03_create_figures.R`) |
| `entorno_completo_bloque6_antes_modelo_1.RData` | `2_Models/` | Entorno parcial antes del Modelo 1 |
| `entorno_completo_bloque6_antes_modelo_2.RData` | `2_Models/` | Entorno parcial antes del Modelo 2 |
| `entorno_completo_bloque6_antes_modelo_3.RData` | `2_Models/` | Entorno parcial antes del Modelo 3 |
| `modelo_1_iv_woodward.rds` | `2_Models/` | Modelo salarial IV-GL |
| `modelo_2_iv_sintetico_w.rds` | `2_Models/` | Modelo salarial IV sintético ponderado (preferido) |
| `modelo_3_iv_sintetico_uw.rds` | `2_Models/` | Modelo salarial IV sintético no ponderado |
| `info_modelo_caballo_de_batalla.rds` | `2_Models/` | Metadatos del modelo preferido |
| `modelo_caballo_de_batalla_lean.rds` | `2_Models/` | Objeto del modelo preferido |
| `lista_modelos_res.rds` | `2_Models/` | Modelos IV de resiliencia |
| `Tabla_1_Modelo_Salarios.xlsx` | `4_Tables/` | Factores que afectan el nivel salarial |
| `Tabla_2_Modelo_Resiliencia.xlsx` | `4_Tables/` | Determinantes de la resiliencia individual |
| `Tabla_3_Descripcion_Variables.xlsx` | `4_Tables/` | Descripción de variables |
| Mapas de potencial, densidad y resiliencia | `3_Figures/` | Figuras principales del chapter |

---

## 8. Nota sobre ACS_depurada.RData

El objeto `ACS_depurada.RData` es la **base intermedia** generada localmente por `Chapter2/01_main_analysis.R` a partir de las tablas ACS 5-Year 2023 descargadas desde `data.census.gov`. Corresponde al objeto R `datos_analisis_escalado` producido al final de la fase de depuración, relativización y estandarización.

**El insumo original replicable son las tablas CSV de la ACS.** `ACS_depurada.RData` es únicamente un producto derivado local cuyo nombre puede conservarse por compatibilidad con versiones previas del flujo.

### Flujo resumido de construcción
```text
Tablas ACS 5-Year 2023 (CSV)
    │
    ├─ importación sin encabezado
    ├─ construcción de nombres: fila1 @ fila2
    ├─ merge secuencial por GEO_ID
    ├─ eliminación de columnas "Margin"
    ├─ eliminación de columnas con valores no válidos
    ├─ limpieza de nombres y conversión numérica
    ├─ merge con shapefile condal (DummyCondado_Project_ExportFeatures.shp)
    ├─ creación de variable AMZ
    ├─ relativización por población
    ├─ estandarización con scale()
    ▼
ACS_depurada.RData  →  3_Outputs/1_Data_Processed/
```

### Ejemplo de guardado al final del bloque de estandarización
```r
datos_analisis_escalado <- data.frame(
  AMZ     = datos_completos$AMZ,
  GEOIDFQ = datos_completos$GEOIDFQ,
  variables_estandarizadas_todas
)

save(
  datos_analisis_escalado,
  file = here::here("3_Outputs", "1_Data_Processed", "ACS_depurada.RData")
)
```

### Rutina mínima de verificación
```r
load(here::here("3_Outputs", "1_Data_Processed", "ACS_depurada.RData"))

stopifnot(
  "datos_analisis_escalado" %in% ls(),
  "AMZ" %in% names(datos_analisis_escalado),
  "GEOIDFQ" %in% names(datos_analisis_escalado),
  nrow(datos_analisis_escalado) > 3000,
  sum(datos_analisis_escalado$AMZ == 1, na.rm = TRUE) > 0
)

cat("✅ ACS_depurada.RData cargado correctamente.\n")
cat("   Filas:", nrow(datos_analisis_escalado), "\n")
cat("   Columnas:", ncol(datos_analisis_escalado), "\n")
cat("   Condados con AMZ=1:", sum(datos_analisis_escalado$AMZ == 1, na.rm = TRUE), "\n")
cat("   Condados con AMZ=0:", sum(datos_analisis_escalado$AMZ == 0, na.rm = TRUE), "\n")
```

> **Nota:** el número exacto de filas depende de las columnas que superen los filtros de varianza y valores válidos. No se recomienda hardcodear un número de filas exacto en el `stopifnot`, sino verificar que el recuento sea razonable (> 3 000 condados contiguos de EE. UU.).

---

## 9. Limitaciones para la reproducibilidad

1. **Datos no redistribuibles (IPUMS USA).** Los extractos de IPUMS no pueden incorporarse al repositorio.
2. **Datos públicos no incluidos.** Los archivos LEHD, TIGER/Line, CRE y las tablas ACS no se versionan en el repositorio.
3. **Shapefiles con variable AMZ propia.** Algunos shapefiles contienen una variable de tratamiento construida por los autores y no son productos geográficos públicos estándar.
4. **Base intermedia construida localmente (Chapter2).** El preproceso de las tablas ACS puede tardar 15–30 minutos según el equipo.
5. **Dependencias de objetos en memoria (Chapter1 y Chapter2).** Los scripts secundarios no son completamente autocontenidos en todas sus fases.
6. **Rutas absolutas heredadas (Chapter2).** La versión de desarrollo puede contener rutas locales que deben sustituirse por `here::here()` antes de ejecutar en otro entorno.
7. **Variabilidad estocástica en INLA (Chapter2).** Puede haber diferencias numéricas menores entre ejecuciones.
8. **Demanda computacional elevada.** Chapter2 y Chapter3 pueden requerir 16–32 GB de RAM.
9. **Dependencia secuencial.** Los scripts deben ejecutarse en el orden indicado.
10. **Encabezados de scripts.** Algunos scripts conservan campos `[Título]`, `[Fecha]` y `[Versión]` sin completar.

---

## 10. Recomendaciones antes de publicar

- Dejar `1_Data/` vacía, salvo `.gitkeep` cuando sea necesario.
- No subir extractos de IPUMS USA ni derivados directos de esos microdatos.
- Mantener `3_Outputs/` fuera del control de versiones, salvo tablas/figuras finales verificadas.
- Sustituir cualquier ruta absoluta por `here::here()`.
- Verificar que `Chapter2/01_main_analysis.R` construya `ACS_depurada.RData` a partir de las tablas CSV (no que lo cargue desde una ruta absoluta local).
- Completar los campos `[Título]`, `[Fecha]` y `[Versión]` de los scripts.
- Verificar la presencia del bloque de guardado de la base intermedia al final de la estandarización en Chapter2.
- Considerar el guardado adicional de objetos intermedios que hoy obligan a mantener la sesión activa en Chapter1 y Chapter2.
- Registrar la versión exacta de INLA usada en Chapter2 (`INLA::inla.version()`).

---

## 11. Nota de uso académico

Este repositorio documenta el flujo analítico completo de los capítulos empíricos de la tesis doctoral *Métodos para analizar la resiliencia del comercio minorista en el contexto de la transformación digital* (Juan Pablo Rodríguez Paredes, Universidad Autónoma de Madrid, 2026).

Su propósito es ofrecer **trazabilidad metodológica** y **reproducibilidad computacional condicionada**. La reproducción íntegra requiere:
- acceso legítimo a las fuentes de datos originales,
- descarga directa de insumos por parte del replicador,
- procesamiento local,
- cumplimiento de términos de uso, licencias y obligaciones de cita.

El código se comparte con fines de transparencia metodológica y reproducibilidad científica. Cualquier uso derivado debe citar la tesis doctoral original y las fuentes de datos empleadas.

---

## 12. Contacto

**Juan Pablo Rodríguez Paredes**  
Universidad Autónoma de Madrid  
Programa de Doctorado en Economía y Empresa  
Correo: `jpablo.rodriguez@estudiante.uam.es`

**Dra. María del Coro Chasco Yrigoyen**  
Directora  
Correo: `coro.chasco@uam.es`

**Dra. Beatriz Sánchez Reyes**  
Tutora  
Correo: `beatriz.sanchez@uam.es`
