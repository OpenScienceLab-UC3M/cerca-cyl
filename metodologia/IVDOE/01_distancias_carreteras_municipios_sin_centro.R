# ============================================================================
# 01_distancias_carreteras_municipios_sin_centro.R
#
# Para cada municipio de Castilla y León SIN centro educativo propio, calcula
# la distancia real por carretera (no en línea recta) hasta el centro
# educativo público más cercano.
#
# Método:
#   1. Preselección de los 10 centros más cercanos a cada municipio en línea
#      recta (sf::st_distance), para no calcular rutas contra los ~841
#      centros de la región.
#   2. Descarga de la red viaria de Castilla y León desde OpenStreetMap
#      (osmextract) y construcción de un grafo de rutas (dodgr).
#   3. Cálculo de la distancia real por carretera de cada municipio a sus
#      centros candidatos (dodgr::dodgr_dists) y selección del más cercano.
#
# Entrada:  IVDOE.xlsx
#             - hoja "sin-centros"    (municipios sin centro educativo)
#             - hoja "centros-filtros" (centros educativos seleccionados)
# Salida:   distancias_carreteras_municipios_sin_centro.xlsx
# ============================================================================

setwd("C:/Users/alvar/Desktop/CyL datos abiertos/ultimo-indicador/R") # Ajustar a la ruta local

# ----------------------------------------------------------------------------
# 0. Paquetes necesarios (ejecutar una única vez)
# ----------------------------------------------------------------------------
# install.packages(c(
#   "readxl", "dplyr", "sf", "writexl", "purrr",
#   "osmextract", "dodgr"
# ))

library(readxl)
library(dplyr)
library(sf)
library(writexl)
library(purrr)
library(osmextract)
library(dodgr)

# ----------------------------------------------------------------------------
# 1. Carga de datos
# ----------------------------------------------------------------------------
archivo <- "IVDOE.xlsx"

municipios <- read_excel(archivo, sheet = "sin-centros")
centros    <- read_excel(archivo, sheet = "centros-filtros")

cat("========================================\n")
cat("DATOS CARGADOS\n")
cat("========================================\n")
cat("Municipios sin centro:", nrow(municipios), "\n")
cat("Centros educativos:", nrow(centros), "\n\n")

cat("Municipios sin coordenadas:",
    sum(is.na(municipios$LONGITUD) | is.na(municipios$LATITUD)), "\n")
cat("Centros sin coordenadas:",
    sum(is.na(centros$`COORD. LONGITUD`) | is.na(centros$`COORD. LATITUD`)), "\n\n")

# ----------------------------------------------------------------------------
# 2. Construcción de objetos espaciales (sf)
# ----------------------------------------------------------------------------
municipios_sf <- municipios %>%
  filter(!is.na(LONGITUD), !is.na(LATITUD)) %>%
  st_as_sf(
    coords = c("LONGITUD", "LATITUD"),
    crs = 4326,
    remove = FALSE
  )

centros_sf <- centros %>%
  filter(!is.na(`COORD. LONGITUD`), !is.na(`COORD. LATITUD`)) %>%
  st_as_sf(
    coords = c("COORD. LONGITUD", "COORD. LATITUD"),
    crs = 4326,
    remove = FALSE
  )

cat("========================================\n")
cat("COORDENADAS\n")
cat("========================================\n")
cat("Municipios con coordenadas:", nrow(municipios_sf), "\n")
cat("Centros con coordenadas:", nrow(centros_sf), "\n\n")

# ----------------------------------------------------------------------------
# 3. Red viaria de Castilla y León (OpenStreetMap)
# ----------------------------------------------------------------------------
cat("========================================\n")
cat("OPENSTREETMAP\n")
cat("========================================\n")
cat("Descargando/cargando red de Castilla y León...\n\n")

red_osm <- oe_get("Castilla y Leon", quiet = FALSE)

cat("Elementos OSM descargados:", nrow(red_osm), "\n\n")

cat("========================================\n")
cat("FILTRANDO RED VIARIA\n")
cat("========================================\n")

red_carreteras <- red_osm %>%
  filter(
    highway %in% c(
      "motorway", "motorway_link",
      "trunk", "trunk_link",
      "primary", "primary_link",
      "secondary", "secondary_link",
      "tertiary", "tertiary_link",
      "residential", "unclassified",
      "road", "living_street"
    )
  )

cat("Segmentos de carretera:", nrow(red_carreteras), "\n\n")

# ----------------------------------------------------------------------------
# 4. Construcción del grafo de rutas (dodgr)
# ----------------------------------------------------------------------------
cat("========================================\n")
cat("CREANDO GRAFO\n")
cat("========================================\n")
cat("Esto puede tardar unos minutos...\n\n")

grafo <- weight_streetnet(red_carreteras, wt_profile = "motorcar")

cat("Aristas del grafo:", nrow(grafo), "\n\n")

# ----------------------------------------------------------------------------
# 5. Preselección de centros candidatos (distancia en línea recta)
# ----------------------------------------------------------------------------
# Para cada municipio, nos quedamos con sus 10 centros más cercanos en línea
# recta. Sobre ese subconjunto (mucho más pequeño que los ~841 centros
# totales) se calculará después la distancia real por carretera.
cat("========================================\n")
cat("BUSCANDO CENTROS CANDIDATOS\n")
cat("========================================\n")
cat("Calculando distancias geográficas...\n")

matriz_dist <- st_distance(municipios_sf, centros_sf)
matriz_km   <- units::drop_units(matriz_dist) / 1000

candidatos <- apply(
  matriz_km, 1,
  function(x) order(x)[1:min(10, length(x))]
)

cat("Candidatos calculados.\n\n")

# ----------------------------------------------------------------------------
# 6. Cálculo de distancias reales por carretera (dodgr)
# ----------------------------------------------------------------------------
# Se procesan TODOS los municipios sin centro en una única llamada a
# dodgr_dists: origenes = todos los municipios, destinos = la unión de todos
# sus centros candidatos (evita recorrer la red municipio a municipio).
indices_municipios <- seq_len(nrow(municipios_sf))

indices_centros <- unique(as.vector(candidatos[, indices_municipios, drop = FALSE]))

municipios_calc <- municipios_sf[indices_municipios, ]
centros_calc    <- centros_sf[indices_centros, ]

origenes <- data.frame(
  lon = municipios_calc$LONGITUD,
  lat = municipios_calc$LATITUD
)

destinos <- data.frame(
  lon = centros_calc$`COORD. LONGITUD`,
  lat = centros_calc$`COORD. LATITUD`
)

cat("========================================\n")
cat("CALCULANDO DISTANCIAS POR CARRETERA\n")
cat("========================================\n")
cat("Municipios:", nrow(origenes), " | Centros candidatos:", nrow(destinos), "\n")
cat("Puede tardar varios minutos...\n\n")

distancias_carretera <- dodgr_dists(
  graph = grafo,
  from = origenes,
  to = destinos,
  shortest = TRUE,
  pairwise = FALSE,
  parallel = TRUE,
  quiet = FALSE
)

cat("Dimensiones de la matriz de distancias:",
    paste(dim(distancias_carretera), collapse = " x "), "\n\n")

# ----------------------------------------------------------------------------
# 7. Selección del centro más cercano por carretera
# ----------------------------------------------------------------------------
mejor_centro       <- apply(distancias_carretera, 1, which.min)
distancia_minima_m <- apply(distancias_carretera, 1, min)
distancia_minima_km <- distancia_minima_m / 1000

# ----------------------------------------------------------------------------
# 8. Tabla de resultados
# ----------------------------------------------------------------------------
resultado <- tibble(
  MUNICIPIO = municipios_calc$MUNICIPIO,
  CODIGO_MUNICIPIO = municipios_calc$C.POSTAL,
  POBLACION_ESCOLAR_POTENCIAL = municipios_calc$`POBLACION ESCOLAR POTENCIAL`,
  CENTRO_MAS_CERCANO = centros_calc$`DENOMINACIÓN ESPECÍFICA`[mejor_centro],
  CODIGO_CENTRO = centros_calc$CÓDIGO[mejor_centro],
  MUNICIPIO_CENTRO = centros_calc$MUNICIPIO[mejor_centro],
  DISTANCIA_CARRETERA_KM = round(distancia_minima_km, 2)
) %>%
  arrange(CODIGO_MUNICIPIO)

cat("========================================\n")
cat("RESULTADO\n")
cat("========================================\n")
print(resultado)

# ----------------------------------------------------------------------------
# 9. Exportación
# ----------------------------------------------------------------------------
write_xlsx(
  resultado,
  "distancias_carreteras_municipios_sin_centro.xlsx"
)

cat("\nArchivo exportado: distancias_carreteras_municipios_sin_centro.xlsx\n")
