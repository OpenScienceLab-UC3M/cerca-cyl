# ============================================================
# 03 - Construir tabla base de municipios
# ============================================================
# Enlaza: municipios + demografia 2025 + Zona Basica de Salud (ZBS) +
# Area de Salud (gerencia) + recursos (farmacias, consultorios,
# centros de salud, hospitales generales) + capacidad ponderada por
# finalidades asistenciales.
#
# ENTRADA (mismo directorio de trabajo):
#   ./raw/registro-de-municipios-de-castilla-y-leon.xlsx
#   ./processed/demografia_2025.xlsx
#   ./raw/mapas-de-areas-de-salud-de-castilla-y-leon.xlsx (hoja "Hoja1")
#   ./processed/farmacias_con_coordenadas_final.xlsx
#   ./processed/centros_sanitarios_limpio.xlsx
#
# SALIDA:
#   ./processed/municipios_base.rds       (tabla municipio a municipio, para los scripts 04 y 05)
#   ./processed/centros_asignados.rds     (cada centro/farmacia con su Cod_INE asignado, para el script 04)

# --- 1. Paquetes ---------------------------------------------
paquetes <- c("readxl", "dplyr", "stringr", "tidyr", "purrr", "stringi", "geosphere", "jsonlite")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)
library(readxl); library(dplyr); library(stringr); library(tidyr); library(purrr); library(stringi); library(geosphere)

# Vecino(s) mas cercano(s) por distancia haversine REAL (esferica, con
# geosphere::distm) - devuelve nn.idx (indices en ref) y nn.dists (km).
vecino_haversine <- function(query_lat, query_lon, ref_lat, ref_lon, k = 1) {
  d_m <- distm(cbind(query_lon, query_lat), cbind(ref_lon, ref_lat), fun = distHaversine)  # metros
  n <- nrow(d_m)
  idx <- matrix(NA_integer_, nrow = n, ncol = k)
  dists_km <- matrix(NA_real_, nrow = n, ncol = k)
  for (i in seq_len(n)) {
    orden <- order(d_m[i, ])[seq_len(k)]
    idx[i, ] <- orden
    dists_km[i, ] <- d_m[i, orden] / 1000
  }
  list(nn.idx = idx, nn.dists = dists_km)
}

# --- 2. Normalizacion de nombres de municipio -----------------
# Mayusculas, sin tildes, sin puntuacion, y el articulo (EL/LA/LOS/LAS)
# movido al principio: "Bohodon (El)" / "Bohodon, El" -> "EL BOHODON"
normaliza <- function(s) {
  s <- str_to_upper(str_trim(s))
  s <- stri_trans_general(s, "Latin-ASCII")           # quita tildes/dieresis
  s <- str_replace_all(s, "[^A-Z0-9 ]", " ")
  s <- str_squish(s)
  palabras <- str_split(s, " ")
  map_chr(palabras, function(p) {
    if (length(p) > 1 && p[length(p)] %in% c("EL","LA","LOS","LAS")) {
      p <- c(p[length(p)], p[-length(p)])
    }
    paste(p, collapse = " ")
  })
}


# --- 3. Municipios + demografia -------------------------------
mun <- read_excel("./raw/registro-de-municipios-de-castilla-y-leon.xlsx") %>%
  mutate(Cod_INE = as.integer(Cod_INE), clave = normaliza(Municipio))

dem <- read_excel("./processed/demografia_2025.xlsx") %>%
  mutate(codigo_municipio = as.integer(codigo_municipio))

mun <- mun %>% left_join(dem, by = c("Cod_INE" = "codigo_municipio"))

message(sprintf("Municipios sin demografia: %d / %d", sum(is.na(mun$poblacion_total)), nrow(mun)))

# --- 3b. Dispersion REAL, ponderada por poblacion (unidades_poblacionales.xlsx) ---
# Sustituye al conteo crudo de "Entidades Locales Menores" (solo cubria el 20%
# de los municipios y no ponderaba por poblacion). Aqui se usa el desglose de
# poblacion por nucleo/diseminado del INE, disponible para el 100% de los
# municipios. La hoja de cada provincia tiene un codigo jerarquico:
#   000000 = total municipio (no sumar)
#   XXXX00 = total de una entidad (no sumar, ya esta en sus hijos)
#   XXXX01..98 = un nucleo con nombre propio (hoja)
#   XXXX99 = diseminado de esa entidad (hoja)
hojas_prov <- c("Avila","Burgos","Leon","Palencia","Salamanca","Segovia","Soria","Valladolid","Zamora")
up <- purrr::map_dfr(hojas_prov, ~ read_excel("./raw/unidades_poblacionales.xlsx", sheet = .x)) %>%
  mutate(
    Cod_INE = as.integer(Provincia) * 1000L + as.integer(Municipio),
    cod6 = str_extract(`Unidad Poblacional`, "^\\d{6}"),
    sufijo = str_sub(cod6, 5, 6),
    es_total_municipio = cod6 == "000000",
    es_diseminado = sufijo == "99",
    es_hoja = sufijo != "00" & !es_total_municipio
  )

hojas <- up %>% filter(es_hoja)

# Nucleo principal = la hoja NO diseminada con mas poblacion de cada municipio
principal <- hojas %>%
  filter(!es_diseminado) %>%
  group_by(Cod_INE) %>%
  slice_max(`Total 2025`, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Cod_INE, poblacion_nucleo_principal = `Total 2025`)

dispersion <- hojas %>%
  filter(`Total 2025` > 0) %>%
  count(Cod_INE, name = "n_nucleos_reales") %>%
  full_join(principal, by = "Cod_INE") %>%
  mutate(poblacion_nucleo_principal = replace_na(poblacion_nucleo_principal, 0))

mun <- mun %>%
  left_join(dispersion, by = "Cod_INE") %>%
  mutate(
    n_nucleos_reales = replace_na(n_nucleos_reales, 0),
    poblacion_nucleo_principal = coalesce(poblacion_nucleo_principal, poblacion_total),
    pct_poblacion_fuera_nucleo_principal = ifelse(
      poblacion_total > 0,
      100 * (1 - poblacion_nucleo_principal / poblacion_total),
      0
    )
  )
message(sprintf("Municipios con mas de 1 nucleo poblado: %d / %d",
                sum(mun$n_nucleos_reales > 1), nrow(mun)))

# --- 4. Zona Basica de Salud (ZBS) y Area de Salud (gerencia) --------
# El campo "NOMBRE GERENCIA" del fichero de ZBS es el Area de Salud real
# (11 areas, coinciden con las 9 provincias, duplicando Leon/El Bierzo y
# Valladolid Este/Oeste) - unidad correcta para agregar hospitales.
areas <- read_excel("./raw/mapas-de-areas-de-salud-de-castilla-y-leon.xlsx", sheet = "Hoja1")

# Correcciones manuales de nombres que no casan de forma automatica
# (cambios de nombre oficial, erratas de fuente, o nombres largos)
overrides <- c(
  "MANJABALAGO" = "MANJABALAGO Y ORTIGOSA DE RIOALMAR",
  "CASTRILLO MATAJUDIOS" = "CASTRILLO MOTA DE JUDIOS",
  "SAN ILDEFONSO" = "REAL SITIO DE SAN ILDEFONSO",
  "SANTA MARIA RIVARREDONDA" = "SANTA MARIA RIBARREDONDA",
  "VILLADECANES" = "TORAL DE LOS VADOS"
)
# Nota conocida: "CANDIN" (Leon) y "PRADALES" (Segovia) no aparecen en
# ningun listado de municipios de ZBS - es un hueco del fichero oficial,
# no un fallo de normalizacion; esos 2 municipios quedaran sin ZBS/gerencia.

zbs_mun <- areas %>%
  filter(!is.na(MUNICIPIO)) %>%
  mutate(municipio_raw = str_split(MUNICIPIO, "/")) %>%
  select(c_zbs_id, `Zona Básica de Salud`, `NOMBRE GERENCIA`, c_hospital, provincia, municipio_raw) %>%
  unnest(municipio_raw) %>%
  mutate(municipio_raw = str_trim(municipio_raw)) %>%
  filter(municipio_raw != "") %>%
  mutate(clave = normaliza(municipio_raw)) %>%
  mutate(clave = recode(clave, !!!overrides))

# Corrales: desambiguar por provincia (hay dos municipios "Corrales" distintos)
zbs_mun <- zbs_mun %>%
  mutate(clave = case_when(
    clave == "CORRALES" & provincia == "Valladolid" ~ "CORRALES DE DUERO",
    clave == "CORRALES" & provincia == "Zamora"      ~ "CORRALES DEL VINO",
    TRUE ~ clave
  ))

# Un municipio puede aparecer en varias ZBS (capitales grandes divididas en
# varias ZBS urbanas) - nos quedamos con la primera para tener 1 ZBS por
# municipio (simplificacion asumida y documentada en la metodologia).
zbs_mun_1a1 <- zbs_mun %>% distinct(clave, .keep_all = TRUE)

# NOTA IMPORTANTE (descubierto tras revisar el ITAS-HO): "gerencia" (NOMBRE
# GERENCIA) es la unidad administrativa correcta para primaria/ZBS, pero NO
# siempre coincide con el area real de un hospital. En Burgos y Valladolid
# Este, una misma "gerencia" agrupa mas de un hospital real distinto
# (comprobado con el campo c_hospital del propio fichero). area_hospital_id
# es la unidad correcta para todo lo relacionado con HOSPITAL (candidatos,
# distancia propia, ratio de carga) - gerencia se sigue usando tal cual para
# todo lo demas (ya es correcta para primaria).
mun <- mun %>%
  left_join(
    zbs_mun_1a1 %>% select(clave, zbs_id = c_zbs_id, zbs_nombre = `Zona Básica de Salud`,
                           gerencia = `NOMBRE GERENCIA`, area_hospital_id = c_hospital),
    by = "clave"
  )
message(sprintf("Municipios sin ZBS/gerencia asignada: %d / %d (hueco conocido del fichero oficial)",
                sum(is.na(mun$zbs_id)), nrow(mun)))

# --- 4b. Respaldo espacial para los que no casaron por nombre --------------
# El fichero oficial no lista por nombre a un puñado de municipios muy
# pequeños en ninguna ZBS (omision de la fuente, no falta de ZBS real). En
# vez de dejarlos sin asignar, se comprueba dentro de que poligono real de
# ZBS caen sus propias coordenadas (Latitud/Longitud) - mas fiable que
# depender de que el municipio apareciera listado en el texto, porque usa
# la geometria oficial directamente. Solo se aplica a los que YA fallaron
# por nombre; a nadie mas se le cambia su ZBS.
if (sum(is.na(mun$zbs_id)) > 0) {
  ruta_geojson <- "./raw/mapas-de-areas-de-salud-de-castilla-y-leon.geojson"
  if (!file.exists(ruta_geojson)) {
    stop(sprintf(
      "No se encuentra el fichero '%s'. Este paso (respaldo espacial de ZBS) necesita el .geojson oficial de zonas de salud en la carpeta ./raw/ - copialo ahi y vuelve a lanzar el script.",
      ruta_geojson
    ))
  }
  con_geojson <- file(ruta_geojson, encoding = "UTF-8")
  geo <- jsonlite::fromJSON(con_geojson, simplifyVector = FALSE)
  # No se cierra la conexion a mano: fromJSON() ya la consume y cierra
  # internamente al leer de un objeto connection - un close() aqui
  # encima daria "conexion invalida" y cortaria el resto de este bloque.

  # Ray casting: cuenta cuantas veces un rayo horizontal desde el punto hacia
  # +infinito cruza los lados del anillo - impar = dentro, par = fuera.
  punto_en_anillo <- function(x, y, anillo) {
    n <- length(anillo)
    dentro <- FALSE
    p1 <- anillo[[n]]
    for (i in seq_len(n)) {
      p2 <- anillo[[i]]
      x1 <- p1[[1]]; y1 <- p1[[2]]; x2 <- p2[[1]]; y2 <- p2[[2]]
      if (y1 != y2 && ((y1 > y) != (y2 > y))) {
        xint <- x1 + (y - y1) * (x2 - x1) / (y2 - y1)
        if (x < xint) dentro <- !dentro
      }
      p1 <- p2
    }
    dentro
  }
  punto_en_poligono <- function(x, y, coords_poligono) {
    if (!punto_en_anillo(x, y, coords_poligono[[1]])) return(FALSE)
    if (length(coords_poligono) > 1) {
      for (agujero in coords_poligono[-1]) if (punto_en_anillo(x, y, agujero)) return(FALSE)
    }
    TRUE
  }
  punto_en_feature <- function(x, y, geometry) {
    if (geometry$type == "Polygon") {
      punto_en_poligono(x, y, geometry$coordinates)
    } else if (geometry$type == "MultiPolygon") {
      any(vapply(geometry$coordinates, function(p) punto_en_poligono(x, y, p), logical(1)))
    } else FALSE
  }
  centroide <- function(feat) {
    gp <- feat$properties$geo_point_2d
    if (is.null(gp)) return(c(NA_real_, NA_real_))
    c(gp$lon, gp$lat)
  }
  encontrar_zbs_espacial <- function(lon, lat) {
    for (feat in geo$features) {
      if (punto_en_feature(lon, lat, feat$geometry)) return(feat$properties)
    }
    # Respaldo: el ZBS con el centroide mas cercano (punto justo en un borde/precision)
    cent <- t(vapply(geo$features, centroide, numeric(2)))
    d2 <- (cent[,1] - lon)^2 + (cent[,2] - lat)^2
    geo$features[[which.min(d2)]]$properties
  }

  # Mapa completo ZBS -> nombre/gerencia/area_hospital, desde TODAS las 247
  # ZBS del fichero (zbs_mun, antes del "1 ZBS por municipio" de mas arriba,
  # para no perder ninguna).
  mapa_zbs_completo <- zbs_mun %>% distinct(c_zbs_id, .keep_all = TRUE) %>%
    select(c_zbs_id, zbs_nombre_sp = `Zona Básica de Salud`,
          gerencia_sp = `NOMBRE GERENCIA`, area_hospital_id_sp = c_hospital)

  pendientes <- mun %>% filter(is.na(zbs_id)) %>% select(Cod_INE, Latitud, Longitud)
  resueltos_espacial <- purrr::pmap_dfr(pendientes, function(Cod_INE, Latitud, Longitud) {
    props <- encontrar_zbs_espacial(Longitud, Latitud)
    tibble(Cod_INE = Cod_INE, c_zbs_id = props$c_zbs_id)
  }) %>% left_join(mapa_zbs_completo, by = "c_zbs_id")

  mun <- mun %>%
    left_join(resueltos_espacial, by = "Cod_INE") %>%
    mutate(
      zbs_asignacion_espacial = !is.na(zbs_nombre_sp) & is.na(zbs_nombre),
      zbs_id = coalesce(zbs_id, c_zbs_id),
      zbs_nombre = coalesce(zbs_nombre, zbs_nombre_sp),
      gerencia = coalesce(gerencia, gerencia_sp),
      area_hospital_id = coalesce(area_hospital_id, area_hospital_id_sp)
    ) %>%
    select(-c_zbs_id, -zbs_nombre_sp, -gerencia_sp, -area_hospital_id_sp)
  # Nota: zbs_id de estos municipios ahora SI es un c_zbs_id real y valido -
  # entran correctamente en la carga/poblacion agregada de su ZBS mas
  # adelante (Seccion 7), no se quedan con ratio_ponderado_primaria en NA.
  message(sprintf("Resueltos por posicion geografica (dentro de que ZBS caen sus coordenadas): %d",
                  sum(mun$zbs_asignacion_espacial, na.rm = TRUE)))
} else {
  mun$zbs_asignacion_espacial <- FALSE
}
message(sprintf("Municipios sin ZBS/gerencia asignada tras el respaldo espacial: %d / %d (hueco conocido del fichero oficial)",
                sum(is.na(mun$gerencia)), nrow(mun)))

# --- 5. Farmacias por municipio (join exacto por codigo INE) --------
far <- read_excel("./processed/farmacias_con_coordenadas_final.xlsx") %>%
  mutate(codigo_municipio = as.integer(codigo_municipio))

n_far <- far %>% count(codigo_municipio, name = "n_farmacias")
mun <- mun %>%
  left_join(n_far, by = c("Cod_INE" = "codigo_municipio")) %>%
  mutate(n_farmacias = replace_na(n_farmacias, 0))

# --- 6. Centros sanitarios: consultorio / centro de salud / hospital general ---
cs <- read_excel("./processed/centros_sanitarios_limpio.xlsx")

cs_rel <- cs %>%
  filter(
    recurso %in% c("Centro de salud", "Consultorio") |
      (recurso == "Hospital" & subtipo_centro == "Hospital general")
  ) %>%
  mutate(clave = normaliza(localidad_normalizada))

match_nombre <- cs_rel %>% left_join(mun %>% select(clave, Cod_INE), by = "clave")

# Fallback espacial (vecino mas cercano REAL, haversine) para las filas que
# no casan por nombre - habitual porque muchos centros estan en pedanias
# con nombre distinto al de su municipio.
sin_match <- match_nombre %>% filter(is.na(Cod_INE))
if (nrow(sin_match) > 0) {
  vecino <- vecino_haversine(sin_match$latitud, sin_match$longitud, mun$Latitud, mun$Longitud, k = 1)
  sin_match$Cod_INE <- mun$Cod_INE[vecino$nn.idx[, 1]]
  match_nombre <- match_nombre %>% filter(!is.na(Cod_INE)) %>% bind_rows(sin_match)
}





centros_asignados <- match_nombre

# --- 6b. area_hospital_id para los hospitales publicos, por numero_registro --
# NO se puede derivar automaticamente desde el municipio del hospital (como
# se hace para el resto): Valladolid capital tiene 2 hospitales publicos
# reales (Clinico y Rio Hortega) que perteneces a areas distintas, y Avila
# tiene 2 (Sonsoles y Provincial) que SI son la misma area - una simple
# busqueda "que area tiene el municipio de este hospital" confundiria ambos
# casos. Tabla verificada a mano cruzando cada hospital con su ZBS real
# (ver metodologia, apartado de correccion del ITAS-HO).
area_hospital_lookup <- tibble::tribble(
  ~numero_registro, ~area_hospital_id,
  "09-C11-0004", 1,   # Hospital Santiago Apostol (Miranda de Ebro)
  "09-C11-0003", 2,   # Hospital Santos Reyes (Aranda de Duero)
  "47-C11-0003", 3,   # Hospital Medina del Campo
  "24-C11-0003", 4,   # Hospital El Bierzo (Ponferrada)
  "47-C11-0001", 5,   # Hospital Clinico Universitario de Valladolid
  "47-C11-0008", 6,   # Hospital Universitario Rio Hortega (Valladolid)
  "05-C190-0001", 7,  # Hospital Provincial de Avila
  "05-C11-0001", 7,   # Hospital Nuestra Senora de Sonsoles (Avila)
  "09-C11-0007", 8,   # Hospital Universitario de Burgos
  "24-C11-0002", 9,   # Hospital de Leon
  "34-C11-0001", 10,  # Hospital Rio Carrion (Palencia)
  "34-C11-0002", 10,  # Hospital San Telmo (Palencia)
  "37-C11-0001", 11,  # Hospital Universitario de Salamanca
  "37-C11-0002", 11,  # Hospital Los Montalvos (Salamanca)
  "40-C11-0001", 12,  # Hospital General de Segovia
  "42-C11-0001", 13,  # Hospital Santa Barbara (Soria)
  "49-C11-0004", 14,  # Hospital de Benavente (Zamora)
  "49-C11-0003", 14,  # Hospital Provincial de Zamora
  "49-C11-0002", 14   # Hospital Virgen de la Concha (Zamora)
)
centros_asignados <- centros_asignados %>% left_join(area_hospital_lookup, by = "numero_registro")

saveRDS(centros_asignados, "./processed/centros_asignados.rds")

n_primaria <- centros_asignados %>% filter(recurso %in% c("Centro de salud","Consultorio")) %>%
  count(Cod_INE, name = "n_primaria")
n_consultorio <- centros_asignados %>% filter(recurso == "Consultorio") %>% count(Cod_INE, name = "n_consultorio")
n_centro_salud <- centros_asignados %>% filter(recurso == "Centro de salud") %>% count(Cod_INE, name = "n_centro_salud")
n_hospitales <- centros_asignados %>% filter(recurso == "Hospital") %>% count(Cod_INE, name = "n_hospitales")

mun <- mun %>%
  left_join(n_primaria, by = "Cod_INE") %>%
  left_join(n_consultorio, by = "Cod_INE") %>%
  left_join(n_centro_salud, by = "Cod_INE") %>%
  left_join(n_hospitales, by = "Cod_INE") %>%
  mutate(across(c(n_primaria, n_consultorio, n_centro_salud, n_hospitales), ~replace_na(., 0)))

# --- 7. Capacidad ponderada por finalidades asistenciales ------------
cap_primaria_mun <- centros_asignados %>%
  filter(recurso %in% c("Centro de salud","Consultorio")) %>%
  group_by(Cod_INE) %>% summarise(capacidad_primaria = sum(n_finalidades, na.rm = TRUE))

mun <- mun %>%
  left_join(cap_primaria_mun, by = "Cod_INE") %>%
  mutate(capacidad_primaria = replace_na(capacidad_primaria, 0))

cap_primaria_zbs <- mun %>% filter(!is.na(zbs_id)) %>%
  group_by(zbs_id) %>%
  summarise(capacidad_primaria_zbs = sum(capacidad_primaria),
            poblacion_zbs = sum(poblacion_total))
mun <- mun %>% left_join(cap_primaria_zbs, by = "zbs_id") %>%
  mutate(ratio_ponderado_primaria = poblacion_zbs / capacidad_primaria_zbs)

ratio_far_zbs <- mun %>% filter(!is.na(zbs_id)) %>%
  group_by(zbs_id) %>%
  summarise(n_farmacias_zbs = sum(n_farmacias))
mun <- mun %>% left_join(ratio_far_zbs, by = "zbs_id") %>%
  mutate(ratio_hab_por_farmacia = poblacion_zbs / n_farmacias_zbs)

cap_hosp_gerencia <- centros_asignados %>%
  filter(recurso == "Hospital") %>%
  left_join(mun %>% select(Cod_INE, gerencia), by = "Cod_INE") %>%
  group_by(gerencia) %>% summarise(capacidad_hospital = sum(n_finalidades, na.rm = TRUE))

pob_gerencia <- mun %>% filter(!is.na(gerencia)) %>%
  group_by(gerencia) %>% summarise(poblacion_gerencia = sum(poblacion_total))

ger <- pob_gerencia %>% left_join(cap_hosp_gerencia, by = "gerencia") %>%
  mutate(ratio_ponderado_hospital = poblacion_gerencia / capacidad_hospital)

mun <- mun %>% left_join(ger %>% select(gerencia, ratio_ponderado_hospital), by = "gerencia")

# --- 8. Guardar --------------------------------------------------------
saveRDS(mun, "./processed/municipios_base.rds")
message(sprintf("\nListo: municipios_base.rds (%d municipios) y centros_asignados.rds (%d recursos)",
                nrow(mun), nrow(centros_asignados)))
