# ============================================================
# 4.1 - Generar candidatos para distancia por carretera
# ============================================================
# Para cada municipio y cada tipo de recurso (farmacia, consultorio,
# centro_salud, hospital), preselecciona los 3 recursos mas cercanos
# EN LINEA RECTA (distancia haversine real, via geosphere::distm - exacta,
# no una aproximacion). El resto de este script usa esa lista para
# pedirle a OSRM la distancia REAL por carretera solo a esos candidatos,
# evitando calcular la ruta contra la totalidad de recursos.
#
# ENTRADA: ./processed/municipios_base.rds y ./processed/centros_asignados.rds (salida del script 03)
#          farmacias_con_coordenadas_final.xlsx
# SALIDA:  ./processed/candidatos_carretera.xlsx

paquetes <- c("readxl", "writexl", "dplyr", "purrr", "geosphere")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)
library(readxl); library(writexl); library(dplyr); library(purrr); library(geosphere)

# Vecino(s) mas cercano(s) por distancia haversine REAL (geosphere::distm,
# exacta) - misma funcion que en 03-construir_base_municipios.R.
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

K <- 3  # numero de candidatos por municipio y tipo

mun <- readRDS("./processed/municipios_base.rds")
centros <- readRDS("./processed/centros_asignados.rds")
far <- read_excel("./processed/farmacias_con_coordenadas_final.xlsx")

# Genera los K candidatos mas cercanos (en linea recta, haversine exacto) de
# un conjunto de recursos (rec_lat/rec_lon/rec_id) para cada municipio de mun_df.
candidatos_tipo <- function(mun_df, rec_lat, rec_lon, rec_id, tipo, k = K) {
  k <- min(k, length(rec_lat))
  vecinos <- vecino_haversine(mun_df$Latitud, mun_df$Longitud, rec_lat, rec_lon, k = k)
  purrr::map_dfr(seq_len(nrow(mun_df)), function(i) {
    idx <- vecinos$nn.idx[i, ]
    tibble(
      Cod_INE = mun_df$Cod_INE[i],
      muni_lat = mun_df$Latitud[i],
      muni_lon = mun_df$Longitud[i],
      tipo = tipo,
      candidato_rank = seq_along(idx),
      candidato_id = rec_id[idx],
      candidato_lat = rec_lat[idx],
      candidato_lon = rec_lon[idx],
      dist_recta_km = vecinos$nn.dists[i, ]
    )
  })
}

consultorio  <- centros %>% filter(recurso == "Consultorio")
centro_salud <- centros %>% filter(recurso == "Centro de salud")
hospital     <- centros %>% filter(recurso == "Hospital")  # ya filtrado a Hospital general en el script 01

c_far <- candidatos_tipo(mun, far$lat, far$long, far$numero_registro, "farmacia")
c_con <- candidatos_tipo(mun, consultorio$latitud, consultorio$longitud, consultorio$numero_registro, "consultorio")
c_cs  <- candidatos_tipo(mun, centro_salud$latitud, centro_salud$longitud, centro_salud$numero_registro, "centro_salud")
c_hos <- candidatos_tipo(mun, hospital$latitud, hospital$longitud, hospital$numero_registro, "hospital")

candidatos_all <- bind_rows(c_far, c_con, c_cs, c_hos)

write_xlsx(candidatos_all, "./processed/candidatos_carretera.xlsx")
message(sprintf("Listo: candidatos_carretera.xlsx (%d filas, %d municipios x 4 tipos x %d candidatos)",
                nrow(candidatos_all), nrow(mun), K))







# ============================================================
# 04.2. Distancia real por carretera (OSRM) a farmacia / primaria / hospital
# ============================================================
# Parte de 'candidatos_carretera.xlsx': para cada municipio y tipo de
# recurso (farmacia, primaria, hospital) trae los 3 candidatos más
# cercanos EN LÍNEA RECTA. Este script pide a OSRM (router.project-osrm.org,
# gratuito, sin API key) la distancia real por carretera a cada uno,
# agrupando los 9 candidatos (3 tipos x 3 candidatos) de cada municipio
# en una sola consulta de tipo "tabla" (1 origen x 9 destinos).
#
# Con 2.248 municipios y ~1 petición/segundo (uso razonable del servidor
# público), tardará aprox. 35-40 minutos. Usa checkpoint para reanudar.
#
# IMPORTANTE: el servidor público de OSRM es para pruebas/uso puntual,
# no para producción continua. Para un uso recurrente, lo correcto sería
# levantar tu propio servidor OSRM (Docker, con el extracto de España)
# o usar un proveedor de pago (Google Distance Matrix, Mapbox, etc.).

# --- 1. Paquetes ---------------------------------------------
paquetes <- c("readxl", "writexl", "httr", "jsonlite", "dplyr")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(readxl)
library(writexl)
library(httr)
library(jsonlite)
library(dplyr)

# --- 2. Cargar candidatos ------------------------------------------
archivo_entrada <- "./processed/candidatos_carretera.xlsx"
archivo_salida  <- "./processed/distancias_carretera.xlsx"
archivo_parcial <- "./processed/distancias_carretera_progreso.rds"

candidatos <- read_excel(archivo_entrada)

municipios <- candidatos %>% distinct(Cod_INE, muni_lat, muni_lon)

# --- 3. Reanudar si hay checkpoint ---------------------------------
if (file.exists(archivo_parcial)) {
  resultados <- readRDS(archivo_parcial)
  message("Reanudando: ", length(unique(resultados$Cod_INE[!is.na(resultados$dist_carretera_km)])),
          " municipios ya resueltos.")
} else {
  resultados <- candidatos %>% mutate(dist_carretera_km = NA_real_, duracion_min = NA_real_)
}

pendientes <- resultados %>%
  group_by(Cod_INE) %>%
  summarise(sin_resolver = all(is.na(dist_carretera_km)), .groups = "drop") %>%
  filter(sin_resolver) %>%
  pull(Cod_INE)
# (antes: sapply(split(...)) indexando resultados$Cod_INE directamente - el
# resultado de sapply tiene un valor POR MUNICIPIO, pero resultados$Cod_INE
# tiene una fila POR CANDIDATO; longitudes distintas => R reciclaba el vector
# corto al indexar, desalineando que fila corresponde a que municipio. Ver
# 06_corregir_hospital_propio.R para el caso donde esto se detecto.)

# --- 3b. Peticion a OSRM con reintento automatico ---------------------------
# Los fallos puntuales durante una secuencia larga de peticiones (miles
# seguidas al servidor publico) parecen ser transitorios: la misma consulta,
# aislada, suele funcionar sin problema. En vez de dejar el municipio en NA
# y tener que relanzar el script a mano, se reintenta un par de veces con una
# pequena espera antes de rendirse.
osrm_get_con_reintentos <- function(url, intentos = 3, esperas_seg = c(5, 15, 40)) {
  for (intento in seq_len(intentos)) {
    resp <- tryCatch(GET(url, timeout(20)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      datos <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")), error = function(e) NULL)
      # Aceptar solo si el JSON es valido Y OSRM confirma "code":"Ok" con
      # distancias presentes. Bajo carga, el servidor publico a veces
      # responde HTTP 200 pero con un code de error o sin "distances" -
      # antes se aceptaba como bueno sin reintentar, dejando esa fila vacia.
      if (!is.null(datos) && !is.null(datos$code) && datos$code == "Ok" && !is.null(datos$distances)) {
        return(datos)
      }
    }
    if (intento < intentos) Sys.sleep(esperas_seg[min(intento, length(esperas_seg))])
  }
  NULL
}

# --- 4. Consultar OSRM (tabla 1 origen x hasta 9 destinos) por municipio ----
base_url <- "http://router.project-osrm.org/table/v1/driving/"

for (i in seq_along(pendientes)) {
  cod <- pendientes[i]
  tryCatch({
    filas <- which(resultados$Cod_INE == cod)
    fila_ref <- resultados[filas[1], ]

    # coordenadas: origen (municipio) + destinos (candidatos), formato lon,lat
    coords <- paste0(fila_ref$muni_lon, ",", fila_ref$muni_lat)
    for (f in filas) {
      coords <- paste0(coords, ";", resultados$candidato_lon[f], ",", resultados$candidato_lat[f])
    }
    n_destinos <- length(filas)
    destinations <- paste(1:n_destinos, collapse = ";")

    url <- paste0(base_url, coords, "?sources=0&destinations=", destinations,
                  "&annotations=distance,duration")

    datos <- osrm_get_con_reintentos(url)
    if (!is.null(datos)) {
      resultados$dist_carretera_km[filas] <- as.numeric(unlist(datos$distances)) / 1000
      resultados$duracion_min[filas]      <- as.numeric(unlist(datos$durations)) / 60
    }
  }, error = function(e) {
    message(sprintf("  [AVISO] fallo en municipio Cod_INE=%s (i=%d): %s - se continua con el siguiente", cod, i, conditionMessage(e)))
  })

  # Respeto al servidor público: 1 petición/segundo aprox.
  Sys.sleep(1)

  if (i %% 25 == 0 || i == length(pendientes)) {
    saveRDS(resultados, archivo_parcial)
    message(sprintf("Progreso: %d/%d municipios", i, length(pendientes)))
  }
}

# --- 4b. Segunda pasada "en frio" para lo que siga sin resolver -------------
# Si el servidor publico se satura tras un uso sostenido, un reintento
# inmediato hereda ese mismo bloqueo. Aqui se espera un buen rato (imitando
# una consulta aislada, como la del diagnostico) y se reintenta una ultima
# vez solo lo que sigue pendiente.
pendientes_2a <- resultados %>%
  group_by(Cod_INE) %>%
  summarise(sin_resolver = all(is.na(dist_carretera_km)), .groups = "drop") %>%
  filter(sin_resolver) %>%
  pull(Cod_INE)
if (length(pendientes_2a) > 0) {
  message(sprintf("\nSegunda pasada: %d municipios siguen pendientes. Esperando 60s antes de reintentar...",
                  length(pendientes_2a)))
  Sys.sleep(60)
  for (i in seq_along(pendientes_2a)) {
    cod <- pendientes_2a[i]
    tryCatch({
      filas <- which(resultados$Cod_INE == cod)
      fila_ref <- resultados[filas[1], ]
      coords <- paste0(fila_ref$muni_lon, ",", fila_ref$muni_lat)
      for (f in filas) coords <- paste0(coords, ";", resultados$candidato_lon[f], ",", resultados$candidato_lat[f])
      destinations <- paste(seq_along(filas), collapse = ";")
      url <- paste0(base_url, coords, "?sources=0&destinations=", destinations, "&annotations=distance,duration")
      datos <- osrm_get_con_reintentos(url)
      if (!is.null(datos)) {
        resultados$dist_carretera_km[filas] <- as.numeric(unlist(datos$distances)) / 1000
        resultados$duracion_min[filas]      <- as.numeric(unlist(datos$durations)) / 60
      }
    }, error = function(e) {
      message(sprintf("  [AVISO] fallo en municipio Cod_INE=%s (i=%d): %s - se continua", cod, i, conditionMessage(e)))
    })
    Sys.sleep(2)
  }
  saveRDS(resultados, archivo_parcial)
  aun_pendientes <- resultados %>%
    group_by(Cod_INE) %>%
    summarise(sin_resolver = all(is.na(dist_carretera_km)), .groups = "drop") %>%
    filter(sin_resolver) %>%
    pull(Cod_INE)
  message(sprintf("Segunda pasada terminada: %d/%d resueltos",
                  length(pendientes_2a) - length(intersect(pendientes_2a, aun_pendientes)),
                  length(pendientes_2a)))
}

# --- 5. Quedarnos con el candidato más cercano POR CARRETERA (no en línea recta) ----
mejores <- resultados %>%
  filter(!is.na(dist_carretera_km)) %>%
  group_by(Cod_INE, tipo) %>%
  slice_min(dist_carretera_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Cod_INE, tipo, candidato_id, dist_recta_km, dist_carretera_km, duracion_min)

# Formato ancho: una columna de distancia y otra de duracion por tipo de recurso
final_dist <- mejores %>%
  select(Cod_INE, tipo, dist_carretera_km) %>%
  tidyr::pivot_wider(names_from = tipo, values_from = dist_carretera_km,
                     names_prefix = "dist_carretera_")

final_dur <- mejores %>%
  select(Cod_INE, tipo, duracion_min) %>%
  tidyr::pivot_wider(names_from = tipo, values_from = duracion_min,
                     names_prefix = "dur_carretera_")

final <- final_dist %>% left_join(final_dur, by = "Cod_INE")

sin_resolver <- setdiff(unique(candidatos$Cod_INE), unique(mejores$Cod_INE))
message(sprintf("\nMunicipios sin distancia por carretera resuelta: %d de %d",
                length(sin_resolver), length(unique(candidatos$Cod_INE))))

write_xlsx(list(resumen = final, detalle_candidatos = mejores), archivo_salida)
message(sprintf("\nGuardado en: %s", archivo_salida))

# ============================================================
# PARTE 3 - Candidatos SOLO de hospitales publicos (Sacyl)
# ============================================================
# Los candidatos de "hospital" de la Parte 1 son los 3 mas cercanos de
# CUALQUIER titularidad (publico, privado, ONG). Para poder medir el
# acceso al hospital PUBLICO especificamente (que es lo que le corresponde
# de verdad al ciudadano dentro del sistema sanitario publico), se genera
# aqui un conjunto de candidatos separado, restringido a los 19 hospitales
# con dependencia_funcional == "SERVICIOS O INSTITUTOS DE SALUD DE LAS CCAA".
# No toca ni reutiliza el checkpoint de la Parte 2 (checkpoint propio).

hospital_publico <- centros %>%
  filter(recurso == "Hospital", dependencia_funcional == "SERVICIOS O INSTITUTOS DE SALUD DE LAS CCAA")
message(sprintf("Hospitales publicos (Sacyl) usados como candidatos: %d", nrow(hospital_publico)))

candidatos_hosp_pub <- candidatos_tipo(
  mun, hospital_publico$latitud, hospital_publico$longitud,
  hospital_publico$numero_registro, "hospital_publico"
)
write_xlsx(candidatos_hosp_pub, "./processed/candidatos_hospital_publico.xlsx")
message(sprintf("Listo: candidatos_hospital_publico.xlsx (%d filas)", nrow(candidatos_hosp_pub)))

# ============================================================
# PARTE 4 - Distancia y duracion REAL por carretera (OSRM), solo hospital publico
# ============================================================
# Misma logica que la Parte 2, pero en un archivo y checkpoint totalmente
# aparte, para no interferir con lo que ya tienes calculado para
# farmacia/consultorio/centro_salud/hospital (cualquier titularidad).

archivo_entrada_hp <- "./processed/candidatos_hospital_publico.xlsx"
archivo_salida_hp  <- "./processed/distancias_hospital_publico.xlsx"
archivo_parcial_hp <- "./processed/distancias_hospital_publico_progreso.rds"

candidatos_hp <- read_excel(archivo_entrada_hp)
municipios_hp <- candidatos_hp %>% distinct(Cod_INE, muni_lat, muni_lon)

if (file.exists(archivo_parcial_hp)) {
  resultados_hp <- readRDS(archivo_parcial_hp)
  message("Reanudando (hospital publico): ", length(unique(resultados_hp$Cod_INE[!is.na(resultados_hp$dist_carretera_km)])),
         " municipios ya resueltos.")
} else {
  resultados_hp <- candidatos_hp %>% mutate(dist_carretera_km = NA_real_, duracion_min = NA_real_)
}

pendientes_hp <- resultados_hp %>%
  group_by(Cod_INE) %>%
  summarise(sin_resolver = all(is.na(dist_carretera_km)), .groups = "drop") %>%
  filter(sin_resolver) %>%
  pull(Cod_INE)

for (i in seq_along(pendientes_hp)) {
  cod <- pendientes_hp[i]
  tryCatch({
    filas <- which(resultados_hp$Cod_INE == cod)
    fila_ref <- resultados_hp[filas[1], ]

    coords <- paste0(fila_ref$muni_lon, ",", fila_ref$muni_lat)
    for (f in filas) coords <- paste0(coords, ";", resultados_hp$candidato_lon[f], ",", resultados_hp$candidato_lat[f])
    n_destinos <- length(filas)
    destinations <- paste(1:n_destinos, collapse = ";")

    url <- paste0("http://router.project-osrm.org/table/v1/driving/", coords,
                 "?sources=0&destinations=", destinations, "&annotations=distance,duration")

    datos <- osrm_get_con_reintentos(url)
    if (!is.null(datos)) {
      resultados_hp$dist_carretera_km[filas] <- as.numeric(unlist(datos$distances)) / 1000
      resultados_hp$duracion_min[filas]      <- as.numeric(unlist(datos$durations)) / 60
    }
  }, error = function(e) {
    message(sprintf("  [AVISO] fallo en municipio Cod_INE=%s (i=%d): %s - se continua", cod, i, conditionMessage(e)))
  })
  Sys.sleep(1)

  if (i %% 25 == 0 || i == length(pendientes_hp)) {
    saveRDS(resultados_hp, archivo_parcial_hp)
    message(sprintf("Progreso (hospital publico): %d/%d municipios", i, length(pendientes_hp)))
  }
}

mejores_hp <- resultados_hp %>%
  filter(!is.na(dist_carretera_km)) %>%
  group_by(Cod_INE) %>%
  slice_min(dist_carretera_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Cod_INE, candidato_id, dist_recta_km,
        dist_hospital_publico_km = dist_carretera_km, dur_hospital_publico_min = duracion_min)

sin_resolver_hp <- setdiff(unique(candidatos_hp$Cod_INE), unique(mejores_hp$Cod_INE))
message(sprintf("\nMunicipios sin distancia a hospital publico resuelta: %d de %d",
               length(sin_resolver_hp), length(unique(candidatos_hp$Cod_INE))))

write_xlsx(list(resumen = mejores_hp %>% select(-candidato_id, -dist_recta_km),
               detalle_candidatos = mejores_hp),
          archivo_salida_hp)
message(sprintf("\nGuardado en: %s", archivo_salida_hp))

# ============================================================
# PARTE 5 - Funcion generica para consultar OSRM (reutilizada de aqui en adelante)
# ============================================================
# Misma logica que las Partes 2 y 4, extraida en funcion para no seguir
# duplicando el bucle. 'candidatos' debe tener: Cod_INE, muni_lat, muni_lon,
# candidato_lat, candidato_lon. nombre_dist/nombre_dur son los nombres de
# columna finales que se quieren en el resumen (como texto).
osrm_resolver <- function(candidatos, archivo_salida, archivo_parcial, nombre_dist, nombre_dur) {
  if (file.exists(archivo_parcial)) {
    resultados <- readRDS(archivo_parcial)
    message("Reanudando: ", length(unique(resultados$Cod_INE[!is.na(resultados$dist_carretera_km)])),
           " municipios ya resueltos.")
  } else {
    resultados <- candidatos %>% mutate(dist_carretera_km = NA_real_, duracion_min = NA_real_)
  }

  pendientes <- resultados %>%
    group_by(Cod_INE) %>%
    summarise(sin_resolver = all(is.na(dist_carretera_km)), .groups = "drop") %>%
    filter(sin_resolver) %>%
    pull(Cod_INE)

  for (i in seq_along(pendientes)) {
    cod <- pendientes[i]
    tryCatch({
      filas <- which(resultados$Cod_INE == cod)
      fila_ref <- resultados[filas[1], ]

      coords <- paste0(fila_ref$muni_lon, ",", fila_ref$muni_lat)
      for (f in filas) coords <- paste0(coords, ";", resultados$candidato_lon[f], ",", resultados$candidato_lat[f])
      destinations <- paste(seq_along(filas), collapse = ";")

      url <- paste0("http://router.project-osrm.org/table/v1/driving/", coords,
                   "?sources=0&destinations=", destinations, "&annotations=distance,duration")

      datos <- osrm_get_con_reintentos(url)
      if (!is.null(datos)) {
        resultados$dist_carretera_km[filas] <- as.numeric(unlist(datos$distances)) / 1000
        resultados$duracion_min[filas]      <- as.numeric(unlist(datos$durations)) / 60
      }
    }, error = function(e) {
      message(sprintf("  [AVISO] fallo en municipio Cod_INE=%s (i=%d): %s - se continua", cod, i, conditionMessage(e)))
    })
    Sys.sleep(1)

    if (i %% 25 == 0 || i == length(pendientes)) {
      saveRDS(resultados, archivo_parcial)
      message(sprintf("Progreso: %d/%d municipios", i, length(pendientes)))
    }
  }

  mejores <- resultados %>%
    filter(!is.na(dist_carretera_km)) %>%
    group_by(Cod_INE) %>%
    slice_min(dist_carretera_km, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Cod_INE, candidato_id, dist_recta_km,
             !!nombre_dist := dist_carretera_km, !!nombre_dur := duracion_min)

  write_xlsx(list(resumen = mejores %>% select(-candidato_id, -dist_recta_km), detalle_candidatos = mejores),
            archivo_salida)
  message(sprintf("Guardado en: %s (%d/%d municipios resueltos)",
                 archivo_salida, nrow(mejores), length(unique(candidatos$Cod_INE))))
  invisible(mejores)
}

# ============================================================
# PARTE 6 - Candidatos y distancia al CENTRO DE SALUD DE LA PROPIA ZBS
# ============================================================
# A diferencia de la Parte 1 (candidatos_tipo, que busca el mas cercano de
# CUALQUIER ZBS), aqui el candidato para cada municipio es EXCLUSIVAMENTE
# el/los centro(s) de salud que pertenecen a su misma ZBS - el "centro de
# salud de cabecera" al que administrativamente le corresponde ir, siguiendo
# la misma logica que Vegas-Sanchez et al. (2022) usan para el hospital de
# referencia. La mayoria de ZBS tienen un unico centro de salud, asi que no
# hace falta preseleccionar top-3, se usan todos los que haya en la zona.

centro_salud_zbs <- centros %>%
  filter(recurso == "Centro de salud") %>%
  left_join(mun %>% select(Cod_INE, zbs_id), by = "Cod_INE") %>%
  filter(!is.na(zbs_id))

candidatos_cs_propio <- mun %>%
  filter(!is.na(zbs_id)) %>%
  select(Cod_INE, zbs_id, muni_lat = Latitud, muni_lon = Longitud) %>%
  inner_join(
    centro_salud_zbs %>% select(zbs_id, candidato_id = numero_registro,
                                candidato_lat = latitud, candidato_lon = longitud),
    by = "zbs_id", relationship = "many-to-many"
  ) %>%
  mutate(dist_recta_km = distHaversine(cbind(muni_lon, muni_lat), cbind(candidato_lon, candidato_lat)) / 1000) %>%
  select(-zbs_id)

zbs_sin_cs_propio <- mun %>% filter(!is.na(zbs_id)) %>% anti_join(candidatos_cs_propio, by = "Cod_INE")
message(sprintf("Municipios cuya ZBS no tiene ningun Centro de Salud propio: %d (se resolveran con respaldo en el script 05)",
               nrow(zbs_sin_cs_propio)))

write_xlsx(candidatos_cs_propio, "./processed/candidatos_centro_salud_propio.xlsx")

osrm_resolver(
  candidatos_cs_propio,
  archivo_salida  = "./processed/distancias_centro_salud_propio.xlsx",
  archivo_parcial = "./processed/distancias_centro_salud_propio_progreso.rds",
  nombre_dist = "dist_centro_salud_propio_km",
  nombre_dur  = "dur_centro_salud_propio_min"
)

# ============================================================
# PARTE 7 - Candidatos y distancia al HOSPITAL PUBLICO DE LA PROPIA GERENCIA
# ============================================================
# Misma logica que la Parte 6, pero a nivel de Gerencia/Area de Salud y solo
# con hospitales publicos (Sacyl) - el hospital de referencia real, siguiendo
# la practica documentada por Vegas-Sanchez et al. (2022): el paciente de un
# Area de Salud usa su propio hospital, exista o no uno mas cercano en otra
# Gerencia (salvo convenio de derivacion explicito, que no modelamos aqui).

hospital_pub_gerencia <- centros %>%
  filter(recurso == "Hospital", dependencia_funcional == "SERVICIOS O INSTITUTOS DE SALUD DE LAS CCAA") %>%
  left_join(mun %>% select(Cod_INE, gerencia), by = "Cod_INE") %>%
  filter(!is.na(gerencia))

candidatos_hosp_propio <- mun %>%
  filter(!is.na(gerencia)) %>%
  select(Cod_INE, gerencia, muni_lat = Latitud, muni_lon = Longitud) %>%
  inner_join(
    hospital_pub_gerencia %>% select(gerencia, candidato_id = numero_registro,
                                     candidato_lat = latitud, candidato_lon = longitud),
    by = "gerencia", relationship = "many-to-many"
  ) %>%
  mutate(dist_recta_km = distHaversine(cbind(muni_lon, muni_lat), cbind(candidato_lon, candidato_lat)) / 1000) %>%
  select(-gerencia)

gerencia_sin_hosp_propio <- mun %>% filter(!is.na(gerencia)) %>% anti_join(candidatos_hosp_propio, by = "Cod_INE")
message(sprintf("Municipios cuya Gerencia no tiene ningun hospital publico propio: %d",
               nrow(gerencia_sin_hosp_propio)))

write_xlsx(candidatos_hosp_propio, "./processed/candidatos_hospital_propio.xlsx")

osrm_resolver(
  candidatos_hosp_propio,
  archivo_salida  = "./processed/distancias_hospital_propio.xlsx",
  archivo_parcial = "./processed/distancias_hospital_propio_progreso.rds",
  nombre_dist = "dist_hospital_propio_km",
  nombre_dur  = "dur_hospital_propio_min"
)
