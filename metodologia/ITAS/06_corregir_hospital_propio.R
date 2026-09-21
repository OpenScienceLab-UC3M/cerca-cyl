# ============================================================
# 06 - Corregir el area hospitalaria real (Burgos y Valladolid)
# ============================================================
# Motivo: "gerencia" (NOMBRE GERENCIA) agrupa mal los hospitales de Burgos
# (3 hospitales reales bajo una sola gerencia) y sobre todo los de
# Valladolid (el Clinico y el Rio Hortega comparten la gerencia de la
# capital -"Oeste"- porque ambos estan en el mismo municipio, cuando en la
# vida real pertenecen a areas distintas -Este y Oeste-). area_hospital_id
# (anadido en el script 03, desde el campo c_hospital del fichero de ZBS)
# es la unidad correcta. Este script:
#   1. Reconstruye los candidatos de hospital propio con area_hospital_id.
#   2. Reaprovecha el checkpoint ya calculado (distancias_hospital_propio_
#      progreso.rds) para cualquier par municipio-hospital que ya se
#      hubiera consultado antes - solo pide a OSRM lo genuinamente nuevo
#      (los ~120 municipios de Valladolid Este contra el Clinico).
#   3. Vuelve a calcular, ya bien, dist/dur_hospital_propio para todos.

paquetes <- c("readxl", "writexl", "dplyr", "tidyr", "geosphere", "httr", "jsonlite")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)
library(readxl); library(writexl); library(dplyr); library(tidyr); library(geosphere); library(httr); library(jsonlite)

mun <- readRDS("./processed/municipios_base.rds")
centros <- readRDS("./processed/centros_asignados.rds")

# Empezar SIEMPRE limpio: si quedó un checkpoint intermedio de una ejecucion
# anterior (posiblemente a medias o con una version con bug de este script),
# se borra para que la reconstruccion parta del checkpoint viejo bueno y no
# arrastre filas mal calculadas.
if (file.exists("./processed/distancias_hospital_propio_v2_progreso.rds")) {
  file.remove("./processed/distancias_hospital_propio_v2_progreso.rds")
  message("Checkpoint intermedio anterior borrado - se parte limpio.")
}

# --- 1. Candidatos corregidos: TODOS los hospitales publicos de la MISMA area_hospital_id ---
hospital_pub_area <- centros %>%
  filter(recurso == "Hospital", !is.na(area_hospital_id))

candidatos_hosp_propio <- mun %>%
  filter(!is.na(area_hospital_id)) %>%
  select(Cod_INE, area_hospital_id, muni_lat = Latitud, muni_lon = Longitud) %>%
  inner_join(
    hospital_pub_area %>% select(area_hospital_id, candidato_id = numero_registro,
                                 candidato_lat = latitud, candidato_lon = longitud),
    by = "area_hospital_id", relationship = "many-to-many"
  ) %>%
  mutate(dist_recta_km = distHaversine(cbind(muni_lon, muni_lat), cbind(candidato_lon, candidato_lat)) / 1000) %>%
  select(-area_hospital_id)

message(sprintf("Candidatos corregidos: %d filas (antes, con gerencia: 4297)", nrow(candidatos_hosp_propio)))
write_xlsx(candidatos_hosp_propio, "./processed/candidatos_hospital_propio_v2.xlsx")

# --- 2. Reaprovechar el checkpoint viejo para lo que ya coincida ------------
viejo <- readRDS("./processed/distancias_hospital_propio_progreso.rds")
viejo_resuelto <- viejo %>% filter(!is.na(dist_carretera_km)) %>%
  select(Cod_INE, candidato_id, dist_carretera_km_viejo = dist_carretera_km, duracion_min_viejo = duracion_min)

resultados <- candidatos_hosp_propio %>%
  left_join(viejo_resuelto, by = c("Cod_INE", "candidato_id")) %>%
  mutate(
    dist_carretera_km = dist_carretera_km_viejo,
    duracion_min = duracion_min_viejo
  ) %>%
  select(-dist_carretera_km_viejo, -duracion_min_viejo)

ya_resuelto <- sum(!is.na(resultados$dist_carretera_km))
message(sprintf("Reaprovechados del checkpoint viejo: %d / %d filas (%.1f%%) - no hace falta pedirlos de nuevo a OSRM",
                ya_resuelto, nrow(resultados), 100*ya_resuelto/nrow(resultados)))

archivo_parcial <- "./processed/distancias_hospital_propio_v2_progreso.rds"
saveRDS(resultados, archivo_parcial)

# --- 3. OSRM solo para lo genuinamente nuevo --------------------------------
osrm_get_con_reintentos <- function(url, intentos = 3, esperas_seg = c(5, 15, 40)) {
  for (intento in seq_len(intentos)) {
    resp <- tryCatch(GET(url, timeout(20)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      datos <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")), error = function(e) NULL)
      # Aceptar solo si el JSON es valido Y OSRM confirma "code":"Ok" con
      # distancias presentes. Bajo carga, el servidor publico a veces responde
      # HTTP 200 pero con un code de error (p.ej. "NoRoute") o sin
      # "distances" - antes se aceptaba como si fuera un resultado bueno, sin
      # reintentar, dejando esa fila vacia para siempre.
      if (!is.null(datos) && !is.null(datos$code) && datos$code == "Ok" && !is.null(datos$distances)) {
        return(datos)
      }
    }
    if (intento < intentos) Sys.sleep(esperas_seg[min(intento, length(esperas_seg))])
  }
  NULL
}

pendientes <- resultados %>%
  group_by(Cod_INE) %>%
  summarise(falta_alguna = any(is.na(dist_carretera_km)), .groups = "drop") %>%
  filter(falta_alguna) %>%
  pull(Cod_INE)
# (Antes se usaba sapply(split(...)) e indexaba resultados$Cod_INE con ese
# resultado directamente - el vector de sapply tiene UN valor POR MUNICIPIO
# (~2196), pero resultados$Cod_INE tiene UNA fila POR CANDIDATO (3553) -
# longitudes distintas, asi que R reciclaba el vector corto para indexar el
# largo, desalineando que fila corresponde a que municipio. Esto explica por
# que cambiar la logica de reintentos/red nunca cambiaba el resultado final:
# el problema estaba aqui, decidiendo mal quien entraba en el bucle, no en
# como se consultaba OSRM una vez dentro.)
message(sprintf("Municipios que SI necesitan consulta nueva a OSRM: %d", length(pendientes)))

base_url <- "http://router.project-osrm.org/table/v1/driving/"
message(sprintf(">>> INICIO del bucle OSRM: %d municipios a consultar <<<", length(pendientes)))
for (i in seq_along(pendientes)) {
  cod <- pendientes[i]
  resultado_iteracion <- tryCatch({
    # Solo las filas (candidatos) que aun no tienen distancia -- las que ya
    # vinieron del checkpoint viejo se dejan intactas.
    filas <- which(resultados$Cod_INE == cod & is.na(resultados$dist_carretera_km))
    if (length(filas) > 0) {
      fila_ref <- resultados[filas[1], ]
      coords <- paste0(fila_ref$muni_lon, ",", fila_ref$muni_lat)
      for (f in filas) coords <- paste0(coords, ";", resultados$candidato_lon[f], ",", resultados$candidato_lat[f])
      destinations <- paste(seq_along(filas), collapse = ";")
      url <- paste0(base_url, coords, "?sources=0&destinations=", destinations, "&annotations=distance,duration")

      datos <- osrm_get_con_reintentos(url)
      if (!is.null(datos)) {
        # unlist() en vez de [1, ]: funciona igual si OSRM/jsonlite devuelve
        # una matriz 1xN o, con un solo candidato, un vector simple - evita
        # un posible "incorrect number of dimensions" que pararia el bucle
        # entero sin avisar en el caso de un unico destino.
        resultados$dist_carretera_km[filas] <- as.numeric(unlist(datos$distances)) / 1000
        resultados$duracion_min[filas]      <- as.numeric(unlist(datos$durations)) / 60
      }
    }
    NULL
  }, error = function(e) {
    message(sprintf("  [AVISO] fallo en municipio Cod_INE=%s (i=%d): %s - se continua con el siguiente", cod, i, conditionMessage(e)))
    NULL
  })
  Sys.sleep(2)  # subido de 1 a 2s: reduce la carga sobre el servidor publico,
                # que bajo muchas peticiones seguidas puede responder HTTP 200
                # con un "code" de error dentro del JSON (ver validacion en
                # osrm_get_con_reintentos) en vez de fallar limpiamente
  if (i %% 10 == 0 || i == length(pendientes)) {
    saveRDS(resultados, archivo_parcial)
    message(sprintf("Progreso: %d/%d municipios nuevos", i, length(pendientes)))
  }
}
message(sprintf(">>> FIN del bucle OSRM (i llego a %d de %d) <<<", i, length(pendientes)))
aun_sin_resolver <- sum(is.na(resultados$dist_carretera_km))
message(sprintf(">>> Filas SIN resolver tras el bucle: %d de %d <<<", aun_sin_resolver, nrow(resultados)))

# --- 4. Resultado final: el candidato mas cercano REAL para cada municipio -
mejores <- resultados %>%
  filter(!is.na(dist_carretera_km)) %>%
  group_by(Cod_INE) %>%
  slice_min(dist_carretera_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(Cod_INE, candidato_id, dist_recta_km,
           dist_hospital_propio_km = dist_carretera_km, dur_hospital_propio_min = duracion_min)

write_xlsx(list(resumen = mejores %>% select(-candidato_id, -dist_recta_km), detalle_candidatos = mejores),
          "./processed/distancias_hospital_propio.xlsx")  # SOBRESCRIBE el fichero anterior, con los datos corregidos

message(sprintf("\nListo: distancias_hospital_propio.xlsx corregido y sobrescrito (%d/%d municipios resueltos)",
                nrow(mejores), length(unique(candidatos_hosp_propio$Cod_INE))))
