# ============================================================
# Diagnostico: por que fallan siempre los mismos municipios en OSRM
# ============================================================
# En vez de descartar el error como hace el script principal, aqui lo
# imprimimos entero para ver la causa real (ej. "NoRoute": un punto no
# se puede enganchar a la red de carreteras).

library(readxl); library(httr); library(jsonlite); library(dplyr)

candidatos <- read_excel("./processed/candidatos_carretera.xlsx")

# Los 6 municipios que llevan fallando en las dos ultimas ejecuciones
sospechosos <- c("SEQUEROS","SERRADILLA DEL ARROYO","SERRADILLA DEL LLANO",
                 "SIETEIGLESIAS DE TORMES","SOBRADILLO","SORIHUELA")

mun <- readRDS("./processed/municipios_base.rds")
codigos <- mun$Cod_INE[mun$Municipio %in% sospechosos]
message("Codigos a diagnosticar: ", paste(codigos, collapse=", "))

for (cod in codigos) {
  filas <- candidatos %>% filter(Cod_INE == cod)
  if (nrow(filas) == 0) { message(cod, ": no tiene candidatos, revisar aparte"); next }

  fila_ref <- filas[1, ]
  coords <- paste0(fila_ref$muni_lon, ",", fila_ref$muni_lat)
  for (i in seq_len(nrow(filas))) coords <- paste0(coords, ";", filas$candidato_lon[i], ",", filas$candidato_lat[i])
  destinations <- paste(seq_len(nrow(filas)), collapse = ";")

  url <- paste0("http://router.project-osrm.org/table/v1/driving/", coords,
                "?sources=0&destinations=", destinations, "&annotations=distance,duration")

  message("\n=== Municipio Cod_INE ", cod, " (", nrow(filas), " destinos) ===")
  resp <- tryCatch(GET(url, timeout(15)), error = function(e) e)
  if (inherits(resp, "error")) {
    message("ERROR DE CONEXION: ", conditionMessage(resp))
    next
  }
  message("HTTP status: ", status_code(resp))
  cuerpo <- content(resp, "text", encoding = "UTF-8")
  message("Respuesta OSRM (primeros 500 caracteres):")
  message(substr(cuerpo, 1, 500))

  # Si OSRM devuelve "code" distinto de "Ok", ahi esta la causa exacta
  datos <- tryCatch(fromJSON(cuerpo), error = function(e) NULL)
  if (!is.null(datos) && !is.null(datos$code)) {
    message(">>> Codigo OSRM: ", datos$code)
    if (!is.null(datos$message)) message(">>> Mensaje OSRM: ", datos$message)
  }

  # Mostrar cada destino individualmente, por si alguno en concreto es el problema
  message("Candidatos de este municipio:")
  print(filas %>% select(tipo, candidato_rank, candidato_lat, candidato_lon, dist_recta_km))

  Sys.sleep(1)
}
