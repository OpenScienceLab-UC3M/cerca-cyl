# ============================================================
# 05 - Calcular ITAB, ITAH, banderas y Excel final
# ============================================================
# ENTRADA: ./processed/municipios_base.rds, centros_asignados.rds (script 01)
#          ./processed/farmacias_con_coordenadas_final.xlsx
#          ./processed/distancias_carretera.xlsx (salida de distancias_carretera_osrm.R)
# SALIDA:  ./results/indicador_tension_sanitaria.xlsx (hojas: Municipios, ZBS (ITAB), Gerencias (ITAH))

paquetes <- c("readxl", "writexl", "dplyr", "tidyr", "geosphere")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)
library(readxl); library(writexl); library(dplyr); library(tidyr); library(geosphere)

mun <- readRDS("./processed/municipios_base.rds")
centros <- readRDS("./processed/centros_asignados.rds")
far <- read_excel("./processed/farmacias_con_coordenadas_final.xlsx")

# Vecino mas cercano por distancia haversine REAL - misma funcion que en
# 03-construir_base_municipios.R y 04-distancias_carretera.R.
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

# --- 1. Distancia en linea recta (respaldo para lo que OSRM no resuelva) ----
dist_recta_min <- function(mun_df, rec_lat, rec_lon) {
  vecino_haversine(mun_df$Latitud, mun_df$Longitud, rec_lat, rec_lon, k = 1)$nn.dists[, 1]
}
consultorio  <- centros %>% filter(recurso == "Consultorio")
centro_salud <- centros %>% filter(recurso == "Centro de salud")
hospital     <- centros %>% filter(recurso == "Hospital")
hospital_publico <- centros %>%
  filter(recurso == "Hospital", dependencia_funcional == "SERVICIOS O INSTITUTOS DE SALUD DE LAS CCAA")

mun$dist_farmacia_recta    <- dist_recta_min(mun, far$lat, far$long)
mun$dist_consultorio_recta <- dist_recta_min(mun, consultorio$latitud, consultorio$longitud)
mun$dist_centro_salud_recta<- dist_recta_min(mun, centro_salud$latitud, centro_salud$longitud)
mun$dist_hospital_recta    <- dist_recta_min(mun, hospital$latitud, hospital$longitud)
mun$dist_hospital_publico_recta <- dist_recta_min(mun, hospital_publico$latitud, hospital_publico$longitud)
mun$dist_primaria_recta    <- pmin(mun$dist_consultorio_recta, mun$dist_centro_salud_recta)

# --- 2. Distancia real por carretera (OSRM) ---------------------------------
resumen <- read_excel("./processed/distancias_carretera.xlsx", sheet = "resumen")
mun <- mun %>% left_join(resumen, by = "Cod_INE")

# Hospital publico (Sacyl), al mas cercano de CUALQUIER Gerencia - ver Parte 3/4
# de 04-distancias_carretera.R. Se mantiene como referencia informativa.
resumen_hp <- read_excel("./processed/distancias_hospital_publico.xlsx", sheet = "resumen")
mun <- mun %>% left_join(resumen_hp, by = "Cod_INE")

# Centro de Salud y Hospital publico DE LA PROPIA ZBS/Gerencia - ver Parte 6/7
# de 04-distancias_carretera.R. Esta es la distancia "de referencia" siguiendo
# la practica documentada en la literatura (Vegas-Sanchez et al., 2022): el
# habitante usa el recurso de su propia zona, exista o no uno mas cercano en
# otra. Es la que se usa de aqui en adelante para el ITAH y para el
# cumplimiento del estandar de primaria - la version "cualquier zona" se
# conserva aparte, solo como comparacion.
resumen_cs_propio <- read_excel("./processed/distancias_centro_salud_propio.xlsx", sheet = "resumen")
resumen_hosp_propio <- read_excel("./processed/distancias_hospital_propio.xlsx", sheet = "resumen")
mun <- mun %>%
  left_join(resumen_cs_propio, by = "Cod_INE") %>%
  left_join(resumen_hosp_propio, by = "Cod_INE")

mun <- mun %>%
  mutate(
    distancia_estimada = is.na(dist_carretera_farmacia),  # marca los municipios sin resultado OSRM (respaldo linea recta)
    dist_primaria_km = coalesce(pmin(dist_carretera_centro_salud, dist_carretera_consultorio), dist_primaria_recta),
    dist_farmacia_km = coalesce(dist_carretera_farmacia, dist_farmacia_recta),

    # "Cualquier zona" (informativo): al mas cercano exista o no en tu propia ZBS/Gerencia
    dist_centro_salud_cualquiera_km = coalesce(dist_carretera_centro_salud, dist_centro_salud_recta),
    dist_hospital_cualquiera_km = coalesce(dist_carretera_hospital, dist_hospital_recta),
    dist_hospital_publico_cualquier_gerencia_km = coalesce(dist_hospital_publico_km, dist_hospital_publico_recta),

    # "Propio" (usado en el indice): al de tu propia zona; si tu zona no tiene
    # ninguno (caso raro), se usa como respaldo el mas cercano de cualquier
    # zona, marcado en *_es_respaldo para poder filtrarlo despues.
    dist_centro_salud_km = coalesce(dist_centro_salud_propio_km, dist_centro_salud_cualquiera_km),
    centro_salud_propio_es_respaldo = is.na(dist_centro_salud_propio_km),
    dist_hospital_km = coalesce(dist_hospital_propio_km, dist_hospital_publico_cualquier_gerencia_km, dist_hospital_cualquiera_km),
    hospital_propio_es_respaldo = is.na(dist_hospital_propio_km)
  )
message(sprintf("Municipios con distancia estimada en linea recta (sin resultado OSRM): %d / %d",
                sum(mun$distancia_estimada), nrow(mun)))
message(sprintf("Municipios usando respaldo (su ZBS no tenia Centro de Salud propio): %d",
                sum(mun$centro_salud_propio_es_respaldo)))
message(sprintf("Municipios usando respaldo (su Gerencia no tenia hospital publico propio): %d",
                sum(mun$hospital_propio_es_respaldo)))

# --- 2c. Ratio de carga hospitalaria, SOLO con capacidad publica -------------
# (antes se usaba ratio_ponderado_hospital de municipios_base.rds, que mezclaba
# capacidad publica y privada/ONG en el denominador - se recalcula aqui limpio)
#
# IMPORTANTE: se agrupa por area_hospital_id (el area hospitalaria REAL, ver
# script 03), no por "gerencia". "Gerencia" agrupaba mal los hospitales de
# Burgos (3 hospitales reales bajo una misma gerencia) y sobre todo los de
# Valladolid (Clinico y Rio Hortega comparten gerencia porque ambos estan en
# el mismo municipio, aunque pertenecen a areas reales distintas). Se usa el
# area_hospital_id que YA trae "centros" directamente (asignado por
# numero_registro en el script 03) - no se vuelve a derivar via Cod_INE, que
# es precisamente donde estaba el error.
cap_hosp_pub_area <- centros %>%
  filter(recurso == "Hospital", dependencia_funcional == "SERVICIOS O INSTITUTOS DE SALUD DE LAS CCAA",
        !is.na(area_hospital_id)) %>%
  group_by(area_hospital_id) %>% summarise(capacidad_hospital_publico = sum(n_finalidades, na.rm = TRUE))

pob_area_hospital <- mun %>% filter(!is.na(area_hospital_id)) %>%
  group_by(area_hospital_id) %>% summarise(poblacion_area_hospital = sum(poblacion_total))

area_pub <- pob_area_hospital %>% left_join(cap_hosp_pub_area, by = "area_hospital_id") %>%
  mutate(ratio_ponderado_hospital_publico = poblacion_area_hospital / capacidad_hospital_publico)

n_hosp_pub <- centros %>%
  filter(recurso == "Hospital", dependencia_funcional == "SERVICIOS O INSTITUTOS DE SALUD DE LAS CCAA") %>%
  count(Cod_INE, name = "n_hospitales_publicos")

mun <- mun %>%
  left_join(n_hosp_pub, by = "Cod_INE") %>%
  mutate(n_hospitales_publicos = replace_na(n_hospitales_publicos, 0))

mun <- mun %>%
  select(-any_of("ratio_ponderado_hospital")) %>%  # se descarta la version mezclada de municipios_base.rds
  left_join(area_pub %>% select(area_hospital_id, ratio_ponderado_hospital_publico), by = "area_hospital_id") %>%
  rename(ratio_ponderado_hospital = ratio_ponderado_hospital_publico)

# --- 2b. Cumplimiento de estandares de tiempo (en coche, para todos los casos) ---
# Dos indicadores DISTINTOS, no se fusionan en uno:
#  (a) "proximidad" = tiempo al recurso de primaria mas cercano (consultorio
#      o centro de salud, el que sea) - refleja si hay ALGO cerca en el dia a dia.
#  (b) "estandar_referencia" = tiempo especificamente al Centro de Salud de
#      referencia (no al consultorio) - es lo que mide de verdad el objetivo
#      normativo de 15-20 min, ya que el consultorio es solo el punto donde
#      el medico del centro de salud pasa consulta unos dias por semana, no
#      el recurso que define la cobertura territorial real.
# LIMITACION ASUMIDA: se usa tiempo EN COCHE para todos los municipios. La
# normativa distingue a pie en entorno urbano y en coche en entorno rural,
# pero no tenemos datos de poblacion/centros a nivel de barrio para evaluar
# accesibilidad peatonal intraurbana, asi que en los municipios grandes este
# indicador es una aproximacion optimista (ver bandera 'no_evaluable_a_pie').
mun <- mun %>%
  mutate(
    dur_primaria_min = pmin(dur_carretera_centro_salud, dur_carretera_consultorio),
    dur_farmacia_min = dur_carretera_farmacia,

    # "Cualquier zona" (informativo)
    dur_centro_salud_cualquiera_min = dur_carretera_centro_salud,
    dur_hospital_cualquiera_min = dur_carretera_hospital,
    dur_hospital_publico_cualquier_gerencia_min = dur_hospital_publico_min,

    # "Propio" (usado en el estandar/ITAH), con respaldo si la zona no tiene recurso propio
    dur_centro_salud_min = coalesce(dur_centro_salud_propio_min, dur_centro_salud_cualquiera_min),
    dur_hospital_min = coalesce(dur_hospital_propio_min, dur_hospital_publico_cualquier_gerencia_min, dur_hospital_cualquiera_min),

    cumple_proximidad_15min = dur_primaria_min <= 15,
    cumple_proximidad_20min = dur_primaria_min <= 20,
    cumple_estandar_primaria_15min = dur_centro_salud_min <= 15,
    cumple_estandar_primaria_20min = dur_centro_salud_min <= 20,
    cumple_estandar_farmacia_20min = dur_farmacia_min <= 20,
    cumple_hospital_30min = dur_hospital_min <= 30,

    # Umbral orientativo de "municipio grande": aqui la comparacion en coche
    # no verifica de verdad el criterio real (que es a pie, por barrio).
    no_evaluable_a_pie = poblacion_total >= 20000
  )

# --- 3. Percentiles (0-100, mayor = mas tension). Sin recurso -> percentil 100 ---
percentil <- function(x) {
  n <- sum(!is.na(x))
  p <- rank(x, ties.method = "average", na.last = "keep") / n * 100
  p[is.na(x)] <- 100
  p
}

mun <- mun %>%
  mutate(
    pctl_dist_primaria  = percentil(dist_primaria_km),
    pctl_dist_farmacia  = percentil(dist_farmacia_km),
    pctl_ratio_primaria = percentil(ratio_ponderado_primaria),
    pctl_ratio_farmacia = percentil(ratio_hab_por_farmacia),
    pctl_pob_mayor      = percentil(porcentaje_65_mas),
    pctl_dispersion     = percentil(pct_poblacion_fuera_nucleo_principal),
    pctl_dist_hospital  = percentil(dist_hospital_km),
    pctl_ratio_hospital = percentil(ratio_ponderado_hospital)
  )

# --- 4. ITAB (Indice de Tension de Atencion Basica) -------------------------
mun$ITAB <- rowMeans(mun[, c("pctl_dist_primaria","pctl_dist_farmacia","pctl_ratio_primaria",
                             "pctl_ratio_farmacia","pctl_pob_mayor","pctl_dispersion")])

# --- 5. ITAH (Indice de Tension de Atencion Hospitalaria) -------------------
# Media de 2 componentes: distancia al hospital de referencia + envejecimiento.
# El ratio de carga hospitalaria SE RETIRO del ITAH (antes era 1/3): tras
# corregir las areas hospitalarias reales (14, no 11), el ratio paso a medir
# esencialmente "tamano de la ciudad" en vez de saturacion -- las areas
# urbanas grandes (Burgos, Valladolid-Rio Hortega, Leon, Salamanca) salian con
# ITAH alto solo por tener mucha poblacion compartiendo un gran hospital, lo
# que es lo normal en una ciudad, no un problema de acceso. Comprobado: al
# quitarlo, el ITAH medio de las 16 ciudades grandes cae de 23,4 a 6,0 (pasan
# a reflejar correctamente que tienen buen acceso hospitalario), la
# correlacion ITAB-ITAH baja de 0,097 a 0,036 (siguen midiendo cosas
# distintas), y coincide con la literatura (Vegas-Sanchez et al.: distancia +
# envejecimiento, sin ratio de camas). El ratio se conserva como columna
# informativa (ratio_ponderado_hospital), solo se saca del indice.
mun$ITAH <- rowMeans(mun[, c("pctl_dist_hospital","pctl_pob_mayor")])

etiquetas <- c("Muy bajo","Bajo","Medio","Alto","Muy alto")
a_quintiles <- function(x) {
  cortes <- quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE)
  cortes[1] <- -Inf; cortes[6] <- Inf
  cut(x, breaks = unique(cortes), labels = etiquetas[1:(length(unique(cortes)) - 1)], include.lowest = TRUE)
}
mun$nivel_ITAB <- a_quintiles(mun$ITAB)
mun$nivel_ITAH <- a_quintiles(mun$ITAH)

# --- 6. Banderas (por accion: abrir / reforzar / bien cubierto) ------------
# Rediseno respecto a la version anterior:
#  - Se basan en el ESTANDAR ABSOLUTO de tiempo (RD 137/1984), no en un umbral
#    de km ni en el nivel relativo de ITAB/ITAH -- ITAB/ITAH quedan para
#    explorar y comparar, las banderas son para decidir una accion concreta.
#  - Primaria se separa en A1 (sin ningun recurso propio) y A2 (tiene
#    consultorio pero el Centro de Salud de referencia de su ZBS le queda
#    lejos: candidato a AMPLIAR el consultorio existente, no a construir
#    uno nuevo desde cero).
#  - No existe "A-hospital" (abrir hospital nuevo): comprobado que ni a nivel
#    municipio (1.662/2.248 fallarian, inservible como lista) ni a nivel
#    Gerencia (ninguna Gerencia falla ni siquiera en su municipio mejor
#    situado) tiene sentido como accion de apertura. En su lugar hay una
#    señal de "prioridad" (no es una recomendacion de abrir nada, es un
#    candidato a revisar/reforzar el PAC de su zona).
#  - Los 52 municipios sin ZBS/Gerencia asignada (hueco del fichero oficial)
#    quedan marcados aparte como no evaluables, en vez de contar como
#    saturacion falsa (bug de la version anterior: el percentil de un NA se
#    fija en 100, lo que los marcaba a los 52 como "saturados" sin serlo).

sin_zona_asignada_vec <- is.na(mun$gerencia)  # mismos 52 que is.na(zbs_id)
message(sprintf("Municipios sin ZBS/Gerencia asignada (no evaluables en B): %d", sum(sin_zona_asignada_vec)))

UMBRAL_HOSPITAL_PRIORIDAD_MIN <- 60  # el doble del estandar legal de 30 min

# NOTA: se RETIRO la bandera "B reforzar hospital". Con las 14 areas
# hospitalarias reales, el ratio de carga hospitalaria mide esencialmente
# "tamano de la ciudad" (las areas urbanas grandes tienen ratios altos por
# tener mucha poblacion, no por peor servicio), asi que ninguna version del
# corte producia una senal de accion fiable -- disparaba a ~1.000 municipios,
# casi la mitad de la region, senalando sobre todo capitales. El acceso
# hospitalario problematico (el rural, por lejania) ya lo capta
# bandera_hospital_prioridad (>60 min). El ratio se conserva como columna
# informativa, pero no genera bandera ni entra en el ITAH (ver Seccion 5).

mun <- mun %>%
  mutate(
    sin_zona_asignada = sin_zona_asignada_vec,

    # --- A: abrir/ampliar recurso ---
    bandera_A1_nueva_primaria   = coalesce(n_primaria == 0 & !cumple_estandar_primaria_20min & poblacion_total >= 100, FALSE),
    bandera_A2_ampliar_consultorio = coalesce(n_consultorio > 0 & n_centro_salud == 0 & !cumple_estandar_primaria_20min, FALSE),
    bandera_A_nueva_farmacia    = coalesce(n_farmacias == 0 & !cumple_estandar_farmacia_20min & poblacion_total >= 300, FALSE),

    # --- Prioridad (no es "abrir hospital", ver nota arriba) ---
    bandera_hospital_prioridad = coalesce(dur_hospital_min > UMBRAL_HOSPITAL_PRIORIDAD_MIN, FALSE),

    # --- B: reforzar capacidad (excluyendo los sin zona asignada) ---
    # Solo primaria: B-hospital se retiro (ver nota arriba).
    bandera_B_reforzar_primaria = pctl_ratio_primaria >= 85 & !sin_zona_asignada,

    # --- C: bien cubierto, una por tipo de recurso ---
    # coalesce(..., FALSE): los 6 municipios de Salamanca sin dato de duracion
    # de OSRM (respaldo linea recta, que no da tiempo) tendrian NA en el estandar
    # de farmacia; se tratan como "no bien cubierto" (no afirmamos algo que no
    # sabemos) en vez de dejar un NA que contamina recuentos y visualizaciones.
    bandera_C_primaria_bien_cubierto = coalesce(cumple_estandar_primaria_20min & !(pctl_ratio_primaria >= 85) & !sin_zona_asignada, FALSE),
    bandera_C_farmacia_bien_cubierto = coalesce(cumple_estandar_farmacia_20min, FALSE),
    bandera_C_hospital_bien_cubierto = coalesce(cumple_hospital_30min & !sin_zona_asignada, FALSE)
  )

# --- 7. Agregados ZBS (ITAB, ponderado por poblacion) -----------------------
zbs <- mun %>%
  filter(!is.na(zbs_id)) %>%
  group_by(zbs_id, zbs_nombre, gerencia) %>%
  summarise(
    n_municipios = n(),
    poblacion = sum(poblacion_total),
    ratioPrimaria = first(ratio_ponderado_primaria),
    ITAB = sum(poblacion_total * ITAB) / sum(poblacion_total),
    .groups = "drop"
  ) %>%
  mutate(nivel_ITAB = a_quintiles(ITAB))

# --- 8. Agregados por Area Hospitalaria real (ITAH, ponderado por poblacion) -
# Antes se agregaba por "gerencia" (11) - ahora por area_hospital_id (14),
# la unidad real corregida. Nombre legible solo para lectura en el Excel.
nombre_area_hospital <- c(
  "1"="Burgos - Miranda de Ebro", "2"="Burgos - Aranda de Duero", "3"="Valladolid - Medina del Campo",
  "4"="El Bierzo", "5"="Valladolid - Clinico", "6"="Valladolid - Rio Hortega", "7"="Avila",
  "8"="Burgos - capital", "9"="Leon", "10"="Palencia", "11"="Salamanca", "12"="Segovia",
  "13"="Soria", "14"="Zamora"
)
ger <- mun %>%
  filter(!is.na(area_hospital_id)) %>%
  group_by(area_hospital_id) %>%
  summarise(
    gerencia = first(gerencia),  # se conserva para referencia, ya no es la unidad de agregacion
    n_municipios = n(),
    poblacion = sum(poblacion_total),
    ratioHospital = first(ratio_ponderado_hospital),
    ITAH = sum(poblacion_total * ITAH) / sum(poblacion_total),
    .groups = "drop"
  ) %>%
  mutate(nombre_area_hospital = nombre_area_hospital[as.character(area_hospital_id)],
        nivel_ITAH = a_quintiles(ITAH)) %>%
  relocate(nombre_area_hospital, .after = area_hospital_id)

# --- 9. Seleccion final de columnas y exportacion ---------------------------
mun_out <- mun %>%
  transmute(
    Cod_INE, Municipio, Provincia, zbs_nombre, gerencia, area_hospital_id,
    poblacion_total, poblacion_65_mas, porcentaje_65_mas,
    n_nucleos_reales, pct_poblacion_fuera_nucleo_principal,
    n_farmacias, n_primaria, n_consultorio, n_centro_salud,
    n_hospitales_publicos, n_hospitales_cualquiera = n_hospitales,
    dist_farmacia_km = round(dist_farmacia_km, 2),
    dist_primaria_km = round(dist_primaria_km, 2),
    dist_centro_salud_km = round(dist_centro_salud_km, 2),                                     # de la propia ZBS - usado en el estandar
    dist_centro_salud_cualquiera_km = round(dist_centro_salud_cualquiera_km, 2),                # informativo, cualquier ZBS
    centro_salud_propio_es_respaldo,
    dist_hospital_km = round(dist_hospital_km, 2),                       # de la propia Gerencia, SOLO publicos - usado en ITAH
    dist_hospital_publico_cualquier_gerencia_km = round(dist_hospital_publico_cualquier_gerencia_km, 2), # informativo
    dist_hospital_cualquiera_km = round(dist_hospital_cualquiera_km, 2), # informativo, incluye privados/ONG
    hospital_propio_es_respaldo,
    distancia_estimada,
    ratio_ponderado_primaria = round(ratio_ponderado_primaria, 2),
    ratio_hab_por_farmacia = round(ratio_hab_por_farmacia, 2),
    ratio_ponderado_hospital = round(ratio_ponderado_hospital, 2),       # SOLO capacidad publica
    ITAB = round(ITAB, 2), nivel_ITAB,
    ITAH = round(ITAH, 2), nivel_ITAH,
    dur_primaria_min = round(dur_primaria_min, 1),
    dur_farmacia_min = round(dur_farmacia_min, 1),
    dur_centro_salud_min = round(dur_centro_salud_min, 1),               # de la propia ZBS - usado en el estandar
    dur_centro_salud_cualquiera_min = round(dur_centro_salud_cualquiera_min, 1),
    dur_hospital_min = round(dur_hospital_min, 1),                       # de la propia Gerencia, SOLO publicos - usado en ITAH
    dur_hospital_publico_cualquier_gerencia_min = round(dur_hospital_publico_cualquier_gerencia_min, 1),
    dur_hospital_cualquiera_min = round(dur_hospital_cualquiera_min, 1), # informativo
    cumple_proximidad_15min, cumple_proximidad_20min,
    cumple_estandar_primaria_15min, cumple_estandar_primaria_20min, cumple_estandar_farmacia_20min,
    cumple_hospital_30min, no_evaluable_a_pie, sin_zona_asignada, zbs_asignacion_espacial,
    bandera_A1_nueva_primaria, bandera_A2_ampliar_consultorio, bandera_A_nueva_farmacia,
    bandera_hospital_prioridad,
    bandera_B_reforzar_primaria,
    bandera_C_primaria_bien_cubierto, bandera_C_farmacia_bien_cubierto, bandera_C_hospital_bien_cubierto,
    Latitud, Longitud
  )

write_xlsx(
  list(
    "Municipios" = mun_out,
    "ZBS (ITAB)" = zbs %>% mutate(ITAB = round(ITAB, 2), ratioPrimaria = round(ratioPrimaria, 2)),
    "Gerencias (ITAH)" = ger %>% mutate(ITAH = round(ITAH, 2), ratioHospital = round(ratioHospital, 2))
  ),
  "./results/indicador_tension_sanitaria.xlsx"
)

message(sprintf("\nListo: indicador_tension_sanitaria.xlsx (%d municipios, %d ZBS, %d gerencias)",
                nrow(mun_out), nrow(zbs), nrow(ger)))
message("\nDistribucion ITAB:"); print(table(mun_out$nivel_ITAB))
message("\nBanderas:")
message(sprintf("  A1 nueva primaria (sin ningun recurso): %d", sum(mun_out$bandera_A1_nueva_primaria, na.rm = TRUE)))
message(sprintf("  A2 ampliar consultorio existente: %d", sum(mun_out$bandera_A2_ampliar_consultorio, na.rm = TRUE)))
message(sprintf("  A nueva farmacia: %d", sum(mun_out$bandera_A_nueva_farmacia, na.rm = TRUE)))
message(sprintf("  Prioridad hospital (>60min, no es 'abrir', revisar PAC): %d", sum(mun_out$bandera_hospital_prioridad, na.rm = TRUE)))
message(sprintf("  B reforzar primaria: %d", sum(mun_out$bandera_B_reforzar_primaria)))
message(sprintf("  B reforzar primaria: %d", sum(mun_out$bandera_B_reforzar_primaria, na.rm = TRUE)))
message(sprintf("  C bien cubierto (primaria): %d", sum(mun_out$bandera_C_primaria_bien_cubierto, na.rm = TRUE)))
message(sprintf("  C bien cubierto (farmacia): %d", sum(mun_out$bandera_C_farmacia_bien_cubierto, na.rm = TRUE)))
message(sprintf("  C bien cubierto (hospital): %d", sum(mun_out$bandera_C_hospital_bien_cubierto, na.rm = TRUE)))
message(sprintf("  Sin ZBS/Gerencia asignada (no evaluables en B/C): %d", sum(mun_out$sin_zona_asignada, na.rm = TRUE)))
message(sprintf("  ...de los cuales, resueltos por posicion geografica en vez de por nombre: %d", sum(mun_out$zbs_asignacion_espacial, na.rm = TRUE)))

pob <- sum(mun_out$poblacion_total)
message("\nCumplimiento de estandares de tiempo (en coche):")
message(sprintf("  Proximidad a primaria (cualquier recurso) <=15min: %.1f%% poblacion / %.1f%% municipios",
                100*sum(mun_out$poblacion_total[mun_out$cumple_proximidad_15min], na.rm = TRUE)/pob,
                100*mean(mun_out$cumple_proximidad_15min, na.rm = TRUE)))
message(sprintf("  Estandar de referencia (Centro de Salud)  <=15min: %.1f%% poblacion / %.1f%% municipios",
                100*sum(mun_out$poblacion_total[mun_out$cumple_estandar_primaria_15min], na.rm = TRUE)/pob,
                100*mean(mun_out$cumple_estandar_primaria_15min, na.rm = TRUE)))
message(sprintf("  Estandar de referencia (Centro de Salud)  <=20min: %.1f%% poblacion / %.1f%% municipios",
                100*sum(mun_out$poblacion_total[mun_out$cumple_estandar_primaria_20min], na.rm = TRUE)/pob,
                100*mean(mun_out$cumple_estandar_primaria_20min, na.rm = TRUE)))
message(sprintf("  Hospital general <=30min: %.1f%% poblacion / %.1f%% municipios",
                100*sum(mun_out$poblacion_total[mun_out$cumple_hospital_30min], na.rm = TRUE)/pob,
                100*mean(mun_out$cumple_hospital_30min, na.rm = TRUE)))
message(sprintf("  Municipios grandes (no evaluables a pie, informativo): %d", sum(mun_out$no_evaluable_a_pie)))
message(sprintf("  (Nota: %d municipios sin dato de duracion quedan excluidos de estos porcentajes, no contabilizados como incumplimiento)",
                sum(is.na(mun_out$cumple_proximidad_15min))))
