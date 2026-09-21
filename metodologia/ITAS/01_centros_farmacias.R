library(tidyverse)
library(readxl)
library(stringr)
library(writexl)

# Carga de datos
centros_raw <- read_excel(
  "./raw/registro-de-centros-sanitarios-de-castilla-y-leon.xlsx"
)

# Cambio nombre variables
centros <- centros_raw %>%
  rename(
    nombre_centro = `Nombre del Centro`,
    numero_registro = `Nº de Registro`,
    direccion = Dirección,
    codigo_postal = `Código postal`,
    localidad = Localidad,
    provincia = Provincia,
    telefono = Teléfono,
    fax = Fax,
    tipo_centro_original = `Tipo de Centro`,
    finalidad_asistencial = `Finalidad Asistencial`,
    titular_centro = Titularidad,
    dependencia_funcional = `Dependencia Funcional`,
    posicion = Posición
  )

# Limpieza básica
centros <- centros %>%
  mutate(
    across(
      where(is.character),
      ~ str_squish(.x)
    ),
    
    codigo_postal = as.character(codigo_postal),
    codigo_postal = na_if(codigo_postal, "null"),
    codigo_postal = str_pad(
      codigo_postal,
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    telefono = as.character(telefono),
    fax = as.character(fax),
    
    telefono = na_if(telefono, "0"),
    fax = na_if(fax, "0")
  )

# Normalizar nombres (para join) manteniendo los originales 
centros <- centros %>%
  mutate(
    localidad_normalizada = localidad %>%
      str_to_upper() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_squish()
  )

# separar coordenadas
centros <- centros %>%
  separate(
    posicion,
    into = c("latitud", "longitud"),
    sep = ",",
    remove = FALSE,
    convert = TRUE
  )



## SECTOR - DEPENDENCIA FUNCIONAL
centros <- centros %>%
  mutate(
    sector = case_when(
      
      # Privado
      dependencia_funcional == "PRIVADOS" ~ "Privado",
      
      # Público
      dependencia_funcional %in% c(
        "SERVICIOS O INSTITUTOS DE SALUD DE LAS CCAA",
        "OTRAS ENTIDADES/ORGANISMOS PUBLIC.DEP.AUTONOMICA",
        "MUNICIPIO",
        "OTRAS ENTIDADES U ORGANISMOS PUBLICOS",
        "DIPUTACION O CABILDO",
        "OTRAS ENTIDADES/ORGANISMOS PUBLIC. DEP. ESTATAL",
        "MINISTERIO DE DEFENSA"
      ) ~ "Público",
      
      # Entidades que conviene conservar aparte
      dependencia_funcional %in% c(
        "MUTUAS DE ACCIDENTES DE TRABAJO Y ENF. PROF",
        "ORGANIZACIONES NO GUBERNAMENTALES"
      ) ~ "Mutua/ONG",
      
      # Ausentes
      is.na(dependencia_funcional) ~ "Sin información",
      
      # Seguridad por si aparece algún valor no contemplado
      TRUE ~ "Revisar"
    )
  )

# GRUPO CENTROS (no servirá para el IAS, solo para organizar el dataset)
centros <- centros %>%
  mutate(
    grupo_centro = case_when(
      
      # Atención primaria
      tipo_centro_original %in% c(
        "CONSULTORIOS DE ATENCION PRIMARIA",
        "CENTROS DE ATENCION PRIMARIA: CENTROS DE SALUD"
      ) ~ "Atención primaria",
      
      # Atención hospitalaria
      tipo_centro_original %in% c(
        "HOSPITALES GENERALES",
        "HOSPITALES DE SALUD MENTAL Y TRATAMIENTO DE TOXICOMANIAS",
        "HOSPITAL DE MEDIA Y LARGA ESTANCIA",
        "HOSPITALES ESPECIALIZADOS"
      ) ~ "Atención hospitalaria",
      
      # Atención especializada
      tipo_centro_original %in% c(
        "OTROS CENTROS ESPECIALIZADOS",
        "ESPECIALIZADOS: CENTROS DE RECONOCIMIENTO",
        "ESPECIALIZADOS: CENTROS DE DIAGNOSTICO",
        "ESPECIALIZADOS: CENTROS DE REPRODUCCION HUMANA ASISTIDA",
        "ESPECIALIZADOS: CENTROS DE DIALISIS",
        "ESPECIALIZADOS: CENTROS DE SALUD MENTAL",
        "ESPECIALIZADOS: CENTROS DE TRANSFUSION",
        "ESPECIALIZADOS: BANCOS DE TEJIDOS",
        "ESPECIALIZADOS: CENTROS DE INTERRUPCION VOLUNTARIA DEL EMBARAZO",
        "ESPECIALIZADOS: CENTROS DE CIRUGIA MAYOR AMBULATORIA"
      ) ~ "Atención especializada",
      
      # Consultas y centros polivalentes
      tipo_centro_original %in% c(
        "CONSULTAS MEDICAS",
        "CONSULTAS DE OTROS PROFESIONALES SANITARIOS",
        "CENTROS POLIVALENTES"
      ) ~ "Consultas y centros polivalentes",
      
      # Salud bucodental
      tipo_centro_original ==
        "ESPECIALIZADOS: CLINICAS DENTALES" ~
        "Salud bucodental",
      
      # Establecimientos sanitarios
      tipo_centro_original %in% c(
        "ESTABLECIMIENTOS SANITARIOS (OPTICAS, ORTOPEDIAS, AUDIOPROTESIS)",
        "ESTABLECIMIENTO DE OPTICA",
        "ESTABLECIMIENTO DE ORTOPEDIA",
        "ESTABLECIMIENTO DE AUDIOPROTESIS"
      ) ~ "Establecimientos sanitarios",
      
      # Servicios integrados en organizaciones no sanitarias
      tipo_centro_original ==
        "SERVICIOS SANITARIOS INTEGRADOS EN ORGANIZACION NO SANITARIA" ~
        "Servicios en organizaciones no sanitarias",
      
      # Centros móviles
      tipo_centro_original ==
        "CENTROS MOVILES DE ASISTENCIA SANITARIA" ~
        "Centros móviles",
      
      # Otros proveedores
      tipo_centro_original ==
        "OTROS PROVEEDORES DE ASIST. SANITARIA SIN INTERNAMIENTO" ~
        "Otros proveedores",
      
      # Sin información
      is.na(tipo_centro_original) ~
        "Sin información",
      
      # Control
      TRUE ~ "Revisar"
    )
  )


# Subtipo de centro
centros <- centros %>%
  mutate(
    subtipo_centro = case_when(
      
      tipo_centro_original == 
        "OTROS PROVEEDORES DE ASIST. SANITARIA SIN INTERNAMIENTO" ~
        "Otros proveedores sin internamiento",
      
      tipo_centro_original == 
        "CONSULTAS DE OTROS PROFESIONALES SANITARIOS" ~
        "Consulta de otros profesionales",
      
      tipo_centro_original == 
        "CENTROS POLIVALENTES" ~
        "Centro polivalente",
      
      tipo_centro_original == 
        "SERVICIOS SANITARIOS INTEGRADOS EN ORGANIZACION NO SANITARIA" ~
        "Servicio sanitario en organización no sanitaria",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CLINICAS DENTALES" ~
        "Clínica dental",
      
      tipo_centro_original == 
        "ESTABLECIMIENTOS SANITARIOS (OPTICAS, ORTOPEDIAS, AUDIOPROTESIS)" ~
        "Establecimiento sanitario mixto",
      
      tipo_centro_original == 
        "ESTABLECIMIENTO DE OPTICA" ~
        "Óptica",
      
      tipo_centro_original == 
        "ESTABLECIMIENTO DE AUDIOPROTESIS" ~
        "Audioprótesis",
      
      tipo_centro_original == 
        "OTROS CENTROS ESPECIALIZADOS" ~
        "Otro centro especializado",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE TRANSFUSION" ~
        "Centro de transfusión",
      
      tipo_centro_original == 
        "CONSULTAS MEDICAS" ~
        "Consulta médica",
      
      tipo_centro_original == 
        "CENTROS MOVILES DE ASISTENCIA SANITARIA" ~
        "Centro móvil",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE REPRODUCCION HUMANA ASISTIDA" ~
        "Centro de reproducción asistida",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE DIAGNOSTICO" ~
        "Centro de diagnóstico",
      
      tipo_centro_original == 
        "CONSULTORIOS DE ATENCION PRIMARIA" ~
        "Consultorio",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE DIALISIS" ~
        "Centro de diálisis",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE RECONOCIMIENTO" ~
        "Centro de reconocimiento",
      
      tipo_centro_original == 
        "CENTROS DE ATENCION PRIMARIA: CENTROS DE SALUD" ~
        "Centro de salud",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE SALUD MENTAL" ~
        "Centro de salud mental",
      
      tipo_centro_original == 
        "ESTABLECIMIENTO DE ORTOPEDIA" ~
        "Ortopedia",
      
      tipo_centro_original == 
        "HOSPITALES GENERALES" ~
        "Hospital general",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: BANCOS DE TEJIDOS" ~
        "Banco de tejidos",
      
      tipo_centro_original == 
        "HOSPITALES DE SALUD MENTAL Y TRATAMIENTO DE TOXICOMANIAS" ~
        "Hospital de salud mental",
      
      tipo_centro_original == 
        "HOSPITAL DE MEDIA Y LARGA ESTANCIA" ~
        "Hospital de media/larga estancia",
      
      tipo_centro_original == 
        "HOSPITALES ESPECIALIZADOS" ~
        "Hospital especializado",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE CIRUGIA MAYOR AMBULATORIA" ~
        "Centro de cirugía mayor ambulatoria",
      
      tipo_centro_original == 
        "ESPECIALIZADOS: CENTROS DE INTERRUPCION VOLUNTARIA DEL EMBARAZO" ~
        "Centro de interrupción voluntaria del embarazo",
      
      is.na(tipo_centro_original) ~
        "Sin información",
      
      TRUE ~ "Revisar"
    )
  )

# variable número de finalidades por centro
centros <- centros %>%
  mutate(
    n_finalidades = case_when(
      is.na(finalidad_asistencial) ~ 0L,
      TRUE ~ str_count(finalidad_asistencial, "#") + 1L
    )
  )

## FINALIDAD ASISTENCIAL (tabla separada)

centros_finalidades <- centros %>%
  select(
    numero_registro,
    nombre_centro,
    provincia,
    localidad,
    grupo_centro,
    subtipo_centro,
    sector,
    finalidad_asistencial
  ) %>%
  separate_rows(
    finalidad_asistencial,
    sep = "#"
  ) %>%
  mutate(
    finalidad_asistencial = str_squish(finalidad_asistencial)
  ) %>%
  filter(
    !is.na(finalidad_asistencial),
    finalidad_asistencial != ""
  ) %>%
  distinct()


# finalidades distintas: 119
centros_finalidades %>%
  summarise(
    n_finalidades = n_distinct(finalidad_asistencial)
  )
# más comunes
centros_finalidades %>%
  count(finalidad_asistencial, sort = TRUE) %>%
  print(n = 120)

# número de finalidades por centro (ID)
finalidades_por_centro <- centros_finalidades %>%
  count(
    numero_registro,
    name = "n_finalidades"
  )




cobertura_finalidad <- centros %>%
  group_by(grupo_centro, subtipo_centro) %>%
  summarise(
    n_centros = n(),
    n_con_finalidad = sum(!is.na(finalidad_asistencial)),
    pct_con_finalidad = 100 * n_con_finalidad / n_centros,
    .groups = "drop"
  ) %>%
  arrange(pct_con_finalidad)

centros_finalidades %>%
  filter(grupo_centro == "Atención primaria") %>%
  count(
    subtipo_centro,
    finalidad_asistencial,
    sort = TRUE
  ) %>%
  print(n = 100)


# Nivel de atencion primaria
# 0 = no es infraestructura de atención primaria
# 1 = consultorio
# 2 = centro de salud
#Nota: 2 no representa el dobre de accesibilidad, solo es una variable ordinal

centros <- centros %>%
  mutate(
    nivel_atencion_primaria = case_when(
      subtipo_centro == "Centro de salud" ~ 2L,
      subtipo_centro == "Consultorio" ~ 1L,
      TRUE ~ 0L
    )
  )
# version categorica
centros <- centros %>%
  mutate(
    categoria_atencion_primaria = case_when(
      subtipo_centro == "Centro de salud" ~ "Centro de salud",
      subtipo_centro == "Consultorio" ~ "Consultorio",
      TRUE ~ "No atención primaria"
    )
  )

centros <- centros %>%
  mutate(
    recurso = case_when(
      grupo_centro == "Atención hospitalaria" ~ "Hospital",
      subtipo_centro == "Centro de salud" ~ "Centro de salud",
      subtipo_centro == "Consultorio" ~ "Consultorio",
      TRUE ~ "Otros"
    )
  )

# Guardar tablas y datasets

# Dataset principal de centros limpio y clasificado
write_csv(
  centros,
  "./processed/centros_sanitarios_limpio.csv"
)
write_xlsx(
  centros,
  "./processed/centros_sanitarios_limpio.xlsx"
)
# Relación centro-finalidad asistencial
write_csv(
  centros_finalidades,
  "./processed/centros_finalidades.csv"
)
write_xlsx(
  centros_finalidades,
  "./processed/centros_finalidades.xlsx"
)

# Número de finalidades por centro
write_csv(
  finalidades_por_centro,
  "./processed/finalidades_por_centro.csv"
)
write_xlsx(
  finalidades_por_centro,
  "./processed/finalidades_por_centro.xlsx"
)

##### FARMACIAS

farmacias_raw <- readr::read_csv2(
  "./raw/registro-de-establecimientos-farmaceuticos-de-castilla-y-leon.csv",
  col_types = cols(
    .default = col_character()
  ),
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)
names(farmacias_raw)[1] <- "NUM_REG"

farmacias <- farmacias_raw %>%
  rename(
    numero_registro = NUM_REG,
    nombre_farmacia = NOMBRE_COMERCIAL,
    telefono = TELEFONO,
    calle = CALLE,
    provincia = PROVINCIA,
    localidad = LOCALIDAD,
    municipio = MUNICIPIO,
    codigo_postal = CODIGO_POSTAL,
    numero = NUMERO
  )
# Limpieza básica
farmacias <- farmacias %>%
  mutate(
    across(
      where(is.character),
      ~ str_squish(.x)
    ),
    telefono = na_if(telefono, "0"),
    codigo_postal = na_if(codigo_postal, "null")
  )
# Normalización para join
farmacias <- farmacias %>%
  mutate(
    municipio_normalizado = municipio %>%
      str_to_upper() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_squish(),
    
    localidad_normalizada = localidad %>%
      str_to_upper() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_squish()
  )

# Normalizar municipios
normalizar_municipio <- function(x) {
  x %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_squish()
}
farmacias <- farmacias %>%
  mutate(
    municipio_normalizado = normalizar_municipio(municipio)
  )





farmacias %>%
  summarise(
    n = n(),
    n_registros = n_distinct(numero_registro),
    n_sin_registro = sum(is.na(numero_registro)),
    n_sin_provincia = sum(is.na(provincia)),
    n_sin_municipio = sum(is.na(municipio)),
    n_sin_localidad = sum(is.na(localidad)),
    n_sin_cp = sum(is.na(codigo_postal)),
    n_sin_telefono = sum(is.na(telefono))
  )
farmacias %>%
  count(numero_registro) %>%
  filter(!is.na(numero_registro), n > 1)

farmacias %>%
  count(provincia, sort = TRUE)

farmacias %>%
  summarise(
    n_localidad_distinta_municipio =
      sum(localidad_normalizada != municipio_normalizado, na.rm = TRUE),
    
    pct_localidad_distinta_municipio =
      100 * mean(localidad_normalizada != municipio_normalizado, na.rm = TRUE)
  )
farmacias %>%
  filter(localidad_normalizada != municipio_normalizado) %>%
  select(
    numero_registro,
    nombre_farmacia,
    provincia,
    localidad,
    municipio,
    codigo_postal
  ) %>%
  slice_head(n = 30)


# MUNICIPIOS MAESTRO
municipios_raw <- read_excel(
  "./raw/registro-de-municipios-de-castilla-y-leon.xlsx"
)
municipios <- municipios_raw %>%
  rename(
    nombre_municipio = Municipio,
    codigo_municipio = Cod_Municipio,
    provincia = Provincia,
    codigo_provincia = Cod_Provincia,
    codigo_ine = Cod_INE,
    poblacion = Población,
    longitud = Longitud,
    latitud = Latitud
  )


municipios <- municipios %>%
  mutate(
    codigo_municipio_ine = paste0(
      codigo_provincia,
      codigo_municipio
    )
  )
municipios <- municipios %>%
  rename(
    codigo_municipio_provincial = codigo_municipio
  ) %>%
  mutate(
    codigo_municipio = paste0(
      codigo_provincia,
      codigo_municipio_provincial
    )
  )

# Normalizar nombres
normalizar_municipio <- function(x) {
  x %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_squish()
}

municipios <- municipios %>%
  mutate(
    municipio_normalizado = normalizar_municipio(nombre_municipio)
  )

farmacias <- farmacias %>%
  mutate(
    municipio_normalizado = normalizar_municipio(municipio)
  )

# cruce con provincia y municipio

farmacias <- farmacias %>%
  mutate(
    provincia_normalizada = provincia %>%
      str_to_upper() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_squish()
  )

municipios <- municipios %>%
  mutate(
    provincia_normalizada = provincia %>%
      str_to_upper() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_squish()
  )

normalizar_municipio <- function(x) {
  x %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_squish() %>%
    str_replace("^EL ", "") %>%
    str_replace("^LA ", "") %>%
    str_replace("^LOS ", "") %>%
    str_replace("^LAS ", "") %>%
    str_replace(", EL$", "") %>%
    str_replace(", LA$", "") %>%
    str_replace(", LOS$", "") %>%
    str_replace(", LAS$", "") %>%
    str_squish()
}

farmacias <- farmacias %>%
  mutate(
    municipio_normalizado = normalizar_municipio(municipio)
  )

municipios <- municipios %>%
  mutate(
    municipio_normalizado = normalizar_municipio(nombre_municipio)
  )
farmacias <- farmacias %>%
  mutate(
    municipio_normalizado = case_when(
      provincia_normalizada == "LEON" &
        municipio_normalizado == "CANDIN" ~ "VALLE DE ANCARES",
      TRUE ~ municipio_normalizado
    )
  )
farmacias_cruce <- farmacias %>%
  left_join(
    municipios %>%
      select(
        codigo_municipio,
        nombre_municipio,
        provincia_normalizada,
        municipio_normalizado
      ),
    by = c(
      "provincia_normalizada",
      "municipio_normalizado"
    )
  )


farmacias_cruce %>%
  summarise(
    n = n(),
    n_con_codigo = sum(!is.na(codigo_municipio)),
    n_sin_codigo = sum(is.na(codigo_municipio)),
    pct_con_codigo = 100 * mean(!is.na(codigo_municipio))
  )




# Guardar farmacias
write_csv(
  farmacias_cruce,
  "./processed/farmacias_limpio.csv"
)
write_xlsx(
  farmacias_cruce,
  "./processed/farmacias_limpio.xlsx"
)

saveRDS(
  farmacias_cruce,
  "./processed/farmacias_limpio.rds"
)


# ============================================================
# Geocodificación de farmacias con Nominatim (OpenStreetMap)
# ============================================================
# No requiere API key. Nominatim limita a 1 petición/segundo,
# así que con ~1.590 filas el script tardará unos 25-30 minutos.
# Se puede interrumpir y reanudar (usa checkpoint automático).
#
# Cambios respecto a la versión anterior:
#   1. Código postal: se conserva el cero inicial (formato de 5 dígitos).
#   2. Municipio: se usa 'nombre_municipio' (con tildes y artículos
#      correctos) en vez de 'municipio_normalizado'.
#   3. Calle: se elimina el prefijo de tipo de vía duplicado
#      (ej. "CALLE AVDA. SAN ANDRES" -> "AVDA. SAN ANDRES").
#   4. Se piden los metadatos de Nominatim (class/type) para poder
#      auditar después la precisión de los casos S/N y numero = 0.

# --- 1. Paquetes ---------------------------------------------
paquetes <- c("readxl", "writexl", "tidygeocoder", "dplyr", "stringr")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(readxl)
library(writexl)
library(tidygeocoder)
library(dplyr)
library(stringr)

# --- 2. Cargar datos --------------------------------------------
archivo_entrada <- "./processed/farmacias_limpio.xlsx"   # ajusta la ruta si hace falta
archivo_salida  <- "./processed/farmacias_con_coordenadas.xlsx"
archivo_parcial <- "./processed/farmacias_progreso.rds"  # checkpoint para reanudar

df <- read_excel(archivo_entrada)

# --- 3. Limpiar 'calle': quitar prefijos de tipo de vía repetidos -----
# El dato original a veces antepone un tipo de vía genérico a una
# calle que YA empezaba con su propio tipo (igual o distinto), p.ej.
# "CALLE AVDA. SAN ANDRES", "AVDA AVDA. GENERAL GOMEZ NUÑEZ",
# "CALLE CALLE EL FRONTON", "CALLE C/ PASEO LA SERNA".
# Se eliminan tokens de tipo de vía repetidos al principio hasta que
# solo queda uno (el más cercano al nombre real de la calle).
tipos_via <- c("CALLE", "C/", "AVDA", "AVDA.", "AVENIDA", "CTRA", "CTRA.",
               "CARRETERA", "PLAZA", "PZA", "PZA.", "PASEO", "CAMINO",
               "TRAVESIA", "TRAVESÍA", "RONDA", "GLORIETA",
               "URB", "URB.", "URBANIZACION", "URBANIZACIÓN")

limpiar_calle <- function(x) {
  if (is.na(x)) return(x)
  palabras <- str_split(str_to_upper(str_trim(x)), "\\s+")[[1]]
  while (length(palabras) >= 2 && palabras[1] %in% tipos_via && palabras[2] %in% tipos_via) {
    palabras <- palabras[-1]
  }
  paste(palabras, collapse = " ")
}

df <- df %>%
  rowwise() %>%
  mutate(calle_limpia = limpiar_calle(calle)) %>%
  ungroup()

# --- 4. Determinar si el número es fiable (S/N, vacío o 0) -----------
numero_norm <- str_to_upper(str_trim(as.character(df$numero)))
df <- df %>%
  mutate(numero_fiable = !(is.na(numero) | numero_norm %in% c("S/N", "0", "0.0")))

# --- 5. Construir la dirección de búsqueda ----------------------------
# calle_limpia + número (solo si es fiable) + CP con cero inicial +
# nombre_municipio (con tilde/artículo correcto) + provincia
construir_direccion <- function(calle, numero, numero_fiable, cp, municipio, provincia) {
  numero_txt <- ifelse(numero_fiable, as.character(numero), "")
  cp_txt <- ifelse(is.na(cp), "", sprintf("%05d", as.integer(cp)))
  partes <- c(
    str_squish(paste(calle, numero_txt)),
    cp_txt,
    municipio,
    provincia,
    "España"
  )
  partes <- partes[!is.na(partes) & partes != ""]
  paste(partes, collapse = ", ")
}

df <- df %>%
  rowwise() %>%
  mutate(direccion_busqueda = construir_direccion(
    calle_limpia, numero, numero_fiable, codigo_postal, nombre_municipio, provincia_normalizada
  )) %>%
  ungroup()

# --- 6. Reanudar si ya hay progreso guardado ----------------------
if (file.exists(archivo_parcial)) {
  df_geo <- readRDS(archivo_parcial)
  # Recalcula las direcciones de búsqueda con la lógica ya corregida
  # (importante si has actualizado el script tras una ejecución previa):
  # las filas ya geocodificadas no se tocan, solo se refresca el texto
  # de búsqueda para las que aún están pendientes.
  df_geo$direccion_busqueda <- df$direccion_busqueda
  message("Reanudando desde checkpoint: ", sum(!is.na(df_geo$lat)), " filas ya geocodificadas.")
} else {
  df_geo <- df %>% mutate(lat = NA_real_, long = NA_real_, osm_class = NA_character_, osm_type = NA_character_)
}

pendientes <- which(is.na(df_geo$lat))

# --- 7. Geocodificar fila a fila, guardando progreso cada 25 -----
nivel_calle <- c("highway", "building", "house", "residential", "road")

for (i in seq_along(pendientes)) {
  idx <- pendientes[i]
  
  resultado <- tryCatch(
    geo(address = df_geo$direccion_busqueda[idx], method = "osm", full_results = TRUE, quiet = TRUE),
    error = function(e) tibble(lat = NA_real_, long = NA_real_, class = NA_character_, type = NA_character_)
  )
  
  df_geo$lat[idx]       <- resultado$lat[1]
  df_geo$long[idx]      <- resultado$long[1]
  df_geo$osm_class[idx] <- if ("class" %in% names(resultado)) resultado$class[1] else NA_character_
  df_geo$osm_type[idx]  <- if ("type" %in% names(resultado)) resultado$type[1] else NA_character_
  
  # Nominatim exige max. 1 petición/segundo
  Sys.sleep(1)
  
  # Checkpoint cada 25 filas por si se interrumpe
  if (i %% 25 == 0 || i == length(pendientes)) {
    saveRDS(df_geo, archivo_parcial)
    message(sprintf("Progreso: %d/%d filas geocodificadas", i, length(pendientes)))
  }
}

# --- 8. Clasificar precisión del resultado --------------------------
df_geo <- df_geo %>%
  mutate(
    precision = case_when(
      is.na(lat) ~ "sin_resultado",
      osm_class %in% nivel_calle | osm_type %in% nivel_calle ~ "nivel_calle",
      TRUE ~ "solo_municipio"
    )
  )

# --- 9. Guardar resultado final -----------------------------------
df_final <- df_geo %>% select(-direccion_busqueda, -calle_limpia)
write_xlsx(df_final, archivo_salida)

sin_coordenadas <- sum(is.na(df_final$lat))
message(sprintf(
  "\nListo. %d/%d farmacias geocodificadas. %d sin resultado.",
  nrow(df_final) - sin_coordenadas, nrow(df_final), sin_coordenadas
))
message("\n--- Resumen de precisión (todas las filas) ---")
print(table(df_final$precision))
message("\n--- Resumen de precisión (solo numero_fiable = FALSE, es decir S/N o 0) ---")
print(table(df_final$precision[!df_final$numero_fiable]))


# ============================================================
# Relleno de coordenadas con Google Geocoding (solo casos problemáticos)
# ============================================================
# Toma el resultado de Nominatim (farmacias_con_coordenadas.xlsx) y
# vuelve a geocodificar SOLO las filas marcadas como:
#   - "sin_resultado"   (Nominatim no encontró nada)
#   - "solo_municipio"  (Nominatim solo acertó el centroide del municipio)
# usando Google Geocoding, que suele tener mejor cobertura rural.
#
# Para las filas sin número fiable (S/N o "0"), se añade el nombre
# de la farmacia/titular a la búsqueda, ya que Google sí sabe usarlo
# como referencia de negocio (a diferencia de Nominatim).
#
# Requiere una API key de Google (ver instrucciones aparte).
# Con ~484 filas te queda muy por debajo de la cuota gratuita de
# 10.000 peticiones/mes.

# --- 1. Paquetes ---------------------------------------------
paquetes <- c("readxl", "writexl", "tidygeocoder", "dplyr", "stringr")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(readxl)
library(writexl)
library(tidygeocoder)
library(dplyr)
library(stringr)

# --- 2. API key ------------------------------------------------
# Nunca escribas la key directamente en el script. Dos formas seguras:
#
# Opción A (recomendada): guárdala una vez en tu .Renviron
#   usethis::edit_r_environ()
#   -> añade la línea: GOOGLE_MAPS_API_KEY=tu_clave_aqui
#   -> guarda, cierra R y reinicia la sesión
#
# Opción B (solo para esta sesión, se pierde al cerrar R):
#   Sys.setenv(GOOGLE_MAPS_API_KEY = "tu_clave_aqui")

api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
if (api_key == "") {
  stop("No se ha encontrado GOOGLE_MAPS_API_KEY. Configúrala antes de continuar (ver comentarios arriba).")
}

# tidygeocoder busca la key de Google en esta variable de entorno concreta,
# no como argumento de geo(). La traducimos aquí para que solo tengas que
# gestionar un nombre de variable (GOOGLE_MAPS_API_KEY) en tu .Renviron.
Sys.setenv(GOOGLEGEOCODE_API_KEY = api_key)

# --- 3. Cargar el resultado de Nominatim ----------------------------
archivo_entrada <- "./processed/farmacias_con_coordenadas.xlsx"
archivo_salida  <- "./processed/farmacias_con_coordenadas_final.xlsx"

df <- read_excel(archivo_entrada)

problematicas <- df %>% filter(precision %in% c("sin_resultado", "solo_municipio"))
message(sprintf("Filas a re-geocodificar con Google: %d", nrow(problematicas)))

# --- 4. Construir dirección de búsqueda para Google ------------------
# Si el número no es fiable (S/N o 0), se antepone el nombre de la
# farmacia/titular como ayuda adicional para que Google la ancle
# como negocio, no solo como punto de calle.
construir_direccion_google <- function(nombre_farmacia, calle, numero, numero_fiable, cp, municipio, provincia) {
  numero_txt <- ifelse(numero_fiable, as.character(numero), "")
  cp_txt <- ifelse(is.na(cp), "", sprintf("%05d", as.integer(cp)))
  nombre_txt <- ifelse(numero_fiable, "", paste0("Farmacia ", nombre_farmacia, ", "))
  partes <- c(
    str_squish(paste0(nombre_txt, calle, " ", numero_txt)),
    cp_txt,
    municipio,
    provincia,
    "España"
  )
  partes <- partes[!is.na(partes) & partes != ""]
  paste(partes, collapse = ", ")
}

problematicas <- problematicas %>%
  rowwise() %>%
  mutate(direccion_google = construir_direccion_google(
    nombre_farmacia, calle, numero, numero_fiable,
    codigo_postal, nombre_municipio, provincia_normalizada
  )) %>%
  ungroup()

# --- 5. Geocodificar con Google --------------------------------------
resultados <- geo(
  address = problematicas$direccion_google,
  method = "google",
  full_results = TRUE,
  quiet = TRUE
)

problematicas$lat_google  <- resultados$lat
problematicas$long_google <- resultados$long
# 'location_type' de Google indica precisión: ROOFTOP es la mejor
problematicas$google_location_type <- if ("location_type" %in% names(resultados)) resultados$location_type else NA_character_

# --- 6. Fusionar de vuelta con el dataset completo -------------------
# Solo se sobrescriben lat/long donde Google SÍ encontró resultado;
# si Google tampoco encuentra nada, se conserva lo que ya había de Nominatim.
df <- df %>%
  left_join(
    problematicas %>% select(numero_registro, lat_google, long_google, google_location_type),
    by = "numero_registro"
  ) %>%
  mutate(
    fuente_coordenada = case_when(
      !is.na(lat_google) ~ "google",
      !is.na(lat) ~ "nominatim",
      TRUE ~ "sin_resultado"
    ),
    lat  = ifelse(!is.na(lat_google), lat_google, lat),
    long = ifelse(!is.na(long_google), long_google, long)
  ) %>%
  select(-lat_google, -long_google)

# --- 7. Guardar resultado final ---------------------------------------
write_xlsx(df, archivo_salida)

message("\n--- Resumen final ---")
print(table(df$fuente_coordenada))
message(sprintf(
  "\nSin coordenadas tras el fallback con Google: %d de %d",
  sum(is.na(df$lat)), nrow(df)
))
message(sprintf("\nGuardado en: %s", archivo_salida))


