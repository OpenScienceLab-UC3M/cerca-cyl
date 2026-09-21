library(tidyverse)
library(readxl)
library(stringr)
library(writexl)

# Lectura y limpieza
censo_g1 <- read_excel(
  "./raw/censo_grupo1.xlsx",
  skip = 1,
  col_names = c(
    "municipio",
    "65_69",
    "70_74",
    "75_79",
    "80_84",
    "85_89",
    "90_94",
    "95_99",
    "100_mas"
  )
)


censo_g1 <- censo_g1 %>%
  filter(str_detect(municipio, "^\\d{5}\\s")) %>%
  mutate(
    codigo_municipio = str_extract(municipio, "^\\d{5}"),
    nombre_municipio = str_remove(municipio, "^\\d{5}\\s+")
  ) %>%
  select(
    codigo_municipio,
    nombre_municipio,
    everything(),
    -municipio
  )

censo_g1 %>%
  summarise(
    n = n(),
    municipios = n_distinct(codigo_municipio)
  )

# grupo 2
censo_g2_raw <- read_delim(
  "./raw/censo_grupo2.csv",
  delim = ";",
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)

censo_g2 <- censo_g2_raw %>%
  transmute(
    codigo_municipio = str_extract(Municipios, "^\\d{5}"),
    nombre_municipio = str_remove(Municipios, "^\\d{5}\\s+"),
    edad = Edad,
    total = Total
  )

censo_g2 <- censo_g2 %>%
  mutate(
    edad = recode(
      edad,
      "De 65 a 69 años" = "65_69",
      "De 70 a 74 años" = "70_74",
      "De 75 a 79 años" = "75_79",
      "De 80 a 84 años" = "80_84",
      "De 85 a 89 años" = "85_89",
      "De 90 a 94 años" = "90_94",
      "De 95 a 99 años" = "95_99",
      "100 y más años"  = "100_mas"
    )
  ) %>%
  pivot_wider(
    names_from = edad,
    values_from = total
  )

censo_g2 %>%
  summarise(
    n = n(),
    municipios = n_distinct(codigo_municipio)
  )

# unir censos de ambos grupos
censo_65mas_2025 <- bind_rows(
  censo_g1,
  censo_g2
)

# control
censo_65mas_2025 %>%
  summarise(
    n = n(),
    municipios = n_distinct(codigo_municipio),
    duplicados = n() - n_distinct(codigo_municipio)
  )

# coincide con el maestro de municipios y viceversa
municipios <- read_excel(
  "./raw/registro-de-municipios-de-castilla-y-leon.xlsx"
) %>% 
  rename(
    codigo_municipio = Cod_INE,
    nombre_municipio = Municipio
  ) %>% 
  mutate(
    codigo_municipio = str_pad(
      as.character(codigo_municipio),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

anti_join(
  municipios,
  censo_65mas_2025,
  by = "codigo_municipio"
) %>%
  select(codigo_municipio, nombre_municipio)

anti_join(
  censo_65mas_2025,
  municipios,
  by = "codigo_municipio"
) %>%
  select(codigo_municipio, nombre_municipio)



# Censos totales
censo_total_2025 <- read_excel(
  "./raw/censo_totales.xlsx"
) %>%
  rename(
    municipio_original = Municipios,
    poblacion_total = `2025`
  ) %>%
  mutate(
    codigo_municipio = str_extract(municipio_original, "^\\d{5}"),
    nombre_municipio = str_remove(municipio_original, "^\\d{5}\\s+")
  ) %>%
  select(
    codigo_municipio,
    nombre_municipio,
    poblacion_total
  )
# Control
censo_total_2025 %>%
  summarise(
    n = n(),
    municipios = n_distinct(codigo_municipio),
    sin_codigo = sum(is.na(codigo_municipio))
  )

# Población de 0 a 14 años
censo_0a14_2025 <- read_excel(
  "./raw/censo_0a14.xlsx"
) %>%
  rename(
    municipio_original = Municipios,
    edad_0_4 = `De 0 a 4 años`,
    edad_5_9 = `De 5 a 9 años`,
    edad_10_14 = `De 10 a 14 años`
  ) %>%
  mutate(
    codigo_municipio = str_extract(municipio_original, "^\\d{5}"),
    nombre_municipio = str_remove(municipio_original, "^\\d{5}\\s+"),
    poblacion_0_14 = edad_0_4 + edad_5_9 + edad_10_14
  ) %>%
  select(
    codigo_municipio,
    nombre_municipio,
    edad_0_4,
    edad_5_9,
    edad_10_14,
    poblacion_0_14
  )

# control
censo_0a14_2025 %>%
  summarise(
    n = n(),
    municipios = n_distinct(codigo_municipio),
    sin_codigo = sum(is.na(codigo_municipio))
  )

# Unir datasets 
columnas_edad_65 <- c(
  "65_69",
  "70_74",
  "75_79",
  "80_84",
  "85_89",
  "90_94",
  "95_99",
  "100_mas"
)

censo_65mas_2025 <- censo_65mas_2025 %>%
  mutate(
    across(
      all_of(columnas_edad_65),
      as.integer
    )
  )
censo_65mas_2025 <- censo_65mas_2025 %>%
  mutate(
    poblacion_65_mas =
      `65_69` +
      `70_74` +
      `75_79` +
      `80_84` +
      `85_89` +
      `90_94` +
      `95_99` +
      `100_mas`
  )

demografia_2025 <- censo_total_2025 %>%
  select(
    codigo_municipio,
    nombre_municipio,
    poblacion_total
  ) %>%
  left_join(
    censo_0a14_2025 %>%
      select(
        codigo_municipio,
        edad_0_4,
        edad_5_9,
        edad_10_14,
        poblacion_0_14
      ),
    by = "codigo_municipio"
  ) %>%
  left_join(
    censo_65mas_2025 %>%
      select(
        codigo_municipio,
        `65_69`,
        `70_74`,
        `75_79`,
        `80_84`,
        `85_89`,
        `90_94`,
        `95_99`,
        `100_mas`,
        poblacion_65_mas
      ),
    by = "codigo_municipio"
  )

# Índice de envejecimiento (%) y porcentaje de mayores de 65
demografia_2025 <- demografia_2025 %>%
  mutate(
    porcentaje_65_mas = 100 * poblacion_65_mas / poblacion_total,
    
    indice_envejecimiento = case_when(
      poblacion_0_14 > 0 ~ 100 * poblacion_65_mas / poblacion_0_14,
      TRUE ~ NA_real_
    )
  )

# 1. Comprobar cobertura
demografia_2025 %>%
  summarise(
    n = n(),
    municipios = n_distinct(codigo_municipio),
    sin_total = sum(is.na(poblacion_total)),
    sin_0_14 = sum(is.na(poblacion_0_14)),
    sin_65_mas = sum(is.na(poblacion_65_mas))
  )

# 2. Detectar municipios sin población menor de 15 años
demografia_2025 %>%
  filter(poblacion_0_14 == 0) %>%
  select(
    codigo_municipio,
    nombre_municipio,
    poblacion_total,
    poblacion_0_14,
    poblacion_65_mas
  )

# 3. Revisar distribución
summary(demografia_2025$porcentaje_65_mas)
summary(demografia_2025$indice_envejecimiento)


demografia_2025 <- demografia_2025 %>%
  mutate(
    across(
      c(
        poblacion_total,
        edad_0_4,
        edad_5_9,
        edad_10_14,
        poblacion_0_14,
        all_of(columnas_edad_65),
        poblacion_65_mas
      ),
      as.integer
    )
  )

demografia_2025 %>%
  select(
    codigo_municipio,
    nombre_municipio,
    poblacion_total,
    poblacion_0_14,
    poblacion_65_mas,
    porcentaje_65_mas,
    indice_envejecimiento
  ) %>%
  arrange(desc(indice_envejecimiento)) %>%
  slice_head(n = 20)
# Guardamos



write_xlsx(
  demografia_2025,
  "./processed/demografia_2025.xlsx"
)










zonas <- read_excel(
  "./raw/mapas-de-areas-de-salud-de-castilla-y-leon.xlsx"
)

zonas %>%
  distinct(MUNICIPIO, `CÓDIGO ZONA`) %>%
  count(MUNICIPIO, name = "n_zbs") %>%
  count(n_zbs)

zonas %>%
  distinct(MUNICIPIO, `CÓDIGO ZONA`, `Zona Básica de Salud`) %>%
  count(MUNICIPIO, name = "n_zbs") %>%
  filter(n_zbs > 1) %>%
  arrange(desc(n_zbs))
