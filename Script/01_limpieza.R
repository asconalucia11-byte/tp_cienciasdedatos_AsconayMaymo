# =============================================================================
# Ciencia de Datos para Economía y Negocios — FCE-UBA
# TP Grupo 7 — Ascona Cortina Lucia | Maymo Skansi Joaquin
#
# Script 01 — Limpieza y unificación de bases
#
# Objetivo: Leer los archivos Excel de la CGI (INDEC) y los Agregados
#           Macroeconómicos (INDEC), limpiarlos y unirlos en una base
#           tidy lista para el análisis. Período: 2016-2022 (datos definitivos).
#
# Inputs:   raw/serie_cgi_01_26.xls
#           raw/sh_VBP_VAB_03_26__1_.xls
# Outputs:  input/cgi_limpia.csv
#           input/macro_limpia.csv
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

rm(list = ls()) #limpiar el entorno

# Evitar notación científica en los outputs
options(scipen = 999)

# Rutas de entrada y salida
instub  <- "Raw"
outstub <- "Input"

# Rutas a los archivos
ruta_cgi   <- file.path(instub, "serie_cgi_01_26.xls")
ruta_macro <- file.path(instub, "sh_VBP_VAB_03_26.xls")

# Años de análisis
anios_analisis <- as.character(2016:2022)


# --- 1. Exploración inicial --------------------------------------------------

# Ver qué hojas tiene cada archivo
excel_sheets(ruta_cgi)
excel_sheets(ruta_macro)

# Hojas que vamos a usar de la CGI:
# VAB_pb, RTA, IBM, Puestos, Puestos AR, Puestos ANR, Puestos NA
# (Excluimos: T-S, EEB y todas las hojas "pp")


# --- 2. Función para leer hojas de la CGI ------------------------------------

# Todas las hojas de la CGI tienen la misma estructura:
#   Fila 3 (índice 2): años (2016, 2017, ...)
#   Fila 4 (índice 3): trimestres (1°, 2°, 3°, 4°, Total)
#   Fila 7 en adelante: datos por sector
#
# La columna "Total" ya tiene el dato anual calculado por el INDEC,
# así que no necesitamos promediar los cuatro trimestres nosotros.
# Esta función extrae solo esas columnas "Total" para los años 2016-2022.

leer_hoja_cgi <- function(archivo, hoja, nombre_variable) {
  
  raw <- read_excel(archivo, sheet = hoja, col_names = FALSE)
  
  # Rellenar los NA de la fila de años hacia adelante
  fila_anios <- as.character(unlist(raw[3, ]))
  for (i in 2:length(fila_anios)) {
    if (is.na(fila_anios[i])) fila_anios[i] <- fila_anios[i - 1]
  }
  
  fila_trimestre <- as.character(unlist(raw[4, ]))
  
  # Columnas donde el año está en 2016-2022 Y el trimestre es "Total"
  cols_total <- which(
    fila_anios %in% anios_analisis &
      fila_trimestre == "Total"
  )
  
  anios  <- fila_anios[cols_total]
  datos  <- raw[7:nrow(raw), ]
  sectores <- as.character(unlist(datos[, 2]))
  valores  <- datos[, cols_total]
  colnames(valores) <- anios
  
  bind_cols(tibble(sector = sectores), valores) |>
    pivot_longer(cols = -sector, names_to = "anio", values_to = nombre_variable) |>
    mutate(
      anio = as.integer(anio),
      !!nombre_variable := as.numeric(.data[[nombre_variable]])
    ) |>
    filter(!is.na(sector), sector != "NA", !is.na(.data[[nombre_variable]]))
}



# --- 3. Leer cada hoja relevante de la CGI -----------------------------------

# VAB a precios básicos por sector
vab <- leer_hoja_cgi(ruta_cgi, "VAB_pb", "vab")

# Remuneración al Trabajo Asalariado
rta <- leer_hoja_cgi(ruta_cgi, "RTA", "rta")

# Ingreso Mixto Bruto
imb <- leer_hoja_cgi(ruta_cgi, "IBM", "imb")

# Puestos de trabajo totales
puestos_total <- leer_hoja_cgi(ruta_cgi, "Puestos", "puestos_total")

# Puestos asalariados registrados
puestos_ar <- leer_hoja_cgi(ruta_cgi, "Puestos AR", "puestos_ar")

# Puestos asalariados no registrados
puestos_anr <- leer_hoja_cgi(ruta_cgi, "Puestos ANR", "puestos_anr")

# Puestos no asalariados
puestos_na <- leer_hoja_cgi(ruta_cgi, "Puestos NA", "puestos_na")

# Verificar estructura de una hoja antes de continuar
glimpse(vab)


# --- 4. Unir todas las hojas de la CGI en una sola base ---------------------

# Usamos left_join() encadenado: todas las hojas comparten sector + anio
# como clave, entonces unimos sobre esas dos columnas.

cgi_limpia <- vab |>
  left_join(rta,           by = c("sector", "anio")) |>
  left_join(imb,           by = c("sector", "anio")) |>
  left_join(puestos_total, by = c("sector", "anio")) |>
  left_join(puestos_ar,    by = c("sector", "anio")) |>
  left_join(puestos_anr,   by = c("sector", "anio")) |>
  left_join(puestos_na,    by = c("sector", "anio"))

# Verificar resultado final
glimpse(cgi_limpia)
dim(cgi_limpia)

# Ver los sectores disponibles
unique(cgi_limpia$sector)

View(cgi_limpia)

# --- 5. Leer Cuadro 2 de Agregados Macroeconómicos --------------------------

# El Cuadro 2 tiene VBP y VAB a precios básicos por sector, con datos
# anuales desde 2004. Nos quedamos solo con 2016-2022.
# Estructura: fila 4 = años, datos desde fila 7.

raw_macro <- read_excel(ruta_macro, sheet = "Cuadro 2", col_names = FALSE)

# Identificar columnas de 2016 a 2022
fila_anios_macro <- as.character(unlist(raw_macro[4, ]))
cols_macro       <- which(fila_anios_macro %in% anios_analisis)
anios_macro      <- fila_anios_macro[cols_macro]

# Datos desde fila 7
datos_macro    <- raw_macro[7:nrow(raw_macro), ]
sectores_macro <- as.character(unlist(datos_macro[, 1]))
valores_macro  <- datos_macro[, cols_macro]
colnames(valores_macro) <- anios_macro

# Armar y pasar a formato largo
macro_limpia <- bind_cols(
  tibble(sector = sectores_macro),
  valores_macro
) |>
  pivot_longer(
    cols      = -sector,
    names_to  = "anio",
    values_to = "vbp_macro"
  ) |>
  mutate(
    anio      = as.integer(anio),
    vbp_macro = as.numeric(vbp_macro)
  ) |>
  filter(!is.na(sector), sector != "NA", !is.na(vbp_macro))

glimpse(macro_limpia)

View(macro_limpia)

# --- 6. Guardar bases limpias ------------------------------------------------

write_csv(cgi_limpia,   file.path(outstub, "cgi_limpia.csv"))
write_csv(macro_limpia, file.path(outstub, "macro_limpia.csv"))

cat("Limpieza completada.\n")
cat("  cgi_limpia:   ", nrow(cgi_limpia),   "filas,", ncol(cgi_limpia),   "columnas\n")
cat("  macro_limpia: ", nrow(macro_limpia),  "filas,", ncol(macro_limpia), "columnas\n")


