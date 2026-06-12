# =============================================================================
# Ciencia de Datos para Economía y Negocios — FCE-UBA
# TP Grupo 7 — Ascona Cortina Lucia | Maymo Skansi Joaquin
#
# Script 00 — Construcción del IPC anual
#
# Objetivo: Leer la serie histórica del IPC (INDEC), calcular el promedio
#           anual y guardar la tabla auxiliar para deflactar las series
#           nominales de la CGI. Período: 2016-2022.
#
# Input:    Auxiliar/0__IPC-Serie_Histórica.xlsx
# Output:   Auxiliar/ipc_anual.csv
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)

options(scipen = 999)

auxiliar <- "Auxiliar"


# --- 1. Leer la serie histórica del IPC --------------------------------------

# La hoja "Hoja1" tiene dos columnas: mes (numérico Excel) e IPC-Indice
ipc_raw <- read_excel(
  file.path(auxiliar, "ipc_serie_historica.xlsx"),
  sheet = "Hoja1"
)

ipc_raw <- clean_names(ipc_raw)

# Convertir la columna mes de número Excel a fecha
ipc_raw <- ipc_raw |>
  mutate(mes = excel_numeric_to_date(mes)) |>
  select(mes, indice_ipc = ipc_indice)

glimpse(ipc_raw)


# --- 2. Calcular el IPC anual promedio ---------------------------------------

# Como nuestro análisis es anual, promediamos los 12 meses de cada año.
# Esto nos da un índice de precios representativo de cada año.

ipc_anual <- ipc_raw |>
  mutate(anio = year(mes)) |>
  filter(anio %in% 2016:2022) |>
  group_by(anio) |>
  summarise(
    ipc_anual = mean(indice_ipc, na.rm = TRUE),
    n_meses   = n(),   # verificar que haya 12 meses por año
    .groups   = "drop"
  )

print(ipc_anual)


# --- 3. Guardar --------------------------------------------------------------

write_csv(ipc_anual, file.path(auxiliar, "ipc_anual.csv"))

cat("IPC anual guardado en Auxiliar/ipc_anual.csv\n")