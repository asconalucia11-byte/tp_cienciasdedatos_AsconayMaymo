# =============================================================================
# Ciencia de Datos para Economía y Negocios — FCE-UBA
# TP Grupo 7 — Ascona Cortina Lucia | Maymo Skansi Joaquin
#
# Script 02 — Análisis de datos
#
# Objetivo: Explorar las variables principales, deflactar las series
#           nominales, construir los indicadores de la hipótesis y
#           calcular las estadísticas descriptivas requeridas en la
#           Instancia 2. Período: 2016-2022.
#
# Hipótesis principal: El empleo en Argentina se desplazó hacia sectores
#   de menor productividad relativa, lo que explica una creciente
#   precarización del empleo y menores ingresos.
#
# Hipótesis complementaria: Incluso detro de estos sectores de baja productividad 
# hubo un crecimiento de la parte no asalariada e informal por lo que el efecto
# sobre el total de la economia es doble.
#
# Inputs:   Input/cgi_limpia.csv
#           Input/macro_limpia.csv
#           Auxiliar/ipc_anual.csv
# Output:   Input/base_analisis.csv
# =============================================================================


# --- 0. Setup ----------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(janitor)
library(skimr)       # resúmenes detallados
library(naniar)      # análisis de valores faltantes
library(dlookr)      # diagnóstico de outliers
library(GGally)      # matriz de correlaciones

options(scipen = 999)

instub   <- "Input"
outstub  <- "Input"
auxiliar <- "Auxiliar"

# Si no tienen instalados estos paquetes, correr primero:
# install.packages(c("skimr", "naniar", "dlookr", "GGally"))


# =============================================================================
# PARTE I: CARGA Y EXPLORACIÓN INICIAL DE LOS DATOS
# =============================================================================

# --- 1.1. Lectura de las bases -----------------------------------------------

cgi_limpia   <- read_csv(file.path(instub, "cgi_limpia.csv"))
macro_limpia <- read_csv(file.path(instub, "macro_limpia.csv"))
ipc_anual    <- read_csv(file.path(auxiliar, "ipc_anual.csv"))

# --- 1.2. Primer vistazo a los datos -----------------------------------------

# ¿Cuántas filas y columnas tiene cada base?
dim(cgi_limpia)
dim(macro_limpia)

# Estructura general: nombre de cada variable, tipo y primeros valores
glimpse(cgi_limpia)

# ¿Qué sectores tenemos?
unique(cgi_limpia$sector)

# ¿Qué período cubre la base?
range(cgi_limpia$anio)

# --- 1.3. Resumen completo con skimr -----------------------------------------

# skim() da en un solo paso: n, NAs, media, desvío, percentiles e histograma.
# Es más completo que summary() y sigue el estilo del curso.

skim(cgi_limpia)

# --- 1.4. Diagnóstico de valores faltantes — cgi_limpia ----------------------

cat("=== NAs por columna — cgi_limpia ===\n")
cgi_limpia |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  print()

cat("\n=== Proporción de NAs (%) — cgi_limpia ===\n")
cgi_limpia |>
  summarise(across(where(is.numeric),
                   ~ round(mean(is.na(.)) * 100, 1),
                   .names = "pct_na_{.col}")) |>
  print()

miss_var_summary(cgi_limpia)
gg_miss_var(cgi_limpia)

# Mapa de calor — confirma que los NAs están concentrados en el sector público
cgi_limpia |>
  mutate(fila = row_number()) |>
  pivot_longer(-fila, values_transform = as.character) |>
  mutate(es_na = is.na(value)) |>
  ggplot(aes(x = name, y = fila, fill = es_na)) +
  geom_tile() +
  scale_fill_manual(values = c("grey90", "tomato"),
                    labels = c("Presente", "Faltante")) +
  labs(x = NULL, y = "Observación", fill = NULL,
       title = "Mapa de faltantes — cgi_limpia") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nota: los NAs en puestos_anr, puestos_na e imb corresponden
# exclusivamente al sector público. Son ceros metodológicos —
# el Estado no puede contratar informalmente ni tiene cuentapropistas.
# No se imputan porque el sector público se excluye del análisis
# en la Parte II por razones teóricas.


# --- 1.4b. Mismo diagnóstico para macro_limpia -------------------------------

cat("=== NAs por columna — macro_limpia ===\n")
macro_limpia |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  print()

cat("\n=== Proporción de NAs (%) — macro_limpia ===\n")
macro_limpia |>
  summarise(across(where(is.numeric),
                   ~ round(mean(is.na(.)) * 100, 1),
                   .names = "pct_na_{.col}")) |>
  print()

miss_var_summary(macro_limpia)
gg_miss_var(macro_limpia)

macro_limpia |>
  mutate(fila = row_number()) |>
  pivot_longer(-fila, values_transform = as.character) |>
  mutate(es_na = is.na(value)) |>
  ggplot(aes(x = name, y = fila, fill = es_na)) +
  geom_tile() +
  scale_fill_manual(values = c("grey90", "tomato"),
                    labels = c("Presente", "Faltante")) +
  labs(x = NULL, y = "Observación", fill = NULL,
       title = "Mapa de faltantes — macro_limpia") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# PARTE II: SEPARACIÓN DE SECTORES Y DEFLACTADO
# =============================================================================

# --- 2.1. Separar benchmark y sectores individuales --------------------------
unique(cgi_limpia$sector)

# El "Total excluido sector público" es nuestro benchmark.
# Es consistente con la decisión de analizar solo el sector privado.
total_economia <- cgi_limpia |>
  filter(sector == "Total excluido sector público")

sectores <- cgi_limpia |>
  filter(!sector %in% c(
    # Totales y subtotales
    "Total general",
    "Total sector público (1)",
    "Total excluido sector público",
    # Sector público — se excluye porque la productividad se mide por
    # costo de insumos y no por valor de mercado, y el cuentapropismo
    # e informalidad son fenómenos del mercado privado de trabajo.
    "Administración pública y defensa; planes de seguridad social de afiliación obligatoria",
    # Agregados mixtos — se reemplazan por sus versiones privadas
    "Enseñanza",
    "Servicios sociales y de salud",
    # Desagregaciones públicas
    "Enseñanza pública",
    "Servicios sociales y de salud públicos"
  ))
# Verificar cuántos sectores individuales quedaron
unique(sectores$sector)
cat("Sectores individuales:", n_distinct(sectores$sector), "\n")


# --- 2.2. Deflactar las series nominales -------------------------------------

# Las series de VAB, RTA e IMB están en pesos corrientes. Para comparar
# entre años necesitamos expresarlas en términos reales, dividiendo por
# el IPC anual. Usamos 2016 como año base (IPC = 100).

ipc_base2016 <- ipc_anual |>
  mutate(
    ipc_base2016 = ipc_anual / ipc_anual[anio == 2016] * 100
  ) |>
  select(anio, ipc_base2016)

print(ipc_base2016)

# Unir el IPC y deflactar
sectores <- sectores |>
  left_join(ipc_base2016, by = "anio") |>
  mutate(
    vab_real = vab / ipc_base2016 * 100,
    rta_real = rta / ipc_base2016 * 100,
    imb_real = imb / ipc_base2016 * 100
  )

total_economia <- total_economia |>
  left_join(ipc_base2016, by = "anio") |>
  mutate(
    vab_real = vab / ipc_base2016 * 100,
    rta_real = rta / ipc_base2016 * 100,
    imb_real = imb / ipc_base2016 * 100
  )

sectores |> 
  select(sector, anio, vab, vab_real, ipc_base2016) |> 
  head(10)


# =============================================================================
# PARTE III: CONSTRUCCIÓN DE INDICADORES
# =============================================================================

# --- 3.1. Indicadores de participación en el ingreso -------------------------

sectores <- sectores |>
  mutate(
    part_rta       = rta_real / vab_real,
    part_imb       = imb_real / vab_real,
    part_excedente = 1 - part_rta - part_imb
  )

total_economia <- total_economia |>
  mutate(
    part_rta       = rta_real / vab_real,
    part_imb       = imb_real / vab_real,
    part_excedente = 1 - part_rta - part_imb
  )


# --- 3.2. Productividad laboral -----------------------------------------------

sectores <- sectores |>
  mutate(productividad = vab_real / puestos_total)

total_economia <- total_economia |>
  mutate(productividad = vab_real / puestos_total)


# --- 3.3. Composición del empleo ---------------------------------------------

sectores <- sectores |>
  mutate(
    part_ar  = puestos_ar  / puestos_total,
    part_anr = puestos_anr / puestos_total,
    part_na  = puestos_na  / puestos_total
  )

total_economia <- total_economia |>
  mutate(
    part_ar  = puestos_ar  / puestos_total,
    part_anr = puestos_anr / puestos_total,
    part_na  = puestos_na  / puestos_total
  )

# =============================================================================
# PARTE IV: ESTADÍSTICAS DESCRIPTIVAS (Instancia 2 — Punto 1)
# =============================================================================

# --- 4.1. Resumen completo de los indicadores construidos --------------------

skim(sectores |> select(vab_real, rta_real, imb_real, puestos_total,
                        productividad, part_rta, part_imb, part_na, part_anr))

# --- 4.2. Estadísticas por sector (promedio del período) ---------------------

resumen_por_sector <- sectores |>
  group_by(sector) |>
  summarise(
    vab_real_medio    = mean(vab_real,      na.rm = TRUE),
    productividad_med = mean(productividad, na.rm = TRUE),
    part_rta_media    = mean(part_rta,      na.rm = TRUE),
    part_imb_media    = mean(part_imb,      na.rm = TRUE),
    part_na_media     = mean(part_na,       na.rm = TRUE),
    puestos_medio     = mean(puestos_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(productividad_med))

print(resumen_por_sector)

resumen_por_grupo <- sectores |>
  group_by(grupo_prod) |>
  summarise(
    n_sectores          = n_distinct(sector),
    productividad_media = round(mean(productividad, na.rm = TRUE), 1),
    part_rta_media      = round(mean(part_rta,      na.rm = TRUE), 3),
    part_na_media       = round(mean(part_na,       na.rm = TRUE), 3),
    part_anr_media      = round(mean(part_anr,      na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  mutate(
    grupo_prod = factor(grupo_prod,
                        levels = c("Baja productividad",
                                   "Media productividad",
                                   "Alta productividad"))
  ) |>
  arrange(grupo_prod)

# Frecuencia de sectores con tabyl (janitor)
sectores |>
  tabyl(sector) |>
  adorn_pct_formatting()

# --- 4.3. Evolución del total de la economía (benchmark) ---------------------

resumen_total <- total_economia |>
  select(anio, vab_real, rta_real, imb_real,
         puestos_total, productividad, part_rta, part_imb)

print(resumen_total)


# =============================================================================
# PARTE V: MÉTODOS ESTADÍSTICOS (Instancia 2 — Punto 3)
# =============================================================================

# --- 5.1. Matriz de correlaciones (GGally) -----------------------------------

# Relación entre productividad, participación salarial y composición del empleo.
# Usamos Spearman porque la relación puede ser no lineal.

sectores |>
  select(productividad, part_rta, part_imb, part_na, part_anr) |>
  drop_na() |>
  ggpairs(
    upper = list(continuous = wrap("cor", method = "spearman")),
    lower = list(continuous = wrap("points", alpha = 0.3)),
    title = "Matriz de correlaciones — indicadores por sector (2016-2022)"
  )

png("Output/ggally_correlaciones.png", width = 1200, height = 1000, res = 150)
sectores |>
  select(productividad, part_rta, part_imb, part_na, part_anr) |>
  drop_na() |>
  ggpairs(
    upper = list(continuous = wrap("cor", method = "spearman")),
    lower = list(continuous = wrap("points", alpha = 0.3)),
    title = "Matriz de correlaciones — indicadores por sector (2016-2022)"
  )
dev.off()

# --- 5.2. Correlaciones individuales Spearman --------------------------------

cor_prod_na <- cor(sectores$productividad, sectores$part_na,
                   use = "complete.obs", method = "spearman")
cor_prod_rta <- cor(sectores$productividad, sectores$part_rta,
                    use = "complete.obs", method = "spearman")
cor_prod_anr <- cor(sectores$productividad, sectores$part_anr,
                    use = "complete.obs", method = "spearman")

cat("Correlación productividad ~ part. no asalariada: ", round(cor_prod_na,  3), "\n")
cat("Correlación productividad ~ participación salarial:", round(cor_prod_rta, 3), "\n")
cat("Correlación productividad ~ part. no registrada:  ", round(cor_prod_anr, 3), "\n")


# --- 5.3. Variación porcentual de puestos por sector (2016-2022) -------------

variacion_puestos <- sectores |>
  filter(anio %in% c(2016, 2022)) |>
  select(sector, anio, puestos_total) |>
  pivot_wider(names_from = anio, values_from = puestos_total,
              names_prefix = "puestos_") |>
  mutate(variacion_pct = (puestos_2022 / puestos_2016 - 1) * 100) |>
  arrange(desc(variacion_pct))

print(variacion_puestos)


# --- 5.4. Variación de la participación salarial (2016-2022) -----------------

variacion_part_rta <- sectores |>
  filter(anio %in% c(2016, 2022)) |>
  select(sector, anio, part_rta) |>
  pivot_wider(names_from = anio, values_from = part_rta,
              names_prefix = "part_rta_") |>
  mutate(cambio_pp = (part_rta_2022 - part_rta_2016) * 100) |>
  arrange(cambio_pp)

print(variacion_part_rta)

# --- 5.5. Clasificación de sectores por grupo de productividad ---------------

# Calculamos la productividad promedio de cada sector para todo el período
# y los dividimos en tres grupos usando terciles.
# Esto nos permite comparar el comportamiento del empleo entre grupos
# de distinta productividad.

prod_sector <- sectores |>
  group_by(sector) |>
  summarise(
    productividad_media = mean(productividad, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    grupo_prod = case_when(
      productividad_media <= quantile(productividad_media, 1/3) ~ "Baja productividad",
      productividad_media <= quantile(productividad_media, 2/3) ~ "Media productividad",
      TRUE                                                       ~ "Alta productividad"
    )
  )

# Verificar cuántos sectores por grupo
prod_sector |> count(grupo_prod)

# Ver qué sectores quedaron en cada grupo
print(prod_sector |> arrange(grupo_prod, productividad_media))

# Unir clasificación a la base principal
sectores <- sectores |>
  left_join(
    prod_sector |> select(sector, grupo_prod, productividad_media),
    by = "sector"
  )

# =============================================================================
# PARTE VI: GUARDAR LA BASE DE ANÁLISIS
# =============================================================================

base_analisis <- bind_rows(sectores, total_economia)

write_csv(base_analisis, file.path(outstub, "base_analisis.csv"))

cat("Base de análisis guardada en Input/base_analisis.csv\n")
cat("Filas:", nrow(base_analisis), "\n")
cat("Columnas:", ncol(base_analisis), "\n")
