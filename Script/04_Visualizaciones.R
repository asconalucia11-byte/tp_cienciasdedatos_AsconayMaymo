# =============================================================================
# Ciencia de Datos para Economía y Negocios — FCE-UBA
# TP Grupo 7 — Ascona Cortina Lucia | Maymo Skansi Joaquin
#
# Script 04 — Visualizaciones
#
# Objetivo: Producir el gráfico exploratorio y el gráfico comunicacional
#           que muestran la variación del empleo, la productividad sectorial
#           y la evolución de la informalidad entre 2016 y 2022.
#
# Input:  Input/base_analisis.csv
# Output: Output/grafico_exploratorio.png
#         Output/grafico_comunicacional.png
# =============================================================================

library(tidyverse)
library(scales)
library(ggrepel)

# --- Tema global (estilo del curso) ------------------------------------------

theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title       = element_text(face = "bold", size = 15),  # ← cambiar de 13 a 15
      plot.subtitle    = element_text(color = "gray40", size = 10),
      plot.caption     = element_text(color = "gray50", hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
)

# --- Carga de datos ----------------------------------------------------------

base <- read_csv("Input/base_analisis.csv", show_col_types = FALSE)

dir.create("Output", showWarnings = FALSE)


# =============================================================================
# GRÁFICO 1 — EXPLORATORIO
# Bubble chart: productividad, desplazamiento del empleo e informalidad
# =============================================================================

# --- Líneas de corte ---------------------------------------------------------

# Línea vertical: variación del empleo total de la economía privada
var_empleo_economia <- base |>
  filter(sector == "Total excluido sector público",
         anio %in% c(2016, 2022)) |>
  select(anio, puestos_total) |>
  pivot_wider(names_from = anio, values_from = puestos_total,
              names_prefix = "puestos_") |>
  mutate(var = (puestos_2022 / puestos_2016 - 1) * 100) |>
  pull(var)

# Línea horizontal: productividad media de la economía privada
prod_media_economia <- base |>
  filter(sector == "Total excluido sector público") |>
  summarise(prod_media = mean(productividad, na.rm = TRUE)) |>
  pull(prod_media)

# --- Datos del gráfico — una fila por sector ---------------------------------

datos_grafico <- base |>
  filter(!is.na(grupo_prod)) |>
  group_by(sector, grupo_prod) |>
  summarise(
    productividad_media = mean(productividad, na.rm = TRUE),
    puestos_medio       = mean(puestos_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    base |>
      filter(!is.na(grupo_prod), anio %in% c(2016, 2022)) |>
      select(sector, anio, puestos_total, part_anr) |>
      pivot_wider(names_from  = anio,
                  values_from = c(puestos_total, part_anr),
                  names_sep   = "_") |>
      mutate(
        var_puestos      = (puestos_total_2022 / puestos_total_2016 - 1) * 100,
        var_informalidad = (part_anr_2022 - part_anr_2016) * 100
      ) |>
      select(sector, var_puestos, var_informalidad),
    by = "sector"
  ) |>
  mutate(sector_corto = case_when(
    sector == "Actividades inmobiliarias, empresariales y de alquiler"             ~ "Act. inmobiliarias",
    sector == "Agricultura, ganadería, caza y silvicultura"                        ~ "Agricultura",
    sector == "Comercio mayorista, minorista y reparaciones"                       ~ "Comercio",
    sector == "Construcción"                                                       ~ "Construcción",
    sector == "Electricidad, gas y agua"                                           ~ "Electricidad",
    sector == "Enseñanza privada"                                                  ~ "Enseñanza privada",
    sector == "Explotación de minas y canteras"                                    ~ "Minas y canteras",
    sector == "Hogares privados con servicio doméstico"                            ~ "Serv. doméstico",
    sector == "Hoteles y restaurantes"                                             ~ "Hoteles",
    sector == "Industria manufacturera"                                            ~ "Industria",
    sector == "Intermediación financiera"                                          ~ "Financiero",
    sector == "Otras actividades de servicios comunitarias, sociales y personales" ~ "Otras act. servicios",
    sector == "Pesca"                                                              ~ "Pesca",
    sector == "Servicios sociales y de salud privados"                             ~ "Salud privada",
    sector == "Transporte, almacenamiento y comunicaciones"                        ~ "Transporte",
    TRUE ~ sector
  ))

# --- Gráfico exploratorio ----------------------------------------------------

g_exploratorio <- ggplot(
  datos_grafico,
  aes(x     = var_puestos,
      y     = productividad_media,
      size  = puestos_medio,
      color = var_informalidad)
) +
  geom_point(alpha = 0.8) +
  geom_text_repel(
    aes(label = sector_corto),
    size         = 3.5,
    max.overlaps = 30,
    show.legend  = FALSE
  ) +
  geom_vline(xintercept = var_empleo_economia,
             linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_hline(yintercept = prod_media_economia,
             linetype = "dashed", color = "gray50", linewidth = 0.6) +
  annotate("text", x = var_empleo_economia + 0.3, y = 2700,
           label = "Crecimiento medio\ndel empleo (9.3%)",
           size = 2.8, color = "gray40", hjust = 0) +
  annotate("text", x = -7, y = prod_media_economia + 80,
           label = "Productividad media\nde la economía",
           size = 2.8, color = "gray40", hjust = 0) +
  scale_color_gradient2(
    low      = "steelblue",
    mid      = "white",
    high     = "tomato",
    midpoint = 0,
    name     = "Var. informalidad (pp)"
  ) +
  scale_size_continuous(range = c(1, 15), guide = "none") +
  labs(
    title    = "Productividad, empleo e informalidad por sector — Argentina 2016-2022",
    subtitle = "Tamaño = puestos promedio del sector | Color = variación de la tasa de informalidad (pp 2016-2022)",
    x        = "Variación % de puestos de trabajo (2016-2022)",
    y        = "Productividad promedio del sector (VAB real / puestos)",
    caption  = "Fuente: INDEC — Cuenta de Generación del Ingreso (CGI)"
  )

g_exploratorio

ggsave(
  "Output/grafico_exploratorio.png",
  g_exploratorio,
  width  = 10,
  height = 7,
  dpi    = 150
)


# =============================================================================
# GRÁFICO 2 — COMUNICACIONAL
# Bubble chart categórico: sectores ordenados por productividad
# =============================================================================

# --- Datos del gráfico -------------------------------------------------------

datos_com <- base |>
  filter(!is.na(grupo_prod)) |>
  group_by(sector, grupo_prod) |>
  summarise(
    puestos_medio       = mean(puestos_total, na.rm = TRUE),
    productividad_media = mean(productividad, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    base |>
      filter(!is.na(grupo_prod), anio %in% c(2016, 2022)) |>
      select(sector, anio, puestos_total) |>
      pivot_wider(names_from   = anio,
                  values_from  = puestos_total,
                  names_prefix = "puestos_") |>
      mutate(var_puestos = (puestos_2022 / puestos_2016 - 1) * 100),
    by = "sector"
  ) |>
  mutate(
    sector_corto = case_when(
      sector == "Actividades inmobiliarias, empresariales y de alquiler"             ~ "Act. inmobiliarias",
      sector == "Agricultura, ganadería, caza y silvicultura"                        ~ "Agricultura",
      sector == "Comercio mayorista, minorista y reparaciones"                       ~ "Comercio",
      sector == "Construcción"                                                       ~ "Construcción",
      sector == "Electricidad, gas y agua"                                           ~ "Electricidad",
      sector == "Enseñanza privada"                                                  ~ "Enseñanza privada",
      sector == "Explotación de minas y canteras"                                    ~ "Minas y canteras",
      sector == "Hogares privados con servicio doméstico"                            ~ "Serv. doméstico",
      sector == "Hoteles y restaurantes"                                             ~ "Hoteles",
      sector == "Industria manufacturera"                                            ~ "Industria",
      sector == "Intermediación financiera"                                          ~ "Financiero",
      sector == "Otras actividades de servicios comunitarias, sociales y personales" ~ "Otras act. servicios",
      sector == "Pesca"                                                              ~ "Pesca",
      sector == "Servicios sociales y de salud privados"                             ~ "Salud privada",
      sector == "Transporte, almacenamiento y comunicaciones"                        ~ "Transporte",
      TRUE ~ sector
    ),
    sector_corto = fct_reorder(sector_corto, productividad_media),
    etiqueta     = paste0(round(var_puestos, 1), "%"),
    grupo_prod   = factor(grupo_prod,
                          levels = c("Baja productividad",
                                     "Media productividad",
                                     "Alta productividad"))
  )

# --- Gráfico comunicacional --------------------------------------------------

g_comunicacional <- ggplot(
  datos_com,
  aes(x     = sector_corto,
      y     = var_puestos,
      size  = puestos_medio,
      color = grupo_prod)
) +
  geom_hline(yintercept = 0,
             linetype = "solid", color = "gray70", linewidth = 0.5) +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = etiqueta),
            size = 3,
            vjust = -1.8,
            show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Baja productividad"  = "#e05c3a",
      "Media productividad" = "#d4d4d4",
      "Alta productividad"  = "#a8c4d4"
    ),
    name = "Grupo de productividad"
  ) +
  scale_size_continuous(range = c(2, 16), guide = "none") +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Sin desplazamiento: el empleo creció a ritmo similar en todos los grupos de productividad",
    subtitle = "Variación % de puestos de trabajo 2016-2022 | Tamaño = puestos promedio del sector",
    x        = "Sectores ordenados de menor a mayor productividad →",
    y        = "Variación % de puestos de trabajo (2016-2022)",
    caption  = "Fuente: INDEC — Cuenta de Generación del Ingreso (CGI)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

g_comunicacional

ggsave(
  "Output/grafico_comunicacional.png",
  g_comunicacional,
  width  = 10,
  height = 7,
  dpi    = 150
)