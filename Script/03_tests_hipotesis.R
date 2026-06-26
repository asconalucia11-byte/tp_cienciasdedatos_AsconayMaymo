# =============================================================================
# Ciencia de Datos para Economía y Negocios — FCE-UBA
# TP Grupo 7 — Ascona Cortina Lucia | Maymo Skansi Joaquin
#
# Script 03 — Tests de hipótesis
#
# Estructura:
#   5.5  Test t pareado: participación salarial 2016 vs 2022
#   5.6  Correlaciones Spearman (globales)
#   5.7  Correlaciones Spearman por grupo de productividad
#   5.8  CAGR por grupo de productividad
#   5.9 DESCOMPOSICIÓN WITHIN/BETWEEN (SHIFT-SHARE)

# Requiere que `sectores` ya esté en el entorno (correr 02_analisis.R antes).
# `sectores` debe tener las columnas: sector, anio, productividad,
#   part_rta, part_na, part_anr, puestos_total, grupo_prod
# =============================================================================


# --- 0. Librerías -------------------------------------------------------------

library(tidyverse)


# =============================================================================
# 5.5 TEST T PAREADO: ¿cambió la participación salarial entre 2016 y 2022?
# =============================================================================

# Hipótesis:
#   H0: la participación salarial media no cambió entre 2016 y 2022
#   H1: sí cambió (test bilateral)
#
# Usamos el test pareado porque cada fila de 2016 y de 2022 corresponde
# al mismo sector — no son muestras independientes.

part_rta_2016 <- sectores |>
  filter(anio == 2016) |>
  arrange(sector) |>
  pull(part_rta)

part_rta_2022 <- sectores |>
  filter(anio == 2022) |>
  arrange(sector) |>
  pull(part_rta)

# Verificar que los sectores coincidan en orden antes de correr el test
sectores_ord_2016 <- sectores |> filter(anio == 2016) |> arrange(sector) |> pull(sector)
sectores_ord_2022 <- sectores |> filter(anio == 2022) |> arrange(sector) |> pull(sector)
stopifnot(all(sectores_ord_2016 == sectores_ord_2022))

test_pareado <- t.test(part_rta_2022, part_rta_2016, paired = TRUE)

cat("=== Test t pareado: participación salarial 2016 vs 2022 ===\n")
cat("H0: no hubo cambio en la participación salarial media\n")
cat("H1: sí hubo cambio\n\n")
cat("Media part_rta 2016:       ", round(mean(part_rta_2016, na.rm = TRUE), 4), "\n")
cat("Media part_rta 2022:       ", round(mean(part_rta_2022, na.rm = TRUE), 4), "\n")
cat("Diferencia media (d-barra):", round(test_pareado$estimate, 4), "\n")
cat("Estadístico t:             ", round(test_pareado$statistic, 3), "\n")
cat("Grados de libertad:        ", test_pareado$parameter, "\n")
cat("p-valor:                   ", format(test_pareado$p.value, scientific = TRUE), "\n")
cat("IC 95% de la diferencia:  [",
    round(test_pareado$conf.int[1], 4), ",",
    round(test_pareado$conf.int[2], 4), "]\n\n")

if (test_pareado$p.value < 0.05) {
  cat("Conclusión: rechazamos H0 al 5%. La participación salarial",
      "cambió significativamente entre 2016 y 2022.\n\n")
} else {
  cat("Conclusión: no rechazamos H0 al 5%. No hay evidencia suficiente",
      "de un cambio significativo en la participación salarial.\n\n")
}

# Nota: el test t pregunta si part_rta cambió en el tiempo (2016 → 2022).
# Un resultado no significativo a nivel global puede deberse a que el efecto
# es heterogéneo entre sectores. El test que sigue acota el análisis a los
# sectores de baja productividad para ver si la caída es más pronunciada ahí.
# Importante: este test responde una pregunta distinta a las correlaciones de
# Spearman — el test mira cambio temporal, Spearman mira asociación en el corte.


# --- 5.5.b Test t pareado — solo sectores de baja productividad --------------

# Acotamos el test a los sectores de baja productividad para ver si la caída
# en part_rta es más pronunciada dentro de ese grupo. Si la diferencia fuera
# mayor y más homogénea que en el test global, sería consistente con la
# hipótesis complementaria. Este test sigue midiendo cambio temporal,
# no asociación entre variables.

sectores_baja <- sectores |> filter(grupo_prod == "Baja productividad")

cat("Sectores incluidos en este test:\n")
print(unique(sectores_baja$sector))
cat("\n")

part_rta_baja_2016 <- sectores_baja |>
  filter(anio == 2016) |>
  arrange(sector) |>
  pull(part_rta)

part_rta_baja_2022 <- sectores_baja |>
  filter(anio == 2022) |>
  arrange(sector) |>
  pull(part_rta)

test_pareado_baja <- t.test(part_rta_baja_2022, part_rta_baja_2016, paired = TRUE)

cat("=== Test t pareado: participación salarial 2016 vs 2022 — baja productividad ===\n")
cat("H0: no hubo cambio en la participación salarial media\n")
cat("H1: sí hubo cambio\n\n")
cat("Media part_rta 2016:       ", round(mean(part_rta_baja_2016, na.rm = TRUE), 4), "\n")
cat("Media part_rta 2022:       ", round(mean(part_rta_baja_2022, na.rm = TRUE), 4), "\n")
cat("Diferencia media (d-barra):", round(test_pareado_baja$estimate, 4), "\n")
cat("Estadístico t:             ", round(test_pareado_baja$statistic, 3), "\n")
cat("Grados de libertad:        ", test_pareado_baja$parameter, "\n")
cat("p-valor:                   ", format(test_pareado_baja$p.value, scientific = TRUE), "\n")
cat("IC 95% de la diferencia:  [",
    round(test_pareado_baja$conf.int[1], 4), ",",
    round(test_pareado_baja$conf.int[2], 4), "]\n\n")

if (test_pareado_baja$p.value < 0.05) {
  cat("Conclusión: rechazamos H0 al 5%. En los sectores de baja productividad,",
      "la participación salarial cayó significativamente entre 2016 y 2022.\n\n")
} else {
  cat("Conclusión: no rechazamos H0 al 5%. No hay evidencia suficiente de una",
      "caída significativa en la participación salarial dentro de los sectores",
      "de baja productividad.\n\n")
}


# =============================================================================
# 5.6 CORRELACIONES SPEARMAN — análisis global
# =============================================================================

# Usamos cor.test() en lugar de cor() para obtener el p-valor y el IC.
# Spearman es más adecuado que Pearson porque la productividad tiene
# distribución asimétrica y la relación con el empleo puede ser no lineal.
#
# Hipótesis principal:
#   H0: rho = 0 (no hay asociación monótona entre productividad y composición del empleo)
#   H1: rho ≠ 0

cat("=== Correlaciones Spearman con test de significancia — muestra completa ===\n\n")

# 5.6.a Productividad ~ participación no asalariada (cuentapropismo)
ct_na <- cor.test(sectores$productividad, sectores$part_na,
                  method = "spearman", exact = FALSE)
cat("--- Productividad ~ part. no asalariada ---\n")
cat("Rho:", round(ct_na$estimate, 3),
    "  p-valor:", format(ct_na$p.value, scientific = TRUE),
    "  Significativa al 5%:", ifelse(ct_na$p.value < 0.05, "sí", "no"), "\n\n")

# 5.6.b Productividad ~ participación no registrada (informalidad)
ct_anr <- cor.test(sectores$productividad, sectores$part_anr,
                   method = "spearman", exact = FALSE)
cat("--- Productividad ~ part. no registrada ---\n")
cat("Rho:", round(ct_anr$estimate, 3),
    "  p-valor:", format(ct_anr$p.value, scientific = TRUE),
    "  Significativa al 5%:", ifelse(ct_anr$p.value < 0.05, "sí", "no"), "\n\n")

# 5.6.c Productividad ~ participación salarial (como referencia)
ct_rta <- cor.test(sectores$productividad, sectores$part_rta,
                   method = "spearman", exact = FALSE)
cat("--- Productividad ~ part. salarial (part_rta) ---\n")
cat("Rho:", round(ct_rta$estimate, 3),
    "  p-valor:", format(ct_rta$p.value, scientific = TRUE),
    "  Significativa al 5%:", ifelse(ct_rta$p.value < 0.05, "sí", "no"), "\n\n")

# Tabla resumen
tabla_cor_global <- tibble(
  relacion    = c("Productividad ~ part. no asalariada",
                  "Productividad ~ part. no registrada",
                  "Productividad ~ part. salarial"),
  rho         = c(round(ct_na$estimate,  3),
                  round(ct_anr$estimate, 3),
                  round(ct_rta$estimate, 3)),
  p_valor     = c(ct_na$p.value, ct_anr$p.value, ct_rta$p.value),
  sig_5pct    = p_valor < 0.05
)

cat("=== Tabla resumen — correlaciones globales ===\n")
print(tabla_cor_global)
cat("\n")


# =============================================================================
# 5.7 CORRELACIONES SPEARMAN — por grupo de productividad
# =============================================================================

# El profesor sugirió separar los ejercicios entre sectores de alta y baja
# productividad. Repetimos las correlaciones dentro de cada grupo para ver
# si la asociación se mantiene, se refuerza o desaparece al estratificar.

cat("=== Correlaciones Spearman por grupo de productividad ===\n\n")

grupos <- c("Baja productividad", "Media productividad", "Alta productividad")

tabla_cor_grupos <- map_dfr(grupos, function(g) {
  datos_g <- sectores |> filter(grupo_prod == g)
  
  ct_na_g  <- cor.test(datos_g$productividad, datos_g$part_na,
                       method = "spearman", exact = FALSE)
  ct_anr_g <- cor.test(datos_g$productividad, datos_g$part_anr,
                       method = "spearman", exact = FALSE)
  ct_rta_g <- cor.test(datos_g$productividad, datos_g$part_rta,
                       method = "spearman", exact = FALSE)
  
  tibble(
    grupo    = g,
    relacion = c("Productividad ~ part. no asalariada",
                 "Productividad ~ part. no registrada",
                 "Productividad ~ part. salarial"),
    rho      = c(round(ct_na_g$estimate,  3),
                 round(ct_anr_g$estimate, 3),
                 round(ct_rta_g$estimate, 3)),
    p_valor  = c(ct_na_g$p.value, ct_anr_g$p.value, ct_rta_g$p.value),
    sig_5pct = p_valor < 0.05
  )
})

print(tabla_cor_grupos)
cat("\n")

# Interpretación de los resultados:
#
# Importante: las correlaciones de Spearman miden asociación en el corte
# transversal (¿los sectores con mayor productividad tienen mayor/menor
# part_rta, part_na, part_anr?). No miden cambio en el tiempo — eso lo
# hace el test t pareado. Son preguntas distintas y los resultados no
# se confirman ni se contradicen entre sí.
#
# Nota sobre los p-valores por grupo: cada grupo tiene ~5 sectores x 2
# períodos = ~10 observaciones. Con ese n el poder estadístico es muy bajo,
# igual que en el test t pareado de 5.5.b. Por eso los rhos se reportan
# como evidencia descriptiva de la fuerza y dirección de la asociación,
# sin afirmar significancia estadística dentro de los grupos.
#
# BAJA PRODUCTIVIDAD
#   - Productividad ~ part_na: rho = +0.48. Dentro del grupo, los sectores
#     relativamente más productivos tienen más cuentapropismo. Es un efecto
#     de composición interna del grupo; vale leerlo junto con las
#     visualizaciones para entender qué sectores traccionan este resultado.
#   - Productividad ~ part_anr: rho = -0.18. Asociación débil y negativa,
#     sin señal clara entre productividad e informalidad dentro del grupo.
#   - Productividad ~ part_rta: rho = -0.90. La asociación más fuerte de
#     toda la tabla. Dentro de los sectores de baja productividad, los
#     relativamente más productivos destinan una fracción menor de su VAB
#     a salarios. Es un resultado descriptivo robusto aunque el n sea chico.
#
# MEDIA PRODUCTIVIDAD
#   - Ningún rho supera 0.31 en valor absoluto. No hay señal clara dentro
#     de este grupo; la dinámica productividad-empleo parece más homogénea
#     en los sectores intermedios.
#
# ALTA PRODUCTIVIDAD
#   - Productividad ~ part_na: rho = -0.70. Los sectores más productivos
#     dentro del grupo tienen menos cuentapropismo, en línea con lo esperado
#     por la hipótesis principal.
#   - Productividad ~ part_anr y ~ part_rta: rhos bajos, sin señal clara.


# =============================================================================
# 5.8 CAGR POR GRUPO DE PRODUCTIVIDAD
# =============================================================================

# La tasa de crecimiento anual compuesta (CAGR) permite comparar el ritmo
# de expansión de puestos y productividad entre grupos en el período 2016-2022
# (6 años). CAGR = (valor_final / valor_inicial)^(1/n) - 1
#
# Si el empleo se hubiera desplazado hacia sectores de menor productividad,
# esperaríamos ver un CAGR de puestos claramente mayor en baja productividad.
# Los resultados muestran que los tres grupos crecieron a tasas muy similares
# (~1.4-1.6% anual), lo que no confirma ese desplazamiento. Lo que sí aparece
# es una caída generalizada de productividad, más pronunciada en alta (-4% anual).

# --- 5.8.a Calcular CAGR por sector ------------------------------------------

cagr_sectores <- sectores |>
  filter(anio %in% c(2016, 2022)) |>
  select(sector, grupo_prod, anio, puestos_total, productividad) |>
  pivot_wider(
    names_from  = anio,
    values_from = c(puestos_total, productividad)
  ) |>
  mutate(
    cagr_puestos = (puestos_total_2022 / puestos_total_2016)^(1/6) - 1,
    cagr_prod    = (productividad_2022  / productividad_2016)^(1/6)  - 1
  )

cat("=== CAGR por sector (2016-2022) ===\n")
cagr_sectores |>
  select(sector, grupo_prod, cagr_puestos, cagr_prod) |>
  mutate(across(c(cagr_puestos, cagr_prod), ~ round(. * 100, 2))) |>
  arrange(grupo_prod, desc(cagr_puestos)) |>
  print()
cat("\n")

# --- 5.8.b Promedios por grupo ------------------------------------------------

cagr_por_grupo <- cagr_sectores |>
  group_by(grupo_prod) |>
  summarise(
    n_sectores         = n(),
    cagr_puestos_medio = mean(cagr_puestos, na.rm = TRUE),
    cagr_prod_medio    = mean(cagr_prod,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    grupo_prod = factor(grupo_prod,
                        levels = c("Baja productividad",
                                   "Media productividad",
                                   "Alta productividad"))
  ) |>
  arrange(grupo_prod)

cat("=== CAGR promedio por grupo de productividad ===\n")
cagr_por_grupo |>
  mutate(across(c(cagr_puestos_medio, cagr_prod_medio), ~ round(. * 100, 2))) |>
  print()
cat("\n")

# --- 5.8.c Gráfico: CAGR de puestos y productividad por grupo ----------------

cagr_largo <- cagr_por_grupo |>
  pivot_longer(
    cols      = c(cagr_puestos_medio, cagr_prod_medio),
    names_to  = "variable",
    values_to = "cagr"
  ) |>
  mutate(
    variable = recode(variable,
                      "cagr_puestos_medio" = "Puestos de trabajo",
                      "cagr_prod_medio"    = "Productividad laboral"),
    etiqueta = paste0(round(cagr * 100, 1), "%")
  )

ggplot(cagr_largo, aes(x = grupo_prod, y = cagr * 100, fill = variable)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = etiqueta),
            position = position_dodge(width = 0.6),
            vjust     = ifelse(cagr_largo$cagr >= 0, -0.4, 1.2),
            size      = 3.5) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  scale_fill_manual(values = c("Puestos de trabajo"    = "#4C72B0",
                               "Productividad laboral" = "#DD8452")) +
  labs(
    title    = "CAGR de puestos y productividad por grupo (2016-2022)",
    subtitle = "Tasa de crecimiento anual compuesta, en porcentaje",
    x        = NULL,
    y        = "CAGR (%)",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# =============================================================================
# 5.9 DESCOMPOSICIÓN WITHIN/BETWEEN (SHIFT-SHARE)
# =============================================================================

# El cambio en la participación salarial agregada (part_rta) puede venir de
# dos fuentes:
#
#   BETWEEN: el empleo se reasignó entre sectores. Si los trabajadores se
#   fueron a sectores con menor part_rta, eso baja el agregado aunque cada
#   sector internamente no haya cambiado nada.
#
#   WITHIN: cada sector cambió su propia part_rta. Si la participación
#   salarial cayó dentro de los sectores, eso baja el agregado aunque la
#   distribución del empleo no se haya movido.
#
# Fórmula:
#   Within  = Σ share_2016 × (part_rta_2022 - part_rta_2016)
#   Between = Σ part_rta_2016 × (share_2022 - share_2016)
#   Total   = Within + Between
#
# Esto es aritmética pura, no inferencia estadística. No hay p-valores ni
# supuestos distribucionales, así que el problema del n chico no aplica.
# El resultado cuantifica directamente lo que ya se observó en el CAGR y
# en los shares: si el término between es cercano a cero, confirma que el
# desplazamiento de empleo entre sectores no explica la caída en part_rta.

# --- 5.9.a Construir tabla base -----------------------------------------------

# Calculamos el total de puestos por año para obtener los shares de empleo
puestos_totales <- sectores |>
  group_by(anio) |>
  summarise(puestos_economia = sum(puestos_total, na.rm = TRUE), .groups = "drop")

shift_share_base <- sectores |>
  filter(anio %in% c(2016, 2022)) |>
  left_join(puestos_totales, by = "anio") |>
  mutate(share_empleo = puestos_total / puestos_economia) |>
  select(sector, grupo_prod, anio, part_rta, share_empleo) |>
  pivot_wider(
    names_from  = anio,
    values_from = c(part_rta, share_empleo)
  )

# --- 5.9.b Calcular términos within y between por sector ---------------------

shift_share <- shift_share_base |>
  mutate(
    within  = share_empleo_2016 * (part_rta_2022 - part_rta_2016),
    between = part_rta_2016     * (share_empleo_2022 - share_empleo_2016)
  )

cat("=== Descomposición within/between por sector ===\n")
shift_share |>
  select(sector, grupo_prod, within, between) |>
  mutate(
    total   = within + between,
    across(c(within, between, total), ~ round(. * 100, 3))
  ) |>
  arrange(grupo_prod) |>
  print()
cat("\n")

# --- 5.9.c Agregado total de la economía -------------------------------------

total_within  <- sum(shift_share$within,  na.rm = TRUE)
total_between <- sum(shift_share$between, na.rm = TRUE)
total_cambio  <- total_within + total_between

cat("=== Descomposición agregada (economía total) ===\n")
cat("Cambio total en part_rta:", round(total_cambio  * 100, 3), "p.p.\n")
cat("  Del cual Within:       ", round(total_within  * 100, 3), "p.p. (",
    round(total_within  / total_cambio * 100, 1), "% del total)\n")
cat("  Del cual Between:      ", round(total_between * 100, 3), "p.p. (",
    round(total_between / total_cambio * 100, 1), "% del total)\n\n")

# --- 5.9.d Agregado por grupo de productividad --------------------------------

shift_share_grupo <- shift_share |>
  group_by(grupo_prod) |>
  summarise(
    within_grupo  = sum(within,  na.rm = TRUE),
    between_grupo = sum(between, na.rm = TRUE),
    total_grupo   = within_grupo + between_grupo,
    .groups = "drop"
  ) |>
  mutate(
    grupo_prod = factor(grupo_prod,
                        levels = c("Baja productividad",
                                   "Media productividad",
                                   "Alta productividad"))
  ) |>
  arrange(grupo_prod)

cat("=== Descomposición por grupo de productividad ===\n")
shift_share_grupo |>
  mutate(across(c(within_grupo, between_grupo, total_grupo),
                ~ round(. * 100, 3))) |>
  print()
cat("\n")

# Interpretación de los resultados:
#
# El cambio total en part_rta fue de -6.8 p.p. en la economía privada entre
# 2016 y 2022. La descomposición muestra que:
#
#   - 90% (−6.1 p.p.) es WITHIN: ocurrió dentro de cada sector. Los propios
#     sectores destinaron menos de su VAB a salarios formales, independiente-
#     mente de si ganaron o perdieron trabajadores.
#
#   - 10% (−0.7 p.p.) es BETWEEN: se explica por reasignación de empleo entre
#     sectores. Este término es cercano a cero, lo que confirma matemáticamente
#     que el desplazamiento de empleo entre sectores no explica la caída.
#
# Por grupo:
#   - Media productividad explica la mayor parte de la caída total (−5.4 p.p.),
#     traccionada principalmente por Industria manufacturera y Comercio.
#   - Baja productividad aporta −2.1 p.p., también mayormente within. Hogares
#     privados con servicio doméstico tiene un between grande (−1.28 p.p.)
#     porque ese sector perdió peso relativo en el empleo total.
#   - Alta productividad es el único grupo con contribución positiva (+0.7 p.p.),
#     es decir, en ese grupo part_rta subió levemente.
#
# Conclusión: la hipótesis original (desplazamiento de empleo hacia sectores
# de baja productividad) no encuentra respaldo en los datos — el término
# between es solo el 10% del cambio total. Lo que sí aparece es un deterioro
# transversal de la participación salarial que ocurrió dentro de los sectores,
# especialmente en los de media productividad.