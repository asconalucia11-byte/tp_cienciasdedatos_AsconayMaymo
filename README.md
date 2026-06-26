# TP — Ciencia de Datos para Economía y Negocios
**Facultad de Ciencias Económicas — UBA | 1er cuatrimestre 2026**

| Integrante | Padrón |
|---|---|
| Ascona Cortina, Lucía | 914440 |
| Maymo Skansi, Joaquín | 912824 |

---

## 📌 Objetivo

Analizar la relación entre productividad sectorial, composición del empleo y distribución del ingreso en Argentina durante el período 2016–2022, con foco en la calidad de los puestos de trabajo generados y la participación salarial en el valor agregado bruto.

---

## 🔍 Hipótesis

**Principal:** El empleo en Argentina se desplazó hacia sectores de menor productividad relativa, lo que explica una creciente precarización del empleo (mayor cuentapropismo e informalidad) y menores ingresos.

**Complementaria:** Incluso dentro de los sectores de baja productividad, aumentaron las formas de empleo no asalariado e informal, generando un efecto doble sobre la participación salarial total.

---

## 🗃️ Bases de datos

### Principal — Cuenta Generación del Ingreso (CGI) | INDEC
Provee información anual por sector económico sobre remuneraciones al trabajo asalariado, ingreso mixto bruto, puestos totales, puestos asalariados registrados, no registrados y no asalariados. Permite estudiar simultáneamente la estructura del empleo, la composición de los puestos, la productividad sectorial y la distribución funcional del ingreso.

**Justificación:** Es la única fuente oficial que combina en un mismo marco contable la información sobre empleo, remuneraciones y valor agregado por sector, lo que la hace ideal para analizar la relación entre productividad y calidad del empleo. El período 2016–2022 corresponde a datos definitivos del INDEC.

### Complementaria — Agregados Macroeconómicos — Cuentas Nacionales | INDEC
Provee información sobre el valor bruto de producción y otras variables macroeconómicas por sector. Se utilizó para complementar y enriquecer la caracterización sectorial de la CGI.

### Auxiliar — IPC | INDEC
Serie histórica del Índice de Precios al Consumidor utilizada para deflactar las variables monetarias de la CGI. Se tomó 2016 como año base. El IPC mensual fue promediado para obtener un índice representativo de cada año. Se aloja en la carpeta `auxiliar/`.

---

## 📊 Variables principales

| Variable | Tipo | Descripción |
|---|---|---|
| `vab_real` | Numérica continua | Valor Agregado Bruto deflactado (base 2016) |
| `rta_real` | Numérica continua | Remuneración al Trabajo Asalariado deflactada |
| `imb_real` | Numérica continua | Ingreso Mixto Bruto deflactado |
| `puestos_total` | Numérica continua | Total de puestos de trabajo |
| `puestos_ar` | Numérica continua | Puestos asalariados registrados |
| `puestos_anr` | Numérica continua | Puestos asalariados no registrados |
| `puestos_na` | Numérica continua | Puestos no asalariados (cuentapropistas) |
| `productividad` | Numérica continua | VAB real / puestos totales |
| `part_rta` | Numérica continua (proporción) | RTA real / VAB real (participación salarial) |
| `part_na` | Numérica continua (proporción) | Puestos no asalariados / puestos totales |
| `part_anr` | Numérica continua (proporción) | Puestos asalariados no registrados / puestos totales |
| `sector` | Categórica nominal | Nombre del sector económico |
| `anio` | Numérica discreta (temporal) | Año de observación (2016–2022) |
| `grupo_prod` | Categórica ordinal | Clasificación en terciles de productividad (baja / media / alta) |

---

## 📐 Decisiones metodológicas

- **Sector público excluido:** la productividad en el sector público se mide por costo de insumos y no por valor de mercado, lo que la hace no comparable con el sector privado. Se excluyeron administración pública, enseñanza pública y salud pública.
- **NAs tratados como ceros metodológicos:** los valores faltantes en `puestos_anr`, `puestos_na` e `imb` corresponden al sector público, que fue excluido del análisis. En la base final no quedan NAs.
- **Outliers conservados:** los valores extremos de productividad (minería, finanzas, hidrocarburos) representan características reales de esos sectores y no errores de medición. Para reducir su influencia se utilizó correlación de Spearman.
- **Spearman sobre Pearson:** la productividad presenta distribución asimétrica con cola derecha, lo que hace que Pearson no sea adecuado.

---

## 🔬 Métodos utilizados

1. **Estadísticas descriptivas** — exploración de la distribución de variables y matriz de correlaciones (GGally)
2. **Test t pareado** — comparación de `part_rta` entre 2016 y 2022, a nivel global y dentro de los sectores de baja productividad
3. **Correlación de Spearman** — asociación entre productividad y composición del empleo, a nivel global y por grupo de productividad
4. **CAGR por grupo** — tasa de crecimiento anual compuesta de puestos y productividad por grupo (2016–2022)
5. **Descomposición within/between (shift-share)** — cuantificación del cambio en `part_rta` atribuible a cambios dentro de cada sector vs. reasignación de empleo entre sectores

---

## 📈 Principales hallazgos

- La hipótesis principal **no se confirmó**: el empleo creció a tasas similares en todos los grupos de productividad (1,4%–1,6% anual) y los shares de empleo prácticamente no cambiaron entre 2016 y 2022.
- La descomposición within/between muestra que el **90% del deterioro en `part_rta` ocurrió dentro de los sectores** (within), no por reasignación de empleo entre ellos (between = 10%).
- Las correlaciones de Spearman **confirman la asociación estructural** entre baja productividad e informalidad (rho = −0,677) y cuentapropismo (rho = −0,333).
- La productividad cayó en casi todos los sectores, más pronunciadamente en los de alta productividad (−4% anual).

---

## 📁 Estructura del repositorio

```
proyecto/
├── raw/                        ← bases originales sin modificar (descargadas del INDEC)
├── auxiliar/                   ← archivos de apoyo (serie IPC histórica)
├── input/                      ← datos procesados y listos para el análisis
├── output/
│   ├── tablas/                 ← tablas de resultados generadas por los scripts
│   └── graficos/               ← visualizaciones generadas por los scripts
├── script/
│   ├── 00_ipc.R                ← construcción de la tabla auxiliar de IPC
│   ├── 01_limpieza.R           ← limpieza y unificación de la CGI
│   ├── 02_analisis.R           ← análisis descriptivo y construcción de variables
│   ├── 03_tests_hipotesis.R    ← tests estadísticos y descomposición shift-share
│   └── 04_Visualizaciones.R    ← gráficos exploratorio y comunicacional
├── utils/                      ← funciones propias (vacío: no se desarrollaron funciones reutilizables)
└── README.md
```

---

## ▶️ Instrucciones para reproducir el análisis

Los scripts deben correrse en orden secuencial. Cada uno depende de los outputs del anterior.

1. `00_ipc.R` — construye la tabla de IPC deflactada y la guarda en `auxiliar/`
2. `01_limpieza.R` — lee los datos de `raw/`, los procesa y guarda la base limpia en `input/`
3. `02_analisis.R` — lee de `input/`, construye variables, genera estadísticas descriptivas y guarda tablas en `output/tablas/`
4. `03_tests_hipotesis.R` — corre los tests estadísticos y la descomposición shift-share
5. `04_Visualizaciones.R` — genera los gráficos y los guarda en `output/graficos/`

**Requisitos:** R 4.x con los siguientes paquetes instalados:
`tidyverse`, `readxl`, `janitor`, `skimr`, `naniar`, `GGally`, `ggrepel`, `broom`

---

*Datos fuente: INDEC — Cuenta de Generación del Ingreso (CGI) y serie histórica IPC*
