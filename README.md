# tp_cienciasdedatos_AsconayMaymo
TP - Ciencias de datos FCE UBA
# TP — Ciencia de Datos para Economía y Negocios
**Facultad de Ciencias Económicas — UBA | 1er cuatrimestre 2026**

Integrantes
| Ascona Cortina, Lucia | 914440 |
| Maymo Skansi, Joaquin | 912824 |

---

## 📌 Tema
Análisis de la estructura y composición del empleo en Argentina con el objetivo de evaluar posibles cambios en la calidad del empleo, en particular en términos de precarización laboral, en el periodo de análisis 2016 a 2022

## 🔍 Hipótesis

**Principal:** El empleo en Argentina se desplazó hacia sectores de menor productividad relativa, lo que explica una creciente precarización del empleo y menores ingresos.

**Secundaria:** Sumado al aumento del empleo en sectores de baja productividad, se produjo también un incremento del trabajo no asalariado (cuentapropismo) y una menor participación de las remuneraciones asalariadas en el ingreso total (informalidad),  por lo que el efecto sobre el total de la economía es doble

---

## 🗃️ Bases de datos

Principal: Cuenta Generación del Ingreso (CGI) | INDEC 
Complementaria: Agregados Macroeconómicos — Cuentas Nacionales | INDEC 

---
## 📊 Variables principales
| Variable | Tipo | Descripción |
|---|---|---|
| Remuneración al trabajo asalariado (RTA) | Numérica | Masa salarial total de la economía |
| Ingreso mixto bruto (IMB) | Numérica | Ingreso de trabajadores no asalariados |
| Participación de la remuneración al trabajo asalariado en el valor agregado bruto  | Numérica | Participación de salarios en el valor agregado |
| Participación del ingreso mixto bruto en el valor agregado bruto | Numérica | Participación del ingreso mixto en el valor agregado |
| Puestos totales de la economía | Numérica | Total de ocupados |
| Puestos asalariados registrados | Numérica | Empleo formal en relación de dependencia |
| Puestos asalariados no registrados | Numérica | Empleo informal en relación de dependencia |
| Puestos no asalariados | Numérica | Cuentapropistas y empleadores |
| Año| Temporal | Análisis anual |
| Período | Temporal | Análisis anual 2016–2022 |
---

## 📐 Benchmarks de comparación

- Total de la economía como referencia para comparar la evolución de cada sector

## 📁 Estructura del repositorio
```
├── raw/          ← bases originales sin modificar (descargadas del INDEC)
├── input/        ← datos ya procesados y listos para analizar
├── output/       ← gráficos y tablas finales exportables
├── script/
│   ├── 01_limpieza.R
│   ├── 02_analisis.R
│   └── 03_visualizaciones.R
└── README.md
```
