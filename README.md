<div align="center">

[![License: MIT](https://img.shields.io/badge/License-MIT-00e5bc.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.4.0-0077b5.svg)](DESCRIPTION)
[![R](https://img.shields.io/badge/R-%3E%3D%204.0-276DC3.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/Status-Active%20Development-brightgreen.svg)](https://github.com/PALP31/easyModels)

# 📦 easyModels

**Herramientas Automatizadas y Reproducibles para Modelos Lineales, Mixtos y Gráficos Científicos en R**

</div>

**easyModels** es un paquete de R desarrollado por **Paúl Alexander López Peña** (Pontificia Universidad Católica de Chile) para automatizar y democratizar el análisis bioestadístico en agronomía, ciencias biológicas y medicina experimental. 

Su filosofía es **eliminar la fricción metodológica** al trabajar con diseños experimentales y modelos jerárquicos: convierte flujos complejos de fórmulas en `lme4`/`glmmTMB`, pruebas de supuestos, diagnósticos de `DHARMa`, comparaciones post-hoc y gráficos de publicación con letras de significancia (Tukey CLD) en comandos directos, robustos y reproducibles.

---

## 🚀 Instalación desde GitHub

```r
# Instalar paquete devtools si no lo tienes
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# Instalar easyModels
devtools::install_github("PALP31/easyModels")
```

Cargar la librería:

```r
library(easyModels)
```

---

## 🌟 Novedades en la Versión 0.4.0

* 🌾 **Nuevos Diseños Experimentales:**
  - `analizar_latino()`: Cuadrados Latinos (LSD) con control de filas y columnas (LMM o LM).
  - `analizar_medidas_repetidas()`: Ensayos longitudinales con medidas repetidas en el tiempo sobre el mismo sujeto/planta.
  - `analizar_strip_plot()`: Diseños en franjas divididas o bloques cruzados (Strip-Plot / Criss-Cross).
* 🔍 **Auditoría Automatizada de Supuestos (`verificar_supuestos()`):**
  - Batería integrada de pruebas: Normalidad (Shapiro-Wilk), Homocedasticidad (Breusch-Pagan / Levene), Multicolinealidad (VIF), Autocorrelación (Durbin-Watson) y Detección de Valores Atípicos/Influyentes (Distancia de Cook $> 4/n$).
* 🤖 **Asistente de Selección de Familias y Transformación (`sugerir_modelo()`):**
  - Inspecciona la distribución biológica (ceros, proporciones $(0,1)$, conteos sobredispersos, sesgo positivo), calcula el parámetro óptimo Box-Cox ($\lambda$) y recomienda la familia óptima de GLM o transformación.
* 📊 **Selección y Comparación de Modelos (`comparar_modelos()`):**
  - Compara múltiples modelos calculando $AIC$, $\Delta AIC$, Pesos de Akaike ($w_i$), $BIC$, $LogLik$ y $R^2$ Marginal/Condicional.
* 📄 **Tablas Formateadas para Tesis y Publicaciones:**
  - `exportar_tabla_anova()`: Tabla ANOVA con grados de libertad, estadístico F/$\chi^2$, $p$-valores con estrellas de significancia ($***, **, *, ns$) y tamaño del efecto ($\eta_p^2$).
  - `exportar_tabla_posthoc()`: Formato limpio para medias marginales y letras Tukey.
* 🎨 **Gráficos Científicos de Publicación:**
  - `graficar_predichos()`: Soporte para gráficos de **barras** (`tipo_grafico = "barras"`), puntos o líneas con letras de significancia (CLD) y paletas académicas (`teal`, `viridis`, `cividis`, `set2`, `okabe_ito`).
  - `graficar_interaccion()`: Gráficos de efectos simples para interacciones 2-vías con bandas o barras de error al 95%.

---

## 🛠️ Flujo Rápido de Ejemplo

```r
library(easyModels)
library(ggplot2)

# Datos de ensayo agronómico: Rendimiento bajo 3 tratamientos en 5 bloques
set.seed(123)
datos <- data.frame(
  Rendimiento = rnorm(30, mean = rep(c(20, 26, 18), each = 10), sd = 2.5),
  Tratamiento = factor(rep(c("Control", "Bioestimulante", "Fertilizacion"), each = 10)),
  Bloque = factor(rep(1:5, times = 6))
)

# 1. Ajustar Bloques Completos al Azar (RCBD)
modelo <- analizar_bloques_azar(
  datos = datos,
  formula_fijos = Rendimiento ~ Tratamiento,
  bloque = "Bloque",
  diagnosticos = FALSE
)

# 2. Auditar supuestos del modelo
verificar_supuestos(modelo)

# 3. Resumen y Tabla ANOVA Tipo III
summary(modelo)

# 4. Tabla ANOVA formateada para publicación
exportar_tabla_anova(modelo, formato = "markdown")

# 5. Gráfico de barras de publicación con letras de Tukey (CLD)
grafico_barras <- graficar_predichos(
  modelo = modelo,
  predictor = "Tratamiento",
  tipo_grafico = "barras",
  mostrar_letras = TRUE,
  paleta = "teal",
  titulo = "Efecto de Tratamientos sobre el Rendimiento",
  eje_y = "Rendimiento de Grano (ton/ha)"
)

print(grafico_barras)
```

---

## 🔬 Catálogo de Diseños Experimentales

| Función | Tipo de Diseño | Estructura Aleatoria / Fórmulas |
| :--- | :--- | :--- |
| `analizar_lm()` | Modelo Lineal Clásico / CRD | Efectos fijos gaussianos |
| `analizar_bloques_azar()` | Bloques Completos al Azar (RCBD) | `(1 \| Bloque)` |
| `analizar_parcelas_divididas()` | Parcelas Divididas (Split-Plot) | `(1 \| Bloque) + (1 \| Bloque:Principal)` |
| `analizar_latino()` | Cuadrado Latino (LSD) | `(1 \| Fila) + (1 \| Columna)` |
| `analizar_medidas_repetidas()` | Medidas Repetidas en el Tiempo | `(1 \| Sujeto)` o `(1 \| Bloque/Sujeto)` |
| `analizar_strip_plot()` | Franjas Divididas (Strip-Plot) | `(1 \| Bloque) + (1 \| Bloque:A) + (1 \| Bloque:B)` |

---

## 📈 Familias de Distribución Disponibles (GLM y GLMM)

### GLM (`analizar_glm`) — 17 Familias
- **Gaussianas / Continuas:** `gaussian`, `gamma`, `gamma_inverse`, `gaussian_inversa`, `tweedie`.
- **Binarias / Proporciones:** `binomial` (logit), `binomial_probit`, `binomial_cloglog`, `quasibinomial`, `beta` (via `betareg`).
- **Conteos:** `poisson`, `quasipoisson`, `negativa_binomial` (via `MASS::glm.nb`), `zip` (Zero-Inflated Poisson), `zinb` (Zero-Inflated Negative Binomial).
- **Categóricas:** `ordinal` (via `MASS::polr`), `multinomial` (via `nnet::multinom`).

### GLMM (`analizar_glmm`) — 16 Distribuciones
Soporte completo para modelos mixtos con `glmmTMB` y `lme4::glmer`:
- `gaussian`, `binomial`, `binomial_probit`, `binomial_cloglog`, `poisson`, `negativa_binomial` (nbinom2), `nbinom1`, `gamma`, `gamma_inverse`, `lognormal`, `beta`, `tweedie`, `zip`, `zinb`, `zifgamma`, `ordinal`.

---

## 📊 Comparación de Modelos (`comparar_modelos`)

```r
m1 <- analizar_lm(iris, Sepal.Length ~ Species, diagnosticos = FALSE)
m2 <- analizar_lm(iris, Sepal.Length ~ Species + Sepal.Width, diagnosticos = FALSE)
m3 <- analizar_lm(iris, Sepal.Length ~ Species * Sepal.Width, diagnosticos = FALSE)

# Comparar y ordenar por AIC
comparar_modelos(m1, m2, m3, nombres = c("Simple", "Aditivo", "Interaccion"))
```

---

## 👨‍🔬 Autor y Contacto

**Paul Alexander López Peña**  
*Profesor de Aplicaciones Estadísticas (Pregrado) • Estudiante de Doctorado en Biotecnología Vegetal*  
**Pontificia Universidad Católica de Chile (PUC)**  
Email: [paullopezpena@gmail.com](mailto:paullopezpena@gmail.com) | [plopezp7@estudiante.uc.cl](mailto:plopezp7@estudiante.uc.cl)  
GitHub: [@PALP31](https://github.com/PALP31)
