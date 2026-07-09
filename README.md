<div align="center">
  <img src="man/figures/logo.png" alt="easyModels hex sticker" width="180"/>
</div>

<div align="center">

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](DESCRIPTION)
[![R](https://img.shields.io/badge/R-%3E%3D%204.0-276DC3.svg)](https://www.r-project.org/)

</div>

# easyModels

**easyModels** es un paquete de R desarrollado por **Paul Alexander Lopez Peña** para agilizar la bioestadística aplicada a experimentos agrícolas y biológicos. Su objetivo es convertir flujos repetitivos de análisis estadístico en funciones claras, reproducibles y fáciles de comunicar: ajuste de modelos, diagnósticos, comparaciones post-hoc y gráficos listos para publicación.

Contacto: [paullopezpena@gmail.com](mailto:paullopezpena@gmail.com) | [plopezp7@estudiante.uc.cl](mailto:plopezp7@estudiante.uc.cl)

El paquete está pensado para investigadores que trabajan con diseños experimentales, bloques, repeticiones, tratamientos, mediciones fisiológicas, variables de crecimiento, conteos biológicos, presencia/ausencia y estructuras jerárquicas comunes en ensayos de laboratorio, invernadero y campo.

## Instalación desde GitHub

```r
install.packages("devtools")
devtools::install_github("PALP31/easyModels")
```

Luego carga el paquete:

```r
library(easyModels)
```

## Flujo rápido de análisis LMM

```r
library(easyModels)
library(ggplot2)

datos <- data.frame(
  biomasa = rnorm(120, mean = 20, sd = 4),
  tratamiento = factor(rep(c("Control", "T1", "T2", "T3"), each = 30)),
  bloque = factor(rep(1:10, times = 12))
)

# 1. Ajustar un modelo lineal mixto
modelo <- analizar_lmm(
  datos = datos,
  formula_fijos = biomasa ~ tratamiento,
  aleatorios = "(1 | bloque)"
)

# 2. Revisar diagnosticos
# analizar_lmm() imprime el resumen y genera graficos diagnosticos basicos.

# 3. Comparaciones post-hoc
posthoc <- obtener_posthoc(modelo, predictor = "tratamiento")

# 4. Grafico de comparaciones
grafico <- graficar_posthoc(
  posthoc,
  eje_x = "Comparaciones entre tratamientos",
  eje_y = "Diferencia estimada de biomasa",
  titulo = "Post-hoc Tukey para biomasa"
)

print(grafico)

# 5. Grafico de predichos marginales
predichos <- graficar_predichos(
  modelo,
  predictor = "tratamiento",
  eje_x = "Tratamiento",
  eje_y = "Biomasa predicha"
)

print(predichos)
```

## Diseños Experimentales Específicos (Agrícolas y Biológicos)

`easyModels` ofrece wrappers especializados con "opinión bioestadística" para evitar escribir fórmulas complejas de efectos mixtos de `lme4` en diseños comunes:

### Bloques Completos al Azar (RCBD)
Para un diseño clásico de bloques al azar, la función `analizar_bloques_azar()` añade automáticamente la estructura `(1 | bloque)`:

```r
modelo_rcbd <- analizar_bloques_azar(
  datos = datos_rcbd,
  formula_fijos = Altura ~ Tratamiento,
  bloque = "Bloque"
)
```

### Parcelas Divididas (Split-Plot)
En experimentos donde un factor se aplica a una unidad mayor (parcela principal) y otro factor a sub-unidades (subparcelas), la función `analizar_parcelas_divididas()` gestiona la correcta anidación del error de la parcela principal `(1 | bloque) + (1 | bloque:parcela_principal)`:

```r
modelo_split <- analizar_parcelas_divididas(
  datos = datos_split,
  formula_fijos = Rendimiento ~ Riego * Genotipo,
  bloque = "Bloque",
  parcela_principal = "Riego"
)
```

## GLM binomial con odds ratios

Para respuestas presencia/ausencia, germinacion, mortalidad o incidencia, usa un GLM binomial. `easyModels` calcula odds ratios y permite graficar probabilidades predichas:

```r
datos_bin <- data.frame(
  germino = rbinom(120, size = 1, prob = 0.55),
  tratamiento = factor(rep(c("Control", "Bioinsumo"), each = 60))
)

modelo_bin <- analizar_glm(
  datos = datos_bin,
  formula = germino ~ tratamiento,
  familia = "binomial",
  diagnostico_dharma = FALSE
)

odds <- analizar_odds_ratio(modelo_bin)
print(odds)

posthoc_or <- obtener_posthoc(
  modelo_bin,
  predictor = "tratamiento",
  tipo_respuesta = "response"
)

graficar_posthoc(
  posthoc_or,
  eje_y = "Odds ratio",
  titulo = "Odds ratio entre tratamientos"
)

graficar_predichos(
  modelo_bin,
  predictor = "tratamiento",
  eje_y = "Probabilidad predicha"
)
```

## Conteos y sobredispersion

Para conteos biologicos, comienza con Poisson. Si el ratio de sobredispersion es alto, considera cuasi-Poisson o binomial negativa:

```r
modelo_pois <- analizar_glm(
  datos = datos,
  formula = conteo ~ tratamiento,
  familia = "poisson"
)

modelo_nb <- analizar_glm(
  datos = datos,
  formula = conteo ~ tratamiento,
  familia = "negativa_binomial"
)
```

Para conteos con bloques, parcelas, placas o mediciones repetidas, usa GLMM. Si hay sobredispersión, puedes definir `tipo = "binomial_negativa"` (o `"negativa_binomial"`) para ajustar un modelo binomial negativo automáticamente usando `lme4::glmer.nb()`:

```r
modelo_glmm_nb <- analizar_glmm(
  datos = datos,
  formula_fijos = conteo ~ tratamiento,
  aleatorios = "(1 | bloque)",
  tipo = "binomial_negativa"
)
```

## Cuando usar `lm`, `aov`, LMM o GLMM

Usa `lm()` cuando la respuesta sea continua, los residuos sean aproximadamente normales, la varianza sea razonablemente homogénea y las observaciones sean independientes. Es una excelente primera opción para experimentos simples con tratamientos fijos y sin estructura jerárquica.

Usa `aov()` cuando estés trabajando con ANOVA clásico en diseños balanceados o pedagógicamente simples. Para modelos lineales modernos, `lm()` suele ser más flexible porque permite diagnósticos, comparaciones y extensiones de forma directa.

Usa LMM (`lme4::lmer`) cuando exista dependencia entre observaciones: bloques, macetas, parcelas, cámaras, placas, días, individuos medidos repetidamente o cualquier unidad experimental que induzca correlación. En estos casos, ignorar el agrupamiento suele inflar el error tipo I.

Usa GLMM (`lme4::glmer`) cuando la respuesta no sea continua normal y ademas exista dependencia entre observaciones: conteos, proporciones, presencia/ausencia, mortalidad, germinacion o incidencias con bloques, parcelas, placas, dias o individuos. Para conteos se suele comenzar con Poisson; si hay sobredispersion, considera binomial negativa (`tipo = "negativa_binomial"`) u observacion como efecto aleatorio.

## Estructuras de interceptos aleatorios en LMM

Usa un intercepto aleatorio simple cuando cada grupo tenga su propio nivel basal:

```r
y ~ tratamiento + (1 | bloque)
```

Esto es apropiado cuando los bloques, parcelas o cámaras cambian el promedio general, pero se asume que el efecto del tratamiento es similar entre ellos.

Usa interceptos anidados cuando una unidad está contenida dentro de otra, por ejemplo plantas dentro de parcelas o submuestras dentro de placa:

```r
y ~ tratamiento + (1 | bloque/parcela)
# Equivalente a:
y ~ tratamiento + (1 | bloque) + (1 | bloque:parcela)
```

Usa interceptos separados cuando los factores de agrupamiento no están anidados, sino cruzados o conceptualmente independientes:

```r
y ~ tratamiento + (1 | bloque) + (1 | fecha)
```

Esto es útil cuando una observación está influida por más de una fuente de variación, por ejemplo bloque y fecha de medición.

Usa pendientes aleatorias cuando esperas que el efecto del tratamiento o de una covariable cambie entre grupos:

```r
y ~ dosis + (1 + dosis | bloque)
```

Si el modelo es singular o inestable, simplifica la estructura:

```r
y ~ dosis + (1 | bloque) + (0 + dosis | bloque)
```

## ML vs REML en modelos mixtos

Usa **REML** para estimar y reportar el modelo final cuando comparas modelos con los mismos efectos fijos y te interesa una estimacion menos sesgada de los componentes de varianza.

Usa **ML** cuando compares modelos con diferentes efectos fijos mediante likelihood ratio tests, AIC o seleccion de predictores:

```r
modelo_1 <- lme4::lmer(y ~ tratamiento + (1 | bloque), data = datos, REML = FALSE)
modelo_2 <- lme4::lmer(y ~ tratamiento + covariable + (1 | bloque), data = datos, REML = FALSE)
anova(modelo_1, modelo_2)
```

Después de seleccionar la estructura fija, vuelve a ajustar el modelo final con `REML = TRUE` si estás en un LMM gaussiano.

## ANOVA tipo II y tipo III con `car`

En modelos lineales o mixtos con efectos fijos, `car::Anova()` es útil cuando el diseño está desbalanceado.

Usa **Tipo II** cuando no hay interacciones importantes o cuando el modelo principal no depende de interpretar una interacción. Suele ser una buena opción para efectos principales en diseños desbalanceados sin interacción:

```r
modelo_lm <- lm(y ~ tratamiento + bloque, data = datos)
car::Anova(modelo_lm, type = 2)
```

Usa **Tipo III** cuando el modelo contiene interacciones y necesitas evaluar cada término ajustado por todos los demás términos. Para Tipo III, configura contrastes apropiados:

```r
options(contrasts = c("contr.sum", "contr.poly"))
modelo_lm_int <- lm(y ~ tratamiento * dosis, data = datos)
car::Anova(modelo_lm_int, type = 3)
```

Regla práctica: si la interacción es significativa o biológicamente central, interpreta primero la interacción y luego usa comparaciones post-hoc o efectos simples con `emmeans`.

## Diagnostico recomendado

Para `lm` y LMM revisa residuos vs ajustados, Q-Q plot, homogeneidad de varianza y observaciones influyentes. Para GLMM revisa sobredispersión, residuos simulados y ajuste global. `DHARMa` es especialmente recomendado para GLMM:

```r
res <- DHARMa::simulateResiduals(modelo)
plot(res)
DHARMa::testDispersion(res)
DHARMa::testZeroInflation(res)
```

## Post-hoc y predichos con `emmeans`

`obtener_posthoc()` usa `emmeans`, por lo que funciona con modelos compatibles como `lm`, `aov`, `glm`, `lmer`, `glmer` y `glmer.nb`. En modelos gaussianos devuelve diferencias estimadas; en modelos binomiales con `tipo_respuesta = "response"` devuelve odds ratios; y en modelos con link log puede devolver razones de tasas.

El paquete permite pintar automáticamente letras de significancia Tukey (Compact Letter Display - CLD) directamente en `graficar_predichos()` con `mostrar_letras = TRUE` (requiere tener el paquete `multcomp` instalado):

```r
posthoc <- obtener_posthoc(modelo, "tratamiento", tipo_respuesta = "response")
graficar_posthoc(posthoc)

# Graficar predichos con letras de significancia Tukey
graficar_predichos(
  modelo,
  predictor = "tratamiento",
  mostrar_letras = TRUE,
  alfa_letras = 0.05,
  eje_x = "Tratamiento",
  eje_y = "Altura Promedio (cm)"
)

# Predichos separados por otro factor
graficar_predichos(
  modelo,
  predictor = "tratamiento",
  por = "dosis",
  tipo_respuesta = "response"
)
```

## Funciones principales

- `analizar_lm()`: ajusta modelos lineales con diagnosticos clasicos.
- `analizar_glm()`: ajusta GLM gaussianos, binomiales, Poisson, cuasi-Poisson y binomial negativa.
- `analizar_lmm()`: ajusta modelos lineales mixtos con `lme4::lmer`, incluyendo control de `REML`.
- `analizar_bloques_azar()`: ajusta diseños de Bloques Completos al Azar (RCBD).
- `analizar_parcelas_divididas()`: ajusta diseños de Parcelas Divididas (Split-Plot) con anidamiento de error.
- `analizar_glmm()`: ajusta GLMM binomiales, Poisson y binomiales negativas con diagnosticos `DHARMa` y soporte automático para `binomial_negativa`.
- `analizar_odds_ratio()`: calcula odds ratios e intervalos de confianza para modelos binomiales.
- `obtener_posthoc()`: calcula comparaciones multiples con `emmeans` para LM, GLM, LMM y GLMM.
- `graficar_posthoc()`: grafica diferencias, razones de tasas u odds ratios.
- `graficar_predichos()`: grafica predichos marginales con soporte para letras de significancia Tukey (CLD).

## Autor

**Paul Alexander Lopez Peña**  
Pontificia Universidad Catolica de Chile  
Paquete orientado a bioestadística aplicada, reproducibilidad y comunicación científica.
