<div align="center">
  <img src="man/figures/logo.png" alt="easyModels hex sticker" width="240"/>
</div>

<div align="center">

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.3.0-blue.svg)](DESCRIPTION)
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

## Flujo rápido de análisis LMM con S3

```r
library(easyModels)
library(ggplot2)

datos <- data.frame(
  biomasa = rnorm(120, mean = 20, sd = 4),
  tratamiento = factor(rep(c("Control", "T1", "T2", "T3"), each = 30)),
  bloque = factor(rep(1:10, times = 12))
)

# 1. Ajustar un modelo lineal mixto (devuelve un objeto S3 de clase easy_model)
modelo <- analizar_lmm(
  datos = datos,
  formula_fijos = biomasa ~ tratamiento,
  aleatorios = "(1 | bloque)",
  diagnosticos = FALSE
)

# 2. Imprimir información resumida (método print S3 estilizado con cli)
print(modelo)

# 3. Mostrar resumen estadístico detallado y tabla ANOVA tipo III
summary(modelo)

# 4. Obtener y graficar diagnósticos de rendimiento unificados (R2, ICC, singularidad, DHARMa)
evaluar_modelo(modelo)
# o simplemente:
plot(modelo)

# 5. Comparaciones post-hoc
posthoc <- obtener_posthoc(modelo, predictor = "tratamiento")

# 6. Grafico de comparaciones
grafico <- graficar_posthoc(
  posthoc,
  eje_x = "Comparaciones entre tratamientos",
  eje_y = "Diferencia estimada de biomasa",
  titulo = "Post-hoc Tukey para biomasa"
)

print(grafico)
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
  diagnosticos = FALSE
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
  familia = "negativa_binomial",
  diagnosticos = FALSE
)
```

## Distribuciones Disponibles en `analizar_glm` y `analizar_glmm`

### GLM (`analizar_glm`)

| Familia | Tipo de Dato | Paquete |
|---|---|---|
| `"gaussian"` | Continua, simetrica | `stats` |
| `"binomial"` | Binaria 0/1, link logit | `stats` |
| `"binomial_probit"` | Binaria 0/1, link probit | `stats` |
| `"binomial_cloglog"` | Binaria/incidencia, link cloglog | `stats` |
| `"poisson"` | Conteos (var = media) | `stats` |
| `"quasipoisson"` | Conteos con sobredispersion | `stats` |
| `"quasibinomial"` | Proporcion con sobredispersion | `stats` |
| `"negativa_binomial"` | Conteos sobredispersos | `MASS` |
| `"gamma"` | Continua positiva asimetrica (concentraciones, tiempos) | `stats` |
| `"gamma_inverse"` | Gamma con link inverso (actuaria) | `stats` |
| `"gaussian_inversa"` | Continua positiva muy sesgada, link 1/mu^2 | `stats` |
| `"tweedie"` | Mix ceros + continua positiva (biomasa, precipitacion) | `statmod` |
| `"beta"` | Proporciones continuas en (0,1) (coberturas, tasas) | `betareg` |
| `"zip"` | Conteos con exceso de ceros (Zero-Inflated Poisson) | `pscl` |
| `"zinb"` | Conteos con exceso de ceros y sobredispersion | `pscl` |
| `"ordinal"` | Respuesta ordinal categorica (escala de dano) | `MASS` |
| `"multinomial"` | Respuesta nominal >2 categorias | `nnet` |

### GLMM (`analizar_glmm`)

| Familia | Tipo de Dato | Motor |
|---|---|---|
| `"poisson"` / `"conteos"` | Conteos con efectos mixtos | `lme4` |
| `"binomial"` / `"presencia_ausencia"` | Binaria 0/1 con efectos mixtos | `lme4` |
| `"binomial_probit"` | Binaria, link probit | `lme4` |
| `"binomial_cloglog"` | Binaria, link cloglog | `lme4` |
| `"negativa_binomial"` | Conteos sobredispersos | `lme4` |
| `"gamma"` | Continua positiva con efectos mixtos | `glmmTMB` |
| `"gamma_inverse"` | Gamma, link inverso | `glmmTMB` |
| `"lognormal"` | Log-normal con efectos mixtos | `glmmTMB` |
| `"beta"` | Proporciones (0,1) con efectos mixtos | `glmmTMB` |
| `"tweedie"` | Mix ceros + continua con efectos mixtos | `glmmTMB` |
| `"nbinom1"` | Binomial negativa NB1 con efectos mixtos | `glmmTMB` |
| `"nbinom2"` | Binomial negativa NB2 con efectos mixtos | `glmmTMB` |
| `"zip"` | Zero-Inflated Poisson mixto | `glmmTMB` |
| `"zinb"` | Zero-Inflated NB2 mixto | `glmmTMB` |
| `"zifgamma"` | Zero-Inflated Gamma mixto | `glmmTMB` |
| `"ordinal"` | Ordinal con efectos mixtos | `ordinal` |

> **Nota**: `glmmTMB`, `betareg`, `statmod`, `pscl`, `ordinal` y `nnet` son paquetes opcionales.
> Se instalan automaticamente si se necesitan, o puedes instalarlos manualmente:
> ```r
> install.packages(c("glmmTMB", "betareg", "statmod", "pscl", "ordinal", "nnet"))
> ```

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

`obtener_emmeans()` y `obtener_posthoc()` separan la lógica de medias marginales y contrastes para brindar mayor flexibilidad y robustez. Soportan múltiples predictores (vectores de caracteres) e interacciones, y detectan automáticamente interacciones significativas en la tabla ANOVA, sugiriendo análisis por efectos simples de forma amigable.

Si especificas `letras = TRUE` en `obtener_posthoc()`, se integra `multcomp::cld()` para adjuntar una columna `Grupo` de compact letter display (letras Tukey) directamente sobre la tabla de medias estimadas:

```r
# 1. Obtener emmeans Grid
medias <- obtener_emmeans(modelo, predictor = "tratamiento")

# 2. Obtener tabla de comparaciones por pares
posthoc_dif <- obtener_posthoc(modelo, predictor = "tratamiento")

# 3. Obtener tabla de medias con letras de Tukey asignadas
medias_letras <- obtener_posthoc(modelo, predictor = "tratamiento", letras = TRUE)
print(medias_letras)

# 4. Graficar diferencias o medias con letras directamente desde obtener_posthoc
grafico_cld <- obtener_posthoc(modelo, predictor = "tratamiento", letras = TRUE, graficar = TRUE)
print(grafico_cld)
```

## Arquitectura S3 y Métodos Genéricos

El paquete implementa Programación Orientada a Objetos S3 mediante la clase unificada `easy_model` (y `easy_splitplot` para modelos de parcelas divididas). Esto permite interactuar de manera consistente mediante métodos clásicos de R:

- `print()`: Muestra una caja resumen del modelo formateada con colores mediante `cli`.
- `summary()`: Imprime el resumen detallado del modelo nativo y calcula la tabla de ANOVA (usando contrastes Tipo III de forma predeterminada para modelos de efectos mixtos o split-plot).
- `plot()`: Despacha automáticamente los gráficos de diagnóstico residuo-ajustados según la familia (base o DHARMa panel).

## Funciones principales

- `analizar_lm()`: ajusta modelos lineales devolviendo un objeto `easy_model`.
- `analizar_glm()`: ajusta GLM con 17 familias de distribucion (gaussian, binomial, probit, cloglog, poisson, quasipoisson, quasibinomial, negativa_binomial, gamma, gaussian_inversa, tweedie, beta, zip, zinb, ordinal, multinomial) devolviendo `easy_model`.
- `analizar_lmm()`: ajusta modelos lineales mixtos con `lme4::lmer`, incluyendo control de REML.
- `analizar_bloques_azar()`: wrapper para Bloques Completos al Azar (RCBD).
- `analizar_parcelas_divididas()`: wrapper para Parcelas Divididas (Split-Plot).
- `analizar_glmm()`: ajusta GLMM con 16 distribuciones (poisson, binomial, probit, cloglog, negativa_binomial, gamma, gamma_inverse, lognormal, beta, tweedie, nbinom1, nbinom2, zip, zinb, zifgamma, ordinal) via lme4 y glmmTMB.
- `analizar_odds_ratio()`: calcula odds ratios para modelos binomiales.
- `evaluar_modelo()`: extrae metricas de ajuste (R2, ICC, sobredispersion, singularidad, DHARMa).
- `obtener_emmeans()`: extrae medias marginales estimadas (EMMeans).
- `obtener_posthoc()`: comparaciones multiples con CLD (letras Tukey) y visualizacion.
- `graficar_posthoc()`: grafica diferencias, razones de tasas u odds ratios.
- `graficar_predichos()`: grafica predichos marginales con letras de significancia.

## Autor

**Paul Alexander Lopez Peña**  
Pontificia Universidad Catolica de Chile  
Paquete orientado a bioestadística aplicada, reproducibilidad y comunicación científica.

## Changelog

### v0.3.0 (2026-07)
- **17 familias en `analizar_glm`**: gamma, gamma_inverse, gaussian_inversa, tweedie, beta, zip, zinb, ordinal, multinomial, binomial_probit, binomial_cloglog (nuevas).
- **16 distribuciones en `analizar_glmm`**: gamma, gamma_inverse, lognormal, beta, tweedie, nbinom1, nbinom2, zip, zinb, zifgamma, ordinal, binomial_probit, binomial_cloglog via `glmmTMB` y `ordinal` (nuevas).
- Mensajes bioestadisticos de guia al seleccionar cada familia.
- Advertencias automaticas de validacion por familia (valores > 0 para Gamma, (0,1) para Beta, etc.).
- Paquetes opcionales (glmmTMB, betareg, statmod, pscl, ordinal, nnet) cargados bajo demanda.

### v0.2.0 (2026-07)
- Arquitectura S3 unificada: clase `easy_model` con `print()`, `summary()`, `plot()`.
- `evaluar_modelo()`: diagnostico unificado de R2, ICC, singularidad, DHARMa.
- Fix critico: error `$ operator invalid for atomic vectors` en modelos singulares.
- Parametro `diagnosticos` consistente en todas las funciones `analizar_*`.

### v0.1.0 (2026-06)
- Release inicial: `analizar_lm`, `analizar_glm`, `analizar_lmm`, `analizar_glmm`.
- `analizar_bloques_azar`, `analizar_parcelas_divididas` (wrappers RCBD y Split-Plot).
- `obtener_posthoc`, `graficar_predichos`, `analizar_odds_ratio`.
