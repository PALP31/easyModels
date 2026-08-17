# easyModels 0.4.2

### ✨ Experiencia de ajuste más clara y reproducible
* Se incorporó `resumir_ajuste()`, una salida tabular de una fila con fórmula, familia, enlace, AIC, BIC, R² y el detalle de observaciones totales, utilizadas y excluidas.
* Los objetos `easy_model` ahora conservan ese detalle de muestra y lo muestran al imprimirse; si se omitieron filas, se advierte de forma explícita para evitar resultados interpretados con una muestra distinta a la esperada.
* Los cuatro ajustadores principales (`analizar_lm()`, `analizar_glm()`, `analizar_lmm()` y `analizar_glmm()`) validan que los datos sean un `data.frame`, que la fórmula sea válida y que las variables existan antes de invocar el motor estadístico.
* Se añadieron pruebas de regresión para el resumen de ajuste, el registro de filas omitidas y los mensajes de validación temprana.

# easyModels 0.4.1

### 📚 Documentación Global del Paquete (`?easyModels`)
* Se agregó `R/easyModels-package.R` y la documentación generada en `man/easyModels.Rd` con alias `easyModels` y `easyModels-package`.
* Al ejecutar `?easyModels` o `help("easyModels")`, se despliega una guía de referencia rápida estructurada por módulos:
  - **Ajuste de Modelos**: `analizar_lm()`, `analizar_lmm()`, `analizar_glm()`, `analizar_glmm()`.
  - **Diseños Experimentales**: `analizar_bloques_azar()`, `analizar_parcelas_divididas()`, `analizar_latino()`, `analizar_medidas_repetidas()`, `analizar_strip_plot()`.
  - **Diagnóstico y Evaluación**: `verificar_supuestos()`, `sugerir_modelo()`, `comparar_modelos()`, `evaluar_modelo()`, métodos S3 `print()`, `summary()`, `plot()`.
  - **Post-Hoc y Visualización Científica**: `obtener_posthoc()`, `obtener_emmeans()`, `exportar_tabla_anova()`, `exportar_tabla_posthoc()`, `graficar_predichos()`, `graficar_interaccion()`, `graficar_posthoc()`, `analizar_odds_ratio()`.

### 🛠️ Mejoras de Robustez y Corrección de Bugs
* **Extracción Robusta de Coeficientes y Matriz de Covarianza**: En `R/utils_modelos.R` y `R/analizar_odds_ratio.R`, se adaptaron `obtener_coeficientes_fijos()` y `vcov()` para soportar transparentemente múltiples motores estadísticos: `glmmTMB` (`fixef$cond` y `vcov$cond`), `lme4` (`merMod`), `MASS` (`glm.nb`, `polr`), `pscl` (`zeroinfl`) y `stats` (`lm`, `glm`).
* **Métodos S3 Estructurados**: En `R/metodos_s3.R`, se implementó `print.summary_easy_model()`, permitiendo que `summary(modelo)` imprima la tabla ANOVA y el resumen del modelo nativo de forma limpia y retorne un objeto estructurado.
* **Separación Visual en Gráficos Multifactores**: En `R/graficar_predichos.R`, se optimizó la separación con `position_dodge()` para modelos factoriales e interacciones, evitando superposición entre puntos, barras de error y letras de significancia Tukey (CLD).
* **Corrección en Viñetas**: Se normalizó el parámetro `diagnosticos = FALSE` en `vignettes/guia-easyModels.Rmd`, asegurando una compilación fluida y libre de advertencias con `rmarkdown::render()`.

### 🧪 Suite de Pruebas Unitarias (`tests/testthat/`)
* Se expandió la suite de pruebas unitarias cubriendo:
  - `test-ajuste-modelos.R`: Ajustes de LM, LMM, GLM (Binomial, Poisson, Gamma), GLMM y RCBD.
  - `test-posthoc-graficos.R`: EMMeans, contrastes pareados, letras CLD, Odds Ratios y objetos ggplot.
  - `test-evaluacion-metodos.R`: Comportamiento de `evaluar_modelo()`, `print()`, `summary()` y `plot()`.
  - `test-documentacion.R`: Verificación del acceso a `help("easyModels")`.
  - `test-arquitectura.R`: Consistencia de las clases S3 `easy_model` y `easy_splitplot`.
  - `test-diagnosticos.R` y `test-disenos.R`: Supuestos estadísticos y diseños experimentales clásicos.

---

# easyModels 0.4.0

* 🌾 **Nuevos Diseños Experimentales:** `analizar_latino()`, `analizar_medidas_repetidas()`, `analizar_strip_plot()`.
* 🔍 **Auditoría Automatizada de Supuestos:** `verificar_supuestos()` para normalidad, homocedasticidad, VIF, Durbin-Watson y distancia de Cook.
* 🤖 **Asistente de Selección de Modelos:** `sugerir_modelo()` con optimización Box-Cox.
* 📊 **Selección y Comparación de Modelos:** `comparar_modelos()` con métricas AIC, $\Delta AIC$, pesos de Akaike, BIC y $R^2$.
* 📄 **Tablas Formateadas:** `exportar_tabla_anova()` y `exportar_tabla_posthoc()`.
* 🎨 **Gráficos Científicos:** Soporte de barras, puntos, líneas y paletas académicas en `graficar_predichos()` y `graficar_interaccion()`.
