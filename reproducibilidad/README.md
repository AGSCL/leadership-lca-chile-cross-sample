# Empieza aquí

**Referencia actual: `../IAOEH/manuscriptv20.docx`.** Esta carpeta ordena su reproducción y las observaciones de Elisa Ansoleaga. No modifica el manuscrito. Las decisiones posteriores a las notas históricas de esta guía se documentan en `../HANDOFF_destructive_leadership_LCA.md`.

**Distribución pública:** Git incluye únicamente las seis fuentes QMD, seis helpers R,
el verificador, la configuración, este README y los CSV de mapa/procedencia.
Los enlaces de esta guía a notas editoriales, verificaciones históricas, manuscrito,
informes y resultados corresponden al espacio de trabajo privado y no estarán
disponibles en un clon público. No son dependencias ejecutables del código.

## Publicar el código en GitHub

El repositorio local empieza en `reproducibilidad/`, no en la carpeta general de
la tesis. `.gitignore` excluye todo salvo una lista explícita de fuentes revisadas:
datos individuales, RDS/RData, réplicas bootstrap, HTML, figuras, logs, notas de
Elisa, planes y revisiones editoriales quedan privados, también si se agregan
archivos nuevos. No uses `git add -f` para saltar esa protección.
`.gitattributes` conserva los bytes y saltos de línea porque los checkpoints
comprueban hashes de las fuentes.

Preparación del 17-10-2026: rama local `main`, sin commit ni remoto. Desde esta
carpeta, revisa lo que publicarás antes de crear el primer commit:

```powershell
git status --short
git add --dry-run .
Rscript --vanilla verificar.R --sources
git add .
git diff --cached --stat
git diff --cached
git commit -m "Add reproducible leadership LCA analysis"
```

Después crea en GitHub un repositorio vacío (sin README ni licencia generados),
copia su URL y ejecuta, sustituyendo el ejemplo:

```powershell
git remote add origin https://github.com/TU_USUARIO/TU_REPOSITORIO.git
git push -u origin main
```

No se ha elegido una licencia en nombre de los autores. Antes de permitir
redistribución del código, acuerden y añadan una licencia apropiada y su entrada
en `.gitignore`. Un repositorio público no concede por sí solo una licencia de uso.

**Alcance de un clon limpio:** `verificar.R --sources` revisa sintaxis y comprobaciones
de los helpers sin datos. Reproducir estimaciones requiere las dos entradas privadas
autorizadas, `LCA_RAW_PATH` y `LCA_HOSPITAL_PATH`, además de R, Quarto y sus paquetes.
Sin resultados guardados se debe reestimar la medición, operación costosa; utiliza
`LCA_REFIT=1` y el orden 01–05 descrito abajo. No se distribuyen datos sintéticos ni
se garantiza un entorno idéntico: las versiones de paquetes aún no están bloqueadas.
Las rutas locales históricas incluidas en 02/03 son búsquedas alternativas; usa las
variables de entorno en otro equipo. El mapa y la procedencia citan archivos privados
anteriores para trazabilidad, no como archivos públicos que haya que descargar.

**Estado, 01-10-2026:** las etapas 01–05 y la verificación completa pasaron. Por decisión final, el bootstrap usa **B=250**, 4.000 inicios por refit, seis procesos y sin warm start: 250 válidas, 0 fallidas. Reutilizó las primeras 250 réplicas válidas de la corrida B=1.000 interrumpida, sin seleccionarlas por resultados. Ver [estado y límites](VERIFICACION.md).

## Hoy: sólo tres cosas

1. Lee las **decisiones aceptadas** en [Observaciones de Ansoleaga](OBSERVACIONES_ANSOLEAGA.md): SMD del DOCX congelados; congruencia de ERI con Palma cerrada.
2. Para encontrar un número, abre [el mapa de tablas, figuras y suplementos](MAPA_MANUSCRITO.csv).
3. Para recalcular tablas y figuras, ejecuta **01 → 02 → 03 → 04**; después, **05** añade el bootstrap costoso. No necesitas abrir los QMD históricos ni restaurar una sesión R.

[Árbol del proyecto](ARBOL_PROYECTO.md) · [Qué se verificó](VERIFICACION.md)

## Cinco etapas analíticas y una auditoría

| Paso | Archivo | Qué resuelve | Punto de control privado |
|---|---|---|---|
| 01 | [Muestra general](01_general.qmd) | Enumeración, caracterización ponderada, probabilidades y modal/BCH | `salidas/01_general.RData`: `general` |
| 02 | [Comparación hospitalaria](02_hospital.qmd) | Medición compartida, invariancia y prevalencias | `salidas/02_hospital.RData`: `hospital` |
| 03 | [Asociaciones](03_asociaciones.qmd) | Ajustes ponderados; propuesta principal de ocho covariables con psicotrópicos; sensibilidades anteriores de ocho/siete | `salidas/03_asociaciones.RData`: `asociaciones` |
| 04 | [Sensibilidad y figuras](04_sensibilidad_figuras.qmd) | Cinco/ocho clases y Figuras 1–4 | `salidas/04_sensibilidad.RData`: `sensibilidad` |
| 05 | [Bootstrap compartido](05_bootstrap.qmd) | Reestimar medición compartida y ambos ajustes por réplica; OR/ROR y probabilidades | `salidas/05_bootstrap.RData`: `bootstrap` |
| 06 | [Multiplicidad y posible solapamiento](06_multiplicidad_solapamiento.qmd) | Auditar inferencia guardada y factibilidad de restricciones geográficas y ocupacionales, sin reajustar modelos | `salidas/06_auditoria.rds`: tablas agregadas y procedencia |

Informes verificados: [01](informes/01_general.html) · [02](informes/02_hospital.html) · [03](informes/03_asociaciones.html) · [04](informes/04_sensibilidad_figuras.html) · [05](informes/05_bootstrap.html). El informe 05 corresponde al resultado final B=250.

Los QMD y seis helpers locales en `R/` contienen la ruta; no extraen bloques de notebooks históricos. [association_models.R](R/association_models.R) comparte recodificación, ajustes y comprobaciones entre 03 y 05; [bootstrap_shared.R](R/bootstrap_shared.R) gestiona remuestreo, alineación de clases y reanudación.

**Cambio autorizado, no reproducción numérica del ajuste anterior:** los modelos multivariables usan pesos finales postestratificados en la muestra general y peso 1 hospitalario. La especificación propuesta como principal reemplaza satisfacción laboral por `psychotrop_flexible`; su promoción al manuscrito está pendiente. Se conservan las dos especificaciones anteriores como sensibilidades, ahora ponderadas. La medición y la matriz de clasificación permanecen sin ponderación.

Los HTML se escriben en `informes/`; CSV, figuras, modelos y checkpoints en `salidas/`. **No confundir procedencia con sustitución editorial:** la Table 4 actual procede del ajuste histórico `tableS8_adjusted_ror_values.csv`; `table4_main_psychotrop_values.csv` es una candidata diferente y `bootstrap_associations.csv` contiene la inferencia del bootstrap final B=250, completado y verificado el 01-10-2026. Ninguna ha sido insertada en el DOCX. Table S2 procede de `tableS3_sensitivity.csv`.

## Ejecutar desde cero o retomar

Necesitas R, Quarto y los paquetes declarados en cada QMD. Los notebooks se detienen si faltan paquetes; no instalan software durante el render. Los resultados guardan `sessionInfo()`. Las versiones se registran, pero todavía no existe un entorno de paquetes bloqueado por versiones.

Desde la carpeta principal del proyecto, en PowerShell:

```powershell
Set-Location reproducibilidad
$quarto = (Get-Command quarto.exe).Source
$etapas = '01_general.qmd', '02_hospital.qmd', '03_asociaciones.qmd', '04_sensibilidad_figuras.qmd'
foreach ($etapa in $etapas) {
    & $quarto render $etapa
    if ($LASTEXITCODE -ne 0) { throw "Falló $etapa; revisar antes de continuar." }
}
Rscript --vanilla verificar.R --without-bootstrap
```

**El render del proyecto sin especificar archivo incluye 01–06 y lanza el bootstrap completo.** La secuencia anterior limita el trabajo a 01–04. También puedes ejecutar un paso individual:

```powershell
& (Get-Command quarto.exe).Source render 03_asociaciones.qmd
```

**Google Drive puede interferir con Quarto:** si la exportación falla al recorrer un `desktop.ini` auxiliar, consulta la limpieza acotada en [VERIFICACION.md](VERIFICACION.md). No borres modelos ni resultados; no necesitas reestimarlos.

Se usa `quarto.exe` porque el lanzador `quarto.cmd` instalado aquí falla con la ruta que contiene `Program Files`. No es un problema del análisis.

**Entradas en otro computador:** definir rutas absolutas a los archivos autorizados antes de ejecutar:

```powershell
$env:LCA_RAW_PATH = 'D:/datos_privados/BD_2020_input3_match_not_miss.rds'
$env:LCA_HOSPITAL_PATH = 'D:/datos_privados/Base de datos 27.8.2019.dta'
```

En este equipo se detectan las ubicaciones existentes. El archivo general se llama `RAW_PATH` por compatibilidad, pero **ya está procesado**. Para transportar la ruta bastan esta carpeta con sus helpers, las dos entradas privadas y R/Quarto con los paquetes requeridos. Los QMD antiguos no son dependencias ejecutables.

**Reutilizar:** es el comportamiento predeterminado. La primera etapa puede importar el `lca_results.rds` del proyecto padre y verifica su correspondencia con los ítems y filas antes de usarlo. La etapa 02 reutiliza su propio checkpoint sólo si coinciden datos, configuración y fuentes. La etapa 03 siempre recalcula asociaciones desde el checkpoint 02. La etapa 04 reutiliza ajustes compatibles o estima los que faltan.

Las salidas anteriores de 03 no constituyen la nueva especificación sólo por conservar el nombre del archivo. La versión nueva registra sus conjuntos de covariables, pesos, casos completos, tamaño efectivo y procedencia. `psicotrop` se conserva sin alterar; `psychotrop_flexible` usa cualquier sí observado, tres no observados o NA para el resto. La ventana de recuerdo todavía requiere confirmación.

Las etapas 02 y 04 usan hasta seis procesos en paralelo, conservando `Mersenne-Twister`, 4.000 inicios y las semillas 2125–2134. Para dejar más recursos disponibles, define `$env:LCA_WORKERS = '2'` antes del render. Guardan los modelos de medición antes de terminar tablas y figuras, de modo que un fallo posterior no obliga a estimarlos otra vez. No ejecutes dos renders de la misma etapa simultáneamente.

**Reestimar toda la medición:** es costoso; incluye 4.000 inicios y, en 01, 1.000 replicaciones BLRT.

```powershell
$env:LCA_REFIT = '1'
foreach ($etapa in $etapas) { & $quarto render $etapa }
Remove-Item Env:\LCA_REFIT
```

No reduzcas inicios para presentar una corrida como definitiva. `LCA_FINAL` ya no es necesario en esta ruta: todos los ajustes de medición usan 4.000 inicios.
La medición general se reestima en 01 y la hospitalaria en 02; 04 consume esos ajustes compatibles, sin repetir la enumeración general recién calculada. Para forzar una nueva medición general, empieza por 01, no por 04.

## 05: bootstrap costoso y reanudable

Requiere 02 y 03 actualizados. Por defecto usa **B=250 réplicas y 4.000 inicios por ajuste de medición**, hasta seis procesos (`LCA_WORKERS`). Cada réplica remuestrea participantes dentro de cada setting, conserva sus pesos finales, reestima la medición compartida, alinea clases y ajusta ambos modelos separados. Los fallos quedan registrados, no se reemplazan silenciosamente.

```powershell
& (Get-Command quarto.exe).Source render 05_bootstrap.qmd
Rscript --vanilla verificar.R
```

Las réplicas privadas se guardan bajo `salidas/bootstrap_shared/`, separadas por firma de datos, fuentes y configuración. Repetir la misma corrida reanuda esos archivos; `LCA_REFIT` pertenece a la medición de 01–04, no sustituye este mecanismo. No ejecutar dos copias simultáneas. Revisar `bootstrap_replicate_status.csv`, el número válido y los parámetros antes de llamar final al resultado.

**Prueba opcional de funcionamiento, no inferencia:** B=2 y sólo cinco inicios. Sobrescribe las salidas agregadas de 05 con un borrador; no compartirlo como análisis final. Quita las variables de prueba antes de ejecutar la corrida predeterminada.

```powershell
$env:LCA_BOOTSTRAP_B = '2'
$env:LCA_BOOTSTRAP_STARTS = '5'
try {
    & (Get-Command quarto.exe).Source render 05_bootstrap.qmd
} finally {
    Remove-Item Env:\LCA_BOOTSTRAP_B, Env:\LCA_BOOTSTRAP_STARTS -ErrorAction SilentlyContinue
}
```

El bootstrap incorpora incertidumbre de la medición compartida, condicionada a cuatro clases e invariancia. Es remuestreo individual con pesos finales fijos, no reconstrucción del diseño: no recupera PSU/estratos, conglomerados hospitalarios, calibración de pesos o posibles participantes compartidos. No elimina no colapsabilidad. Sus intervalos no son los intervalos condicionales de Hessiana exportados por 03.

## 06: auditoría de multiplicidad y posible solapamiento

Reutiliza los checkpoints privados 02, 03 y 05, los resultados bootstrap y las bases
identificadas en su procedencia. No vuelve a estimar modelos ni modifica la v20.
Desde `reproducibilidad/`:

```powershell
& (Get-Command quarto.exe).Source render 06_multiplicidad_solapamiento.qmd
Rscript --vanilla verificar.R --audit06
```

El informe queda en `informes/06_multiplicidad_solapamiento.html`. Sus salidas
privadas en `salidas/` son `06_ror.csv`, `06_omnibus.csv`,
`06_probabilidades.csv`, `06_geografia.csv`, `06_salud.csv`,
`06_restricciones.csv`, `06_procedencia.csv`, `06_ror.png` y
`06_auditoria.rds`. El RDS contiene resultados agregados, no filas individuales.
La lista pública sólo admite la nueva fuente QMD; estos resultados siguen excluidos.

Los contrastes globales son exploratorios y las restricciones calculan cuántos casos
quedarían. No constituyen una sensibilidad con modelos reajustados, no identifican
personas duplicadas y no establecen equivalencia entre muestras. Las diferencias
entre probabilidades marginales usan la distribución de covariables de cada muestra.

`--audit06` comprueba las salidas de esta auditoría sin certificar de nuevo 01–05;
`--sources` comprueba también su sintaxis sin datos. El verificador completo conserva
su control de antigüedad de HTML frente a `_quarto.yml`: añadir 06 puede hacer que
solicite actualizar informes anteriores aunque sus modelos no hayan cambiado.
Para esta auditoría basta renderizar y verificar 06 explícitamente.

## Abrir un resultado sin cargar toda la sesión

En R, con directorio de trabajo `reproducibilidad/`:

```r
checkpoint <- new.env(parent = emptyenv())
load("salidas/03_asociaciones.RData", envir = checkpoint)
checkpoint$asociaciones$tables$main_ror
checkpoint$asociaciones$specifications
checkpoint$asociaciones$tables$diagnostics
```

Cada `.RData` contiene un solo objeto nombrado. Guarda datos y resultados explícitos, no una copia de `.GlobalEnv`. Los `.rds` de apoyo siguen siendo compatibles con los exports existentes; no hay que abrirlos manualmente para ejecutar la ruta.

## Qué significa que la ruta pasó

`Rscript --vanilla verificar.R --sources` comprueba fuentes sin estimar modelos. `--without-bootstrap` certifica solo 01–04. Sin esa opción, la verificación exige el B=250 final, sin fallos, con 4.000 inicios y HTML actualizados; pasó el 01-10-2026 a las 16:34:45.

Una ejecución correcta reproduce una especificación; no valida por sí sola su interpretación. El [informe de observaciones](OBSERVACIONES_ANSOLEAGA.md) diferencia acuerdos y auditoría histórica. **Los SMD del DOCX se conservan y ERI está cerrado**; el uso nuevo de pesos no reabre esas decisiones. El ridge existente se activó en dos ajustes generales por coeficientes extremos; no es Firth y está documentado. La nomenclatura Tyrannical, *laissez-faire* y Constructive se propagó a las figuras regeneradas e inspeccionadas, sin cambiar su medición.

## Límites que siguen abiertos

- **Antes del RDS general:** falta establecer una preparación ejecutable comprobada desde la encuesta original. Esta ruta comienza con las dos entradas analíticas identificadas.
- **STROBE y Figure S1:** son piezas editoriales existentes; su contenido/paginación y diagrama de flujo se cotejan manualmente. El mapa indica sus fuentes.
- **Manuscrito:** denominadores y etiquetas se revisan sin alterar los SMD acordados. La nueva especificación de asociaciones es candidata, no una sustitución editorial ya aprobada o aplicada.
- **Ejecución nueva:** 01–05 verificados, incluido B=250. Pruebas de prevalencia, Firth y alcohol como desenlace no están incorporados. La especificación propuesta principal tiene 1.824/866 casos completos; la equivalencia de ventanas de psicotrópicos y su promoción editorial siguen pendientes.
- **Privacidad:** `datos/`, `salidas/`, `.RData` y los modelos contienen información individual, aunque sea anónima. No publicar esa carpeta ni compartir automáticamente los HTML.

Los históricos permanecen intactos. `leadership_LCA_analysis_part3_or_adjusted.qmd` queda identificado como análisis auxiliar: usa la solución general y no produce las Tables 4, S5 o S6 del manuscrito actual.
