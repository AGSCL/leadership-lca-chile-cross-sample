# Leadership profiles and occupational health in Chile

**Actualización, 12 noviembre 2026:** empieza en
[reproducibilidad/README.md](reproducibilidad/README.md). Incluye seis etapas QMD
con checkpoints explícitos. La nueva
[etapa 06](reproducibilidad/06_multiplicidad_solapamiento.qmd) audita la
multiplicidad y el posible solapamiento de muestras; las restricciones todavía
no se han reestimado. Se publica el código, conservando privados los datos,
modelos y salidas de esta ruta. Los archivos históricos se conservan.

Analysis sources and results for the study of constructive, destructive, and
*laissez-faire* leadership among Chilean salaried workers and hospital workers.
The manuscript is being prepared for *International Archives of Occupational and
Environmental Health*.

**Start with [the reader's guide](index.qmd).** Its rendered version,
`index.html`, provides a didactic directory of the reports, explains their
relationship, and gives the execution order. To build the guide without
participant data or statistical analysis:

```powershell
quarto render index.qmd
```

## Scientific reading order

| Step | Question | Source | Saved report |
|---|---|---|---|
| 1. Primary axis | Which profiles are retained, and why four classes? | [Main analysis](leadership_LCA_analysis.qmd) | [HTML](leadership_LCA_analysis.html) |
| 2. Primary axis | Are item-response profiles shared across the general and hospital samples? | [Multiple-group analysis](leadership_LCA_multigroup_hospital.qmd) | [HTML](leadership_LCA_multigroup_hospital.html) |
| 3. Secondary | How do covariate associations compare across settings? | [Part 2](leadership_LCA_analysis_part2_sample_comparison.qmd) | [HTML](leadership_LCA_analysis_part2_sample_comparison.html) |
| 4. Secondary | How do general-sample associations change with mutual adjustment? | [Part 3](leadership_LCA_analysis_part3_or_adjusted.qmd) | [HTML](leadership_LCA_analysis_part3_or_adjusted.html) |
| 5. Sensitivity | What changes with five or eight classes? | [Enumeration uncertainty](leadership_LCA_8class_sensitivity.qmd) | [HTML](leadership_LCA_8class_sensitivity.html) |
| 6. Sensitivity | What changes when job satisfaction leaves the adjustment set? | [Seven-covariate comparison](leadership_LCA_part2_adjusted_nojobsat.qmd) | [HTML](leadership_LCA_part2_adjusted_nojobsat.html) |
| 7. Figures | Which saved results generate manuscript Figures 1–4? | [Consolidated figures](Figures_IAOEH_consolidated.qmd) | [HTML](Figures_IAOEH_consolidated.html) |

Part 2 includes both one-covariate-at-a-time and mutually adjusted models. Part 3
uses the general-sample model. Their job-satisfaction coefficients have different
units: per within-sample standard deviation in Part 2's adjusted model and per
score point in Part 3. The index explains these distinctions and how to read
counts, weighted shares, model prevalence, P-values, and SMDs.

## Reusing the results

Read [EXPORTS.md](EXPORTS.md) before building a downstream notebook. Use the
[results loader](R/load_leadership_results.R) and documented numeric exports to
retain precision, class labels, provenance, and row alignment. The
[Table 2 helper](R/leadership_table2.R) contains the weighted descriptive and
comparison calculations. The export manifest records checksums for the files
produced by the main notebook.

The main notebook refreshes tables from compatible saved measurement results by
default. `LCA_REFIT=1` requests a fresh measurement fit. `LCA_RAW_PATH` can point
the main notebook to an authorized general-survey input file. Other notebooks
still have their own data-path configuration. Consult the index for the full
execution order, cache dependencies, and software requirements.

## Data and publication

The workflow requires the general-survey file
`BD_2020_input3_match_not_miss.rds` and, for hospital comparisons,
`Base de datos 27.8.2019.dta`. Individual-level data are confidential and cannot
be made publicly available. Only aggregate results may be released. This
repository does not grant public access to participant records.

Keep raw data and participant-containing objects private, including
`lca_results.rds`, model caches, and enumeration assignments. The separate
`lca_results_public.rds` is the aggregate-only bundle. Publish reviewed aggregate
exports, figures, and reports together with their source and run provenance.
Inspect HTML reports before release because code output can include individual
records or local paths. A file's presence or an old successful render log alone
does not establish that it matches the latest source.

The main HTML report and aggregate exports were refreshed on September 7, 2026,
using saved measurement estimates. Multiple-group and Part 2 sources received
export-only updates; their HTML reports and the other linked reports retain
earlier runs. The new multiple-group and Part 2 exports will be written at their
next render. These saved reports were reviewed for participant-level output and
are included in the public-file allowlist as documented snapshots. All analysis
and report links were checked locally. The current folder also contains older
projects and drafts outside this manuscript's pipeline.

GitHub displays this README and the source files. To serve the saved HTML reports
as webpages, publish the reviewed site files with GitHub Pages or another host,
preserving their relative paths. No repository URL or deployment has been set in
this documentation.
