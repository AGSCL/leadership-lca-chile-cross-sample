# Results and reuse contract

## Produce and read the results

Render `leadership_LCA_analysis.qmd` from the project directory. It uses the
existing processed survey file, preserves the 13-item measurement specification,
and regenerates tables and figures from validated saved measurement results.
`LCA_REFIT=1` explicitly requests full measurement estimation; if no results
bundle exists, full estimation is necessary. The notebook stops on an error.

```powershell
# Optional on a different computer:
$env:LCA_RAW_PATH = 'D:/private-data/BD_2020_input3_match_not_miss.rds'
quarto render leadership_LCA_analysis.qmd

# Explicit fresh measurement estimation (4,000 starts and 1,000 BLRT replicates):
$env:LCA_REFIT = '1'
quarto render leadership_LCA_analysis.qmd
Remove-Item Env:\LCA_REFIT
```

The main report's input is already processed. This repository does not yet
contain an established executable path from the original survey data to that
input. That upstream preparation script must be documented by the investigators
before claiming end-to-end reproducibility. Individual-level data are confidential;
the investigators permit public release of aggregate results only.

## Table 2

| File | Contents and intended use |
|---|---|
| `table2_full_13item.csv` | Display-ready complete table, including actual participant counts and weighted class shares |
| `table2_binary.csv` | Binary display rows retained for older consumers |
| `table2_class_sizes_13item.csv` | Actual modal n, class label, sum of weights, weighted modal percentage, and mean posterior prevalence |
| `table2_summary_numeric.csv` | Full precision estimates by variable/class, observed and missing n, observed weight sum, weighted mean/SD, percentage or quantiles |
| `table2_tests_numeric.csv` | Numeric global P-value, test method, maximum absolute pairwise SMD, test status and named test components |
| `table2_smd_pairwise.csv` | All six signed SMDs per variable, the compared classes, mean difference and pooled SD |

Counts are actual observed people. The sum of weights is a separate quantity
whose scale depends on weight normalization. Weighted modal percentages and
unweighted model-estimated mean posterior probabilities answer different
questions; do not interchange them.

Binary P-values use the Rao-Scott F test. Continuous P-values use the
survey-adjusted Kruskal-Wallis rank test, which compares ranks/distributions
rather than exclusively medians. Tests use the available survey design, currently
weights only because PSU and strata identifiers are not mapped. They condition
on fixed modal classes, are exploratory, and are not multiplicity-adjusted.

SMD for classes a and b is `(mean_a - mean_b) / sqrt((variance_a + variance_b)/2)`.
Means are survey-weighted. Continuous variances are population-variance estimates
from `survey::svyvar`; binary variances are `p*(1-p)`. The displayed SMD is the
maximum of the six absolute values. It is a difference of means, including when
the displayed descriptive summary is a median. Equal constants have SMD zero;
different constants with zero pooled variance have infinite SMD. Missing class
estimates yield `Not estimable`, with machine-readable NA and a test reason.

Quantiles preserve the original midpoint weighted-ECDF interpolation. Variables
are described using their own observed cases. Denominators are exported so that
missingness cannot be mistaken for absence of exposure.

References: [survey rank tests](https://search.r-project.org/CRAN/refmans/survey/html/svyranktest.html)
and [survey means and population variances](https://search.r-project.org/CRAN/refmans/survey/html/surveysummary.html).

## Other aggregate exports

| File | What it establishes |
|---|---|
| `sample_flow.csv` | Actual input and complete-case counts |
| `class_key.csv` | Canonical labels and original model permutation when available |
| `variable_dictionary.csv` | Source columns, recoding and score units |
| `table1_enumeration.csv` | Numeric fit indices and classification summaries |
| `table3_itemprob.csv` | Historical rounded top-two-category table |
| `table3_itemprob_long.csv` | Full precision probabilities for all four response categories |
| `table_classification_diagnostics.csv` | Modal n, mean posterior prevalence, AvePP and OCC |
| `tableS1_or_numeric.csv` | ORs, interval endpoints, comparison/reference, n and convergence |
| `tableS2_margins_numeric.csv` | Predictions and differences with interval endpoints in percent/percentage points |
| `tableS3_sensitivity_numeric.csv` | Existing unweighted modal/BCH comparison with explicit units |
| `output_manifest.csv` | Generated files, producer, measurement reuse/refit status and SHA-256 hashes |
| `source_manifest.csv` | Exact main QMD and helper source hashes from the same run |
| `session_info_main.txt` | R and package versions for the main run |

The display CSVs `tableS1_or.csv`, `tableS2_margins.csv` and
`tableS3_sensitivity.csv` remain available. Numeric exports avoid parsing
rounded strings. The S2 summaries retain the existing parameter-simulation
medians and percentile intervals, using a fixed simulation seed independent of
whether measurement estimation was reused. S1 models include one covariate at a
time; mutually adjusted models belong to the downstream notebooks.

**Manuscript numbering:** v17 manuscript Table S2 is the naive/BCH comparison,
exported as `tableS3_sensitivity.csv` by the main QMD. It includes seven binary
variables (including sexual harassment and female sex) plus mean job satisfaction.
The QMD's Table S2 (`tableS2_margins.csv`) instead gives class-membership
probabilities conditional on exposure. These are different estimands; do not
choose the source merely by its table number.

## Private bundle for future QMDs: schema 2.0.0

```r
source("R/load_leadership_results.R")
results <- load_leadership_results()
d_lca <- results$analysis_data
post4 <- results$post4
modal4 <- results$modal4
class_labels <- results$class_labels
results$tables[["table2_tests_numeric.csv"]]
```

`lca_results.rds` preserves all previously used top-level fields, including
`fit_tbl`, `rho4`, `sizes4`, `post4`, `modal4`, `item_order`, `class_diag`,
`class_labels`, `n`, `n_init`, and `seed`. Additions include:

- `analysis_data`: ordered model items, cleaned covariates, weights and
  `.source_row`, an ordinal position in the input, not a public identifier.
- `model_input_hash` and `analysis_data_hash`: SHA-256 fingerprints. The loader
  verifies the analysis frame and correspondence with posterior/modal dimensions.
- `classification_matrix`, `ml_models` (coefficients/covariance), `margins`,
  `table2`, `tables`, `variable_dictionary`, `class_key` and `item_meta`.
- `provenance`: source/input fingerprints, runtime, weighting, measurement action
  and alignment validation. Successful fallback seeds are recorded on new fits;
  an old bundle cannot retrospectively establish unsaved fallback information.

New downstream work should load the saved analysis frame, rather than independently
rebuild and join rows by position. Current older consumers remain compatible,
but not every historical notebook has been migrated to this interface.
Subsets must use the same logical index for data, posteriors and assignments.
Do not transfer general-sample class numbering to the multiple-group model.

On a fresh fit, `lca_measurement_fits.rds` saves the full fits and
`enumeration_modal_all.rds` saves the assignments needed by enumeration figures.
Both are private. Reuse mode does not invent unsaved fits.

## Hospital and cross-sample outputs

The updated multiple-group source exports `mg_sample_sizes.csv`,
`mg_invariance_fit.csv`, `mg_invariance_decision.csv`, `mg_prevalence_values.csv`,
`mg_class_key.csv`, and `mg_item_top2_values.csv`. Its invariance tables retain
the notebook's existing rounding. Private `mg_invariant_fit.rds` and
`mg_separate_fits.rds` support subsequent work and figure reconstruction.
Part 2 adds `tableS8_adjusted_ror_values.csv`,
`tableS8_adjusted_ror_formatted.csv`, and `part2_class_key.csv` alongside S7,
and saves the validated raw invariant fit as private `cache_g4_inv_part2.rds`.
These new downstream exports require rendering their producing notebooks.

Both notebooks accept `LCA_RAW_PATH` and `LCA_HOSPITAL_PATH`; the general and
hospital input files remain private. The main manifest covers only main outputs,
not an assertion that all downstream reports were regenerated.

## Sharing

Only aggregate results may be made publicly available. Individual-level data,
including anonymous records and participant-level model outputs, are confidential.

`lca_results_public.rds` is selected positively from aggregate fields and tables;
it excludes the analysis frame, posterior matrix, modal assignments and full fits.
It supports inspection of estimates without releasing individual records.
Checksums establish file correspondence, not scientific validity by themselves.

The root `.gitignore` uses an explicit list of this manuscript's sources and
aggregate outputs. Private inputs, RDS caches, backups, historical manuscripts
and other projects are excluded by default. It does not upload anything or
override files already tracked in an existing repository. Review any additional
file before adding it. Licensing and the public repository address have not yet
been specified. There is no public individual-level data access.
