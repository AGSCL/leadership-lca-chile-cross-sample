# Integration checks against the existing outputs; never estimates an LCA.
source("R/load_leadership_results.R")
x <- load_leadership_results()
p <- readRDS("lca_results_public.rds")
stopifnot(x$provenance$measurement_action %in% c("reused", "refitted"))
forbidden <- c("analysis_data", "post4", "modal4", "fits", "ml_models", ".source_row")
check_public <- function(z) {
  if (is.list(z)) {
    stopifnot(!any(names(z) %in% forbidden))
    invisible(lapply(z, check_public))
  }
}
check_public(p)
stopifnot(identical(x$tables, p$tables), sum(x$table2$class_sizes$n_modal) == x$n,
  abs(sum(x$table2$class_sizes$weighted_percent) - 100) < 1e-8,
  nrow(x$table2$smd_pairs) == 6*nrow(x$table2$tests),
  all(x$table2$tests$test_status == "ok"),
  all(is.finite(x$table2$tests$p_value)),
  all(is.finite(x$table2$tests$max_abs_smd)),
  all(x$table2$summary$n_nonmissing + x$table2$summary$n_missing ==
        x$table2$class_sizes$n_modal[match(x$table2$summary$class, x$table2$class_sizes$Class)]),
  identical(names(x$table2$full), c("Characteristic", "C1", "C2", "C3", "C4", "P", "SMD")),
  nrow(x$tables[["table3_itemprob_long.csv"]]) == 13*4*4)
for (manifest_file in c("output_manifest.csv", "source_manifest.csv")) {
  m <- readr::read_csv(manifest_file, show_col_types = FALSE)
  stopifnot(all(file.exists(m$file)))
  hashes <- vapply(m$file, function(f) digest::digest(file = f, algo = "sha256"), character(1))
  stopifnot(all(hashes == m$sha256))
}
for (qmd in c("leadership_LCA_analysis.qmd", "leadership_LCA_multigroup_hospital.qmd",
              "leadership_LCA_analysis_part2_sample_comparison.qmd")) {
  stopifnot(!any(grepl("COMPLETAR", readLines(qmd, warn = FALSE))))
}
cat("Export integration checks passed: complete statistics, denominators, schemas, aggregate-only bundle and matching hashes.\n")
