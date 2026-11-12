# Called by the main QMD after every table has been constructed successfully.
export_leadership_results <- function(env) {
  with(as.list(env), {
    s1_numeric <- purrr::imap_dfr(or_vars, function(key, lab) {
      m <- ml_models[[lab]]
      dplyr::mutate(m$ors, variable = key, covariate = lab,
        class_label = class_labels[class], reference_class = "C1",
        n = sum(!is.na(d_lca[[key]])), convergence = m$conv)
    })
    s2_numeric <- purrr::imap_dfr(or_vars, function(key, lab) {
      mg <- margin_objects[[lab]]
      tibble::tibble(variable = key, covariate = lab, class = paste0("C", 1:K),
        class_label = class_labels, no_percent = 100*mg$z0[2,],
        no_lower = 100*mg$z0[1,], no_upper = 100*mg$z0[3,],
        yes_percent = 100*mg$z1[2,], yes_lower = 100*mg$z1[1,], yes_upper = 100*mg$z1[3,],
        delta_pp = 100*mg$delta[2,], delta_lower = 100*mg$delta[1,], delta_upper = 100*mg$delta[3,])
    })
    s3_numeric <- purrr::imap_dfr(c(s3_vars, "Job satisfaction, mean" = "job_sat"), function(key, lab) {
      scale <- if (key == "job_sat") 1 else 100
      nv <- naive_class_mean(d_lca[[key]], modal4)*scale
      bv <- bch_class_mean(d_lca[[key]], bch$w)*scale
      tibble::tibble(variable = key, covariate = lab, class = paste0("C", 1:K),
        class_label = class_labels, naive = nv, bch = bv, difference = bv-nv,
        units = if (scale == 100) "percent; difference in percentage points" else "score points",
        weighting = "unweighted")
    })
    sizes_export <- dplyr::rename(table2$class_sizes, `n (modal)` = n_modal, `Weighted %` = weighted_percent)
    class_key <- tibble::tibble(Class = paste0("C", 1:K), label = class_labels,
      raw_glca_class = if (is.null(perm)) rep(NA_integer_, K) else perm,
      mapping_source = if (is.null(perm)) "Inherited verified canonical order; original permutation not saved" else "Item-profile canonicalization")
    variable_dictionary <- tibble::tibble(
      variable = c(vars$items, names(yes_levels), "psychotrop_flexible", cont_vars, ".w"),
      source_column = c(vars$items, unlist(vars[names(yes_levels)]), paste(psychotrop_items, collapse = "; "),
        unlist(vars[cont_vars]), vars$weight),
      coding = c(rep("Ordered levels 1-4; source 0-3 shifted +1 when detected", length(vars$items)),
        vapply(yes_levels, function(z) paste0("1 = ", paste(z, collapse = " or "), "; 0 = other observed levels"), character(1)),
        "1 = any item yes (code 2); 0 = all three no (code 1); NA otherwise (code 3 = no response)",
        rep("Numeric, original score units", length(cont_vars)), "Positive survey weight"))
    outputs <- list(
      "table1_enumeration.csv" = fit_tbl, "table3_itemprob.csv" = tab3,
      "table3_itemprob_long.csv" = dplyr::mutate(rho_long, class = as.character(class), cat = as.character(cat)),
      "table2_binary.csv" = t2_bin, "table2_full_13item.csv" = t2_full,
      "table2_class_sizes_13item.csv" = sizes_export,
      "table2_summary_numeric.csv" = table2$summary,
      "table2_tests_numeric.csv" = table2$tests,
      "table2_smd_pairwise.csv" = table2$smd_pairs,
      "table2_smd_max.csv" = table2_smd_max,
      "tableS1_or.csv" = s1, "tableS1_or_numeric.csv" = s1_numeric,
      "tableS2_margins.csv" = s2, "tableS2_margins_numeric.csv" = s2_numeric,
      "tableS3_sensitivity.csv" = s3, "tableS3_sensitivity_numeric.csv" = s3_numeric,
      "table_classification_diagnostics.csv" = class_diag,
      "sample_flow.csv" = sample_flow, "class_key.csv" = class_key,
      "variable_dictionary.csv" = variable_dictionary)
    for (file in names(outputs)) readr::write_csv(outputs[[file]], file, na = "NA")
    analysis_data <- as.data.frame(d_lca[, unique(c(".source_row", vars$items, names(yes_levels), "psychotrop_flexible", cont_vars, ".w")), drop = FALSE])
    source_files <- c("../01_general.qmd", "../R/leadership_table2.R",
      "../R/load_leadership_results.R", "../R/export_leadership_results.R")
    source_hashes <- tibble::tibble(file = source_files,
      sha256 = vapply(source_files, function(f) digest::digest(file = f, algo = "sha256"), character(1)))
    readr::write_csv(source_hashes, "source_manifest.csv")
    provenance <- list(render_started_utc = render_started_utc, source_hashes = source_hashes,
      measurement_action = if (reuse_measurement) "reused" else "refitted",
      measurement_validation = measurement_validation,
      prior_bundle_sha256 = if (reuse_measurement) digest::digest(file = "lca_results.rds", algo = "sha256") else NA_character_,
      input_basename = basename(raw_path),
      input_sha256 = digest::digest(file = raw_path, algo = "sha256"),
      source_sha256 = digest::digest(file = "../01_general.qmd", algo = "sha256"),
      survey_design = if (has("strata") && has("psu")) "PSU, strata and weights" else "weights only; ids=1",
      measurement_weighting = "unweighted likelihood; sex concomitant",
      table2_weighting = "survey weighted; modal class assignment",
      successful_seeds = if (!reuse_measurement) vapply(fits[2:Kmax], function(g) attr(g, "successful_seed"), integer(1)) else saved_measurement$provenance$successful_seeds,
      session = capture.output(sessionInfo()))
    bundle <- list(schema_version = "2.0.0", fit_tbl = fit_tbl, retained = 4L, n = n_lca,
      rho4 = rho, sizes4 = sizes, post4 = post4, modal4 = modal4,
      class_diag = class_diag, class_labels = class_labels, class_permutation = perm,
      item_order = vars$items, item_meta = item_meta, n_init = n_init, seed = 2125L, nboot = nboot,
      model_input_hash = model_input_hash, analysis_data = analysis_data,
      analysis_data_hash = digest::digest(analysis_data, algo = "sha256"),
      classification_matrix = class_matrix(post4, modal4),
      variable_dictionary = variable_dictionary, class_key = class_key,
      tables = outputs, table2 = table2, ml_models = ml_models, margins = margin_objects,
      provenance = provenance)
    # Versioned consumer validation occurs before replacing the existing file.
    temp_bundle <- tempfile(pattern = "lca_results_", tmpdir = ".", fileext = ".rds")
    saveRDS(bundle, temp_bundle)
    load_leadership_results(temp_bundle)
    if (!file.copy(temp_bundle, "lca_results.rds", overwrite = TRUE)) stop("Cannot write private results bundle.")
    unlink(temp_bundle)
    # Positive selection, never a subtraction list: no posterior rows or people.
    public <- bundle[c("schema_version", "fit_tbl", "retained", "n", "rho4", "sizes4",
      "class_diag", "class_labels", "item_order", "item_meta", "n_init", "seed", "nboot",
      "classification_matrix", "variable_dictionary", "class_key", "tables")]
    public$provenance <- provenance[setdiff(names(provenance), "prior_bundle_sha256")]
    saveRDS(public, "lca_results_public.rds")
    if (!reuse_measurement) {
      # PRIVATE full measurement fits and modal assignments support downstream
      # diagnostics without another estimation. Never publish these RDS files.
      saveRDS(list(fits = fits, model_input_hash = model_input_hash, provenance = provenance), "lca_measurement_fits.rds")
      saveRDS(list(modal_all = setNames(lapply(fits[2:Kmax], function(g) max.col(get_post(g), ties.method = "first")), 2:Kmax),
        logLik = setNames(fit_tbl$logLik, fit_tbl$Classes), n = n_lca, n_init = n_init,
        seed = 2125L, items = vars$items, source = "../01_general.qmd", date = Sys.Date()), "enumeration_modal_all.rds")
    }
    writeLines(c("Main report runtime", provenance$session), "session_info_main.txt")
    artifact_paths <- c(names(outputs), "lca_results.rds", "lca_results_public.rds",
      "Figure1_enumeration.png", "Figure2_itemprob.png", "session_info_main.txt", "source_manifest.csv")
    manifest <- tibble::tibble(file = artifact_paths,
      producer = "../01_general.qmd",
      generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      measurement_action = provenance$measurement_action,
      access = ifelse(artifact_paths == "lca_results.rds", "PRIVATE: participant-level", "aggregate/report"),
      sha256 = vapply(artifact_paths, function(f) digest::digest(file = f, algo = "sha256"), character(1)))
    readr::write_csv(manifest, "output_manifest.csv")
    cat("Exported", length(outputs), "CSV tables, private versioned results, public aggregate results, session info and SHA-256 manifest.\n")
  })
  invisible(NULL)
}
