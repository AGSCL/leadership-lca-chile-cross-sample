arguments <- commandArgs(trailingOnly = TRUE)
sources_only <- "--sources" %in% arguments
without_bootstrap <- "--without-bootstrap" %in% arguments
audit06_only <- "--audit06" %in% arguments
notebooks <- c("01_general.qmd", "02_hospital.qmd", "03_asociaciones.qmd",
               "04_sensibilidad_figuras.qmd", "05_bootstrap.qmd",
               "06_multiplicidad_solapamiento.qmd")
stopifnot(all(file.exists(notebooks)))
for (notebook in notebooks) {
  notebook_lines <- readLines(notebook, warn = FALSE, encoding = "UTF-8")
  labels <- grep("^#\\| label:", notebook_lines, value = TRUE)
  stopifnot(!anyDuplicated(labels))
  script <- tempfile(fileext = ".R")
  knitr::purl(notebook, output = script, documentation = 0L, quiet = TRUE)
  parse(file = script, encoding = "UTF-8")
  unlink(script)
  cat(notebook, ": syntax and", length(labels), "unique chunks OK\n")
}
helpers <- list.files("R", pattern = "[.]R$", full.names = TRUE)
for (helper in helpers) parse(file = helper, encoding = "UTF-8")
cat(length(helpers), "local R helpers: syntax OK\n")
source("R/association_models.R")
source("R/bootstrap_shared.R")
check_association_models()
check_bootstrap_shared()
sensitivity_lines <- readLines("04_sensibilidad_figuras.qmd", encoding = "UTF-8")
caption_start <- match("#| label: pub-captions", sensitivity_lines)
caption_end <- which(seq_along(sensitivity_lines) > caption_start & sensitivity_lines == "```")[1]
stopifnot(!is.na(caption_start), !is.na(caption_end))
caption_env <- list2env(list(lca = list(n = 1847L), group_sizes = c(1850L, 972L)),
                       parent = baseenv())
caption_output <- paste(capture.output(eval(parse(text = sensitivity_lines[
  seq.int(caption_start + 1L, caption_end - 1L)]), envir = caption_env)), collapse = "\n")
stopifnot(grepl("Chilean salaried workers (n= 1,847)", caption_output, fixed = TRUE),
          grepl("pooled n= 2,822", caption_output, fixed = TRUE),
          grepl("general salaried workers (n= 1,850)", caption_output, fixed = TRUE),
          grepl("hospital healthcare workers (n= 972)", caption_output, fixed = TRUE))
stopifnot(grepl("AWE= approximate weight of evidence.", caption_output, fixed = TRUE),
          grepl("AIC, SABIC at ten classes, BIC at eight, CAIC at seven, AWE at five",
                caption_output, fixed = TRUE))
cat("Caption sample sizes remain distinct when denominators differ: OK\n")
if (sources_only) quit(status = 0L)

if (audit06_only) {
  audit_tables <- file.path("salidas", paste0("06_", c("ror", "omnibus",
    "probabilidades", "geografia", "salud", "restricciones", "procedencia"), ".csv"))
  audit_rds <- "salidas/06_auditoria.rds"
  audit_report <- "informes/06_multiplicidad_solapamiento.html"
  audit_outputs <- c(audit_tables, audit_rds, "salidas/06_ror.png")
  stopifnot(all(file.exists(c(audit_outputs, audit_report))),
            all(file.info(c(audit_outputs, audit_report))$size > 0))
  audit <- readRDS(audit_rds)
  stopifnot(is.list(audit), identical(audit$status, "audit_only_no_refit"))
  tables06 <- setNames(lapply(audit_tables, read.csv, check.names = FALSE, encoding = "UTF-8"),
    c("ror", "omnibus", "probabilities", "geography", "health", "restrictions", "provenance"))
  stopifnot(all(vapply(tables06, nrow, integer(1)) > 0L))
  ror06 <- tables06$ror
  omnibus06 <- tables06$omnibus
  stopifnot(all(c("covariate", "class", "p_wald_bootstrap", "q_bh") %in% names(ror06)),
            all(c("covariate", "statistic", "df", "p_value", "q_bh",
                  "min_eigenvalue") %in% names(omnibus06)),
            all(c("role", "file", "sha256", "size_bytes") %in% names(tables06$provenance)),
            nrow(ror06) == 24L, nrow(omnibus06) == 8L,
            nrow(tables06$probabilities) == 8L,
            !anyDuplicated(ror06[, c("covariate", "class")]),
            !anyDuplicated(omnibus06$covariate),
            all(is.finite(ror06$p_wald_bootstrap)),
            all(ror06$p_wald_bootstrap >= 0 & ror06$p_wald_bootstrap <= 1),
            max(abs(ror06$q_bh - p.adjust(ror06$p_wald_bootstrap, "BH"))) < 1e-12,
            all(omnibus06$df == 3L), all(omnibus06$min_eigenvalue > 0),
            all(is.finite(omnibus06$statistic)),
            max(abs(omnibus06$p_value - pchisq(omnibus06$statistic,
              omnibus06$df, lower.tail = FALSE))) < 1e-12,
            max(abs(omnibus06$q_bh - p.adjust(omnibus06$p_value, "BH"))) < 1e-12)
  for (name in names(tables06))
    stopifnot(isTRUE(all.equal(audit[[name]], tables06[[name]],
                              check.attributes = FALSE, tolerance = 1e-12)))
  audit_sources <- c("06_multiplicidad_solapamiento.qmd",
    "salidas/02_hospital.RData", "salidas/03_asociaciones.RData",
    "salidas/05_bootstrap.RData", "salidas/bootstrap_associations.csv")
  provenance_rows <- match(basename(audit_sources), tables06$provenance$file)
  stopifnot(all(file.exists(audit_sources)), !anyNA(provenance_rows),
            all(tables06$provenance$sha256[provenance_rows] == vapply(audit_sources,
              function(path) digest::digest(file = path, algo = "sha256"), character(1))),
            file.mtime(audit_report) >= max(file.mtime(c(audit_outputs,
              "06_multiplicidad_solapamiento.qmd", "_quarto.yml"))))
  cat("06 audit: 24 RORs, 8 exploratory omnibus tests, aggregate exports, source hashes and HTML checked.\n",
      "AUDIT ONLY: no model refits or new certification of stages 01-05.\n", sep = "")
  quit(status = 0L)
}

read_checkpoint <- function(file, object) {
  checkpoint_env <- new.env(parent = emptyenv())
  stopifnot(identical(load(file, envir = checkpoint_env), object))
  checkpoint_env[[object]]
}
verify_sources <- function(source_hashes, algorithm) {
  source_files <- file.path("salidas", names(source_hashes))
  stopifnot(all(file.exists(source_files)))
  current_hashes <- vapply(source_files, function(source_file)
    digest::digest(file = source_file, algo = algorithm), character(1))
  stopifnot(identical(unname(source_hashes), unname(current_hashes)))
}
source("R/load_leadership_results.R")
general <- read_checkpoint("salidas/01_general.RData", "general")
validated <- load_leadership_results("salidas/lca_results.rds")
verify_sources(setNames(general$provenance$source_hashes$sha256,
                        general$provenance$source_hashes$file), "sha256")
stopifnot(identical(general, validated), general$n_init == 4000,
          length(general$item_order) == 13L, !"lid_des3" %in% general$item_order,
          all(vapply(general$analysis_data[general$item_order], is.ordered, logical(1))))
hospital <- read_checkpoint("salidas/02_hospital.RData", "hospital")
verify_sources(hospital$provenance$code_hash, "md5")
stopifnot(hospital$n_init == 4000L, hospital$seed == 2125L,
          length(hospital$items) == 13L,
          identical(hospital$provenance$model_input_hash,
                    digest::digest(hospital$input, algo = "sha256")))
stopifnot(all(vapply(hospital$input[hospital$items], is.ordered, logical(1))))
for (setting in hospital$group_levels) {
  posterior <- as.matrix(hospital$g4_inv$posterior[[setting]])
  stopifnot(nrow(posterior) == sum(hospital$input$setting == setting),
            all(is.finite(posterior)), all(posterior >= 0 & posterior <= 1),
            all(abs(rowSums(posterior) - 1) < 1e-8))
}
asociaciones <- read_checkpoint("salidas/03_asociaciones.RData", "asociaciones")
verify_sources(asociaciones$provenance$code_hash, "md5")
stopifnot(identical(asociaciones$measurement$hash,
                    unname(tools::md5sum("salidas/02_hospital.RData"))),
          nrow(asociaciones$tables$unadjusted_ror) == 24L,
          nrow(asociaciones$tables$adjusted_ror) == 24L,
          nrow(asociaciones$tables$without_job_satisfaction) == 21L,
          nrow(asociaciones$tables$main_ror) == 24L,
          nrow(asociaciones$tables$main_unadjusted_ror) == 24L,
          all(asociaciones$tables$diagnostics$convergence == 0),
          all(asociaciones$tables$diagnostics$lambda %in% c(0, 0.1)),
          all(asociaciones$tables$diagnostics$weighted ==
                (asociaciones$tables$diagnostics$sample == "general")),
          "psychotrop_flexible" %in% asociaciones$specifications$main,
          !"job_sat" %in% asociaciones$specifications$main,
          all(asociaciones$covariates$hospital$.w == 1))
for (sample in c("general", "hospital")) {
  fit <- asociaciones$models$main[[sample]]
  stopifnot(identical(fit$weights_original,
                     as.numeric(asociaciones$covariates[[sample]]$.w[fit$rows_complete])),
            abs(mean(fit$weights_normalized) - 1) < 1e-12,
            all(na.omit(asociaciones$covariates[[sample]]$psychotrop_flexible) %in% c(0L, 1L)))
}
sensibilidad <- read_checkpoint("salidas/04_sensibilidad.RData", "sensibilidad")
stopifnot(identical(sensibilidad$status, "complete"),
          identical(sensibilidad$code_hash,
                    digest::digest(file = "04_sensibilidad_figuras.qmd", algo = "sha256")),
          identical(sensibilidad$source_general_hash,
                    digest::digest(file = "salidas/01_general.RData", algo = "sha256")),
          identical(sensibilidad$input_hash,
                    digest::digest(general$analysis_data[, c(general$item_order, "sex_woman")],
                                   algo = "sha256")),
          identical(sensibilidad$hospital_hash,
                    digest::digest(file = "salidas/02_hospital.RData", algo = "sha256")),
          all(file.exists(file.path("salidas", sensibilidad$figure_files))),
          nrow(sensibilidad$tables$profiles8) == 8L,
          nrow(sensibilidad$tables$profiles5) == 5L,
          nrow(sensibilidad$enumeration) == 9L)
check_k <- c(4L, 5L, 8L)
check_loglik <- vapply(sensibilidad$fits, function(model) model$gof$loglik, numeric(1))
stopifnot(all(abs(check_loglik - general$fit_tbl$logLik[
  match(check_k, general$fit_tbl$Classes)]) < 1e-3))
enumeration <- sensibilidad$enumeration
criteria <- c("AIC", "BIC", "CAIC", "SABIC", "AWE")
minima <- vapply(criteria, function(criterion)
  as.integer(enumeration$Classes[which.min(enumeration[[criterion]])]), integer(1))
stopifnot(identical(unname(minima), c(10L, 8L, 7L, 10L, 5L)),
          all(is.finite(as.matrix(enumeration[, c("Entropy", "Mean AvePP")]))),
          identical(is.na(enumeration$ari_prev), enumeration$Classes == 2L),
          all(abs(na.omit(enumeration$ari_prev)) <= 1))
cat("Figure 1: five criterion minima, entropy, AvePP and adjacent-solution ARI checked.\n")
checked_notebooks <- if (without_bootstrap) notebooks[1:4] else notebooks[1:5]
reports <- file.path("informes", sub("[.]qmd$", ".html", checked_notebooks))
checkpoints <- file.path("salidas", c("01_general.RData", "02_hospital.RData",
                                     "03_asociaciones.RData", "04_sensibilidad.RData"))
if (!without_bootstrap) {
  bootstrap <- read_checkpoint("salidas/05_bootstrap.RData", "bootstrap")
  verify_sources(bootstrap$source_hash, "sha256")
  stopifnot(bootstrap$requested == 250L, bootstrap$valid == 250L,
            bootstrap$failed == 0L, identical(bootstrap$status, "complete"),
            bootstrap$configuration$n_init == 4000L,
            !bootstrap$configuration$warm_start,
            identical(bootstrap$association_hash,
                      unname(tools::md5sum("salidas/03_asociaciones.RData"))),
            ncol(bootstrap$draws) == 250L, all(is.finite(bootstrap$draws)),
            all(bootstrap$summary$n_valid == 250L))
  checkpoints <- c(checkpoints, "salidas/05_bootstrap.RData")
}
stopifnot(all(file.exists(reports)))
report_sources <- pmax(file.mtime(checked_notebooks), file.mtime(checkpoints),
                      file.mtime("_quarto.yml"))
stale_reports <- reports[file.mtime(reports) < report_sources]
if (length(stale_reports))
  stop("HTML anterior a sus fuentes o checkpoint; repetir render: ",
       paste(stale_reports, collapse = ", "))
cat("Checked checkpoints, posterior alignment, weighted main/sensitivity RORs,\n",
    "sensitivity likelihoods, figures and corresponding HTML reports: OK\n", sep = "")
if (without_bootstrap) cat("PARTIAL CHECK ONLY: B=250 bootstrap was not certified.\n")

baseline_files <- c("table1_enumeration.csv", "table2_summary_numeric.csv",
                    "table2_tests_numeric.csv", "tableS1_or_numeric.csv",
                    "tableS2_margins_numeric.csv", "tableS3_sensitivity_numeric.csv")
if (all(file.exists(file.path("..", baseline_files)))) {
  for (filename in baseline_files) {
    previous <- read.csv(file.path("..", filename), check.names = FALSE)
    current <- read.csv(file.path("salidas", filename), check.names = FALSE)
    # 2026-10-18: psychotropic rows were added after the baseline; compare its rows only.
    key <- intersect(c("variable", "class"), names(previous))
    if (length(key)) {
      rows <- match(do.call(paste, previous[key]), do.call(paste, current[key]))
      stopifnot(!anyNA(rows))
      current <- current[rows, , drop = FALSE]
      rownames(current) <- NULL
    }
    comparable <- setdiff(names(previous), "class_label")
    stopifnot(identical(names(previous), names(current)),
              isTRUE(all.equal(previous[, comparable, drop = FALSE],
                               current[, comparable, drop = FALSE], tolerance = 1e-8)))
  }
  cat("Six general exports agree with baseline apart from approved presentation labels.\n")
}
cat("Verification completed at", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
