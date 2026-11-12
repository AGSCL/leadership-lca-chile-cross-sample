bootstrap_rho <- function(model) {
  probabilities <- model$param$rho
  if (is.list(probabilities[[1L]])) probabilities <- probabilities[[1L]]
  do.call(cbind, probabilities)
}

align_bootstrap_classes <- function(reference, candidate) {
  stopifnot(is.matrix(reference), is.matrix(candidate),
            identical(dim(reference), dim(candidate)), nrow(reference) == 4L,
            all(is.finite(reference)), all(is.finite(candidate)))
  permutations <- as.matrix(expand.grid(rep(list(seq_len(4L)), 4L)))
  permutations <- permutations[apply(permutations, 1L, function(order)
    length(unique(order)) == 4L), , drop = FALSE]
  losses <- apply(permutations, 1L, function(order)
    sum((reference - candidate[order, , drop = FALSE])^2))
  ranking <- order(losses)
  list(order = as.integer(permutations[ranking[1L], ]),
       loss = losses[ranking[1L]], gap = losses[ranking[2L]] - losses[ranking[1L]])
}

fit_bootstrap_measurement <- function(data, reference_model, items, starts,
                                      warm_start = FALSE) {
  formula <- stats::as.formula(paste0("item(", paste(items, collapse = ","), ") ~ 1"),
                               env = asNamespace("glca"))
  candidates <- list()
  warnings <- character()
  fit_once <- function(fit_seed, initial = NULL) {
    tryCatch(withCallingHandlers(
      glca::glca(formula, group = setting, data = data, nclass = 4L,
        measure.inv = TRUE, n.init = starts, init.param = initial,
        std.err = FALSE, seed = fit_seed, maxiter = 5000L,
        eps = 1e-6, verbose = FALSE),
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }), error = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        NULL
      })
  }
  usable <- function(model) !is.null(model) &&
    isTRUE(model$convergence$converged) && is.finite(model$gof$loglik)
  if (warm_start) {
    warm <- fit_once(2125L, reference_model$param)
    if (usable(warm)) candidates$warm <- warm
  }
  for (fit_seed in 2125:2134) {
    random <- fit_once(fit_seed)
    if (usable(random)) {
      candidates$random <- random
      break
    }
  }
  if (!length(candidates)) stop("No converged measurement fit: ",
                                paste(unique(warnings), collapse = "; "))
  best <- which.max(vapply(candidates, function(model) model$gof$loglik, numeric(1)))
  list(model = candidates[[best]], selected = names(candidates)[best],
       random_seed = fit_seed, warnings = unique(warnings),
       loglik_candidates = vapply(candidates, function(model) model$gof$loglik, numeric(1)))
}

bootstrap_model_statistics <- function(data_by_sample, posterior, specifications,
                                      reference_class, include_margins = TRUE) {
  rows <- list()
  diagnostics <- list()
  for (specification in names(specifications)) {
    keys <- specifications[[specification]]
    fits <- list()
    for (sample in names(data_by_sample)) {
      frame <- data_by_sample[[sample]]
      predictors <- as.data.frame(frame[, keys, drop = FALSE])
      if ("job_sat" %in% keys) predictors$job_sat <- as.numeric(scale(predictors$job_sat))
      fits[[sample]] <- fit_adj(predictors, posterior[[sample]],
        max.col(posterior[[sample]], ties.method = "first"),
        sw = frame$.w, ref = reference_class, label = NULL)
      fitted <- fits[[sample]]
      if (fitted$conv != 0L || any(!is.finite(fitted$ors$b)))
        stop("Association fit failed: ", specification, "/", sample)
      effects <- fitted$ors
      rows[[length(rows) + 1L]] <- data.frame(specification, sample,
        estimand = "log_OR", covariate = effects$covariate,
        class = effects$class, exposure = NA_integer_, value = effects$b)
      diagnostics[[length(diagnostics) + 1L]] <- data.frame(specification, sample,
        n = fitted$n_complete, n_eff = fitted$n_eff, lambda = fitted$lambda,
        convergence = fitted$conv, weighted = fitted$weighted)
      if (include_margins && specification == "main") {
        design <- as.matrix(predictors[fitted$rows_complete, , drop = FALSE])
        coefficients <- matrix(fitted$par, nrow = ncol(design) + 1L)
        for (covariate in keys) {
          if (!all(design[, covariate] %in% c(0, 1))) next
          probabilities <- list()
          for (exposure in 0:1) {
            modified <- design
            modified[, covariate] <- exposure
            linear <- cbind(0, cbind(1, modified) %*% coefficients)
            linear <- linear - apply(linear, 1L, max)
            predicted <- exp(linear) / rowSums(exp(linear))
            averages <- colSums(predicted * fitted$weights_normalized) /
              sum(fitted$weights_normalized)
            probabilities[[exposure + 1L]] <- averages
            rows[[length(rows) + 1L]] <- data.frame(specification, sample,
              estimand = "probability", covariate, class = fitted$ord,
              exposure, value = averages)
          }
          rows[[length(rows) + 1L]] <- data.frame(specification, sample,
            estimand = "probability_difference", covariate, class = fitted$ord,
            exposure = NA_integer_, value = probabilities[[2L]] - probabilities[[1L]])
        }
      }
    }
    general <- fits$general$ors
    hospital <- fits$hospital$ors
    stopifnot(identical(general[, c("class", "covariate")],
                        hospital[, c("class", "covariate")]))
    rows[[length(rows) + 1L]] <- data.frame(specification,
      sample = "hospital/general", estimand = "log_ROR",
      covariate = general$covariate, class = general$class,
      exposure = NA_integer_, value = hospital$b - general$b)
  }
  list(estimates = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

bootstrap_one <- function(replicate_id, input, reference_model, configuration,
                          cache_dir) {
  destination <- file.path(cache_dir, sprintf("replicate_%04d.rds", replicate_id))
  previous <- if (file.exists(destination))
    tryCatch(readRDS(destination), error = function(condition) NULL) else NULL
  if (!is.null(previous)) {
    stopifnot(identical(previous$signature, configuration$signature),
              previous$replicate == replicate_id)
    return(previous)
  }
  start_time <- Sys.time()
  result <- tryCatch({
    RNGkind("Mersenne-Twister")
    set.seed(configuration$resampling_seed + replicate_id)
    sampled <- lapply(input, function(frame)
      frame[sample.int(nrow(frame), nrow(frame), replace = TRUE), , drop = FALSE])
    pooled <- do.call(rbind, lapply(seq_along(sampled), function(sample_index) {
      frame <- sampled[[sample_index]][, configuration$items, drop = FALSE]
      frame$setting <- factor(configuration$group_levels[sample_index],
                               levels = configuration$group_levels)
      frame
    }))
    rownames(pooled) <- NULL
    measurement <- fit_bootstrap_measurement(pooled, reference_model,
      configuration$items, configuration$n_init, configuration$warm_start)
    model <- measurement$model
    alignment <- align_bootstrap_classes(bootstrap_rho(reference_model), bootstrap_rho(model))
    if (alignment$gap <= 1e-8) stop("Ambiguous class alignment")
    posterior <- lapply(model$posterior[configuration$group_levels], function(probabilities)
      as.matrix(probabilities)[, alignment$order, drop = FALSE])
    names(posterior) <- names(input)
    for (sample_index in seq_along(sampled)) {
      stopifnot(isTRUE(all.equal(unname(data.matrix(sampled[[sample_index]][,
        configuration$items, drop = FALSE])),
        unname(model$datalist$y[[sample_index]]), check.attributes = FALSE)),
        nrow(posterior[[sample_index]]) == nrow(sampled[[sample_index]]),
        all(is.finite(posterior[[sample_index]])),
        all(abs(rowSums(posterior[[sample_index]]) - 1) < 1e-8))
    }
    statistics <- bootstrap_model_statistics(sampled, posterior,
      configuration$specifications, configuration$reference_class)
    list(status = "ok", estimates = statistics$estimates,
      diagnostics = statistics$diagnostics, alignment = alignment,
      loglik = model$gof$loglik, niter = model$convergence$niter,
      measurement_seed = measurement$random_seed,
      selected_start = measurement$selected,
      loglik_candidates = measurement$loglik_candidates,
      warnings = measurement$warnings)
  }, error = function(condition) list(status = "failed", error = conditionMessage(condition)))
  result$replicate <- replicate_id
  result$signature <- configuration$signature
  result$elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  saveRDS(result, destination, compress = "gzip")
  result
}

run_shared_bootstrap <- function(input, reference_model, specifications,
                                 reference_class, replicates = 1000L, starts = 4000L,
                                 workers = 6L, warm_start = FALSE,
                                 output_dir = "bootstrap_shared") {
  stopifnot(identical(names(input), c("general", "hospital")),
            all(c("main", "legacy_jobsat", "legacy_nojobsat") %in% names(specifications)),
            replicates >= 2L, replicates == as.integer(replicates),
            starts >= 1L, starts == as.integer(starts), workers >= 1L)
  items <- reference_model$var.names$y.names
  for (sample_index in seq_along(input)) {
    stopifnot(all(c(items, ".w", unique(unlist(specifications))) %in%
                    names(input[[sample_index]])),
      isTRUE(all.equal(unname(data.matrix(input[[sample_index]][, items, drop = FALSE])),
        unname(reference_model$datalist$y[[sample_index]]), check.attributes = FALSE)),
      all(is.finite(input[[sample_index]]$.w)), all(input[[sample_index]]$.w > 0))
  }
  stopifnot(all(input$hospital$.w == 1))
  configuration <- list(items = items, group_levels = reference_model$var.names$g.names,
    specifications = specifications, reference_class = reference_class,
    n_init = as.integer(starts), warm_start = warm_start, resampling_seed = 2125L,
    measurement_weighting = "unweighted", structural_weighting = "general final weights; hospital 1",
    resampling = "uniform iid participants within setting; final weights carried with rows")
  source_files <- c("../R/bootstrap_shared.R", "../R/association_models.R")
  source_hash <- setNames(vapply(source_files, function(path)
    digest::digest(file = path, algo = "sha256"), character(1)), source_files)
  configuration$signature <- digest::digest(list(configuration,
    input = lapply(input, function(frame) as.data.frame(frame[, unique(c(items,
      ".w", unlist(specifications))), drop = FALSE])),
    parameters = reference_model$param, source_hash = source_hash), algo = "sha256")
  cache_dir <- file.path(output_dir, substr(configuration$signature, 1L, 16L))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
  posterior <- lapply(reference_model$posterior, as.matrix)
  posterior <- posterior[configuration$group_levels]
  names(posterior) <- names(input)
  point <- bootstrap_model_statistics(input, posterior, specifications, reference_class)
  available <- parallel::detectCores(logical = FALSE)
  if (is.na(available)) available <- 1L
  workers <- as.integer(min(workers, available, replicates))
  previous_plan <- future::plan()
  on.exit(future::plan(previous_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)
  results <- vector("list", replicates)
  batches <- split(seq_len(replicates), ceiling(seq_len(replicates) / max(1L, workers * 2L)))
  for (batch in batches) {
    results[batch] <- furrr::future_map(batch, bootstrap_one, input = input,
      reference_model = reference_model, configuration = configuration, cache_dir = cache_dir,
      .options = furrr::furrr_options(seed = TRUE, packages = c("glca", "digest"),
                                      scheduling = Inf))
    status <- vapply(results[seq_len(max(batch))], function(result) result$status, character(1))
    cat(sprintf("Bootstrap %d/%d; valid=%d; failed=%d; %s\n", max(batch), replicates,
      sum(status == "ok"), sum(status != "ok"), format(Sys.time(), "%H:%M:%S")))
    flush.console()
    if (all(vapply(results[batch], function(result) result$status != "ok", logical(1))))
      stop("All replicates in a batch failed; inspect ", cache_dir)
  }
  valid <- vapply(results, function(result) result$status == "ok", logical(1))
  draws <- if (any(valid)) do.call(cbind, lapply(results[valid], function(result) {
    stopifnot(identical(result$estimates[, setdiff(names(point$estimates), "value")],
                       point$estimates[, setdiff(names(point$estimates), "value")]))
    result$estimates$value
  })) else matrix(NA_real_, nrow(point$estimates), 0L)
  summary <- point$estimates
  summary$n_valid <- sum(valid)
  summary$bootstrap_se <- if (sum(valid) >= 2L) apply(draws, 1L, stats::sd) else NA_real_
  summary$lower <- if (sum(valid) >= 2L) apply(draws, 1L, stats::quantile, probs = .025) else NA_real_
  summary$upper <- if (sum(valid) >= 2L) apply(draws, 1L, stats::quantile, probs = .975) else NA_real_
  logarithmic <- summary$estimand %in% c("log_OR", "log_ROR")
  summary$estimate <- summary$value
  summary$estimate[logarithmic] <- exp(summary$value[logarithmic])
  summary$lower[logarithmic] <- exp(summary$lower[logarithmic])
  summary$upper[logarithmic] <- exp(summary$upper[logarithmic])
  summary$p_wald_bootstrap <- NA_real_
  ror <- summary$estimand == "log_ROR"
  summary$p_wald_bootstrap[ror] <- 2 * stats::pnorm(-abs(summary$value[ror] /
                                                        summary$bootstrap_se[ror]))
  summary$p_holm <- NA_real_
  for (specification in names(specifications)) {
    family <- ror & summary$specification == specification
    summary$p_holm[family] <- stats::p.adjust(summary$p_wald_bootstrap[family], method = "holm")
  }
  list(configuration = configuration, requested = replicates, valid = sum(valid),
    failed = sum(!valid), status = if (all(valid) && replicates == 1000L) "complete" else "draft_or_incomplete",
    point = point, summary = summary, draws = draws, replicates = results,
    cache_dir = cache_dir, source_hash = source_hash, created = Sys.time(), session = sessionInfo())
}

check_bootstrap_shared <- function() {
  reference <- matrix(seq_len(64L) / 64, nrow = 4L)
  permutation <- c(3L, 1L, 4L, 2L)
  aligned <- align_bootstrap_classes(reference, reference[permutation, ])
  stopifnot(identical(reference, reference[permutation, ][aligned$order, ]), aligned$loss == 0)
  general <- c(-.3, .2, .4, 1.1)
  hospital <- c(.2, .6, .9, 1.2)
  stopifnot(abs(stats::var(hospital - general) -
    (stats::var(hospital) + stats::var(general) - 2 * stats::cov(general, hospital))) < 1e-12,
    stats::var(hospital - general) < stats::var(hospital) + stats::var(general))
  cat("Bootstrap label alignment and paired covariance identity: OK\n")
  invisible(TRUE)
}
