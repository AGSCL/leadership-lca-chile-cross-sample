# PRIVATE consumer interface. Do not commit the input bundle: it contains people.
load_leadership_results <- function(path = "lca_results.rds") {
  x <- readRDS(path)
  required <- c("schema_version", "analysis_data", "analysis_data_hash", "post4", "modal4", "item_order", "class_labels")
  if (!all(required %in% names(x))) stop("Render the updated main QMD to create the versioned export.")
  if (x$schema_version != "2.0.0") stop("Unsupported results schema: ", x$schema_version)
  if (!identical(digest::digest(x$analysis_data, algo = "sha256"), x$analysis_data_hash))
    stop("The saved analysis frame does not match its fingerprint.")
  stopifnot(".source_row" %in% names(x$analysis_data),
            !anyNA(x$analysis_data$.source_row),
            all(is.finite(x$post4)), all(x$post4 >= 0 & x$post4 <= 1),
            length(x$class_labels) == ncol(x$post4),
            !anyNA(x$class_labels), all(nzchar(x$class_labels)),
            !anyDuplicated(x$class_labels))
  stopifnot(nrow(x$analysis_data) == nrow(x$post4), length(x$modal4) == nrow(x$post4),
            !anyDuplicated(x$analysis_data$.source_row),
            all(abs(rowSums(x$post4) - 1) < 1e-8),
            all(x$modal4 == max.col(x$post4, ties.method = "first")))
  x
}

validate_saved_measurement <- function(x, data, items, concomitant = "sex_woman") {
  input <- data[, c(items, concomitant), drop = FALSE]
  fingerprint <- digest::digest(input, algo = "sha256")
  stopifnot(identical(x$item_order, items), nrow(x$post4) == nrow(data),
            x$retained == 4L, x$n_init == 4000, x$seed == 2125L,
            all(is.finite(x$post4)), all(x$post4 >= 0),
            all(abs(rowSums(x$post4) - 1) < 1e-8))
  if (!is.null(x$model_input_hash)) {
    if (!identical(fingerprint, x$model_input_hash))
      stop("Measurement inputs or row order changed. Set LCA_REFIT=1 to estimate from these inputs.")
    return("Matched SHA-256 fingerprint of ordered measurement inputs.")
  }
  # One-time migration of the historical bundle, which has no row identifiers.
  # Bayes' identity: posterior odds / item-likelihood odds must depend ONLY
  # on sex, the sole concomitant. This checks every row against its item pattern.
  loglik <- matrix(0, nrow(data), ncol(x$post4))
  for (j in seq_along(items)) {
    r <- x$rho4[[j]]
    if (any(r <= 0)) stop("Legacy posterior validation needs positive item probabilities; refit explicitly.")
    loglik <- loglik + t(log(r[, as.integer(data[[items[j]]]), drop = FALSE]))
  }
  if (any(x$post4 <= 0)) stop("Legacy posterior validation needs positive posteriors; refit explicitly.")
  prior_logodds <- sweep(log(x$post4), 1, log(x$post4[,1]), "-") - sweep(loglik, 1, loglik[,1], "-")
  reconstructed <- matrix(NA_real_, nrow(data), ncol(x$post4))
  for (s in unique(data[[concomitant]])) {
    idx <- data[[concomitant]] == s
    z <- prior_logodds[idx, , drop = FALSE]
    # Near-zero probabilities amplify final-EM tolerance on the log-odds scale.
    eta <- sweep(loglik[idx, , drop = FALSE], 2, apply(z, 2, median), "+")
    e <- exp(sweep(eta, 1, apply(eta, 1, max), "-"))
    reconstructed[idx, ] <- e/rowSums(e)
  }
  residual <- max(abs(reconstructed - x$post4))
  if (!is.finite(residual) || residual > 1e-5 ||
      any(max.col(reconstructed, ties.method = "first") != x$modal4))
    stop("Legacy posterior/item alignment could not be verified. Set LCA_REFIT=1.")
  sprintf("Legacy migration: row-wise posterior reconstruction within EM tolerance (max absolute difference %.3g); all modal assignments matched. Identical response/sex patterns cannot identify unique people.", residual)
}
