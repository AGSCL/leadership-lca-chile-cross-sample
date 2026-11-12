derive_psychotrop_flexible <- function(data, columns, yes, no, missing_codes = numeric()) {
  stopifnot(length(columns) > 0L, !anyDuplicated(columns), length(yes) == 1L,
            length(no) == 1L, yes != no, !any(c(yes, no) %in% missing_codes))
  if (!all(columns %in% names(data)))
    stop("Missing psychotropic item columns: ", paste(setdiff(columns, names(data)), collapse = ", "))
  coded <- lapply(columns, function(column) {
    original <- data[[column]]
    values <- suppressWarnings(if (is.factor(original)) as.numeric(as.character(original)) else as.numeric(original))
    if (any(!is.na(original) & is.na(values)) ||
        any(!is.na(values) & !values %in% c(yes, no, missing_codes)))
      stop("Unexpected numeric code in psychotropic item: ", column)
    values[values %in% missing_codes] <- NA_real_
    values
  })
  coded <- do.call(cbind, coded)
  result <- rep(NA_integer_, nrow(data))
  result[rowSums(coded == no, na.rm = TRUE) == length(columns)] <- 0L
  result[rowSums(coded == yes, na.rm = TRUE) > 0L] <- 1L
  result
}

class_matrix <- function(post, modal) {
  post <- as.matrix(post)
  classes <- ncol(post)
  stopifnot(length(modal) == nrow(post), all(modal %in% seq_len(classes)),
            all(is.finite(post)), all(post >= 0), all(abs(rowSums(post) - 1) < 1e-8))
  expected_counts <- crossprod(post, outer(modal, seq_len(classes), `==`) * 1)
  if (any(rowSums(expected_counts) <= 0)) stop("Empty latent class in classification matrix.")
  expected_counts / rowSums(expected_counts)
}

weighted_classification_objective <- function(parameters, design, correction, weights, lambda = 0) {
  classes <- ncol(correction)
  coefficients <- matrix(parameters, nrow = ncol(design), ncol = classes - 1L)
  linear_predictors <- cbind(0, design %*% coefficients)
  linear_predictors <- linear_predictors - apply(linear_predictors, 1L, max)
  probabilities <- exp(linear_predictors)
  probabilities <- probabilities / rowSums(probabilities)
  observed_probability <- rowSums(probabilities * correction)
  conditional_class <- probabilities * correction / pmax(observed_probability, 1e-12)
  residuals <- (probabilities - conditional_class) * weights * (observed_probability > 1e-12)
  list(value = -sum(weights * log(pmax(observed_probability, 1e-12))) + lambda * sum(parameters^2),
       gradient = as.vector(crossprod(design, residuals[, -1L, drop = FALSE])) + 2 * lambda * parameters)
}

ml3step_multi <- function(Z, post, modal, sw, D = NULL, ref = 1L, lambda = 0) {
  post <- as.matrix(post)
  Z <- as.matrix(Z)
  classes <- ncol(post)
  ref <- as.integer(ref)
  if (!is.numeric(Z) || nrow(Z) != nrow(post) || length(modal) != nrow(post) ||
      length(sw) != nrow(post)) stop("Predictor, posterior, modal and weight rows must align.")
  if (!ncol(Z) || any(!is.na(Z) & !is.finite(Z))) stop("Predictors must be finite numeric values or NA.")
  if (any(!is.finite(sw)) || any(sw <= 0)) stop("All sampling weights must be finite and positive.")
  stopifnot(length(ref) == 1L, ref %in% seq_len(classes), lambda >= 0, is.finite(lambda))
  stopifnot(all(modal %in% seq_len(classes)), all(is.finite(post)), all(post >= 0),
            all(abs(rowSums(post) - 1) < 1e-8))
  if (is.null(colnames(Z))) colnames(Z) <- paste0("covariate_", seq_len(ncol(Z)))
  stopifnot(!anyDuplicated(colnames(Z)))
  order <- c(ref, setdiff(seq_len(classes), ref))
  if (is.null(D)) D <- class_matrix(post, modal)
  stopifnot(identical(dim(D), c(classes, classes)), all(is.finite(D)), all(D >= 0),
            all(abs(rowSums(D) - 1) < 1e-8))
  D <- D[order, order, drop = FALSE]
  complete <- complete.cases(Z)
  if (!any(complete)) stop("No complete cases for this association specification.")
  predictors <- Z[complete, , drop = FALSE]
  design <- cbind(`(Intercept)` = 1, predictors)
  modal_complete <- match(modal[complete], order)
  weights_original <- as.numeric(sw[complete])
  weight_scale <- mean(weights_original)
  weights <- weights_original / weight_scale
  correction <- t(D[, modal_complete, drop = FALSE])
  parameter_count <- ncol(design) * (classes - 1L)
  objective <- function(parameters)
    weighted_classification_objective(parameters, design, correction, weights, lambda)
  fitted <- stats::optim(rep(0, parameter_count), function(parameters) objective(parameters)$value,
                        gr = function(parameters) objective(parameters)$gradient,
                        method = "BFGS", hessian = TRUE)
  variance <- tryCatch(solve(fitted$hessian),
                       error = function(condition) matrix(NA_real_, parameter_count, parameter_count))
  standard_errors <- suppressWarnings(sqrt(diag(variance)))
  odds_ratios <- do.call(rbind, lapply(seq_len(classes - 1L), function(class_index) {
    parameter_indices <- (class_index - 1L) * ncol(design) + 1L + seq_len(ncol(predictors))
    coefficients <- fitted$par[parameter_indices]
    errors <- standard_errors[parameter_indices]
    data.frame(class = order[class_index + 1L], covariate = colnames(predictors),
               OR = exp(coefficients), lo = exp(coefficients - 1.96 * errors),
               hi = exp(coefficients + 1.96 * errors), b = coefficients, se = errors)
  }))
  list(ors = odds_ratios, par = fitted$par, V = variance, ref = ref, ord = order,
       covariate_names = colnames(predictors), conv = fitted$convergence, lambda = lambda,
       n_complete = sum(complete), n_total = nrow(Z), rows_complete = which(complete),
       weights_normalized = weights, weights_original = weights_original,
       weight_scale = weight_scale, weight_sum = sum(weights_original),
       n_eff = sum(weights_original)^2 / sum(weights_original^2),
       weighted = any(abs(weights - 1) > sqrt(.Machine$double.eps)),
       objective = fitted$value, classification_matrix = D)
}

fit_adj <- function(Z, post, modal, sw, ref, label = NULL, D = NULL) {
  fitted <- ml3step_multi(Z, post, modal, sw = sw, D = D, ref = ref, lambda = 0)
  needs_ridge <- fitted$conv != 0 || any(!is.finite(fitted$ors$se)) ||
    any(abs(fitted$ors$b) > 5, na.rm = TRUE)
  initial_convergence <- fitted$conv
  unpenalized <- list(ors = fitted$ors, par = fitted$par, objective = fitted$objective,
    conv = fitted$conv, max_abs_b = max(abs(fitted$ors$b), na.rm = TRUE),
    max_se = if (all(is.na(fitted$ors$se))) NA_real_ else max(fitted$ors$se, na.rm = TRUE))
  ridge_reason <- paste(c(if (fitted$conv != 0) "nonconvergence",
    if (any(!is.finite(fitted$ors$se))) "nonfinite_se",
    if (any(abs(fitted$ors$b) > 5, na.rm = TRUE)) "abs_beta_gt_5"), collapse = ";")
  if (needs_ridge)
    fitted <- ml3step_multi(Z, post, modal, sw = sw, D = D, ref = ref, lambda = 0.1)
  fitted$ridge_triggered <- needs_ridge
  fitted$ridge_reason <- if (needs_ridge) ridge_reason else "none"
  fitted$unpenalized <- unpenalized
  fitted$unpenalized_convergence <- initial_convergence
  if (!is.null(label)) message(sprintf("%s: n_complete=%d/%d, effective_n=%.1f, lambda=%.1f, conv=%d",
    label, fitted$n_complete, fitted$n_total, fitted$n_eff, fitted$lambda, fitted$conv))
  fitted
}

check_association_models <- function() {
  fixture <- data.frame(first = c(2, 1, 1, 3, 2), second = c(3, 1, 3, 3, 1),
                        third = c(1, 1, 1, 3, 3))
  expected <- c(1L, 0L, NA_integer_, NA_integer_, 1L)
  stopifnot(identical(derive_psychotrop_flexible(fixture, names(fixture), 2, 1, 3), expected))
  hospital_fixture <- fixture
  hospital_fixture[] <- lapply(hospital_fixture, function(values) ifelse(values == 3, NA, values - 1))
  stopifnot(identical(derive_psychotrop_flexible(hospital_fixture, names(fixture), 1, 0), expected))
  invalid_fixture <- fixture
  invalid_fixture$first[1L] <- 9
  stopifnot(inherits(try(derive_psychotrop_flexible(invalid_fixture, names(fixture), 2, 1, 3),
                        silent = TRUE), "try-error"))
  predictor <- rep(c(0, 0, 1, 1, 0, 1), 20L)
  modal <- rep(c(1L, 2L, 3L, 1L, 3L, 2L), 20L)
  post <- matrix(0.1, nrow = length(modal), ncol = 3L)
  post[cbind(seq_along(modal), modal)] <- 0.8
  predictors <- cbind(exposure = predictor, context = rep(c(0, 1, 0, 1), 30L))
  predictors[4L, 1L] <- NA_real_
  weights <- rep(c(0.5, 1, 2, 1.5, 1, 0.75), 20L)
  fitted <- ml3step_multi(predictors, post, modal, sw = weights, ref = 2L, lambda = 0.1)
  rescaled <- ml3step_multi(predictors, post, modal, sw = weights * 32, ref = 2L, lambda = 0.1)
  stopifnot(identical(fitted$rows_complete, which(complete.cases(predictors))),
            identical(fitted$weights_original, weights[fitted$rows_complete]),
            abs(mean(fitted$weights_normalized) - 1) < 1e-12,
            max(abs(fitted$par - rescaled$par)) < 1e-8,
            max(abs(fitted$V - rescaled$V)) < 1e-8,
            inherits(try(ml3step_multi(predictors, post, modal, sw = weights[-1L]), silent = TRUE), "try-error"),
            inherits(try(ml3step_multi(predictors, post, modal, sw = replace(weights, 1L, 0)), silent = TRUE), "try-error"))
  design <- cbind(1, predictors[fitted$rows_complete, , drop = FALSE])
  correction <- t(fitted$classification_matrix[, match(modal[fitted$rows_complete], fitted$ord), drop = FALSE])
  objective <- function(parameters)
    weighted_classification_objective(parameters, design, correction, fitted$weights_normalized, 0.1)
  parameters <- seq(-0.2, 0.2, length.out = length(fitted$par))
  finite_difference <- vapply(seq_along(parameters), function(index) {
    perturbation <- numeric(length(parameters))
    perturbation[index] <- 1e-6
    (objective(parameters + perturbation)$value - objective(parameters - perturbation)$value) / 2e-6
  }, numeric(1L))
  numerical_fit <- stats::optim(rep(0, length(parameters)), function(parameters) objective(parameters)$value,
                               method = "BFGS", hessian = TRUE)
  stopifnot(max(abs(finite_difference - objective(parameters)$gradient)) < 1e-5,
            max(abs(exp(numerical_fit$par) - exp(fitted$par))) < 1e-3)
  adjusted <- fit_adj(predictors, post, modal, sw = weights, ref = 2L)
  unpenalized <- ml3step_multi(predictors, post, modal, sw = weights, ref = 2L, lambda = 0)
  stopifnot(identical(adjusted$unpenalized$par, unpenalized$par),
            identical(adjusted$unpenalized$ors, unpenalized$ors),
            adjusted$unpenalized$conv == adjusted$unpenalized_convergence,
            identical(adjusted$ridge_reason != "none", adjusted$ridge_triggered))
  invisible(TRUE)
}
