# Table 2: aggregate estimates from fixed modal classes. No LCA fitting.
weighted_quantiles <- function(x, w, probs = c(.25, .5, .75)) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  if (length(x) == 1L) return(rep(x, length(probs)))
  o <- order(x); x <- x[o]; w <- w[o]
  # Preserve the original notebook's midpoint weighted-ECDF interpolation.
  stats::approx((cumsum(w) - .5*w)/sum(w), x, xout = probs,
                rule = 2, ties = "ordered")$y
}

table2_p_format <- function(p) {
  ifelse(is.na(p), "Not estimable", ifelse(p < .001, "<0.001", sprintf("%.3f", p)))
}

build_leadership_table2 <- function(design, modal, post, class_labels, bin_vars,
                                    cont_vars = c("Age, median (Q1-Q3), years" = "age",
                                      "Job satisfaction, median (Q1-Q3)" = "job_sat")) {
  K <- ncol(post); dat <- design$variables
  stopifnot(nrow(dat) == length(modal), nrow(post) == nrow(dat),
            all(modal %in% seq_len(K)), length(class_labels) == K)
  w <- as.numeric(stats::weights(design))
  if (any(!is.finite(w) | w <= 0)) stop("Table 2 requires finite positive weights.")
  design$variables$class <- factor(modal, levels = seq_len(K), labels = paste0("C", seq_len(K)))
  specs <- c(bin_vars, cont_vars)
  if (!all(unname(specs) %in% names(dat))) stop("Missing Table 2 variables.")

  summaries <- tests <- pairs <- list()
  for (label in names(specs)) {
    key <- specs[[label]]; x <- as.numeric(dat[[key]])
    binary <- key %in% unname(bin_vars)
    if (binary && any(!is.na(x) & !x %in% c(0, 1))) stop("Nonbinary values: ", key)
    design$variables$.x <- x
    ss <- lapply(seq_len(K), function(k) {
      ok <- modal == k & is.finite(x); n <- sum(ok)
      dk <- design[ok, ]; q <- weighted_quantiles(x[ok], w[ok])
      mu <- if (n) as.numeric(stats::coef(survey::svymean(~.x, dk))) else NA_real_
      # Binary SMD uses Bernoulli variance; continuous SMD uses svyvar's
      # estimate of population variance, NOT the variance of the mean.
      variance <- if (binary) mu * (1 - mu) else if (n > 1L)
        as.numeric(stats::coef(survey::svyvar(~.x, dk))) else NA_real_
      data.frame(variable = key, characteristic = label,
        type = if (binary) "binary" else "continuous", class = paste0("C", k),
        class_label = class_labels[k], n_nonmissing = n,
        n_missing = sum(modal == k) - n, weight_sum_nonmissing = sum(w[ok]),
        mean = mu, sd = sqrt(variance), percent = if (binary) 100*mu else NA_real_,
        q1 = if (binary) NA_real_ else q[1], median = if (binary) NA_real_ else q[2],
        q3 = if (binary) NA_real_ else q[3])
    })
    ss <- dplyr::bind_rows(ss); summaries[[key]] <- ss
    pp <- t(utils::combn(seq_len(K), 2))
    pairs[[key]] <- dplyr::bind_rows(lapply(seq_len(nrow(pp)), function(i) {
      a <- pp[i,1]; b <- pp[i,2]
      den <- sqrt((ss$sd[a]^2 + ss$sd[b]^2)/2)
      diff <- ss$mean[a] - ss$mean[b]
      smd <- if (is.na(den)) NA_real_ else if (den > 0) diff/den else if (diff == 0) 0 else sign(diff)*Inf
      data.frame(variable = key, class_a = ss$class[a], class_b = ss$class[b],
        mean_difference = diff, pooled_sd = den, smd = smd, abs_smd = abs(smd))
    }))
    method <- if (binary) "Rao-Scott F test" else "Survey-adjusted Kruskal-Wallis rank test"
    test_result <- tryCatch({
      if (length(unique(x[is.finite(x)])) < 2L) stop("Outcome is constant or entirely missing.")
      if (any(ss$n_nonmissing == 0L)) stop("At least one class has no observed values.")
      tt <- if (binary) survey::svychisq(~class + .x, design, statistic = "F") else
        survey::svyranktest(.x ~ class, design, test = "KruskalWallis")
      # For k>2 rank tests, survey stores F in parameter and numerator df
      # in statistic; retain named components instead of mislabelling them.
      components <- paste(c(paste(names(tt$statistic), signif(tt$statistic, 10), sep = "="),
                            paste(names(tt$parameter), signif(tt$parameter, 10), sep = "=")), collapse = "; ")
      list(p = as.numeric(tt$p.value), status = "ok", details = components)
    }, error = function(e) list(p = NA_real_, status = "not_estimable", details = conditionMessage(e)))
    all_smd <- pairs[[key]]$abs_smd
    max_smd <- if (anyNA(all_smd)) NA_real_ else max(all_smd)
    tests[[key]] <- data.frame(variable = key, characteristic = label, method = method,
      p_value = test_result$p, max_abs_smd = max_smd,
      test_status = test_result$status, test_details = test_result$details)
  }
  summary <- dplyr::bind_rows(summaries); tests <- dplyr::bind_rows(tests)
  pairs <- dplyr::bind_rows(pairs)
  display <- dplyr::bind_rows(lapply(seq_len(nrow(tests)), function(i) {
    tt <- tests[i, ]; ss <- summary[summary$variable == tt$variable, ]
    values <- if (ss$type[1] == "binary") sprintf("%.1f", ss$percent) else
      sprintf("%.1f (%.1f-%.1f)", ss$median, ss$q1, ss$q3)
    values[ss$n_nonmissing == 0] <- "Not estimable"
    row <- data.frame(Characteristic = tt$characteristic, stringsAsFactors = FALSE)
    for (k in seq_len(K)) row[[paste0("C", k)]] <- values[k]
    row$P <- table2_p_format(tt$p_value)
    row$SMD <- if (is.na(tt$max_abs_smd)) "Not estimable" else sprintf("%.2f", tt$max_abs_smd)
    row
  }))
  sizes <- data.frame(Class = paste0("C", seq_len(K)), class_label = class_labels,
    n_modal = tabulate(modal, K), weight_sum = vapply(seq_len(K), function(k) sum(w[modal == k]), numeric(1)),
    model_prevalence = colMeans(post), check.names = FALSE)
  sizes$weighted_percent <- 100*sizes$weight_sum/sum(w)
  counts <- data.frame(Characteristic = c("Participants, unweighted n", "Class share, survey-weighted %"))
  for (k in seq_len(K)) counts[[paste0("C", k)]] <- c(as.character(sizes$n_modal[k]), sprintf("%.1f", sizes$weighted_percent[k]))
  counts$P <- counts$SMD <- ""
  full <- dplyr::bind_rows(counts, display)[, c("Characteristic", paste0("C", seq_len(K)), "P", "SMD")]
  list(full = full,
       binary = display[tests$variable %in% unname(bin_vars), ],
       continuous = display[tests$variable %in% unname(cont_vars), ],
       summary = summary, tests = tests, smd_pairs = pairs, class_sizes = sizes)
}
