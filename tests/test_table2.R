# Independent checks on aggregate descriptive calculations and the private loader.
# Synthetic records only; no participant data and no latent-class model fitting.
source("R/leadership_table2.R")
source("R/load_leadership_results.R")
stopifnot(requireNamespace("survey", quietly = TRUE),
          requireNamespace("dplyr", quietly = TRUE),
          requireNamespace("digest", quietly = TRUE))

failures <- character()
check <- function(label, expr) {
  tryCatch({ force(expr); cat("PASS:", label, "\n") }, error = function(e) {
    failures <<- c(failures, paste(label, conditionMessage(e), sep = ": "))
    cat("FAIL:", label, "-", conditionMessage(e), "\n")
  })
}
close_to <- function(actual, expected, tolerance = 1e-9) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected),
                             tolerance = tolerance)))
}
expect_error <- function(expr) {
  rejected <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(rejected)
}

classes <- rep(seq_len(4), each = 4)
labels <- c("Moderately constructive", "Disengaged", "Constructive", "Ambivalent")
post <- matrix(.1, 16, 4)
post[cbind(seq_len(16), classes)] <- .7
d <- data.frame(.source_row = seq_len(16), w = rep(c(1, 1, 2, 2), 4),
  binary = c(0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0),
  age = rep(0:3, 4) + rep(c(0, 2, 4, 6), each = 4))
build <- function(dat = d) {
  des <- survey::svydesign(ids = ~1, weights = ~w, data = dat)
  build_leadership_table2(des, classes, post, labels,
    bin_vars = c("Binary exposure, %" = "binary"),
    cont_vars = c("Age, median (Q1-Q3)" = "age"))
}
base <- build()

check("hand-calculated weighted proportions and binary pairwise SMD", {
  bs <- subset(base$summary, variable == "binary")
  close_to(bs$percent, 100 * c(1/3, 2/3, 5/6, 0))
  pair <- subset(base$smd_pairs, variable == "binary" & class_a == "C1" & class_b == "C2")
  close_to(pair$smd, (-1/3)/sqrt(2/9))
  close_to(pair$pooled_sd, sqrt(2/9))
})

check("all six unique pairs and maximum absolute SMD for each variable", {
  for (key in c("binary", "age")) {
    pp <- subset(base$smd_pairs, variable == key)
    stopifnot(nrow(pp) == 6L, !anyDuplicated(paste(pp$class_a, pp$class_b)),
              setequal(paste(pp$class_a, pp$class_b),
                       c("C1 C2", "C1 C3", "C1 C4", "C2 C3", "C2 C4", "C3 C4")))
    close_to(base$tests$max_abs_smd[base$tests$variable == key], max(abs(pp$smd)))
  }
})

check("hand-calculated continuous weighted mean, variance, median and SMD", {
  cs <- subset(base$summary, variable == "age")
  # Weighted mean=11/6, weighted central second moment=41/36;
  # survey::svyvar's finite-sample correction for n=4 yields 41/27.
  close_to(cs$mean, 11/6 + c(0, 2, 4, 6))
  close_to(cs$sd^2, rep(41/27, 4))
  close_to(cs$median, c(2, 4, 6, 8))
  close_to(cs$q1, c(1, 3, 5, 7))
  close_to(cs$q3, c(2.75, 4.75, 6.75, 8.75))
  pair <- subset(base$smd_pairs, variable == "age" & class_a == "C1" & class_b == "C2")
  close_to(pair$smd, -2/sqrt(41/27))
})

check("continuous global P-value is finite, identified and displayed", {
  tt <- subset(base$tests, variable == "age")
  stopifnot(tt$test_status == "ok", is.finite(tt$p_value),
            tt$p_value >= 0, tt$p_value <= 1,
            grepl("Kruskal", tt$method), nzchar(tt$test_details),
            base$continuous$P != "Not estimable", nzchar(base$continuous$P))
})

check("weight rescaling preserves descriptive estimates, SMDs and P-values", {
  scaled <- d; scaled$w <- 17 * scaled$w
  other <- build(scaled)
  for (nm in c("mean", "sd", "percent", "q1", "median", "q3"))
    close_to(other$summary[[nm]], base$summary[[nm]])
  close_to(other$smd_pairs$smd, base$smd_pairs$smd)
  close_to(other$tests$p_value, base$tests$p_value)
  close_to(other$class_sizes$weighted_percent, base$class_sizes$weighted_percent)
  close_to(other$class_sizes$weight_sum, 17 * base$class_sizes$weight_sum)
  stopifnot(identical(other$class_sizes$n_modal, base$class_sizes$n_modal))
})

check("missing observations leave the variable-specific weighted denominator", {
  missing <- d; missing$binary[4] <- NA_real_
  z <- build(missing)
  first <- subset(z$summary, variable == "binary" & class == "C1")
  stopifnot(first$n_nonmissing == 3L, first$n_missing == 1L,
            first$weight_sum_nonmissing == 4, first$percent == 0,
            z$class_sizes$n_modal[1] == 4L, z$class_sizes$weight_sum[1] == 6)
})

check("singleton continuous class keeps median but marks SMD unavailable", {
  singleton <- d; singleton$age[2:4] <- NA_real_
  z <- build(singleton)
  first <- subset(z$summary, variable == "age" & class == "C1")
  stopifnot(first$n_nonmissing == 1L, first$median == 0,
            is.na(first$sd), is.na(z$tests$max_abs_smd[z$tests$variable == "age"]),
            z$continuous$SMD == "Not estimable")
})

check("constant outcome has explicit nonestimable test and zero SMD", {
  constant <- d; constant$age <- 8; constant$binary <- 0
  z <- build(constant)
  stopifnot(all(z$tests$test_status == "not_estimable"),
            all(is.na(z$tests$p_value)), all(z$tests$max_abs_smd == 0),
            all(z$tests$test_details != ""))
})

check("class with no observations has explicit status and no invented estimate", {
  missing <- d; missing$age[classes == 4] <- NA_real_
  z <- build(missing)
  last <- subset(z$summary, variable == "age" & class == "C4")
  tt <- subset(z$tests, variable == "age")
  stopifnot(last$n_nonmissing == 0, last$n_missing == 4,
            last$weight_sum_nonmissing == 0, is.na(last$median),
            tt$test_status == "not_estimable", is.na(tt$p_value),
            is.na(tt$max_abs_smd), z$continuous$C4 == "Not estimable")
})

check("entirely missing continuous variable returns an explicit status", {
  missing <- d; missing$age <- NA_real_
  z <- build(missing)
  tt <- subset(z$tests, variable == "age")
  stopifnot(tt$test_status == "not_estimable", is.na(tt$p_value),
            all(is.na(subset(z$summary, variable == "age")$median)))
})

check("class-label mapping, actual counts and model shares stay aligned", {
  stopifnot(identical(names(base$full), c("Characteristic", paste0("C", 1:4), "P", "SMD")))
  stopifnot(identical(base$class_sizes$class_label, labels),
            identical(base$class_sizes$Class, paste0("C", 1:4)),
            all(base$class_sizes$n_modal == 4L),
            identical(subset(base$summary, variable == "age")$class_label, labels))
  close_to(base$class_sizes$model_prevalence, rep(.25, 4))
  close_to(base$class_sizes$weighted_percent, rep(25, 4))
  stopifnot(base$full$Characteristic[1] == "Participants, unweighted n")
})

check("invalid weights, nonbinary values and mismatched class labels are rejected", {
  wrong <- d; wrong$binary[1] <- 2
  expect_error(build(wrong))
  wrong <- d; wrong$w[1] <- 0
  expect_error(build(wrong))
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  expect_error(build_leadership_table2(des, classes, post, labels[-1],
    bin_vars = c("Binary" = "binary"), cont_vars = c("Age" = "age")))
})

bundle <- list(schema_version = "2.0.0", analysis_data = d,
  analysis_data_hash = digest::digest(d, algo = "sha256"),
  post4 = post, modal4 = classes, item_order = "age", class_labels = labels)
read_fixture <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path)
  load_leadership_results(path)
}

check("loader accepts a valid versioned fixture", {
  stopifnot(identical(read_fixture(bundle), bundle))
})

check("loader rejects altered data, unsupported schema and broken assignment", {
  z <- bundle; z$analysis_data$age[1] <- 100
  expect_error(read_fixture(z))
  z <- bundle; z$schema_version <- "99.0.0"
  expect_error(read_fixture(z))
  z <- bundle; z$modal4[1] <- 2L
  expect_error(read_fixture(z))
  z <- bundle; z$analysis_data$.source_row[2] <- 1L
  z$analysis_data_hash <- digest::digest(z$analysis_data, algo = "sha256")
  expect_error(read_fixture(z))
})

check("loader rejects missing source-row identity", {
  z <- bundle; z$analysis_data$.source_row <- NULL
  z$analysis_data_hash <- digest::digest(z$analysis_data, algo = "sha256")
  expect_error(read_fixture(z))
})

check("loader rejects impossible posterior probabilities even when rows sum to one", {
  z <- bundle; z$post4[1, ] <- c(1.1, -.1, 0, 0)
  expect_error(read_fixture(z))
})

check("loader rejects incomplete class-label mapping", {
  z <- bundle; z$class_labels <- labels[-1]
  expect_error(read_fixture(z))
})

check("measurement fingerprint permits identical inputs and rejects changed row order", {
  md <- data.frame(lid_des1 = factor(rep(1:4, 4), levels = 1:4),
                   sex_woman = rep(c(0, 1), 8))
  fit <- list(post4 = post, retained = 4L, n_init = 4000L, seed = 2125L,
              item_order = "lid_des1", model_input_hash = digest::digest(md, algo = "sha256"))
  stopifnot(grepl("Matched", validate_saved_measurement(fit, md, "lid_des1")))
  expect_error(validate_saved_measurement(fit, md[16:1, ], "lid_des1"))
  modified <- md; modified$lid_des1[1] <- "2"
  expect_error(validate_saved_measurement(fit, modified, "lid_des1"))
})

if (length(failures)) cat("\nFAILED CHECKS:\n", paste(failures, collapse = "\n"), "\n", sep = "")
stopifnot(length(failures) == 0L)
cat("All Table 2 and loader checks passed. No LCA models fitted.\n")
