## =====================================================================
## refit_enumeration_modal.R
##
## Purpose: recover the per-k modal class assignments (k = 2..Kmax) that the
## enumeration figure needs for the adjacent-solution adjusted Rand index (ARI),
## WITHOUT re-running the parametric bootstrap LRT, which is what makes a full
## Part 1 render take 15-20 minutes. Only the nine measurement models are refitted.
##
## How it stays consistent with Table 1: the data preparation is not re-typed
## here. Part 1's `setup` and `load-data` chunks are parsed out of
## leadership_LCA_analysis.qmd and evaluated verbatim (same pattern used by
## leadership_LCA_part2_adjusted_nojobsat.qmd), and the fits use the same formula,
## seed sequence, Mersenne-Twister forcing and n.init as Part 1's `enumeration`
## chunk. The script then REFUSES to write its output unless every refitted
## log-likelihood matches the one reported in lca_results.rds$fit_tbl to within
## 1e-3, so a mismatch stops the script instead of producing a plausible figure.
##
## Output: enumeration_modal_all.rds, read automatically by the Figure 1 chunk.
## Usage:  setwd() to this folder, then  source("refit_enumeration_modal.R")
## =====================================================================

PART1_QMD <- "leadership_LCA_analysis.qmd"
RES_RDS   <- "lca_results.rds"
OUT_RDS   <- "enumeration_modal_all.rds"
KMAX      <- 10L
TOL_LL    <- 1e-3

stopifnot(file.exists(PART1_QMD), file.exists(RES_RDS))

## ---- 1. Inherit Part 1's setup and data preparation -------------------------
parse_qmd_chunks <- function(path) {
  ln    <- readLines(path, warn = FALSE)
  open  <- grep("^```\\{r\\b", ln)
  close <- grep("^```\\s*$",   ln)
  out   <- list()
  for (o in open) {
    e <- close[close > o][1]
    if (is.na(e)) next
    body <- ln[(o + 1):(e - 1)]
    lab  <- sub("^#\\|\\s*label:\\s*", "",
                grep("^#\\|\\s*label:", body, value = TRUE)[1])
    if (is.na(lab)) next
    out[[trimws(lab)]] <- paste(body[!grepl("^#\\|", body)], collapse = "\n")
  }
  out
}

## Part 1's `setup` begins with rm(list = ls()); neutralise it so this driver
## survives, exactly as the nojobsat notebook does.
sanitize_chunk <- function(code)
  gsub("rm\\(\\s*list\\s*=\\s*ls\\(\\)\\s*\\)", "invisible(NULL)", code)

local({
  chunks <- parse_qmd_chunks(PART1_QMD)
  needed <- c("setup", "load-data")
  miss   <- setdiff(needed, names(chunks))
  if (length(miss))
    stop("Part 1 chunk labels not found: ", paste(miss, collapse = ", "),
         ". leadership_LCA_analysis.qmd changed; update `needed`.")
  for (lab in needed) {
    message("== running Part 1 chunk: ", lab)
    eval(parse(text = sanitize_chunk(chunks[[lab]])), envir = globalenv())
  }
})

stopifnot(exists("d_lca"), exists("vars"), exists("n_init"))
message(sprintf("Analytic sample: n= %d; items= %d; n.init= %s",
                nrow(d_lca), length(vars$items), format(n_init)))

## ---- 2. Refit k = 2..KMAX (no bootstrap) ------------------------------------
item_f <- as.formula(paste0("item(", paste(vars$items, collapse = ", "),
                            ") ~ sex_woman"))

for (pkg in c("future", "furrr")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

n_phys <- parallel::detectCores(logical = FALSE)
if (is.na(n_phys)) n_phys <- parallel::detectCores()
if (is.na(n_phys)) n_phys <- 4L
n_workers <- max(8L, min(n_phys - 2L, 16L))
future::plan(future::multisession, workers = n_workers)
message("Parallel refit on ", n_workers, " workers (no bootstrap LRT).")

## Each worker returns only the log-likelihood and the modal assignment vector,
## so the nine fitted objects are never collected back into this session.
res_k <- furrr::future_map(2:KMAX, function(k) {
  RNGkind("Mersenne-Twister")          # same generator as Part 1 (see its notes)
  g <- NULL
  for (s in 2125:2134) {               # same lucky seed + fallback ladder
    g <- tryCatch(glca::glca(item_f, data = d_lca, nclass = k, seed = s,
                             n.init = n_init, verbose = FALSE),
                  error = function(e) NULL)
    if (!is.null(g)) break
  }
  if (is.null(g))
    stop("glca failed for nclass=", k, " across fallback seeds 2125-2134.")
  p  <- g$posterior; if (is.list(p)) p <- p[[1]]
  gg <- g$gof; ll <- gg[["loglik"]]; if (is.null(ll)) ll <- gg[["logLik"]]
  list(k = k, ll = as.numeric(ll)[1],
       modal = max.col(as.matrix(p), ties.method = "first"))
}, .options = furrr::furrr_options(seed = TRUE, packages = "glca"))
future::plan(future::sequential); gc()

## ---- 3. Failable check against the reported Table 1 --------------------------
res      <- readRDS(RES_RDS)
fit_tbl  <- as.data.frame(res$fit_tbl)
ks       <- vapply(res_k, `[[`, integer(1), "k")
ll_new   <- vapply(res_k, `[[`, numeric(1), "ll")
ll_rep   <- fit_tbl$logLik[match(ks, fit_tbl$Classes)]

cmp <- data.frame(Classes = ks, logLik_refit = ll_new,
                  logLik_reported = ll_rep, diff = ll_new - ll_rep)
print(cmp, digits = 10)

bad <- !is.finite(ll_new) | !is.finite(ll_rep) | abs(ll_new - ll_rep) >= TOL_LL
if (any(bad))
  stop("Refitted log-likelihoods differ from lca_results.rds for k= ",
       paste(ks[bad], collapse = ", "), ".\n",
       "  These fits are NOT the ones behind Table 1, so the ARI would describe a\n",
       "  different set of solutions. Likely causes: lca_results.rds predates a\n",
       "  change in the data preparation, or it was written by a draft run with a\n",
       "  smaller n.init. Re-render Part 1 (with the export patch) instead.")

## Modal vectors must all cover the same respondents, in the same order.
stopifnot(length(unique(vapply(res_k, function(x) length(x$modal), integer(1)))) == 1L,
          unique(vapply(res_k, function(x) length(x$modal), integer(1))) == nrow(d_lca))

## ---- 4. Save ----------------------------------------------------------------
modal_all <- setNames(lapply(res_k, `[[`, "modal"), paste0("k", ks))
saveRDS(list(modal_all = modal_all,
             logLik    = setNames(ll_new, as.character(ks)),
             n         = nrow(d_lca),
             n_init    = n_init,
             seed      = 2125L,
             items     = vars$items,
             source    = "refit_enumeration_modal.R (no bootstrap LRT)",
             date      = Sys.Date()),
        OUT_RDS)

cat(sprintf("\nWrote %s: modal assignments for k= %d to %d, n= %d, n.init= %s.\n",
            OUT_RDS, min(ks), max(ks), nrow(d_lca), format(n_init)))
cat("All refitted log-likelihoods match Table 1 within ", TOL_LL, ".\n", sep = "")
cat("The Figure 1 chunk will now pick this file up automatically.\n")
