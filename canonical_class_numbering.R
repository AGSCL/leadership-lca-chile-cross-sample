## =====================================================================
## canonical_class_numbering.R
##
## Problem this solves: glca returns classes in an arbitrary order, so the raw
## index of a class carries no meaning and can differ between models and between
## renders. The notebooks printed that raw index ("C2 vs C1"), and the manuscript
## then renumbered the rows by hand. That manual step is where a table can end up
## contradicting Table 2.
##
## This script derives the MANUSCRIPT numbering from the behavioral signature of
## each class, so the notebooks can print manuscript-ready labels directly:
##
##   C1  Moderately constructive
##   C2  Disengaged
##   C3  Constructive                (reference in every three-step model)
##   C4  Ambivalent/intermittently destructive
##
## Assignment rule (uses only the family scores, never a raw index):
##   C4 = highest (destructive + laissez-faire)
##   C2 = lowest constructive among the remaining three
##   C3 = highest constructive among the remaining two
##   C1 = the last one
##
## Requires in the calling environment: `fam_inv` (3 x K matrix with rows
## "Destructive", "Laissez-faire", "Constructive", as built by the `shared-labels`
## chunk of leadership_LCA_analysis_part2_sample_comparison.qmd) and `ref_class`.
##
## Creates: ms_num, ms_lab, ms_tag(), ms_contrast(), ms_order  (all indexed by RAW class)
## Usage:  source("canonical_class_numbering.R")   # right after `shared-labels`
## =====================================================================

local({
  if (!exists("fam_inv", envir = globalenv()))
    stop("canonical_class_numbering.R: `fam_inv` not found. Run the `shared-labels` ",
         "chunk of Part 2 first (it builds the family scores of the invariant model).")
  fam <- get("fam_inv", envir = globalenv())
  stopifnot(is.matrix(fam),
            all(c("Destructive", "Laissez-faire", "Constructive") %in% rownames(fam)))

  con <- fam["Constructive", ]; lf <- fam["Laissez-faire", ]; de <- fam["Destructive", ]
  K   <- length(con)
  if (K != 4L)
    stop("canonical_class_numbering.R: expected a four-class model, got K= ", K,
         ". The manuscript numbering is defined for four classes only.")

  num <- integer(K)
  i4  <- which.max(de + lf)                      # ambivalent / intermittently destructive
  num[i4] <- 4L
  r1  <- setdiff(seq_len(K), i4)
  i2  <- r1[which.min(con[r1])]                  # disengaged: lowest constructive
  num[i2] <- 2L
  r2  <- setdiff(r1, i2)
  i3  <- r2[which.max(con[r2])]                  # constructive (reference)
  num[i3] <- 3L
  num[setdiff(r2, i3)] <- 1L                     # moderately constructive

  ## Failable checks: the mapping must be a bijection onto 1:4, the reference used
  ## by the three-step models must be the constructive class, and the signature
  ## must actually look like the manuscript's description of each class.
  stopifnot(!anyDuplicated(num), identical(sort(num), 1:4))
  if (exists("ref_class", envir = globalenv())) {
    rc <- get("ref_class", envir = globalenv())
    if (num[rc] != 3L)
      stop("canonical_class_numbering.R: the reference class used by the three-step ",
           "models (raw C", rc, ") does not map to manuscript C3 (constructive). ",
           "Inspect fam_inv before reporting any table.")
  }
  i1 <- setdiff(r2, i3)                          # moderately constructive
  if (!(con[i3] > con[i1] && con[i2] < con[i1]))
    warning("canonical_class_numbering.R: constructive ordering is not the expected ",
            "C3 > C1 > C2; check fam_inv, the labels may not describe these classes.")

  labs <- c("Moderately constructive", "Disengaged", "Constructive",
            "Ambivalent/intermittently destructive")

  assign("ms_num", num,          envir = globalenv())   # raw class -> manuscript number
  assign("ms_lab", labs[num],    envir = globalenv())   # raw class -> manuscript label
  assign("ms_order", order(num), envir = globalenv())   # raw indices in C1..C4 order
  assign("ms_tag", function(k) sprintf("C%d %s", num[k], labs[num[k]]), envir = globalenv())
  assign("ms_contrast", function(k, ref) sprintf("C%d vs C%d", num[k], num[ref]),
         envir = globalenv())

  message("Manuscript numbering (raw glca index -> manuscript class):")
  print(data.frame(raw = paste0("raw C", seq_len(K)),
                   manuscript = paste0("C", num),
                   label = labs[num],
                   constructive = round(con * 100),
                   `laissez_faire` = round(lf * 100),
                   destructive = round(de * 100),
                   check.names = FALSE), row.names = FALSE)
})
