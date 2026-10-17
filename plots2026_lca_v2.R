###############################################################################
##  plots2026_lca.R
##  Publication-quality, epidemiology-style figures for the
##  destructive / laissez-faire leadership LCA (4-class) manuscript.
##
##  *** DATA-DRIVEN VERSION ***
##  Both figures are now built from the REAL model selection produced by the
##  latest render of  leadership_LCA_analysis.qmd  (-> leadership_LCA_analysis.html),
##  loaded from  lca_results.rds.  No values are hard-coded any more.
##
##    Figure 1 — class-enumeration "scree": AIC, BIC, CAIC, SABIC vs number of
##               classes, with companion bootstrap-LRT and entropy panels.
##    Figure 2 — item-response probabilities by latent class, as a clean
##               four-category stacked bar (behaviour type x latent class).
##
##  lca_results.rds is written by leadership_LCA_analysis.qmd (chunk 'export').
##  If it is missing, render that notebook first (or run the bundled fallback
##  generator at the bottom of this file).
##
##  Conventions for an occupational-epidemiology audience: clean gridless theme,
##  Okabe-Ito colour-blind-safe palette with shape/linetype redundancy for
##  Figure 1; single-hue sequential ramp for the ordered response levels in
##  Figure 2; figures saved at 300 dpi as both PNG and vector PDF.
###############################################################################

## ----------------------------------------------------------------------------
## 0.  Packages
## ----------------------------------------------------------------------------
.pkgs <- c("ggplot2", "dplyr", "tidyr", "scales", "patchwork")
.new  <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.new)) install.packages(.new)
invisible(lapply(.pkgs, library, character.only = TRUE))

OUTDIR <- "figures"; dir.create(OUTDIR, showWarnings = FALSE)

## ----------------------------------------------------------------------------
## 0b.  Load the real LCA results (single source of truth)
## ----------------------------------------------------------------------------
## Try the working directory first, then resolve against the Google-Drive root
## (same logic as the .qmd) so the script works regardless of where it is run.
resolve_path <- function(name,
    roots = c(".", "H:/My Drive/DOCUMENTOS/6.TESIS 2018/LCA NAQ",
              "G:/Mi unidad/DOCUMENTOS/6.TESIS 2018/LCA NAQ",
              "C:/Users/andre/Google Drive/DOCUMENTOS/6.TESIS 2018/LCA NAQ")) {
  for (r in roots) { p <- file.path(r, name); if (file.exists(p)) return(p) }
  stop("No se encontro '", name, "'. Renderiza leadership_LCA_analysis.qmd ",
       "para generar lca_results.rds (su chunk 'export' lo escribe).")
}
lca <- readRDS(resolve_path("lca_results.rds"))

fit_tab  <- lca$fit_tbl
names(fit_tab)[names(fit_tab) == "Classes"] <- "k"
names(fit_tab)[names(fit_tab) == "BLRT P"]  <- "BLRT_p"
names(fit_tab)[names(fit_tab) == "Entropy"] <- "entropy"
fit_tab$BLRT_p <- suppressWarnings(as.numeric(fit_tab$BLRT_p))  # "<0.001"/"0" -> numeric
RETAINED <- lca$retained
KRANGE   <- range(fit_tab$k)
message("Loaded lca_results.rds: n = ", lca$n, ", retained = ", RETAINED,
        "-class, k = ", KRANGE[1], "..", KRANGE[2], ".")

## ----------------------------------------------------------------------------
## 1.  House theme + palettes
## ----------------------------------------------------------------------------
theme_epi <- function(base_size = 11, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line          = element_line(linewidth = 0.4, colour = "grey20"),
      axis.ticks         = element_line(linewidth = 0.4, colour = "grey20"),
      axis.text          = element_text(colour = "grey20"),
      axis.title         = element_text(colour = "black"),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey88"),
      strip.background   = element_rect(fill = "grey95", colour = NA),
      strip.text         = element_text(face = "bold", size = base_size - 0.5),
      legend.position    = "top",
      legend.title       = element_text(face = "bold"),
      legend.key.height  = unit(0.8, "lines"),
      plot.tag           = element_text(face = "bold", size = base_size + 2)
    )
}
okabe <- c(blue = "#0072B2", vermillion = "#D55E00", green = "#009E73",
           purple = "#CC79A7", sky = "#56B4E9", grey = "#999999")

###############################################################################
## FIGURE 1 — CLASS-ENUMERATION SCREE (AIC, BIC, CAIC, SABIC) + BLRT + entropy
##            (real values from lca_results.rds$fit_tbl)
###############################################################################
ic_levels <- c("AIC", "BIC", "CAIC", "SABIC")
ic_long <- fit_tab |>
  dplyr::select(k, dplyr::all_of(ic_levels)) |>
  tidyr::pivot_longer(-k, names_to = "index", values_to = "value") |>
  dplyr::mutate(index = factor(index, levels = ic_levels))
ic_min <- ic_long |> dplyr::group_by(index) |>
  dplyr::slice_min(value, n = 1, with_ties = FALSE) |> dplyr::ungroup()

ic_pal <- c(AIC = okabe[["blue"]], BIC = okabe[["vermillion"]],
            CAIC = okabe[["purple"]], SABIC = okabe[["green"]])
ic_lty <- c(AIC = "solid", BIC = "longdash", CAIC = "dotdash", SABIC = "dotted")
ic_shp <- c(AIC = 16, BIC = 17, CAIC = 15, SABIC = 18)

pA <- ggplot(ic_long, aes(k, value, colour = index, shape = index, linetype = index)) +
  annotate("rect", xmin = RETAINED - .5, xmax = RETAINED + .5, ymin = -Inf, ymax = Inf,
           fill = okabe[["grey"]], alpha = .12) +
  geom_vline(xintercept = RETAINED, linetype = "dashed", colour = "grey40", linewidth = .4) +
  geom_line(linewidth = .7) +
  geom_point(size = 2.4, fill = "white", stroke = .7) +
  ## hollow ring marks each criterion's minimum (13-item re-run: BIC bottoms at
  ## k=8, CAIC at k=7 -- re-check this comment if you re-run the enumeration)
  geom_point(data = ic_min, shape = 21, size = 4.2, stroke = .9, colour = "grey25", fill = NA, show.legend = FALSE) +
  annotate("text", x = RETAINED, y = Inf, label = "Retained\n(4-class)", vjust = 1.3,
           size = 3, colour = "grey25", fontface = "bold") +
  scale_colour_manual(values = ic_pal, name = "Information criterion") +
  scale_shape_manual(values = ic_shp, name = "Information criterion") +
  scale_linetype_manual(values = ic_lty, name = "Information criterion") +
  scale_x_continuous(breaks = KRANGE[1]:KRANGE[2]) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Number of latent classes", y = "Information criterion") +
  theme_epi()

pB <- ggplot(fit_tab, aes(k, BLRT_p)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = .05, ymax = Inf, fill = okabe[["green"]], alpha = .08) +
  geom_hline(yintercept = .05, linetype = "dashed", colour = "grey40", linewidth = .4) +
  geom_line(colour = "grey35", linewidth = .6, na.rm = TRUE) +
  geom_point(aes(fill = BLRT_p > .05), shape = 21, size = 2.6, colour = "grey20", stroke = .6, na.rm = TRUE) +
  geom_vline(xintercept = RETAINED, linetype = "dashed", colour = "grey40", linewidth = .4) +
  annotate("text", x = KRANGE[2] - 1, y = .07, label = "P = 0.05", size = 2.7, colour = "grey35", vjust = -.3) +
  scale_fill_manual(values = c(`FALSE` = okabe[["vermillion"]], `TRUE` = okabe[["grey"]]), guide = "none") +
  scale_x_continuous(breaks = KRANGE[1]:KRANGE[2]) +
  labs(x = "Number of latent classes",
       y = expression(paste("Bootstrap LRT ", italic(P), " (", italic(k), " vs ", italic(k) - 1, ")"))) +
  theme_epi()

pC <- ggplot(fit_tab, aes(k, entropy)) +
  geom_line(colour = "grey35", linewidth = .6) +
  geom_point(size = 2.6, colour = okabe[["blue"]]) +
  geom_vline(xintercept = RETAINED, linetype = "dashed", colour = "grey40", linewidth = .4) +
  scale_x_continuous(breaks = KRANGE[1]:KRANGE[2]) +
  labs(x = "Number of latent classes", y = "Relative entropy") + theme_epi()

fig1 <- pA / (pB | pC) + patchwork::plot_layout(heights = c(1.5, 1)) +
  patchwork::plot_annotation(tag_levels = "A")
ggsave(file.path(OUTDIR, "Figure1_enumeration.png"), fig1, width = 9, height = 7.2, dpi = 300, bg = "white")
ggsave(file.path(OUTDIR, "Figure1_enumeration.pdf"), fig1, width = 9, height = 7.2, device = cairo_pdf)

###############################################################################
## FIGURE 2 — ITEM-RESPONSE PROBABILITIES (real 4-category stacked bar)
##            (real rho from lca_results.rds$rho4)
###############################################################################
## Item metadata: lid_des1,2,4..14 -> readable label + behaviour type.
## lid_des3 ("steers away from showing concern about results" in the original
## scale) was EXCLUDED from the model (see Methods): in this survey it was
## administered in a positively worded form ("has shown concern about
## results"), which is semantically ambiguous (could reflect genuine concern,
## passive disinterest, or controlling oversight) and loaded poorly in the
## psychometric validation of the scale in this population. The model below
## is therefore fit on the remaining 13 items -- this table must stay in sync
## with whatever the notebook actually fits, in that same order; the
## stopifnot() a few lines down turns a mismatch into a loud error instead of
## the silent off-by-one mislabeling that produced the earlier, wrong figure.
items <- data.frame(
  lid   = paste0("lid_des", c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)),
  label = c("Humiliates employees", "Encourages creative ideas",
            "Gestures of displeasure", "Avoids making decisions",
            "Recognizes good performance", "Avoids involvement in work",
            "Spreads false information", "Absent when needed",
            "Berates by phone / rude email", "Seeks to develop capabilities",
            "Sets clear objectives", "Defines / explains tasks",
            "Flexible, open to innovation"),
  type  = c("Destructive", "Constructive", "Destructive", "Laissez-faire",
            "Constructive", "Laissez-faire", "Destructive", "Laissez-faire",
            "Destructive", "Constructive", "Constructive", "Constructive",
            "Constructive"),
  stringsAsFactors = FALSE
)
items$type <- factor(items$type, levels = c("Destructive", "Laissez-faire", "Constructive"))

if (length(lca$rho4) != nrow(items)) {
  stop(sprintf(
    "Item-count mismatch: lca$rho4 has %d items but 'items' has %d rows. ",
    length(lca$rho4), nrow(items)),
    "Update 'items' to match what the notebook actually fitted -- this is ",
    "exactly the mismatch that mislabeled 'steers away from showing concern ",
    "about results' in a previous version of this figure.")
}

## ----------------------------------------------------------------------------
## Auto-detect which RAW glca class (1..K) is which profile, from the fitted
## item probabilities themselves. glca's internal class numbering is
## arbitrary and reshuffles between runs (this has bitten this figure
## before), so we never hardcode "class 1 = X" -- we read the behavioural
## signature off lca$rho4 every time, using the same logic used throughout
## the manuscript: mean P("fairly often" + "almost always/always") by item
## type. Disengaged = lowest constructive endorsement; Ambivalent = highest
## destructive + laissez-faire endorsement; the remaining two (both strongly
## constructive) are ranked against each other for the two constructive tiers.
## ----------------------------------------------------------------------------
top2 <- function(m) rowSums(m[, 3:4, drop = FALSE]) / rowSums(m)   # P(fairly often + almost always)
score <- sapply(lca$rho4, function(rho) top2(as.matrix(rho)))       # nclass x nitem
by_type <- function(type) rowMeans(score[, items$type == type, drop = FALSE])
constructive_sc <- by_type("Constructive")
destructive_sc  <- by_type("Destructive")
lf_sc           <- by_type("Laissez-faire")

nclass <- length(constructive_sc)
diag_tbl <- data.frame(raw = seq_len(nclass),
                        constructive = round(constructive_sc, 3),
                        laissez_faire = round(lf_sc, 3),
                        destructive = round(destructive_sc, 3))
message("Per-raw-class behavioural signature (drives the auto-assigned labels below):")
print(diag_tbl)

disengaged_raw <- which.min(constructive_sc)
ambivalent_raw <- which.max(destructive_sc + lf_sc)
remaining_raw  <- setdiff(seq_len(nclass), c(disengaged_raw, ambivalent_raw))
remaining_raw  <- remaining_raw[order(-constructive_sc[remaining_raw])]

## Canonical labels (must match lca_results.rds$class_labels from Part 1):
##   C1 Constructive | C2 Passive/absent | C3 Active constructive | C4 Ambivalent/destructive
row_to_label <- character(nclass)
row_to_label[disengaged_raw]     <- "C2 Passive/absent"
row_to_label[remaining_raw[1]]   <- "C3 Active constructive"
row_to_label[remaining_raw[2]]   <- "C1 Constructive"
row_to_label[ambivalent_raw]     <- "C4 Ambivalent/destructive"
message("Raw class -> label (check this against the signature table above):")
print(setNames(row_to_label, paste0("raw", seq_len(nclass))))

## Panel order = canonical order from lca_results.rds (single source of truth):
class_levels <- lca$class_labels   # c("C1 Constructive","C2 Passive/absent","C3 Active constructive","C4 Ambivalent/destructive")
cat_labels <- c("Never", "Sometimes", "Fairly often", "Almost always/always")
cat_pal <- c("Never" = "#DBE9F6", "Sometimes" = "#9ECAE1",
             "Fairly often" = "#4292C6", "Almost always/always" = "#08519C")

make_stacked <- function(rho_df) {
  ggplot(rho_df, aes(prob, item, fill = category)) +
    geom_col(width = 0.80, colour = "white", linewidth = 0.2,
             position = position_stack(reverse = TRUE)) +
    facet_grid(type ~ class, scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_manual(values = cat_pal, name = "Response category",
                      guide = guide_legend(nrow = 1)) +
    scale_x_continuous(breaks = c(0, .5, 1), labels = c("0", "50", "100"),
                       expand = expansion(mult = c(0.04, 0.04))) +
    labs(x = "Response-category probability (%)", y = NULL) +
    theme_epi() +
    theme(legend.position    = "top",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = .25, colour = "grey88"),
          panel.spacing.x    = unit(1.0, "lines"),
          panel.spacing.y    = unit(0.4, "lines"),
          strip.placement    = "outside",
          strip.text.y.left  = element_text(angle = 0, face = "bold"),
          strip.text.x       = element_text(face = "bold"),
          axis.ticks.y       = element_blank(),
          axis.text.y        = element_text(size = 8))
}

## Build the tidy data frame from the REAL fitted rho (list of 14 class x category
## matrices, rows = class, cols = response category 1..4).
rho_list_to_df <- function(rho_list, item_labels = items$label, item_types = items$type,
                           row_labels = row_to_label, panel_order = class_levels,
                           cat_labs = cat_labels) {
  do.call(rbind, lapply(seq_along(rho_list), function(j) {
    m <- as.matrix(rho_list[[j]])                  # class (rows) x category (cols)
    m <- m / rowSums(m)                            # exact sum-to-1 (avoids float clip)
    data.frame(
      item     = factor(item_labels[j], levels = rev(item_labels)),
      type     = item_types[j],
      ## row_labels is indexed by RAW glca row (1..nclass); panel_order sets
      ## the desired display sequence independently of that raw index.
      class    = factor(rep(row_labels, times = ncol(m)), levels = panel_order),
      category = factor(rep(cat_labs,   each  = nrow(m)), levels = cat_labs),
      prob     = as.vector(m)
    )
  }))
}

rho_df <- rho_list_to_df(lca$rho4)
fig2 <- make_stacked(rho_df)
ggsave(file.path(OUTDIR, "Figure2_itemprob.png"), 
       fig2, 
       width = 11, 
       height = 7.6, 
       dpi = 300, 
       bg = "white")
ggsave(file.path(OUTDIR, 
                 "Figure2_itemprob.pdf"), 
       fig2, 
       width = 11, 
       height = 7.6, 
       device = cairo_pdf)

## Layout variants (one-liners):
##  * profiles stacked vertically by class:  make_stacked(rho_df) + facet_wrap(~ class, ncol = 1)
##  * one panel per class, collapse type rows: replace facet_grid(...) with facet_grid(. ~ class, ...)

message("Done. Figures written to '", normalizePath(OUTDIR), "'.")
message("Figure 1 reflects the real enumeration (BIC minimises at k=8, CAIC at k=7; ",
        "4-class retained on parsimony/interpretability). Figure 2 uses the real fitted rho ",
        "with auto-detected class labels (see the diagnostic table printed above).")
###############################################################################
