params <-
list(family = "red")

## ----setup, include=FALSE-----------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) ggplot2::theme_set(neuroim2::theme_neuro(base_family = params$family))
knitr::opts_chunk$set(
  collapse = TRUE, echo = TRUE, message = FALSE, warning = FALSE,
  fig.width = 7, fig.height = 5, dpi = 120, fig.alt = 'Plot output'
)
suppressPackageStartupMessages({
  library(ggplot2)
  library(neuroim2)
})

## -----------------------------------------------------------------------------
set.seed(1)

make_synthetic_vol <- function(dims = c(96, 96, 72), vox = c(2, 2, 2)) {
  i <- array(rep(seq_len(dims[1]), times = dims[2]*dims[3]), dims)
  j <- array(rep(rep(seq_len(dims[2]), each = dims[1]), times = dims[3]), dims)
  k <- array(rep(seq_len(dims[3]), each = dims[1]*dims[2]), dims)

  c0 <- dims / 2
  g1 <- exp(-((i - c0[1])^2 + (j - c0[2])^2 + (k - c0[3])^2) / (2*(min(dims)/4)^2))
  g2 <- 0.5 * exp(-((i - (c0[1] + 15))^2 + (j - (c0[2] - 10))^2 + (k - (c0[3] + 8))^2) / (2*(min(dims)/6)^2))
  x  <- g1 + g2 + 0.05 * array(stats::rnorm(prod(dims)), dims)

  sp <- NeuroSpace(dims, spacing = vox)
  NeuroVol(x, sp)
}

# Prefer an included demo file. Use a real example from inst/extdata.
demo_path <- system.file("extdata", "mni_downsampled.nii.gz", package = "neuroim2")

t1 <- if (nzchar(demo_path)) {
  read_vol(demo_path)
} else {
  make_synthetic_vol()
}

dims <- dim(t1)

# Build a synthetic "z-statistic" overlay matched to t1's dims
mk_blob <- function(mu, sigma = 8) {
  i <- array(rep(seq_len(dims[1]), times = dims[2]*dims[3]), dims)
  j <- array(rep(rep(seq_len(dims[2]), each = dims[1]), times = dims[3]), dims)
  k <- array(rep(seq_len(dims[3]), each = dims[1]*dims[2]), dims)
  exp(-((i - mu[1])^2 + (j - mu[2])^2 + (k - mu[3])^2) / (2*sigma^2))
}
ov_arr <- 3.5 * mk_blob(mu = round(dims * c(.60, .45, .55)), sigma = 7) -
          3.2 * mk_blob(mu = round(dims * c(.35, .72, .40)), sigma = 6) +
          0.3 * array(stats::rnorm(prod(dims)), dims)

overlay <- NeuroVol(ov_arr, space(t1))

## -----------------------------------------------------------------------------
# Choose a sensible set of axial slices
zlevels <- unique(round(seq( round(dims[3]*.25), round(dims[3]*.85), length.out = 12 )))

p <- plot_montage(
  t1, zlevels = zlevels, along = 3,
  cmap = "grays", range = "robust", probs = c(.02, .98),
  ncol = 6, title = "Axial montage (robust scaling)"
)
p + theme_neuro()

## -----------------------------------------------------------------------------
plot_montage(
  t1, zlevels = zlevels, along = 3,
  cmap = "grays", range = "robust", ncol = 6, downsample = 2,
  title = "Downsampled montage (for speed)"
)

## -----------------------------------------------------------------------------
center_voxel <- round(dim(t1) / 2)
plot_ortho(
  t1, coord = center_voxel, unit = "index",
  cmap = "grays", range = "robust",
  crosshair = TRUE, annotate = TRUE
)

## -----------------------------------------------------------------------------
plot_overlay(
  bgvol = t1, overlay = overlay,
  zlevels = zlevels[seq(2, length(zlevels), by = 2)],  # fewer panels for the vignette
  bg_cmap = "grays", ov_cmap = "inferno",
  bg_range = "robust", ov_range = "robust", probs = c(.02, .98),
  ov_thresh = 2.5,   # make weaker signal transparent
  ov_alpha  = 0.65,
  ncol = 3, title = "Statistical overlay (threshold 2.5, alpha 0.65)"
)

## -----------------------------------------------------------------------------
plot_montage(
  t1, zlevels = zlevels[1:6], along = 3,
  cmap = "viridis", range = "robust", ncol = 6,
  title = "Same data, Viridis palette"
)

## -----------------------------------------------------------------------------
sessionInfo()

