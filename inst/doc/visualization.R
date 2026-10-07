params <-
list(family = "red", preset = "interaction")

## ----setup, include = FALSE---------------------------------------------------
if (requireNamespace("ragg", quietly = TRUE)) knitr::opts_chunk$set(dev = "ragg_png")
if (requireNamespace("systemfonts", quietly = TRUE)) albersdown::albers_register_fonts()
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("albersdown", quietly = TRUE)) ggplot2::theme_set(albersdown::theme_albers(family = params$family, preset = params$preset))
source("_common.R")
knitr::opts_chunk$set(fig.width = 8, dpi = 120)

## ----albers-classes, echo=FALSE, results='asis'-------------------------------
cat(sprintf(
  paste0(
    '<script>document.addEventListener("DOMContentLoaded",function(){',
    'document.body.classList.remove("palette-red","palette-lapis","palette-ochre","palette-teal","palette-green","palette-violet","preset-homage","preset-interaction","preset-study","preset-structural","preset-adobe","preset-midnight");',
    'document.body.classList.add("palette-%s","preset-%s");',
    '});</script>'
  ),
  params$family,
  params$preset
))

## ----load---------------------------------------------------------------------
library(neuroim2)

## ----data---------------------------------------------------------------------
anat <- demo_anatomy()
brain <- demo_anatomy_mask()
stat_map <- demo_stat_map(brain, radius_mm = 6)

identical(space(anat), space(stat_map))

## ----zlevels------------------------------------------------------------------
zlevels <- round(seq(12, 38, length.out = 6))

## ----montage, fig.cap = "Six axial slices with robust intensity scaling.", fig.alt = "Six axial anatomical slices on a dark background.", fig.height = 3.4, dev.args = list(bg = "#141414")----
plot_montage(anat,
  zlevels = zlevels, ncol = 6,
  cmap = anatomy_cmap, range = "robust",
  title = "Anatomical montage", style = "dark"
)

## ----montage-light, fig.cap = "The same slices in the light style.", fig.alt = "Four axial anatomical slices, dark brain tiles on a light page.", fig.height = 3.4----
plot_montage(anat,
  zlevels = zlevels[1:4], ncol = 4,
  cmap = anatomy_cmap, range = "robust",
  title = "Light style", style = "light"
)

## ----ortho, fig.cap = "Axial, coronal and sagittal views through one voxel.", fig.alt = "Three orthogonal slices with crosshairs.", fig.height = 3.6, dev.args = list(bg = "#141414")----
plot_ortho(anat,
  coord = round(dim(anat) / 2), unit = "index",
  cmap = anatomy_cmap, title = "Three-plane view", style = "dark"
)

## ----ortho-panels-------------------------------------------------------------
names(plot_ortho(anat, coord = round(dim(anat) / 2), assemble = FALSE))

## ----overlay, fig.cap = "Thresholded signed t-map over the anatomy.", fig.alt = "Six anatomical slices with blue and orange statistical clusters.", fig.height = 3.6, dev.args = list(bg = "#141414")----
plot_overlay(
  bgvol = anat, overlay = stat_map,
  zlevels = zlevels, ncol = 6,
  bg_cmap = anatomy_cmap, bg_range = "robust",
  ov_cmap = "coldhot", ov_thresh = 3, ov_symmetric = TRUE, ov_alpha = 0.8,
  title = "t > 3", style = "dark"
)

## ----overlay-counts-----------------------------------------------------------
a <- as.array(stat_map)
c(positive = sum(a > 3), negative = sum(a < -3), total_brain = sum(a != 0))

## ----enhance------------------------------------------------------------------
enhanced <- enhance_stat_map(stat_map)

raw <- as.array(stat_map)
enh <- as.array(enhanced)

# Judge the noise floor on voxels chosen from the RAW map, so both columns
# describe the same voxels rather than each map's own quiet region.
quiet <- which(as.vector(brain) & abs(raw) < 2)

round(rbind(
  raw = c(peak = max(raw), above_3 = sum(raw > 3), quiet_sd = sd(raw[quiet])),
  enhanced = c(max(enh), sum(enh > 3), sd(enh[quiet]))
), 3)

## ----checkerboard, fig.cap = "Checkerboard against a copy shifted by four voxels. Anatomy steps across tile boundaries.", fig.alt = "Three large checkerboard slices; brain structures are offset between alternating tiles.", fig.height = 4.6, dev.args = list(bg = "#141414")----
shifted <- demo_shifted(anat, by = 4L)

plot_checkerboard(anat, shifted,
  zlevels = zlevels[2:4], tile = 12, ncol = 3,
  cmap = anatomy_cmap, title = "Checkerboard QC", style = "dark"
)

## ----edges, fig.cap = "Fixed and moving edge maps. Where the colours separate, the images disagree.", fig.alt = "Edge overlay slices with two colours of outline separating.", fig.height = 3.6, dev.args = list(bg = "#141414")----
fixed_edges <- demo_edges(brain)
moving_edges <- demo_edges(demo_shifted(brain, by = 4L))

plot_edge_overlay(anat, fixed_edges, moving_edges,
  zlevels = zlevels, ncol = 6,
  bg_cmap = anatomy_cmap, title = "Edge overlay QC", style = "dark"
)

## ----colours------------------------------------------------------------------
coldhot <- resolve_cmap("coldhot")
head(coldhot, 3)

probe <- c(-4, -1, 0, 1, 4)
mapToColors(probe, col = coldhot, irange = c(-4, 4))

## ----colours-threshold--------------------------------------------------------
mapToColors(probe, col = coldhot, irange = c(-4, 4), zero_col = "#00FF00")
mapToColors(probe, col = coldhot, irange = c(-4, 4), threshold = c(-2, 2))

