params <-
list(family = "red", preset = "interaction")

## ----setup, include = FALSE---------------------------------------------------
if (requireNamespace("ragg", quietly = TRUE)) knitr::opts_chunk$set(dev = "ragg_png")
if (requireNamespace("systemfonts", quietly = TRUE)) albersdown::albers_register_fonts()
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("albersdown", quietly = TRUE)) ggplot2::theme_set(albersdown::theme_albers(family = params$family, preset = params$preset))
source("_common.R")

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

## ----anatomy, fig.cap = "plot() on a NeuroVol: a montage of evenly spaced axial slices.", fig.alt = "Nine axial slices of an anatomical brain image.", fig.height = 6.5----
anat <- read_vol(system.file("extdata", "mni_downsampled.nii.gz", package = "neuroim2"))
plot(anat, zlevels = round(seq(6, 40, length.out = 9)))

## ----array-like---------------------------------------------------------------
dim(anat)
anat[24, 28, 24]
max(anat / 2)

## ----still-an-image-----------------------------------------------------------
class(anat > 100)[1]

## ----mask---------------------------------------------------------------------
mask <- read_vol(system.file("extdata", "global_mask2.nii.gz", package = "neuroim2"))

spacing(mask)
affine_to_axcodes(trans(mask))

## ----coord--------------------------------------------------------------------
g <- coord_to_grid(mask, c(-34, -28, 10))
g

## ----coord-back---------------------------------------------------------------
grid_to_coord(mask, matrix(g, nrow = 1))

## ----coord-round--------------------------------------------------------------
grid_to_coord(mask, matrix(round(g), nrow = 1))

## ----bold---------------------------------------------------------------------
bold <- simulate_fmri(mask, n_time = 60, seed = 1)

dim(bold)

## ----series, fig.cap = "One voxel's simulated BOLD time course.", fig.alt = "Line plot of a single voxel time series over 60 scans."----
vox <- round(g)
ts <- series(bold, vox[1], vox[2], vox[3])

plot(ts, type = "l", xlab = "scan", ylab = "signal", main = "Single voxel")

## ----roi----------------------------------------------------------------------
roi <- spherical_roi(mask, vox, radius = 10, nonzero = TRUE)
length(roi)

## ----roi-values---------------------------------------------------------------
roi_ts <- series_roi(bold, roi)
dim(values(roi_ts))

## ----roi-mean, fig.cap = "Single voxel and region mean on a shared axis. Averaging halves the amplitude.", fig.alt = "Two time series on the same axes; the region mean has visibly smaller swings.", fig.height = 3.6----
roi_mean <- rowMeans(values(roi_ts))

plot(ts, type = "l", col = "grey60", xlab = "scan", ylab = "signal",
     main = paste("1 voxel vs mean of", length(roi)))
lines(roi_mean, lwd = 2)
legend("topright", c("voxel", "ROI mean"), col = c("grey60", "black"),
       lwd = c(1, 2), bty = "n")

## ----roi-sd-------------------------------------------------------------------
c(voxel = sd(ts), roi_mean = sd(roi_mean))

## ----sdmap, fig.cap = "Temporal standard deviation per voxel, on a robust intensity range.", fig.alt = "Nine axial slices of a temporal standard deviation map.", fig.height = 6.5----
mat <- as.matrix(bold)
sd_map <- NeuroVol(apply(mat, 1, sd), drop_dim(space(bold)))

plot(sd_map,
  zlevels = round(seq(4, 22, length.out = 9)),
  irange = c(0, quantile(sd_map[sd_map > 0], 0.99))
)

## ----write--------------------------------------------------------------------
out <- tempfile(fileext = ".nii.gz")
write_vol(sd_map, out)

back <- read_vol(out)
all.equal(spacing(back), spacing(sd_map))
max(abs(back - sd_map))

## ----cleanup, include = FALSE-------------------------------------------------
unlink(out)

