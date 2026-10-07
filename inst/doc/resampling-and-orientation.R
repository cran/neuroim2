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

## ----data---------------------------------------------------------------------
anat <- demo_anatomy()
mask <- demo_mask()

## ----downsample---------------------------------------------------------------
half <- downsample(anat, factor = 0.5)

dim(anat)
dim(half)
round(spacing(half), 3)

## ----downsample-fig, fig.cap = "Full and half resolution, drawn at matched physical extent.", fig.alt = "Two axial slices, the second visibly blockier.", fig.height = 3.4----
op <- par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))
image(seq_len(dim(anat)[1]), seq_len(dim(anat)[2]), anat[, , 24],
  main = "original", col = gray.colors(256), axes = FALSE, ann = TRUE,
  xlab = "", ylab = "", asp = 1
)
image(seq(1, dim(anat)[1], length.out = dim(half)[1]),
  seq(1, dim(anat)[2], length.out = dim(half)[2]), half[, , 12],
  main = "factor = 0.5", col = gray.colors(256), axes = FALSE, ann = TRUE,
  xlab = "", ylab = "", asp = 1
)
par(op)

## ----bad-target---------------------------------------------------------------
affine_to_axcodes(trans(mask))

naive <- NeuroSpace(round(dim(mask) * 1.6), spacing(mask) / 1.6, origin = origin(mask))
affine_to_axcodes(trans(naive))

## ----bad-resample-------------------------------------------------------------
sum(resample_to(mask, naive, method = "nearest"))

## ----good-target--------------------------------------------------------------
tr <- trans(mask)
tr[1:3, 1:3] <- tr[1:3, 1:3] / 1.6

finer <- NeuroSpace(round(dim(mask) * 1.6), trans = tr)

affine_to_axcodes(trans(finer))
round(spacing(finer), 3)

## ----good-resample------------------------------------------------------------
up <- resample_to(mask, finer, method = "nearest")

expected <- sum(mask) * prod(dim(finer) / dim(mask))
c(source = sum(mask), resampled = sum(up), expected = expected)

abs(sum(up) / expected - 1)

## ----interpolation------------------------------------------------------------
lin <- resample_to(mask, finer, method = "linear")
a <- as.array(lin)

fractional <- sum(abs(a - round(a)) > 1e-6)
c(voxels = fractional, of_resampled_mask = fractional / sum(up))

## ----match-image--------------------------------------------------------------
coarse <- downsample(mask, factor = 0.5) # used only as a grid donor
matched <- resample_to(mask, coarse, method = "nearest")

identical(dim(matched), dim(coarse))
identical(trans(space(matched)), trans(space(coarse)))

## ----reorient-----------------------------------------------------------------
ras <- reorient(space(mask), c("R", "A", "S"))

affine_to_axcodes(trans(space(mask)))
affine_to_axcodes(trans(ras))

## ----reorient-moves-----------------------------------------------------------
rbind(
  before = as.vector(grid_to_coord(space(mask), matrix(c(1, 1, 1), nrow = 1))),
  after = as.vector(grid_to_coord(ras, matrix(c(1, 1, 1), nrow = 1)))
)

## ----reorient-resample--------------------------------------------------------
flipped <- resample_to(mask, reorient(space(mask), c("R", "A", "S")), method = "nearest")

c(source = sum(mask), flipped = sum(flipped))

## ----reorient-permute---------------------------------------------------------
permuted <- reorient(space(mask), c("P", "S", "R"))
lost <- resample_to(mask, permuted, method = "nearest")

c(source = sum(mask), permuted = sum(lost), kept = sum(lost) / sum(mask))

## ----oblique------------------------------------------------------------------
aff <- matrix(c(
   3.0,  0.3,  0.0,  -90,
   0.0,  3.0,  0.15, -126,
   0.0,  0.0,  4.0,  -72,
   0.0,  0.0,  0.0,    1
), nrow = 4, byrow = TRUE)

tilted <- NeuroVol(array(rnorm(64 * 64 * 30), c(64, 64, 30)),
                   NeuroSpace(c(64L, 64L, 30L), trans = aff))

round(obliquity(trans(space(tilted))) * 180 / pi, 2)

## ----deoblique----------------------------------------------------------------
straight <- deoblique(tilted)

rbind(before = c(dim(tilted), round(spacing(space(tilted)), 2)),
      after = c(dim(straight), round(spacing(space(straight)), 2)))

## ----deoblique-mask-----------------------------------------------------------
frac <- function(x) mean(abs(as.array(x) - round(as.array(x))) > 1e-6)

c(linear = frac(deoblique(mask)), nearest = frac(deoblique(mask, method = "nearest")))

## ----canonical----------------------------------------------------------------
affine_to_axcodes(trans(space(mask)))
affine_to_axcodes(trans(space(as_canonical(mask))))

