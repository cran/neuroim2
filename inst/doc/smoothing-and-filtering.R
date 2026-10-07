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
brain <- demo_anatomy_mask()

spacing(anat)

## ----inmask-------------------------------------------------------------------
inside <- which(as.vector(brain))
baseline <- as.array(anat)[inside]

## ----panel-helper-------------------------------------------------------------
compare <- function(..., z = 24) {
  imgs <- list(...)
  zlim <- range(vapply(imgs, function(v) range(v[, , z]), numeric(2)))
  op <- par(mfrow = c(1, length(imgs)), mar = c(0.5, 0.5, 2, 0.5))
  for (nm in names(imgs)) {
    image(imgs[[nm]][, , z],
      main = nm, zlim = zlim, col = gray.colors(256), axes = FALSE, asp = 1
    )
  }
  par(op)
}

## ----gaussian, fig.cap = "Gaussian smoothing at two kernel widths.", fig.alt = "Three axial slices: original, lightly smoothed, heavily smoothed.", fig.height = 3.2----
light <- gaussian_blur(anat, brain, fwhm = 4)
heavy <- gaussian_blur(anat, brain, fwhm = 9)

compare(original = anat, `FWHM 4 mm` = light, `FWHM 9 mm` = heavy)

## ----psf----------------------------------------------------------------------
# The point-spread function is the operator: smooth an impulse, read the width
# back out. This is the only honest check that a request was honoured.
psf_fwhm <- function(v, sp) {
  a <- as.array(v); a[a < 0] <- 0
  prof <- apply(a, 1, sum); off <- (seq_along(prof) - 21) * sp
  mu <- sum(prof * off) / sum(prof)
  2 * sqrt(2 * log(2)) * sqrt(sum(prof * (off - mu)^2) / sum(prof))
}
imp_sp <- NeuroSpace(c(41L, 41L, 41L), c(2, 2, 2))
imp <- array(0, c(41, 41, 41)); imp[21, 21, 21] <- 1
imp <- NeuroVol(imp, imp_sp)

c(
  `asked 4 mm` = psf_fwhm(gaussian_blur(imp, fwhm = 4, normalize = FALSE), 2),
  `asked 9 mm` = psf_fwhm(gaussian_blur(imp, fwhm = 9, normalize = FALSE), 2)
)

## ----sigma-window-------------------------------------------------------------
c(
  `sigma 4, window 1` = psf_fwhm(gaussian_blur(imp, sigma = 4, window = 1, normalize = FALSE), 2),
  `sigma 4, window 2` = psf_fwhm(gaussian_blur(imp, sigma = 4, window = 2, normalize = FALSE), 2),
  `sigma 4, derived`  = psf_fwhm(gaussian_blur(imp, sigma = 4, normalize = FALSE), 2),
  `requested`         = 2 * sqrt(2 * log(2)) * 4
)

## ----edge-preserving, fig.cap = "Gaussian blur against two edge-preserving filters at matched support.", fig.alt = "Four axial slices comparing original, Gaussian, guided and bilateral filtering.", fig.height = 3.2----
sd_in <- sd(baseline)

guided <- guided_filter(anat, radius = 1, epsilon = (0.6 * sd_in)^2)
bilateral <- bilateral_filter(anat, brain, spatial_sigma = 2, intensity_sigma = 1, window = 1)

compare(original = anat, gaussian = light, guided = guided, bilateral = bilateral)

## ----edge-numbers-------------------------------------------------------------
score <- function(x) {
  v <- as.array(x)[inside]
  removed <- 1 - var(v) / var(baseline)
  distortion <- 1 - cor(v, baseline)
  c(sd = sd(v), removed = removed, distortion = distortion,
    ratio = removed / distortion)
}

round(rbind(
  original = score(anat),
  gaussian = score(light),
  guided = score(guided),
  bilateral = score(bilateral)
), 3)

## ----sharpen, fig.cap = "Laplacian enhancement increases local contrast rather than reducing it.", fig.alt = "Two axial slices: original and sharpened.", fig.height = 3.4----
sharp <- laplace_enhance(anat, brain, k = 2, patch_size = 3, search_radius = 1, h = 0.7)

compare(original = anat, enhanced = sharp)

## ----sharpen-numbers----------------------------------------------------------
outside <- which(as.vector(brain) == 0)
sharp_arr <- as.array(sharp)

round(rbind(
  original = c(inside = sd(baseline), outside = sd(as.array(anat)[outside])),
  enhanced = c(sd(sharp_arr[inside]), sd(sharp_arr[outside]))
), 2)

## ----sharpen-range------------------------------------------------------------
range(sharp_arr)

## ----bold---------------------------------------------------------------------
bold_mask <- demo_mask()
bold <- demo_bold(n_time = 40)

attr(bold, "TR")

## ----bilat4d------------------------------------------------------------------
bf4d <- bilateral_filter_4d(
  bold, bold_mask,
  spatial_sigma = 4, intensity_sigma = 1, temporal_sigma = 1,
  spatial_window = 1, temporal_window = 1,
  temporal_spacing = attr(bold, "TR")
)

## ----cgb----------------------------------------------------------------------
out <- cgb_filter(
  bold, mask = bold_mask,
  spatial_sigma = 5, window = 1,
  corr_map = "power", corr_param = 2,
  topk = 16, passes = 1, lambda = 1,
  return_graph = TRUE
)

## ----fourd-numbers------------------------------------------------------------
set.seed(1)
sample_idx <- sample(which(as.vector(bold_mask) > 0), 300)

noise <- function(x) mean(apply(as.matrix(x)[sample_idx, ], 1, sd))

c(raw = noise(bold), bilateral_4d = noise(bf4d), cgb = noise(out$result))

## ----cgb-reuse----------------------------------------------------------------
c(
  raw = noise(bold),
  pass_1 = noise(out$result),
  pass_2 = noise(cgb_smooth(bold, out$graph, passes = 2))
)

