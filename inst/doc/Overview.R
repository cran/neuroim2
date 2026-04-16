params <-
list(family = "red", preset = "homage")

## ----setup, include = FALSE---------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("albersdown", quietly = TRUE)) ggplot2::theme_set(albersdown::theme_albers(family = params$family, preset = params$preset))
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)
suppressPackageStartupMessages(library(neuroim2))

## ----quick-start--------------------------------------------------------------
img <- read_vol(system.file("extdata", "global_mask2.nii.gz", package = "neuroim2"))

dim(img)
spacing(img)
origin(img)

## ----core-objects-------------------------------------------------------------
mask <- img > 0
sum(mask)

vec <- read_vec(system.file("extdata", "global_mask_v4.nii", package = "neuroim2"))
dim(vec)

roi <- spherical_roi(space(vec), c(45, 45, 20), radius = 4)
length(roi)

## ----roi-workflow-------------------------------------------------------------
roi_ts <- series_roi(vec, roi)
roi_mat <- values(roi_ts)
mean_ts <- rowMeans(roi_mat)

stopifnot(
  nrow(roi_mat) == dim(vec)[4],
  ncol(roi_mat) == length(roi),
  all(is.finite(mean_ts))
)

head(mean_ts)

## ----spatial-ops--------------------------------------------------------------
img_down <- downsample(img, spacing = c(2, 2, 2))

dim(img)
dim(img_down)
spacing(img_down)

## ----filebacked-example, eval = FALSE-----------------------------------------
# big_vec <- read_vec(
#   system.file("extdata", "global_mask_v4.nii", package = "neuroim2"),
#   mode = "filebacked"
# )
# 
# series(big_vec, 45, 45, 20)

## ----eval = FALSE-------------------------------------------------------------
# help(package = "neuroim2")
# help.search("roi", package = "neuroim2")

