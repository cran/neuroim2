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

## ----read-volume--------------------------------------------------------------
vol_file <- system.file("extdata", "global_mask2.nii.gz", package = "neuroim2")
vol <- read_vol(vol_file)

dim(vol)
spacing(vol)
origin(vol)

## ----volume-check-------------------------------------------------------------
stopifnot(length(dim(vol)) == 3L)
stopifnot(all(spacing(vol) > 0))

## ----inspect-space------------------------------------------------------------
space(vol)
vol[1, 1, 1]

## ----read-vector--------------------------------------------------------------
vec_file <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vec <- read_vec(vec_file)

dim(vec)
vec

## ----vector-check-------------------------------------------------------------
stopifnot(length(dim(vec)) == 4L)
stopifnot(dim(vec)[4] > 1L)

## ----subset-vector------------------------------------------------------------
first_two <- sub_vector(vec, 1:2)

dim(first_two)

## ----subset-check-------------------------------------------------------------
stopifnot(dim(first_two)[4] == 2L)

## ----matrix-view--------------------------------------------------------------
mat <- as.matrix(vec)

dim(mat)
mat[1:4, 1:2]

## ----matrix-check-------------------------------------------------------------
stopifnot(nrow(mat) == prod(dim(vec)[1:3]))
stopifnot(ncol(mat) == dim(vec)[4])

## ----voxel-series-------------------------------------------------------------
voxel_ts <- series(vec, 12, 12, 12)
voxel_ts

## ----roi-series---------------------------------------------------------------
roi <- spherical_roi(drop(first_two[[1]]), c(12, 12, 12), radius = 6)
roi_ts <- series_roi(vec, roi)

dim(values(roi_ts))

## ----roi-check----------------------------------------------------------------
stopifnot(length(roi) > 0L)
stopifnot(nrow(values(roi_ts)) == dim(vec)[4])

## ----sparse-vector------------------------------------------------------------
mask_vol <- read_vol(vec_file) > 0
sparse_vec <- read_vec(vec_file, mask = mask_vol)

class(sparse_vec)
dim(sparse_vec)

## ----sparse-check-------------------------------------------------------------
stopifnot(inherits(sparse_vec, "SparseNeuroVec"))
stopifnot(identical(dim(sparse_vec), dim(vec)))

