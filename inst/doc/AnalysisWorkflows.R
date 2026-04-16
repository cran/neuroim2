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
suppressPackageStartupMessages({
  library(neuroim2)
  library(purrr)
})

## ----load-demo----------------------------------------------------------------
vec_file <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vol <- read_vol(vec_file)
vec <- read_vec(vec_file)

## ----roi-example--------------------------------------------------------------
roi <- spherical_roi(vol, c(12, 12, 12), radius = 6)
roi_ts <- series_roi(vec, roi)

dim(values(roi_ts))

## ----roi-example-check--------------------------------------------------------
stopifnot(length(roi) > 0L)
stopifnot(nrow(values(roi_ts)) == dim(vec)[4])

## ----cluster-setup------------------------------------------------------------
set.seed(1)

mask_vol <- vol > 0
cluster_ids <- sample(1:4, sum(mask_vol), replace = TRUE)
clustered <- ClusteredNeuroVol(mask_vol, cluster_ids)
parts <- split_clusters(vec, clustered)

length(parts)

## ----cluster-summary----------------------------------------------------------
part_means <- map_dbl(parts, ~ mean(values(.)))
part_means

## ----cluster-check------------------------------------------------------------
stopifnot(length(parts) == 4L)
stopifnot(all(is.finite(part_means)))

## ----searchlight-demo---------------------------------------------------------
sl <- searchlight(mask_vol, radius = 4, eager = FALSE, nonzero = FALSE)
first_sl <- sl[[1]]

nrow(coords(first_sl))

## ----searchlight-check--------------------------------------------------------
stopifnot(nrow(coords(first_sl)) > 0L)

## ----map-demo-----------------------------------------------------------------
first_five <- lapply(seq_len(5), function(i) sl[[i]])
first_five_means <- map_dbl(first_five, ~ mean(values(.)))

first_five_means

## ----map-check----------------------------------------------------------------
stopifnot(length(first_five_means) == 5L)
stopifnot(all(is.finite(first_five_means)))

