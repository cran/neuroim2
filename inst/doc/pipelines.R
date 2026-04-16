params <-
list(family = "red", preset = "homage")

## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", fig.alt = 'Plot output', message = FALSE, warning = FALSE)
suppressPackageStartupMessages({
  library(purrr)
  library(neuroim2)
})
options(mc.cores=1)

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vol <- read_vol(file_name)

## -----------------------------------------------------------------------------
slice_means <- vol %>% slices %>% map_dbl(~ mean(.))
plot(slice_means, type='l', ylab="mean intensity", xlab="slice number")

## -----------------------------------------------------------------------------
vec <- concat(vol,vol,vol,vol,vol)
vec
mean_vec <- vec %>% vols %>% map_dbl(~ mean(.))
sd_vec <- vec %>% vols %>% map_dbl(~ sd(.))
stopifnot(length(mean_vec) == dim(vec)[4])
stopifnot(length(sd_vec) == dim(vec)[4])

## -----------------------------------------------------------------------------
vec <- concat(vol,vol,vol,vol,vol)
vec
mean_vol <- vec %>% vectors() %>% map_dbl(~ mean(.)) %>% NeuroVol(., space=space(vol))
stopifnot(all(dim(mean_vol) == dim(vol)))

