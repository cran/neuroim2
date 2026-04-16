params <-
list(family = "red", preset = "homage")

## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)
suppressPackageStartupMessages(library(neuroim2))

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vec <- read_vec(file_name)

class(vec)
dim(vec)

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
mask_vol <- read_vol(file_name) > 0
svec <- read_vec(file_name, mask = mask_vol)

class(svec)
dim(svec)
sum(mask(svec))

## ----eval = FALSE-------------------------------------------------------------
# file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
# mvec <- read_vec(file_name, mode = "mmap")
# 
# series(mvec, 1, 1, 1)
# sub_vector(mvec, 1:5)

## ----eval = FALSE-------------------------------------------------------------
# file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
# fbvec <- read_vec(file_name, mode = "filebacked")
# 
# fbvec[[1]]
# sub_vector(fbvec, 1:3)

## ----eval = FALSE-------------------------------------------------------------
# img5d <- read_image("some_5d_image.nii.gz")
# class(img5d)

