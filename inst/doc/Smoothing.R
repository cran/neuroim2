## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(neuroim2)

## -----------------------------------------------------------------------------
set.seed(1)

demo_path <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vol3d <- read_vol(demo_path)          # 3D volume
vec4d <- read_vec(demo_path)          # 4D NeuroVec (same file)

sp3  <- space(vol3d)
dims <- dim(vol3d)

## -----------------------------------------------------------------------------
blur_light <- gaussian_blur(vol3d, vol3d, sigma = 2, window = 1)
blur_strong <- gaussian_blur(vol3d, vol3d, sigma = 4, window = 2)

dim(blur_light)

## -----------------------------------------------------------------------------
gf_vol <- guided_filter(vol3d, radius = 4, epsilon = 0.7^2)
gf_vol

## -----------------------------------------------------------------------------
bf_vol <- bilateral_filter(
  vol3d,
  spatial_sigma   = 2,
  intensity_sigma = 1,
  window          = 1
)
bf_vol

## -----------------------------------------------------------------------------
sharp_vol <- laplace_enhance(vol3d, k = 2, patch_size = 3,
                             search_radius = 2, h = 0.7)
sharp_vol

## -----------------------------------------------------------------------------
mask3d <- read_vol(system.file("extdata", "global_mask_v4.nii",
                               package = "neuroim2"))

bf4d <- bilateral_filter_4d(
  vec4d, mask3d,
  spatial_sigma   = 2,
  intensity_sigma = 1,
  temporal_sigma  = 1,
  spatial_window  = 1,
  temporal_window = 1,
  temporal_spacing = 1
)

dim(bf4d)

## -----------------------------------------------------------------------------
cgf <- cgb_filter(
  vec4d,
  mask          = mask3d,
  spatial_sigma = 3,
  window        = NULL,      # auto from spatial_sigma and spacing
  corr_map      = "power",
  corr_param    = 2,
  topk          = 16,
  passes        = 1,
  lambda        = 1
)

dim(cgf)

## -----------------------------------------------------------------------------
cg_out <- cgb_filter(vec4d, mask3d,
                     spatial_sigma = 3, window = NULL,
                     topk = 16, return_graph = TRUE)

str(cg_out$graph)

