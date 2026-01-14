## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(neuroim2)

## -----------------------------------------------------------------------------
demo_path <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vol  <- read_vol(demo_path)  # 3D NeuroVol
vec4 <- read_vec(demo_path)  # 4D NeuroVec

sp   <- space(vol)
dim(vol)
spacing(vol)

## -----------------------------------------------------------------------------
# Create a target space with 2× smaller voxels in each dimension
sp_fine <- NeuroSpace(
  dim    = sp@dim * 2L,
  spacing = sp@spacing / 2,
  origin  = sp@origin,
  trans   = trans(vol)
)

vol_fine_lin <- resample_to(vol, sp_fine, method = "linear")
dim(vol_fine_lin)
spacing(vol_fine_lin)

## -----------------------------------------------------------------------------
# Downsample by a factor of 0.5 in each spatial dimension
vec_down_factor <- downsample(vec4, factor = 0.5)

dim(vec4)
dim(vec_down_factor)
spacing(vec4)
spacing(vec_down_factor)

## -----------------------------------------------------------------------------
# Target spacing (mm)
vec_down_spacing <- downsample(vec4, spacing = c(4, 4, 4))

# Target spatial dimensions
vec_down_outdim  <- downsample(vec4, outdim = c(32, 32, 16))

## -----------------------------------------------------------------------------
vol_down_factor <- downsample(vol, factor = 0.5)

dim(vol)
dim(vol_down_factor)

## -----------------------------------------------------------------------------
sp_lpi <- sp                      # assume input is LPI‑like
sp_ras <- reorient(sp_lpi, c("R", "A", "S"))

sp_lpi
sp_ras

## -----------------------------------------------------------------------------
vol_ras <- NeuroVol(as.array(vol), sp_ras)
dim(vol_ras)
spacing(vol_ras)

