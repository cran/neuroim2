params <-
list(family = "red", preset = "homage")

## ----setup, echo = FALSE, message = FALSE-------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("albersdown", quietly = TRUE)) ggplot2::theme_set(albersdown::theme_albers(family = params$family, preset = params$preset))
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)
suppressPackageStartupMessages({
  library(purrr)
  library(neuroim2)
})
options(mc.cores = 1)

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vec <- read_vec(file_name)
vec_multi <- read_vec(c(file_name, file_name, file_name))
dim(vec_multi)

vec2 <- read_vec(rep(file_name, 10))
vec2

## -----------------------------------------------------------------------------
set.seed(1)
dims <- c(24, 24, 24, 5)
arr  <- array(rnorm(prod(dims)), dims)
sp4  <- NeuroSpace(dims, spacing = c(2,2,2))
dvec <- NeuroVec(arr, sp4)
dim(dvec)

## -----------------------------------------------------------------------------
mat  <- matrix(rnorm(prod(dims)), nrow = prod(dims[1:3]), ncol = dims[4])
dvec2 <- DenseNeuroVec(mat, sp4)
all.equal(dim(dvec), dim(dvec2))

## -----------------------------------------------------------------------------
vec_z <- scale_series(dvec, center = TRUE, scale = TRUE)
dim(vec_z)

## -----------------------------------------------------------------------------
M      <- as.matrix(dvec)              # voxels × time
vmean  <- rowMeans(M)                  # per-voxel mean
mean3d <- NeuroVol(vmean, drop_dim(space(dvec)))
mean3d

## -----------------------------------------------------------------------------
dvec_more <- concat(dvec, dvec)        # doubles the time dimension
dim(dvec_more)

## -----------------------------------------------------------------------------
# Build a random mask and convert a dense vec to sparse
mask_arr <- array(runif(prod(dims[1:3])) > 0.7, dims[1:3])
mask_vol <- LogicalNeuroVol(mask_arr, drop_dim(sp4))

svec <- as.sparse(dvec, mask_vol)     # SparseNeuroVec with explicit mask
svec                                 # note the stored mask and cardinality

# Convert back to dense if needed
dense_again <- as.dense(svec)
all.equal(dim(dense_again), dim(dvec))

## -----------------------------------------------------------------------------
dv_dense <- DenseNeuroVec(as.matrix(vec), space(vec))

## -----------------------------------------------------------------------------
tmp_vec <- tempfile(fileext = ".nii.gz")
write_vec(sub_vector(vec, 1:3), tmp_vec)
file.exists(tmp_vec)
unlink(tmp_vec)

## -----------------------------------------------------------------------------
vol3d <- read_vol(system.file("extdata", "global_mask_v4.nii", package="neuroim2"))
roi   <- spherical_roi(vol3d, c(12,12,12), radius = 6)
rts   <- series_roi(vec, roi)          # ROIVec (T × N with coords)

# z-score each column (voxel) across time, then average within ROI
mat_roi  <- values(rts)                # T × N
mat_z    <- base::scale(mat_roi, center=TRUE, scale=TRUE)
roi_mean <- rowMeans(mat_z)
length(roi_mean)                       # matches time dimension

