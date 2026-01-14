params <-
list(family = "red")

## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(purrr)
library(assertthat)
library(neuroim2)
options(mc.cores=1)

## -----------------------------------------------------------------------------
      library(purrr)
      library(ggplot2)
      file_name <- system.file("extdata", "global_mask_v4.nii", package="neuroim2")
      vec <- read_vec(file_name)
      dim(vec)
      vec

## -----------------------------------------------------------------------------
    
      file_name <- system.file("extdata", "global_mask_v4.nii", package="neuroim2")
      vec_multi <- read_vec(c(file_name, file_name, file_name))
      dim(vec_multi)
      
      vec2 <- read_vec(rep(file_name, 10))
      vec2

## -----------------------------------------------------------------------------
      # Extract a subset of volumes (first 3 timepoints)
      vec_1_3 <- sub_vector(vec, 1:3)
      dim(vec_1_3)
      vec_1_3

## -----------------------------------------------------------------------------
      
      series(vec_1_3, 1,1,1)

## -----------------------------------------------------------------------------
      file_name <- system.file("extdata", "global_mask_v4.nii", package="neuroim2")
      vol <- read_vol(file_name)
      roi <- spherical_roi(vol, c(12,12,12), radius=8)
      rvec1 <- series_roi(vec, roi)
      
      ## or alternatively as a pipeline
      rvec2 <- read_vol(file_name) %>% spherical_roi(c(12,12,12), radius=8) %>% series_roi(vec,.)
      rvec2
      
      ## we can extract the ROI values with the `values` method.
      assertthat::assert_that(all(values(rvec1) == values(rvec2)))
      assertthat::assert_that(all(coords(rvec1) == coords(rvec2)))
      

## -----------------------------------------------------------------------------

r1 <- series_roi(vec, 1:100)
r1

## -----------------------------------------------------------------------------
r2 <- series(vec, 1:100)
dim(r2)

## -----------------------------------------------------------------------------
mask <- read_vol(system.file("extdata", "global_mask_v4.nii", package="neuroim2"))

## -----------------------------------------------------------------------------
vox <- index_to_grid(mask, 1:100)

r3 <- series(vec, vox)
dim(r3)

## -----------------------------------------------------------------------------
r4 <- series_roi(vec,vox)
r4

## -----------------------------------------------------------------------------
sp <- space(vec)
sp                   # dimensions, spacing, origin, axes
dim(vec)             # 4D dims: X × Y × Z × T
spacing(vec)         # voxel spacing (mm)
origin(vec)          # image origin
ndim(vec)            # == 4 for time series

## -----------------------------------------------------------------------------
m <- mask(vec)
m

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
write_vec(vec_1_3, tmp_vec)
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

