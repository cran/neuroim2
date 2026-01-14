params <-
list(family = "red")

## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(neuroim2)
library(purrr)
library(assertthat)

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vec <- read_vec(file_name)  # 4D NeuroVec

reduce_ts_to_vol <- function(x, FUN) {
  dm <- dim(x)
  stopifnot(length(dm) == 4)
  mat <- as.matrix(x)         # voxels × time
  vals <- apply(mat, 1, FUN)  # one value per voxel
  NeuroVol(array(vals, dm[1:3]), drop_dim(space(x)))
}

# Example: temporal mean volume
mean_vol <- reduce_ts_to_vol(vec, mean)
mean_vol

## -----------------------------------------------------------------------------
space4 <- NeuroSpace(c(10, 10, 10, 9), c(1, 1, 1))
vec4d  <- NeuroVec(array(rnorm(10 * 10 * 10 * 9),
                         dim = c(10, 10, 10, 9)),
                   space4)

# Suppose timepoints 1–3 belong to block 1, 4–6 to block 2, 7–9 to block 3
block_idx <- c(1, 1, 1,
               2, 2, 2,
               3, 3, 3)

idx_list <- split(seq_len(dim(vec4d)[4]), block_idx)
blocks <- lapply(idx_list, function(ii) sub_vector(vec4d, ii))

length(blocks)        # 3 blocks
dim(blocks[[1]])      # first block: 10×10×10×3
dim(blocks[[2]])      # second block: 10×10×10×3
dim(blocks[[3]])      # third block: 10×10×10×3

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vec <- read_vec(file_name)  # DenseNeuroVec in memory

# Convert to a memory-mapped representation backed by a temporary .nii file
mvec <- as_mmap(vec)
mvec

# Or explicitly choose an output file (must be uncompressed for mmap)
tmp_nii <- tempfile(fileext = ".nii")
mvec2   <- as_mmap(vec, file = tmp_nii, overwrite = TRUE)

inherits(mvec2, "MappedNeuroVec")

## -----------------------------------------------------------------------------
bspace <- NeuroSpace(c(10, 10, 10), c(1, 1, 1))
vol    <- NeuroVol(array(rnorm(10 * 10 * 10), c(10, 10, 10)), bspace)

# Simple 3×3×3 mean kernel
kern <- Kernel(c(3, 3, 3), vdim = c(3, 3, 3))

smoothed_vol <- mapf(vol, kern)
smoothed_vol

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vec <- read_vec(file_name)

# Fake cluster labels over the full 3D grid
n_vox  <- prod(dim(vec)[1:3])
cl_lab <- sample(1:5, n_vox, replace = TRUE)

roi_list <- split_clusters(vec, cl_lab)

length(roi_list)   # number of non-empty clusters
roi_list[[1]]      # ROIVec for cluster "1"
coords(roi_list[[1]])[1:5, ]   # first few voxel coordinates
dim(values(roi_list[[1]]))     # time × voxels in that cluster

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
vec <- read_vec(file_name)

n_vox <- prod(dim(vec)[1:3])

# Assign each voxel to one of three arbitrary groups
fac <- factor(sample(1:3, n_vox, replace = TRUE))

# Default: mean over voxels in each group (per timepoint)
group_ts <- split_reduce(vec, fac)

dim(group_ts)         # groups × timepoints
rownames(group_ts)    # "1", "2", "3"

## -----------------------------------------------------------------------------
sp3  <- NeuroSpace(c(10, 10, 10), c(1, 1, 1))
vol1 <- NeuroVol(array(rnorm(10 * 10 * 10), c(10, 10, 10)), sp3)
vol2 <- NeuroVol(array(rnorm(10 * 10 * 10), c(10, 10, 10)), sp3)
vol3 <- NeuroVol(array(rnorm(10 * 10 * 10), c(10, 10, 10)), sp3)

# Concatenate volumes into a 4D NeuroVec (time dimension length 3)
vec_3 <- concat(vol1, vol2, vol3)

dim(vec_3)   # 10 × 10 × 10 × 3
space(vec_3) # inherits spatial metadata from inputs

## -----------------------------------------------------------------------------
file_name <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")

run1 <- read_vec(file_name)          # 4D NeuroVec
run2 <- read_vec(file_name)          # same space and shape

# Concatenate timepoints: result has dim(...)[4] = dim(run1)[4] + dim(run2)[4]
run12 <- concat(run1, run2)

dim(run1)
dim(run2)
dim(run12)

## -----------------------------------------------------------------------------
sp  <- NeuroSpace(c(10, 10, 10), c(1, 1, 1))
arr <- array(0, c(10, 10, 10))

# Two small 2×2×2 clusters in opposite corners
arr[1:2, 1:2, 1:2] <- 1
arr[8:9, 8:9, 8:9] <- 1

vol <- NeuroVol(arr, sp)

# Find connected components above threshold 0 (26-connectivity by default)
cc <- conn_comp(vol, threshold = 0)

max(cc$index)      # number of clusters (should be 2)
cc$size[cc$size > 0]  # cluster sizes in voxels

## -----------------------------------------------------------------------------
cc2 <- conn_comp(vol, threshold = 0, cluster_table = TRUE)
head(cc2$cluster_table)

