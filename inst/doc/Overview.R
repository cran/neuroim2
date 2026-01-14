params <-
list(family = "red")

## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(neuroim2)

## -----------------------------------------------------------------------------
library(neuroim2)

# Load a 3D brain volume
img <- read_vol(system.file("extdata", "global_mask2.nii.gz", package = "neuroim2"))

# Inspect the spatial properties
space(img)      # Complete spatial information
dim(img)        # Dimensions
spacing(img)    # Voxel sizes
origin(img)     # Origin coordinates

## -----------------------------------------------------------------------------
# Create a synthetic 3D volume
dims <- c(64, 64, 40)
dat <- array(rnorm(prod(dims)), dims)
vol <- NeuroVol(dat, NeuroSpace(dims))

# Basic operations
vol_mean <- mean(vol)
vol_sd <- sd(vol)
cat("Volume mean:", vol_mean, "SD:", vol_sd, "\n")

# Logical volumes for masks
mask <- vol > 0
cat("Voxels above zero:", sum(mask), "\n")

## -----------------------------------------------------------------------------
# Create a 4D time-series (small example)
dims_4d <- c(10, 10, 10, 20)  # 10x10x10 volume, 20 time points
dat_4d <- array(rnorm(prod(dims_4d)), dims_4d)
vec_4d <- NeuroVec(dat_4d, NeuroSpace(dims_4d))

# Extract time-series at a specific voxel
ts <- series(vec_4d, 5, 5, 5)
cat("Time-series length at voxel (5,5,5):", length(ts), "\n")

# Extract a single volume at time point 10
vol_t10 <- vec_4d[,,,10]
cat("Volume at t=10 dimensions:", dim(vol_t10), "\n")

## -----------------------------------------------------------------------------
# Spherical ROI - most common for searchlight analyses
sphere <- spherical_roi(img, c(30, 30, 20), radius = 5)
cat("Spherical ROI contains", length(sphere), "voxels\n")

# Cuboid ROI - rectangular box
cube <- cuboid_roi(space(img), c(30, 30, 20), surround = 3)
cat("Cuboid ROI contains", length(cube), "voxels\n")

# Extract values from the original image using ROI
roi_values <- img[coords(sphere)]
cat("Mean value in spherical ROI:", mean(roi_values), "\n")

## -----------------------------------------------------------------------------
# Create searchlights with 6mm radius
lights <- searchlight(img, radius = 6, eager = FALSE)

# Process first few searchlights (normally you'd process all)
first_5 <- lights[1:5]
means <- sapply(first_5, function(roi) mean(img[coords(roi)]))
cat("First 5 searchlight means:", round(means, 2), "\n")

## -----------------------------------------------------------------------------
# Voxel coordinates to world coordinates (mm)
voxel_coords <- c(30, 30, 20)
world_coords <- grid_to_coord(img, matrix(voxel_coords, nrow = 1))
cat("Voxel", voxel_coords, "-> World", round(world_coords, 2), "mm\n")

# World coordinates back to voxel
voxel_back <- coord_to_grid(img, world_coords)
cat("World", round(world_coords, 2), "-> Voxel", voxel_back, "\n")

# Linear indices
idx <- grid_to_index(img, matrix(voxel_coords, nrow = 1))
cat("Voxel", voxel_coords, "-> Linear index", idx, "\n")

## -----------------------------------------------------------------------------
# Create a sparse representation directly from an ROI
roi <- spherical_roi(img, c(30, 30, 20), radius = 8, fill = 1)

# Convert ROI to sparse volume
sparse_roi <- as.sparse(roi)

# Compare memory usage (convert original ROI to dense for baseline)
dense_vol <- as.dense(roi)
cat("Dense size:", format(object.size(dense_vol), units = "auto"), "\n")
cat("Sparse size:", format(object.size(sparse_roi), units = "auto"), "\n")
cat("Non-zero voxels:", length(roi), "out of", prod(dim(img)), "total\n")
cat("Space savings:", round((1 - as.numeric(object.size(sparse_roi)) / 
                              as.numeric(object.size(dense_vol))) * 100, 1), "%\n")

## ----eval=FALSE---------------------------------------------------------------
# # Create a file-backed 4D dataset (not run in vignette)
# big_vec <- FileBackedNeuroVec(
#   dims = c(91, 109, 91, 1000),  # Standard MNI space, 1000 volumes
#   dtype = "float32",
#   file_name = "big_data.dat"
# )
# 
# # Access works like regular arrays but data stays on disk
# subset <- big_vec[45, 55, 45, 1:10]  # Load only what you need

## -----------------------------------------------------------------------------
# Gaussian smoothing with single sigma value (pass mask explicitly)
img_smooth <- gaussian_blur(img, img, sigma = 2)

# Compare original vs smoothed
orig_vals <- img[30:32, 30:32, 20]
smooth_vals <- img_smooth[30:32, 30:32, 20]
cat("Original variance:", var(as.vector(orig_vals)), "\n")
cat("Smoothed variance:", var(as.vector(smooth_vals)), "\n")

## -----------------------------------------------------------------------------
# Downsample by factor of 2
img_down <- downsample(img, c(2, 2, 2))
cat("Original dimensions:", dim(img), "\n")
cat("Downsampled dimensions:", dim(img_down), "\n")

## -----------------------------------------------------------------------------
# Create a simple parcellation
coords <- index_to_coord(img, which(as.logical(img)))
set.seed(123)
k <- 10  # 10 parcels
if (nrow(coords) > k) {
  km <- kmeans(coords, centers = k, iter.max = 100)
  
  # Create clustered volume
  cvol <- ClusteredNeuroVol(img, km$cluster)
  cat("Created", num_clusters(cvol), "parcels\n")
  
  # Get centroids of each parcel
  centers <- centroids(cvol)
  cat("First parcel centroid:", round(centers[1,], 1), "mm\n")
}

## -----------------------------------------------------------------------------
# Write a volume to a temporary file
tmp_file <- tempfile(fileext = ".nii.gz")
write_vol(img, tmp_file)
cat("Wrote volume to:", tmp_file, "\n")

# Read it back
img_read <- read_vol(tmp_file)
cat("Read volume with dimensions:", dim(img_read), "\n")

# Clean up
file.remove(tmp_file)

## ----eval=FALSE---------------------------------------------------------------
# # Read multiple volumes (not run)
# files <- c("scan1.nii", "scan2.nii", "scan3.nii")
# vols <- read_vec(files)  # Creates a NeuroVecSeq
# 
# # Or read as a single concatenated 4D volume
# vols_list <- lapply(files, read_vol)
# vec_concat <- vec_from_vols(vols_list)

## -----------------------------------------------------------------------------
# Create synthetic fMRI-like data
dims_fmri <- c(20, 20, 15, 50)  # Small for example
fmri_data <- array(rnorm(prod(dims_fmri), mean = 1000, sd = 50), dims_fmri)
fmri <- NeuroVec(fmri_data, NeuroSpace(dims_fmri))

# Define an ROI
roi <- spherical_roi(space(fmri), c(10, 10, 8), radius = 3)
cat("ROI size:", length(roi), "voxels\n")

# Extract time-series from ROI
roi_ts <- series_roi(fmri, roi)
roi_mat <- values(roi_ts)  # T x N matrix
cat("ROI time-series matrix:", dim(roi_mat), "\n")

# Compute mean time-series
mean_ts <- rowMeans(roi_mat)

# Z-score the mean time-series
z_ts <- as.numeric(base::scale(mean_ts))
cat("Mean time-series - Mean:", round(mean(z_ts), 4), 
    "SD:", round(sd(z_ts), 4), "\n")

# Find peak activation time
peak_time <- which.max(z_ts)
cat("Peak activation at time point:", peak_time, 
    "with z-score:", round(z_ts[peak_time], 2), "\n")

## ----eval=FALSE---------------------------------------------------------------
# # List all functions
# help(package = "neuroim2")
# 
# # Search for specific topics
# help.search("roi", package = "neuroim2")

