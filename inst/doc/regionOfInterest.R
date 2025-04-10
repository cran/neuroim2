## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(assertthat)
library(purrr)
library(neuroim2)
options(mc.cores=1)

## -----------------------------------------------------------------------------
      library(neuroim2)        
      file_name <- system.file("extdata", "global_mask2.nii.gz", package="neuroim2")
      vol <- read_vol(file_name)

## -----------------------------------------------------------------------------
      sphere <- spherical_roi(vol, c(20,20,20), radius=5, fill=100, use_cpp=FALSE)

## ----echo=FALSE---------------------------------------------------------------
sphere

## -----------------------------------------------------------------------------

    rpoint <- c(-34,-28,10)

## -----------------------------------------------------------------------------

    vox <- coord_to_grid(vol, rpoint)
    sphere <- spherical_roi(vol, vox, radius=10, fill=1)
    dim(coords(sphere))

## -----------------------------------------------------------------------------
    coords <- index_to_coord(vol, indices(sphere))
    center_of_mass <- colMeans(coords)
    center_of_mass

## -----------------------------------------------------------------------------

    sphere <- spherical_roi(vol, c(50,10,10), radius=10, fill=1)
    sphere

## -----------------------------------------------------------------------------
    sparsevol <- SparseNeuroVol(sphere, space(vol),indices=indices(sphere))
    sum(sparsevol) == sum(sphere)
    all(dim(sparsevol) == dim(vol))

## -----------------------------------------------------------------------------
library(purrr)

## generate a list of searchlight ROIs
slist <- searchlight(vol, eager=TRUE, radius=8)

## compute the mean value in each searchlight ROI.
ret <- slist %>% purrr::map(~ mean(vol[coords(.)]))

## -----------------------------------------------------------------------------
ret <- vol %>% random_searchlight(radius=8) %>% purrr::map(~ mean(vol[coords(.)]))

## -----------------------------------------------------------------------------

grid <- index_to_coord(vol, which(vol > 0))
kres <- kmeans(grid, centers=50, iter.max=500)

## -----------------------------------------------------------------------------
kvol <- ClusteredNeuroVol(vol, kres$cluster)
ret <- vol %>% clustered_searchlight(cvol=kvol) %>% purrr::map(~ mean(vol[coords(.)]))


## -----------------------------------------------------------------------------

pset <- patch_set(vol, dims=c(3,3,1))
length(pset)
ret <- pset %>% purrr::map(~ mean(.))


## -----------------------------------------------------------------------------

pset <- patch_set(vol, dims=c(3,3,1), mask=as.logical(vol))
length(pset)
ret <- pset %>% purrr::map(~ mean(.))


