params <-
list(family = "red")

## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")

## -----------------------------------------------------------------------------
    library(neuroim2)
    file_name <- system.file("extdata", "global_mask2.nii.gz", package="neuroim2")
    vol <- read_vol(file_name)

## -----------------------------------------------------------------------------
    print(vol)

## -----------------------------------------------------------------------------
    class(vol)
    
    is.array(vol)
    
    dim(vol)
    
    vol[1,1,1]
    
    vol[64,64,24]
    

## -----------------------------------------------------------------------------
    
    vol2 <- vol + vol
    sum(vol2) == 2 * sum(vol)
    
    vol3 <- vol2 - 2*vol
    all(vol3 == 0)

## -----------------------------------------------------------------------------
    sp <- space(vol)
    sp                 # human-readable summary
    dim(vol)           # spatial dimensions (x, y, z)
    spacing(vol)       # voxel size in mm
    origin(vol)        # image origin

## -----------------------------------------------------------------------------
    idx <- 1:5
    g   <- index_to_grid(vol, idx)     # 1D index -> (i,j,k)
    w   <- index_to_coord(vol, idx)    # 1D index -> world coords
    idx2 <- coord_to_index(vol, w)     # back to indices
    all.equal(idx, idx2)

## -----------------------------------------------------------------------------
    
    vol2 <- as.logical(vol)
    class(vol2)
    print(vol2[1,1,1])

## -----------------------------------------------------------------------------
    # Threshold-based mask
    mask1 <- as.mask(vol > 0.5)
    mask1

    # Index-based mask
    idx_hi <- which(vol > 0.8)
    mask2 <- as.mask(vol, idx_hi)
    sum(mask2) == length(idx_hi)

    # Use a mask to compute a summary
    mean_in_mask <- mean(vol[mask1@.Data])
    mean_in_mask

## -----------------------------------------------------------------------------
    x <- array(0, c(64,64,64))

## -----------------------------------------------------------------------------
    bspace <- NeuroSpace(dim=c(64,64,64), spacing=c(1,1,1))
    vol <- NeuroVol(x, bspace)
    vol

## -----------------------------------------------------------------------------
    vol2 <- NeuroVol((vol+1)*25, space(vol))
    max(vol2)
    space(vol2)

## ----slice_mid, fig.alt='Mid-slice of example volume (grayscale image).', fig.cap='Mid-slice of example volume'----
    z <- ceiling(dim(vol)[3] / 2)
    image(vol[,,z], main = paste("Slice z=", z))

## -----------------------------------------------------------------------------
    # Reorient the space (LPI -> RAS) and compare coordinate mappings
    sp_lpi <- space(vol)
    sp_ras <- reorient(sp_lpi, c("R","A","S"))
    g     <- t(matrix(c(10, 10, 10)))
    world_lpi <- grid_to_coord(sp_lpi, g)
    world_ras <- grid_to_coord(sp_ras, g)
    # world_lpi and world_ras differ due to axis remapping

## ----eval=FALSE---------------------------------------------------------------
#     # Create a target space with 2x finer resolution
#     sp  <- space(vol)
#     sp2 <- NeuroSpace(sp@dim * c(2,2,2), sp@spacing/2, origin=sp@origin, trans=trans(vol))
# 
#     # Resample (trilinear)
#     vol_resamp <- resample_to(vol, sp2, method = "linear")
#     dim(vol_resamp)

## -----------------------------------------------------------------------------
    # Downsample by target spacing
    vol_ds1 <- downsample(vol, spacing = spacing(vol)[1:3] * 2)
    dim(vol_ds1)

    # Or by factor
    vol_ds2 <- downsample(vol, factor = 0.5)
    dim(vol_ds2)

## ----eval=FALSE---------------------------------------------------------------
#     write_vol(vol2, "output.nii")
# 
#     ## adding a '.gz' extension results ina gzipped file.
#     write_vol(vol2, "output.nii.gz")

## -----------------------------------------------------------------------------
    tmp <- tempfile(fileext = ".nii.gz")
    write_vol(vol2, tmp)
    file.exists(tmp)
    unlink(tmp)

