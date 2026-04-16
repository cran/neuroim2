params <-
list(family = "red", preset = "homage")

## ----setup, include = FALSE---------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("albersdown", quietly = TRUE)) ggplot2::theme_set(albersdown::theme_albers(family = params$family, preset = params$preset))
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----load-pkg-----------------------------------------------------------------
library(neuroim2)

## ----create-space-------------------------------------------------------------
sp <- NeuroSpace(
  dim     = c(64L, 64L, 40L),
  spacing = c(2, 2, 2),
  origin  = c(-90, -126, -72)
)
sp

## ----inspect-space------------------------------------------------------------
dim(sp)       # grid dimensions
spacing(sp)   # voxel sizes in mm
origin(sp)    # world coords of voxel (1,1,1)
ndim(sp)      # number of spatial dimensions

## ----show-trans---------------------------------------------------------------
trans(sp)

## ----decompose-affine---------------------------------------------------------
# Translation column = origin
trans(sp)[1:3, 4]

# Diagonal of linear block = voxel sizes
diag(trans(sp)[1:3, 1:3])

## ----inverse-trans------------------------------------------------------------
inverse_trans(sp)

## ----explicit-affine----------------------------------------------------------
aff <- diag(c(3, 3, 4, 1))          # 3 × 3 × 4 mm voxels
aff[1:3, 4] <- c(-90, -126, -72)    # origin

sp_aff <- NeuroSpace(dim = c(60L, 60L, 35L), trans = aff)
spacing(sp_aff)   # derived from column norms of linear block
origin(sp_aff)    # derived from translation column

## ----coord-systems-diagram, echo=FALSE, fig.cap="The three addressing schemes and the functions that convert between them.", fig.width=5.5, fig.height=2.2----
op <- par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 3))

# Boxes
rect(0.2, 0.8, 2.8, 2.2, col = "#dce8f5", border = "#3a7abf", lwd = 1.5)
rect(3.7, 0.8, 6.3, 2.2, col = "#dce8f5", border = "#3a7abf", lwd = 1.5)
rect(7.2, 0.8, 9.8, 2.2, col = "#dce8f5", border = "#3a7abf", lwd = 1.5)

text(1.5, 1.7, "Linear\nindex", cex = 0.85, font = 2)
text(1.5, 1.15, "1 ... prod(dim)", cex = 0.68, col = "#555555")
text(5.0, 1.7, "Grid\nindex", cex = 0.85, font = 2)
text(5.0, 1.15, "(i, j, k)  1-based", cex = 0.68, col = "#555555")
text(8.5, 1.7, "World\ncoords", cex = 0.85, font = 2)
text(8.5, 1.15, "x, y, z  mm", cex = 0.68, col = "#555555")

# Arrows and labels
arrows(2.85, 1.5, 3.65, 1.5, length = 0.08, lwd = 1.4, col = "#3a7abf")
arrows(3.65, 1.3, 2.85, 1.3, length = 0.08, lwd = 1.4, col = "#888888")
text(3.25, 1.75, "index_to_grid", cex = 0.58, col = "#3a7abf")
text(3.25, 1.05, "grid_to_index", cex = 0.58, col = "#888888")

arrows(6.35, 1.5, 7.15, 1.5, length = 0.08, lwd = 1.4, col = "#3a7abf")
arrows(7.15, 1.3, 6.35, 1.3, length = 0.08, lwd = 1.4, col = "#888888")
text(6.75, 1.75, "grid_to_coord", cex = 0.58, col = "#3a7abf")
text(6.75, 1.05, "coord_to_grid", cex = 0.58, col = "#888888")

# Shortcut arrows below
arrows(2.85, 0.9, 7.15, 0.9, length = 0.08, lwd = 1.2, col = "#3a7abf", lty = 2)
arrows(7.15, 0.7, 2.85, 0.7, length = 0.08, lwd = 1.2, col = "#888888", lty = 2)
text(5.0, 1.0, "index_to_coord", cex = 0.55, col = "#3a7abf")
text(5.0, 0.6, "coord_to_index", cex = 0.55, col = "#888888")

par(op)

## ----grid-index---------------------------------------------------------------
# Which linear index does grid position (10, 12, 5) map to?
grid_to_index(sp, matrix(c(10, 12, 5), nrow = 1))

# And back to grid
index_to_grid(sp, 8394L)

## ----grid-to-coord------------------------------------------------------------
grid_to_coord(sp, matrix(c(1, 1, 1), nrow = 1))   # should equal origin(sp)
grid_to_coord(sp, matrix(c(10, 12, 5), nrow = 1))

## ----grid-to-coord-multi------------------------------------------------------
pts <- matrix(c(
   1,  1,  1,
  32, 32, 20,
  64, 64, 40
), ncol = 3, byrow = TRUE)

grid_to_coord(sp, pts)

## ----coord-to-grid------------------------------------------------------------
coord_to_grid(sp, c(0, 0, 0))          # near isocenter
coord_to_grid(sp, matrix(c(0, 0, 0,
                           10, -20, 8), ncol = 3, byrow = TRUE))

## ----index-coord--------------------------------------------------------------
# Linear index 1 should land at the origin (pass integer)
index_to_coord(sp, 1L)

# World coord back to linear index
coord_to_index(sp, matrix(c(-90, -126, -72), nrow = 1))

## ----roundtrip----------------------------------------------------------------
idx   <- 12345L
grid  <- index_to_grid(sp, idx)
world <- grid_to_coord(sp, grid)
back  <- coord_to_index(sp, world)

cat("index:", idx, "-> grid:", grid, "-> world:", round(world, 2),
    "-> index:", back, "\n")

## ----axcodes------------------------------------------------------------------
affine_to_axcodes(trans(sp))

## ----axes-slot----------------------------------------------------------------
axes(sp)

## ----reorient-----------------------------------------------------------------
sp_ras <- reorient(sp, c("R", "A", "S"))
affine_to_axcodes(trans(sp_ras))

## ----create-simple------------------------------------------------------------
# 2 mm isotropic, MNI-ish origin
sp_mni <- NeuroSpace(
  dim     = c(91L, 109L, 91L),
  spacing = c(2, 2, 2),
  origin  = c(-90, -126, -72)
)
sp_mni

## ----create-from-affine-------------------------------------------------------
# Oblique affine: slight off-diagonal terms (scanner tilt)
aff_obl <- matrix(c(
   2.0,  0.2,  0.0,  -90,
   0.0,  2.0,  0.1, -126,
   0.0,  0.0,  2.0,  -72,
   0.0,  0.0,  0.0,    1
), nrow = 4, byrow = TRUE)

sp_obl <- NeuroSpace(dim = c(91L, 109L, 91L), trans = aff_obl)

# spacing() returns column norms — the true physical voxel sizes
spacing(sp_obl)

# diagonal is not exactly 2 mm when the image is tilted
diag(aff_obl[1:3, 1:3])

## ----voxel-sizes--------------------------------------------------------------
voxel_sizes(aff_obl)

## ----add-dim------------------------------------------------------------------
sp_3d <- NeuroSpace(c(64L, 64L, 40L), spacing = c(2, 2, 2),
                    origin = c(-90, -126, -72))
sp_4d <- add_dim(sp_3d, 200)   # 200 time points
dim(sp_4d)
trans(sp_4d)                   # 4x4 spatial affine unchanged

## ----drop-dim-----------------------------------------------------------------
sp_back <- drop_dim(sp_4d)
dim(sp_back)
all.equal(trans(sp_back), trans(sp_3d))   # affine preserved

## ----resample-space-----------------------------------------------------------
vol_mni <- NeuroVol(array(rnorm(prod(dim(sp_mni))), dim(sp_mni)), sp_mni)
sp_coarse <- NeuroSpace(c(45L, 54L, 45L), spacing = c(4, 4, 4),
                        origin = origin(sp_mni))
vol_coarse <- resample(vol_mni, sp_coarse)
dim(vol_coarse)
spacing(space(vol_coarse))

## ----deoblique----------------------------------------------------------------
sp_deob <- deoblique(sp_obl)
affine_to_axcodes(trans(sp_deob))   # now axis-aligned
obliquity(trans(sp_deob))           # near zero

## ----gotcha-obliquity---------------------------------------------------------
obliquity(aff_obl)        # non-zero: image is slightly tilted
obliquity(trans(sp_mni))  # near zero: axis-aligned

