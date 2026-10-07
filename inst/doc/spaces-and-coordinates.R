params <-
list(family = "red", preset = "interaction")

## ----setup, include = FALSE---------------------------------------------------
if (requireNamespace("ragg", quietly = TRUE)) knitr::opts_chunk$set(dev = "ragg_png")
if (requireNamespace("systemfonts", quietly = TRUE)) albersdown::albers_register_fonts()
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("albersdown", quietly = TRUE)) ggplot2::theme_set(albersdown::theme_albers(family = params$family, preset = params$preset))
source("_common.R")

## ----albers-classes, echo=FALSE, results='asis'-------------------------------
cat(sprintf(
  paste0(
    '<script>document.addEventListener("DOMContentLoaded",function(){',
    'document.body.classList.remove("palette-red","palette-lapis","palette-ochre","palette-teal","palette-green","palette-violet","preset-homage","preset-interaction","preset-study","preset-structural","preset-adobe","preset-midnight");',
    'document.body.classList.add("palette-%s","preset-%s");',
    '});</script>'
  ),
  params$family,
  params$preset
))

## ----load---------------------------------------------------------------------
library(neuroim2)

## ----real-space---------------------------------------------------------------
mask <- demo_mask()
space(mask)

## ----create-space-------------------------------------------------------------
sp <- NeuroSpace(
  dim     = c(64L, 64L, 40L),
  spacing = c(2, 2, 2),
  origin  = c(-90, -126, -72)
)

dim(sp)
spacing(sp)
origin(sp)

## ----show-trans---------------------------------------------------------------
trans(sp)

## ----decompose-affine---------------------------------------------------------
trans(sp)[1:3, 4]          # translation column = origin
diag(trans(sp)[1:3, 1:3])  # diagonal of linear block = voxel sizes

## ----inverse-trans------------------------------------------------------------
inverse_trans(sp)

## ----explicit-affine----------------------------------------------------------
aff <- diag(c(3, 3, 4, 1))
aff[1:3, 4] <- c(-90, -126, -72)

sp_aff <- NeuroSpace(dim = c(60L, 60L, 35L), trans = aff)
spacing(sp_aff)
origin(sp_aff)

## ----coord-diagram, echo = FALSE, fig.cap = "The three addressing schemes and the functions that convert between them.", fig.alt = "Diagram showing linear index, grid index and world coordinates connected by conversion functions.", fig.width = 6.4, fig.height = 2.4----
op <- par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 10), ylim = c(0.4, 2.6))

rect(0.2, 0.8, 2.8, 2.2, col = "#dce8f5", border = "#3a7abf", lwd = 1.5)
rect(3.7, 0.8, 6.3, 2.2, col = "#dce8f5", border = "#3a7abf", lwd = 1.5)
rect(7.2, 0.8, 9.8, 2.2, col = "#dce8f5", border = "#3a7abf", lwd = 1.5)

text(1.5, 1.7, "Linear\nindex", cex = 0.85, font = 2)
text(1.5, 1.15, "1 ... prod(dim)", cex = 0.68, col = "#555555")
text(5.0, 1.7, "Grid\nindex", cex = 0.85, font = 2)
text(5.0, 1.15, "(i, j, k)  1-based", cex = 0.68, col = "#555555")
text(8.5, 1.7, "World\ncoords", cex = 0.85, font = 2)
text(8.5, 1.15, "x, y, z  mm", cex = 0.68, col = "#555555")

arrows(2.85, 1.5, 3.65, 1.5, length = 0.08, lwd = 1.4, col = "#3a7abf")
arrows(3.65, 1.3, 2.85, 1.3, length = 0.08, lwd = 1.4, col = "#888888")
text(3.25, 1.78, "index_to_grid", cex = 0.58, col = "#3a7abf")
text(3.25, 1.02, "grid_to_index", cex = 0.58, col = "#888888")

arrows(6.35, 1.5, 7.15, 1.5, length = 0.08, lwd = 1.4, col = "#3a7abf")
arrows(7.15, 1.3, 6.35, 1.3, length = 0.08, lwd = 1.4, col = "#888888")
text(6.75, 1.78, "grid_to_coord", cex = 0.58, col = "#3a7abf")
text(6.75, 1.02, "coord_to_grid", cex = 0.58, col = "#888888")

arrows(2.85, 0.72, 7.15, 0.72, length = 0.08, lwd = 1.2, col = "#3a7abf", lty = 2)
arrows(7.15, 0.52, 2.85, 0.52, length = 0.08, lwd = 1.2, col = "#888888", lty = 2)
text(5.0, 0.85, "index_to_coord", cex = 0.55, col = "#3a7abf")
text(5.0, 0.42, "coord_to_index", cex = 0.55, col = "#888888")

par(op)

## ----grid-index---------------------------------------------------------------
grid_to_index(sp, matrix(c(10, 12, 5), nrow = 1))
index_to_grid(sp, 17098L)

## ----grid-to-coord------------------------------------------------------------
grid_to_coord(sp, matrix(c(1, 1, 1), nrow = 1))
origin(sp)

## ----grid-to-coord-multi------------------------------------------------------
pts <- matrix(c(
   1,  1,  1,
  32, 32, 20,
  64, 64, 40
), ncol = 3, byrow = TRUE)

grid_to_coord(sp, pts)

## ----coord-to-grid------------------------------------------------------------
coord_to_grid(sp, c(0, 0, 0))

## ----shortcuts----------------------------------------------------------------
index_to_coord(sp, 12345L)
coord_to_index(sp, matrix(c(22, -126, -66), nrow = 1))

## ----roundtrip----------------------------------------------------------------
idx <- 12345L

grid_to_index(sp, coord_to_grid(sp, grid_to_coord(sp, index_to_grid(sp, idx))))
coord_to_index(sp, index_to_coord(sp, idx))

## ----axcodes------------------------------------------------------------------
affine_to_axcodes(trans(sp))
affine_to_axcodes(trans(mask))

## ----reorient-----------------------------------------------------------------
sp_ras <- reorient(space(mask), c("R", "A", "S"))
affine_to_axcodes(trans(sp_ras))

## ----reorient-noop------------------------------------------------------------
identical(trans(reorient(space(mask), c("L", "A", "S"))), trans(space(mask)))

## ----oblique------------------------------------------------------------------
aff_obl <- matrix(c(
   2.0,  0.2,  0.0,  -90,
   0.0,  2.0,  0.1, -126,
   0.0,  0.0,  2.0,  -72,
   0.0,  0.0,  0.0,    1
), nrow = 4, byrow = TRUE)

sp_obl <- NeuroSpace(dim = c(91L, 109L, 91L), trans = aff_obl)

## ----oblique-spacing----------------------------------------------------------
spacing(sp_obl)
diag(aff_obl[1:3, 1:3])

## ----obliquity----------------------------------------------------------------
obliquity(aff_obl)
obliquity(trans(sp))
voxel_sizes(aff_obl)

## ----dims---------------------------------------------------------------------
sp_4d <- add_dim(sp, 200L)
dim(sp_4d)

sp_back <- drop_dim(sp_4d)
identical(trans(sp_back), trans(sp))

