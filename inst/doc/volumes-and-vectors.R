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

## ----data---------------------------------------------------------------------
anat <- demo_anatomy()
mask <- demo_mask()
bold <- demo_bold(n_time = 20)

class(anat)[1]
class(bold)[1]

## ----arraylike----------------------------------------------------------------
class(anat * 2)[1]
class(anat > 100)[1]

## ----indexing-----------------------------------------------------------------
dim(anat)
anat[24, 28, 24]

class(anat[, , 24])[1]
class(anat[anat > 100])[1]

## ----mask-class---------------------------------------------------------------
brain <- anat > 100
class(brain)[1]
sum(brain)

## ----as-mask------------------------------------------------------------------
from_logical <- as.mask(anat > 100)
from_indices <- as.mask(anat, which(anat > 6000))

c(brain = sum(from_logical), bright = sum(from_indices))

## ----mask-index---------------------------------------------------------------
vals <- anat[brain]

length(vals)
mean(vals)

## ----mask-fig, fig.cap = "A volume and the mask derived from it, same grid, same geometry.", fig.alt = "Two axial slices side by side: an anatomical slice and its binary mask.", fig.height = 3.4----
op <- par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))
image(anat[, , 24], main = "anat", col = gray.colors(256), axes = FALSE, asp = 1)
image(brain[, , 24], main = "anat > 100", col = gray.colors(2), axes = FALSE, asp = 1)
par(op)

## ----build-vol----------------------------------------------------------------
set.seed(1)

sp <- NeuroSpace(dim = c(16L, 16L, 8L), spacing = c(2, 2, 2))
vol <- NeuroVol(array(rnorm(16 * 16 * 8), c(16, 16, 8)), sp)

vol

## ----copy-space---------------------------------------------------------------
values <- as.array(anat)[]        # a plain numeric vector: no space
class(values)

derived <- NeuroVol(values, space(anat))
identical(space(derived), space(anat))

## ----build-vec----------------------------------------------------------------
sp4 <- NeuroSpace(c(16L, 16L, 8L, 5L), spacing = c(2, 2, 2))
d <- rnorm(16 * 16 * 8 * 5)

v_arr <- NeuroVec(array(d, c(16, 16, 8, 5)), sp4)
v_mat <- NeuroVec(matrix(d, nrow = 16 * 16 * 8), sp4)

dim(v_arr)
all.equal(as.array(v_arr), as.array(v_mat))

## ----extract-vol--------------------------------------------------------------
dim(bold[[3]])
class(bold[[3]])[1]

## ----sub-vector---------------------------------------------------------------
dim(sub_vector(bold, 1:5))

## ----vols---------------------------------------------------------------------
length(vols(bold))

## ----slices-------------------------------------------------------------------
length(slices(anat))
class(slice(anat, 24, along = 3))[1]

## ----as-matrix----------------------------------------------------------------
mat <- as.matrix(bold)
dim(mat)

## ----reduce-------------------------------------------------------------------
inside <- which(as.vector(mask) > 0)

ar1 <- numeric(nrow(mat))
ar1[inside] <- apply(mat[inside, ], 1, function(x) cor(x[-1], x[-length(x)]))

ar1_map <- NeuroVol(ar1, drop_dim(space(bold)))

c(voxels = nrow(mat), reduced = length(inside))
round(range(ar1[inside]), 3)

## ----vectors------------------------------------------------------------------
length(vectors(bold))

## ----concat-vols--------------------------------------------------------------
dim(concat(anat, anat, anat))

## ----concat-vecs--------------------------------------------------------------
run1 <- sub_vector(bold, 1:5)
run2 <- sub_vector(bold, 6:12)

dim(concat(run1, run2))

## ----concat-check-------------------------------------------------------------
identical(space(run1), space(run2))

## ----split-blocks-------------------------------------------------------------
joined <- concat(run1, run2)
blocks <- split_blocks(joined, rep(1:2, c(5, 7)))

length(blocks)
vapply(blocks, function(b) dim(b)[4], integer(1))

## ----sparse-------------------------------------------------------------------
sparse <- as.sparse(bold, as.mask(mask))

class(sparse)[1]
dim(sparse)

c(dense_MB = as.numeric(object.size(bold)) / 1e6,
  sparse_MB = as.numeric(object.size(sparse)) / 1e6)

## ----dense-again--------------------------------------------------------------
class(as.dense(sparse))[1]

