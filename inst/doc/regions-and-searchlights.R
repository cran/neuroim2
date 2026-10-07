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
mask <- demo_mask()
brain <- as.mask(mask > 0)
bold <- demo_bold(n_time = 40)

sum(brain)

## ----sphere-------------------------------------------------------------------
roi <- spherical_roi(mask, c(32, 32, 12), radius = 8, nonzero = TRUE)

length(roi)
head(coords(roi), 3)

## ----nonzero------------------------------------------------------------------
edge <- c(20, 10, 4)

c(
  all = length(spherical_roi(mask, edge, radius = 8)),
  in_mask = length(spherical_roi(mask, edge, radius = 8, nonzero = TRUE))
)

## ----series-roi---------------------------------------------------------------
rts <- series_roi(bold, roi)
dim(values(rts))

## ----other-shapes-------------------------------------------------------------
sp <- NeuroSpace(c(20L, 20L, 20L), c(1, 1, 1))

length(cuboid_roi(sp, c(10, 10, 10), surround = 3))
length(square_roi(sp, c(10, 10, 10), surround = 2, fixdim = 3))

## ----roi-set------------------------------------------------------------------
centres <- rbind(c(20, 20, 10), c(40, 40, 14), c(32, 32, 12))
rois <- spherical_roi_set(mask, centroids = centres, radius = 8, nonzero = TRUE)

lengths(lapply(rois, indices))

## ----set-ops------------------------------------------------------------------
a <- spherical_roi(mask, c(32, 32, 12), radius = 8, nonzero = TRUE)
b <- spherical_roi(mask, c(35, 32, 12), radius = 8, nonzero = TRUE)

c(
  intersection = length(intersect(indices(a), indices(b))),
  union = length(union(indices(a), indices(b))),
  only_a = length(setdiff(indices(a), indices(b)))
)

## ----parcels------------------------------------------------------------------
set.seed(1)
parcels <- ClusteredNeuroVol(brain, sample(1:12, sum(brain), replace = TRUE))

num_clusters(parcels)

## ----split-clusters-----------------------------------------------------------
parts <- split_clusters(bold, parcels)

length(parts)
dim(values(parts[[1]]))

## ----split-reduce-------------------------------------------------------------
labels <- integer(prod(dim(mask)))
labels[which(as.vector(brain))] <- parcels@clusters

parcel_ts <- split_reduce(bold, factor(labels))
dim(parcel_ts)
rownames(parcel_ts)

## ----searchlight--------------------------------------------------------------
sl <- searchlight(brain, radius = 8, eager = FALSE, nonzero = TRUE)

length(sl)
nrow(coords(sl[[1]]))

## ----random-searchlight-------------------------------------------------------
set.seed(42)
rsl <- random_searchlight(brain, radius = 8)

length(rsl)
summary(lengths(lapply(rsl, indices)))

## ----clustered-searchlight----------------------------------------------------
length(clustered_searchlight(brain, cvol = parcels))

## ----plant--------------------------------------------------------------------
design <- demo_design(40)
target <- spherical_roi(mask, c(20, 34, 12), radius = 12, nonzero = TRUE)

Y <- as.matrix(bold)
Y[indices(target), ] <- Y[indices(target), ] +
  1.3 * rep(design, each = length(target))
planted <- DenseNeuroVec(Y, space(bold))

length(target)

## ----score--------------------------------------------------------------------
score <- sapply(rsl, function(r) mean(cor(values(series_roi(planted, r)), design)))

range(score)

## ----score-map----------------------------------------------------------------
arr <- array(0, dim(mask))
for (i in seq_along(rsl)) arr[coords(rsl[[i]])] <- score[i]
score_map <- NeuroVol(arr, space(mask))

## ----verify-------------------------------------------------------------------
outside_idx <- setdiff(which(as.vector(brain)), indices(target))
inside <- as.vector(score_map)[indices(target)]
outside <- as.vector(score_map)[outside_idx]

c(inside = mean(inside), outside = mean(outside), sd_outside = sd(outside),
  z = (mean(inside) - mean(outside)) / sd(outside))

## ----score-fig, fig.cap = "Searchlight score map. The bright cluster is where the signal was planted.", fig.alt = "Three axial slices of a searchlight correlation map with one bright cluster.", fig.height = 3.2----
plot(score_map, zlevels = c(9, 12, 15))

## ----conn-comp----------------------------------------------------------------
cc <- conn_comp(score_map, threshold = 0.35, cluster_table = TRUE)

head(cc$cluster_table[order(-cc$cluster_table$N), ], 4)

## ----iterate------------------------------------------------------------------
anat <- demo_anatomy()

slice_means <- vapply(slices(anat), mean, numeric(1))
length(slice_means)

vol_means <- vapply(vols(bold), mean, numeric(1))
length(vol_means)

## ----vectors------------------------------------------------------------------
mean_vol <- NeuroVol(vapply(vectors(bold), mean, numeric(1)), space(mask))
dim(mean_vol)

## ----parallel, eval = FALSE---------------------------------------------------
# library(future.apply)
# plan(multisession, workers = 4)
# 
# score <- future_sapply(rsl, function(r) {
#   mean(cor(values(series_roi(planted, r)), design))
# })
# 
# plan(sequential)

