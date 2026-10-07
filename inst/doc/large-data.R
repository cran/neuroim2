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
brain <- as.mask(mask)
bold <- demo_bold(n_time = 60)

path <- tempfile(fileext = ".nii")
write_vec(bold, path)

dim(bold)
round(file.size(path) / 1e6, 1)

## ----backends-----------------------------------------------------------------
dense <- read_vec(path)
sparse <- read_vec(path, mask = brain)
mapped <- read_vec(path, mode = "mmap")
filebacked <- read_vec(path, mode = "filebacked")

mb <- function(x) round(as.numeric(object.size(x)) / 1e6, 1)

data.frame(
  backend = c("DenseNeuroVec", "SparseNeuroVec", "MappedNeuroVec", "FileBackedNeuroVec"),
  memory_MB = c(mb(dense), mb(sparse), mb(mapped), mb(filebacked))
)

## ----agreement----------------------------------------------------------------
ref <- as.numeric(series(dense, 32, 32, 12))

vapply(
  list(sparse = sparse, mapped = mapped, filebacked = filebacked),
  function(x) isTRUE(all.equal(as.numeric(series(x, 32, 32, 12)), ref)),
  logical(1)
)

## ----timing-------------------------------------------------------------------
per_call <- function(f, n) unname(system.time(for (i in seq_len(n)) f(i))["elapsed"] / n)

bench <- function(x, n = 20) {
  c(
    volumes = per_call(function(i) invisible(x[[i]]), n),
    series = per_call(function(i) series(x, 32, 32, 12), n)
  )
}

timings <- rbind(
  dense = bench(dense),
  sparse = bench(sparse),
  mapped = bench(mapped),
  filebacked = bench(filebacked, n = 5)
)

signif(timings, 2)

## ----timing-ratio-------------------------------------------------------------
round(timings["filebacked", "series"] / timings[c("dense", "sparse", "mapped"), "series"])

## ----sparse-------------------------------------------------------------------
c(in_mask = sum(brain), total = prod(dim(brain)))
c(dense_MB = mb(dense), sparse_MB = mb(sparse))

## ----mmap---------------------------------------------------------------------
class(mapped)[1]
dim(mapped)

## ----as-mmap------------------------------------------------------------------
mmap_path <- tempfile(fileext = ".nii")
converted <- as_mmap(dense, file = mmap_path, overwrite = TRUE)

class(converted)[1]
isTRUE(all.equal(as.numeric(series(converted, 32, 32, 12)), ref))

## ----bigvec-------------------------------------------------------------------
big <- read_vec(path, mode = "bigvec", mask = brain)

class(big)[1]
round(mb(big), 2)

## ----clustered----------------------------------------------------------------
set.seed(1)
coords_in <- index_to_grid(mask, which(as.vector(brain)))
km <- kmeans(coords_in, centers = 100, iter.max = 30)

parcels <- ClusteredNeuroVol(brain, km$cluster)
cvec <- ClusteredNeuroVec(bold, parcels)

num_clusters(cvec)
dim(cvec)
round(apply(centroids(cvec), 2, sd), 1)

## ----clustered-size-----------------------------------------------------------
dim(as.matrix(cvec, by = "cluster"))

c(dense_MB = mb(dense), clustered_MB = mb(cvec))

## ----clustered-access---------------------------------------------------------
length(series(cvec, 32, 32, 12))
dim(cvec[, , , 1])

c(voxel_series = sum(brain), stored_series = num_clusters(cvec),
  fold = round(sum(brain) / num_clusters(cvec)))

## ----cluster-searchlight------------------------------------------------------
windows <- cluster_searchlight_series(cvec, k = 3)

length(windows)
dim(values(windows[[1]]))

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(path, mmap_path, big@data$backingfile))

