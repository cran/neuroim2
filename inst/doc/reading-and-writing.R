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

## ----paths--------------------------------------------------------------------
mask_file <- system.file("extdata", "global_mask2.nii.gz", package = "neuroim2")
series_file <- system.file("extdata", "global_mask_v4.nii", package = "neuroim2")
anat_file <- system.file("extdata", "mni_downsampled.nii.gz", package = "neuroim2")

## ----readers------------------------------------------------------------------
vol <- read_vol(mask_file)
vec <- read_vec(series_file)

dim(vol)
dim(vec)

## ----read-index---------------------------------------------------------------
dim(read_vol(series_file, index = 3))

## ----read-image---------------------------------------------------------------
class(read_image(series_file))[1]

## ----multi--------------------------------------------------------------------
class(read_vol_list(c(mask_file, mask_file)))[1]
class(read_vec(c(series_file, series_file)))[1]

dim(read_vec(c(series_file, series_file)))

## ----header-------------------------------------------------------------------
hdr <- read_header(mask_file)

dim(hdr)
hdr@data_type
hdr@spacing

## ----header-trans-------------------------------------------------------------
trans(hdr)

## ----header-slot--------------------------------------------------------------
hdr@header$datatype
hdr@header$encoding
hdr@header$vox_offset

## ----affines-good-------------------------------------------------------------
c(qform_code = hdr@header$qform_code, sform_code = hdr@header$sform_code)

all.equal(hdr@header$qform, hdr@header$sform)

## ----affines-bad--------------------------------------------------------------
anat_hdr <- read_header(anat_file)

anat_hdr@header$pixdim[2:4]
anat_hdr@header$qform
anat_hdr@header$sform

## ----affines-consequence------------------------------------------------------
spacing(read_vol(anat_file))

## ----affines-divergence-------------------------------------------------------
far <- matrix(dim(read_vol(anat_file)), nrow = 1)

sform_xyz <- grid_to_coord(space(read_vol(anat_file)), far)
qform_xyz <- (anat_hdr@header$qform %*% c(far - 1, 1))[1:3]

rbind(sform = as.vector(sform_xyz), qform = qform_xyz)

## ----affine-check-------------------------------------------------------------
affines_agree <- function(file) {
  h <- read_header(file)
  qc <- h@header$qform_code
  sc <- h@header$sform_code
  if (is.null(qc) || is.null(sc) || qc <= 0 || sc <= 0) return(NA)
  isTRUE(all.equal(h@header$qform, h@header$sform, tolerance = 1e-4))
}

c(mask = affines_agree(mask_file), anat = affines_agree(anat_file))

## ----repair-------------------------------------------------------------------
bad <- read_vol(anat_file)
fixed <- NeuroVol(
  as.array(bad),
  NeuroSpace(dim(bad),
    spacing = anat_hdr@header$pixdim[2:4],
    origin = anat_hdr@header$qform[1:3, 4],
    axes = space(bad)@axes
  )
)

spacing(fixed)

## ----write--------------------------------------------------------------------
out_nii <- tempfile(fileext = ".nii")
out_gz <- tempfile(fileext = ".nii.gz")

write_vol(vol, out_nii)
write_vol(vol, out_gz)

c(plain = file.size(out_nii), gzipped = file.size(out_gz))

## ----datatype-----------------------------------------------------------------
out_byte <- tempfile(fileext = ".nii")
write_vol(vol, out_byte, data_type = "UBYTE")

c(float = file.size(out_nii), ubyte = file.size(out_byte))
read_header(out_byte)@data_type

## ----roundtrip----------------------------------------------------------------
back <- read_vol(out_gz)

all.equal(as.array(back), as.array(vol))
identical(trans(space(back)), trans(space(vol)))

## ----write-vec----------------------------------------------------------------
out_vec <- tempfile(fileext = ".nii.gz")
write_vec(sub_vector(vec, 1:2), out_vec)

dim(read_vec(out_vec))

## ----precision----------------------------------------------------------------
sp <- NeuroSpace(c(4L, 4L, 4L), spacing = c(1 / 3, 0.123456789012, pi))

spacing(sp)

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(out_nii, out_gz, out_byte, out_vec))

## ----afni, eval = FALSE-------------------------------------------------------
# vol <- read_vol("subject_anat+orig.HEAD")
# names(read_header("subject_anat+orig.HEAD")@header)

