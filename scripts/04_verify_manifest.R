args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) normalizePath(args[[1]], mustWork = TRUE) else getwd()

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required. Install it with install.packages('digest').")
}

manifest_path <- file.path(root, "MANIFEST_SHA256.csv")
if (!file.exists(manifest_path)) {
  stop("MANIFEST_SHA256.csv was not found. Run scripts/03_create_manifest.R first.")
}

manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
required <- c("path", "bytes", "sha256")
if (!all(required %in% names(manifest))) {
  stop("Manifest must contain path, bytes, and sha256 columns.")
}

full_paths <- file.path(root, manifest$path)
exists <- file.exists(full_paths)
actual_bytes <- rep(NA_real_, nrow(manifest))
actual_hash <- rep(NA_character_, nrow(manifest))

actual_bytes[exists] <- file.info(full_paths[exists])$size
actual_hash[exists] <- vapply(
  full_paths[exists],
  function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE),
  character(1)
)

ok <- exists & actual_bytes == manifest$bytes & actual_hash == manifest$sha256
if (!all(ok)) {
  failed <- data.frame(
    path = manifest$path[!ok],
    exists = exists[!ok],
    expected_bytes = manifest$bytes[!ok],
    actual_bytes = actual_bytes[!ok],
    expected_sha256 = manifest$sha256[!ok],
    actual_sha256 = actual_hash[!ok],
    stringsAsFactors = FALSE
  )
  print(failed, row.names = FALSE)
  stop(sum(!ok), " manifest entries failed verification.")
}

message("Verified ", nrow(manifest), " files against MANIFEST_SHA256.csv")
