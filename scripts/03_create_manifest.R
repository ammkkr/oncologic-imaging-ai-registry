args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) normalizePath(args[[1]], mustWork = TRUE) else getwd()

manifest_path <- file.path(root, "MANIFEST_SHA256.csv")
files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
files <- files[file.info(files)$isdir %in% FALSE]
files <- files[basename(files) != basename(manifest_path)]
files <- files[!grepl("(^|[/\\])\\.git([/\\]|$)", files)]
files <- sort(normalizePath(files, winslash = "/", mustWork = TRUE))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required. Install it with install.packages('digest').")
}

sha256 <- vapply(
  files,
  function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE),
  character(1)
)

relative <- substring(files, nchar(normalizePath(root, winslash = "/")) + 2L)
manifest <- data.frame(
  path = relative,
  bytes = unname(file.info(files)$size),
  sha256 = unname(sha256),
  stringsAsFactors = FALSE
)

write.csv(manifest, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
message("Wrote ", nrow(manifest), " hashes to ", manifest_path)
