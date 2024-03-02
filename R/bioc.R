bioc_version <- function(pkg = NULL) {
  if (is_installed("BiocManager")) {
    vn <- try(BiocManager::version(), silent = TRUE)
    if (inherits(vn, "numeric_version"))
      return(vn[, 1:2])
  }

  vn <- try(packageVersion("BiocVersion"), silent = TRUE)
  if (inherits(vn, "numeric_version"))
    return(vn[, 1:2])

  NULL
}

bioc_repo <- function(version) {
  paste0("https://bioconductor.org/packages/", version, "/bioc")
}
