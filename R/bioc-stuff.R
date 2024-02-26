bioc_version <- function() {

  if (is_installed("BiocManager")) {
    vn <- try(BiocManager::version(), silent = TRUE)
    if (inherits(vn, "numeric_version"))
      return(vn)
  }

  vn <- try(packageVersion("BiocVersion"), silent = TRUE)
  if (inherits(vn, "numeric_version"))
    return(vn)
}
