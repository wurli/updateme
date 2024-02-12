available_packages <- function() {
  pkgs |>
    as.data.frame(row.names = FALSE) |>
    subset(select = c(Package, Version))
}

# Check fields (in order):
# ~~ renv approach ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. The RemoteType field; typically written for packages installed by the
#    devtools, remotes and pak packages,
#
#    * If
#
# 2. The Repository field; for example, packages retrieved from CRAN will
#    typically have the Repository: CRAN field,
#
# 3. The biocViews field; typically present for packages installed from the
#    Bioconductor repositories,
#
# 4. Just use available.packages()
# ~~ Old ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   1. Repository
#     - if exists, check for match in available.packages(). if match, return
#     (covers CRAN/r-universe installs using any method, I think)
#   2. RemoteType
#     - if 'github' then make a URL using GithubRepo and GithubUsername fields
#     (covers GitHub installs)
#   3. RemoteUrl
#     - Use to ~ somehow ~ get pkg description? Know how to do this with
#       GitHub, but not sure with other locations.
#     (covers Git installs)
#   4. git_url
#     - Use to ~ somehow ~ get pkg description
#     (covers Bioconductor)
#   5. Other methods not supported, so return NULL
package_source <- function(pkg) {
  installed <- as.data.frame(installed.packages(
    fields = c("RemoteType", "Repository", "biocViews")
  ))

  pkg_row <- installed[installed[["Package"]] == package, , drop = FALSE]

  if (nrow(pkg_row) == 0L) {
    cli::cli_abort("{.pkg {pkg}} is not installed")
  }





}


available_version <- function(pkg, available_pkgs = NULL) {

  available_pkgs <- available_pkgs %||% available_packages()

  # Return NULL if pkg not in available.packages()
  if (!pkg %in% available_pkgs[["Package"]]) {
    return(NULL)
  }

  available_pkgs |>
    subset(Package == pkg) |>
    _[["Version"]]
}

inform_load <- function(pkg) {

  current   <- packageVersion(pkg)
  available <- available_version(pkg)

  currentness <- if (current < available) {
    "outdated"
  } else if (current == available) {
    "up_to_date"
  } else {
    "ahead"
  }

  fmt_currentness <- switch(currentness,
    outdated = cli::col_red,
    up_to_date = cli::col_green,
    ahead = cli::col_blue
  )

  new_version <- if (currentness == "outdated") {
    cli::col_grey(cli::style_italic(" (", available, " now available)"))
  }

  cli::cat_line(
    "Loading ",
    cli::format_inline("{.pkg {pkg}}"), " ",
    fmt_currentness(current),
    new_version
  )

  invisible(NULL)

}
