#' Configure updateme lookup of new package versions
#'
#' This function is a helper for setting the `"updateme.sources"`
#' global option. It provides a user-friendly interface and validation of the
#' options you set.
#'
#' @param ... Named or unnamed arguments. Values should be either:
#'
#'   -   One of `names(getOption("repo"))`: latest versions will be taken from
#'       this source, if available
#'
#'   -   `"bioc"`: new versions will be looked for on Bioconductor
#'
#'   -   `"github"`/`"gitlab"`: new versions will looked for on on
#'       GitHub/GitLab, if a repo can be identified using the package
#'       `DESCRIPTION`
#'
#'   -   A URL pointing to a GitHub/GitLab repo, e.g.
#'       `"https://github.com/wurli/updateme"`: the latest version *for this
#'       particular package* will be taken from this project
#'
#'   -  `NA`: updateme will not attempt to query new versions.
#'      Note that `NA` inputs must always be named (i.e. you must specify this
#'      'per package')
#'
#'   -  `NULL`: return to the default behaviour
#'
#'   If arguments are named, names should indicate package which the option
#'   should apply to. If unnamed, the option will apply to all packages. See
#'   examples for more information.
#'
#' @section Private Repositories:
#' updateme supports packages installed from private repositories on GitHub
#' and GitLab. To get upstream package version from either, you should only have
#' to configure a personal access token (PAT):
#'
#' * For GitHub packages, updateme checks, in order:
#'   * The `GITHUB_PAT` environmental variable
#'   * The `GITHUB_TOKEN` environmental variable
#'   * Any personal access tokens configured using [gitcreds::gitcreds_set()]
#' * For GitLab packages, updateme checks, in order:
#'   * The `GITLAB_PAT` environmental variable
#'   * The `GITLAB_TOKEN` environmental variable
#'   * Any personal access tokens configured using [gitcreds::gitcreds_set()]
#'
#' @return The result of setting
#'   `options(updateme.sources = <new_options>)`
#'
#' @export
#'
#' @seealso [updateme_on()] and [updateme_off()] to disable updateme for all
#'   packages
#'
#' @examples
#' # If you want to check non-standard repos for new versions of packages,
#' # you'll first have to set the repos global option. Note that each
#' # option must be named for compatibility with updateme
#' old_repos <- options(repos = c(
#'
#'   # Your default CRAN mirror will likely be something like this
#'   CRAN = "https://cloud.r-project.org",
#'
#'   # The r-lib r-universe, including dev versions of infrastructure packages
#'   # like cli, rlang, etc
#'   `r-lib` = "https://r-lib.r-universe.dev"
#' ))
#'
#' # 1. New versions will first be looked up from the r-lib R universe by default
#' # 2. If not found, they will be looked up from the usual CRAN mirror
#' # 3. dplyr will always be first looked up from GitHub
#' # 4. ggplot2 won't be looked up or notified about
#' old_updateme_sources <- updateme_sources_set(
#'   "r-lib",
#'   "CRAN",
#'   dplyr = "https://github.com/tidyverse/dplyr", # Name is optional here
#'   ggplot2 = NA
#' )
#'
#' # memoise should now be looked up from the r-lib r-universe
#' if (interactive()) {
#'   library(memoise)
#' }
#'
#' # Restore old options
#' options(old_repos)
#' options(old_updateme_sources)
updateme_sources_set <- function(...) {
  options(updateme.sources = updateme_sources_set_impl(...))
}

updateme_sources_set_impl <- function(...) {

  if (...length() == 0L)
    cli::cli_abort(call = caller_call(1), c(
      "At least one argument must be supplied",
      i = "Use {.code updateme::updateme_off()} to disable updateme"
    ))

  # dots_list() prevents names being NULL
  dots     <- dots_list(...)
  out      <- imap(dots, updateme_sources_validate)
  sources  <- map(out, function(x) x[["Source_Name"]])
  packages <- map_chr(out, function(x) x[["Package"]] %||% "")
  compact(set_names(sources, packages))
}

preferred_sources <- function(pkg) {
  opt <- updateme_sources_get()

  if (length(opt) == 0L)
    return(NULL)

  c(
    opt |> keep(function(x) identical(x[["Package"]], pkg)),
    opt |> keep(function(x) is.null(x[["Package"]]))
  )
}

updateme_sources_validate <- function(src, pkg = NULL, throw = cli::cli_abort) {

  handle_no_sources <- function() {
    if (!is.null(throw)) {
      repos <- cli::cli_vec(names(getOption("repos")), style = list("vec-last" = " or "))
      throw(call = caller_call(6), c(
        "Invalid package source {.val {src}}.",
        "i" = "Inputs must be:",
        " " = '- One of {.code names(getOption("repos"))}, i.e. {.val {repos}}',
        " " = "- {.val bioc} to check the version on Bioconductor",
        " " = "- {.val github}/{.val gitlab} to check the version on GitHub/GitLab if possible",
        " " = "- The URL of a specific GitHub repository, e.g. {.val https://github.com/wurli/updateme}",
        " " = "- The URL of a specific GitLab repository, e.g. {.val https://gitlab.com/r-packages/yum}",
        " " = "- {.val NA} to turn {.pkg updateme} off for a package",
        " " = "- {.val NULL} to return to the default behaviour"
      ))
    }
    NULL
  }

  src_ok <- is.character(src) || is.null(src) || is.na(src)
  pkg_ok <- is.character(pkg) || is.null(pkg)

  if (!src_ok || !pkg_ok) {
    return(handle_no_sources())
  }

  if (pkg == "")
    pkg <- NULL

  if (is.null(src) && is.null(pkg)) {
    throw(call = caller_call(6), c(
      "Invalid package source",
      i = "All {.val NULL} arguments must be named"
    ))
  }

  if (is.null(src))
    return(NULL)

  out <- list(
    Available_Sources = NULL,
    Preferred_Source  = NULL,
    Package           = pkg,
    Source_Name       = src,
    Repository        = NULL,
    Github_Username   = NULL,
    Github_Repository = NULL,
    Remote_URL        = NULL,
    Bioc_Views        = NULL
  )

  if (is.na(src)) {
    out[["Preferred_Source"]] <- .updateme_skip
    return(out)
  }

  if (is_valid_repo(src)) {
    out[["Available_Sources"]] <- "repo"
    out[["Preferred_Source"]]  <- "repo"
    out[["Repository"]]        <- src
    return(out)
  }

  if (identical(src, "bioc")) {
    out[["Available_Sources"]] <- "bioc"
    out[["Preferred_Source"]]  <- "bioc"
    return(out)
  }

  if (identical(src, "github") || identical(src, "gitlab")) {
    out[["Preferred_Source"]] <- src
    return(out)
  }

  if (is_github_url(src)) {
    out[["Preferred_Source"]]  <- "github"
    out[["Github_Username"]]   <- github_username_from_url(src)
    out[["Github_Repository"]] <- github_repo_from_url(src)
    out[["Package"]]           <- out[["Package"]] %||% out[["Github_Repository"]]
    return(out)
  }

  if (is_gitlab_url(src)) {
    out[["Preferred_Source"]]  <- "gitlab"
    out[["Gitlab_Username"]]   <- gitlab_username_from_url(src)
    out[["Gitlab_Repository"]] <- gitlab_repo_from_url(src)
    out[["Package"]]           <- out[["Package"]] %||% out[["Gitlab_Repository"]]
    return(out)
  }

  handle_no_sources()

}

.updateme_skip <- structure("updateme_skip", class = c("updateme_skip", "character"))

updateme_sources_get <- function(check = FALSE) {
  opt <- as.list(getOption("updateme.sources"))

  if (length(opt) == 0L)
    return(NULL)

  names(opt) <- names(opt) %||% rep("", length(opt))

  opt |>
    imap(updateme_sources_validate, throw = if (check) cli::cli_abort) |>
    compact() |>
    unname()
}

is_valid_repo <- function(x) {
  x %in% names(getOption("repos"))
}
