#' Configure {updateme} lookup of new package versions
#'
#' This function is a helper for setting the `"updateme.sources"`
#' global option. It provides a user-friendly interface and validation of the
#' options you set.
#'
#' @param ... Named or unnamed arguments. Values should be either:
#'
#'   -   `"github"`/`"gitlab"`: new versions will be found from the package
#'       development version on GitHub/GitLab, if a repo can be identified using
#'       the package `DESCRIPTION`
#'
#'   -   One of `names(getOption("repo"))`: latest versions will be taken from
#'       this source, if available
#'
#'   -   A URL pointing to a GitHub/GitLab repo, e.g.
#'       `"https://github.com/wurli/updateme"`: the latest version *for this
#'       particular package* will be taken from this project
#'
#'   If arguments are named, names should indicate package which the option
#'   should apply to. If unnamed, the option will apply to all packages. See
#'   examples for more information.
#'
#' @section Private Repositories:
#' {updateme} supports packages stored in private repositories on GitHub and
#' GitLab. To get upstream package version from either, you should only
#' have to configure a personal access token (PAT).
#'
#' * For GitHub packages, {updateme} checks, in order:
#'   * The `UPDATEME_GITHUB_PAT` environmental variable
#'   * Any personal access tokens configured using [gitcreds::gitcreds_set()]
#'   * The `GITHUB_PAT` environmental variable
#'   * The `GITHUB_TOKEN` environmental variable
#' * For GitLab packages, {updateme} checks, in order:
#'   * The `UPDATEME_GITLAB_PAT` environmental variable
#'   * Any personal access tokens configured using [gitcreds::gitcreds_set()]
#'   * The `GITLAB_PAT` environmental variable
#'   * The `GITLAB_TOKEN` environmental variable
#'
#'
#' @return The result of setting
#'   `options(updateme.sources = <new_options>)`
#' @export
#'
#' @examples
#' if (FALSE) {
#'
#'   # If you want to check non-standard repos for new versions of packages,
#'   # you'll first have to set the repos global option. Note that each
#'   # option must be named for compatibility with {updateme}
#'   options(repos = c(
#'
#'     # Your default repos, e.g. c(CRAN = "https://cloud.r-project.org")
#'     getOption("repos"),
#'
#'     # The tidyverse r-universe, including dev versions of tidyverse packages
#'     tidyverse = "https://tidyverse.r-universe.dev",
#'
#'     # The r-lib r-universe, including dev versions of infrastructure packages
#'     # like {cli}, {rlang}, etc
#'     rlib = "https://r-lib.r-universe.dev"
#'   ))
#'
#'   # 1. New versions will first be looked up in the tidyverse r-universe
#'   # 2. If not found, they will be looked up from your usual CRAN mirror
#'   # 3. {bslib} will always be first looked up from GitHub
#'   # 4. {cli} will always be first looked up from the r-lib r-universe
#'   updateme_sources_set(
#'     "tidyverse",
#'     "CRAN",
#'     bslib = "https://github.com/rstudio/bslib", # Name is optional here
#'     cli = "rlib"
#'   )
#' }
#'
# TODO: Add .append arg?
updateme_sources_set <- function(...) {
  options(updateme.sources = updateme_sources_set_impl(...))
}

updateme_sources_set_impl <- function(...) {
  # dots_list() prevents names being NULL
  dots     <- dots_list(...)
  out      <- imap(dots, updateme_sources_validate)
  sources  <- map(out, function(x) x[["Source_Name"]])
  packages <- map_chr(out, function(x) x[["Package"]] %||% "")
  set_names(sources, packages)
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
      repos <- names(getOption("repos"))
      throw(call = caller_call(6), c(
        "Invalid package source {.val {src}}.",
        "i" = "Package sources must be either:",
        " " = "1. {.val github}/{.val gitlab}, to check the version on GitHub/GitLab if possible",
        " " = '2. One of {.code names(getOption("repos"))}',
        " " = "3. The URL of a specific GitHub repository, e.g. {.url https://github.com/wurli/updateme}",
        " " = "4. The URL of a specific GitLab repository, e.g. {.url https://gitlab.com/r-packages/yum}"
      ))
    }
    NULL
  }

  src_ok <- is.character(src) || is.null(src)
  pkg_ok <- is.character(pkg) || is.null(pkg)

  if (!src_ok || !pkg_ok) {
    return(handle_no_sources())
  }

  if (pkg == "")
    pkg <- NULL

  out <- list(
    Preferred_Source  = NULL,
    Package           = pkg,
    Source_Name       = src,
    Repository        = NULL,
    Github_Username   = NULL,
    Github_Repository = NULL,
    Remote_URL        = NULL,
    Bioc_Views        = NULL
  )

  if (is_valid_repo(src)) {
    out[["Preferred_Source"]] <- "repo"
    out[["Repository"]] <- src
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
  }

  handle_no_sources()

}

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
