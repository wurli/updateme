inform_load <- function(pkg) {

  installation_info <- package_installation_info(pkg)

  installed_version <- packageVersion(pkg)
  available         <- available_version(installation_info)
  repo_version      <- available[["Source_Version"]]

  new_version_found <- !is.null(repo_version)

  currentness_unknown <- !new_version_found ||
    !is.package_version(repo_version) ||
    !is.package_version(installed_version)

  currentness <- if (currentness_unknown)
    "unknown"
  else if (installed_version < repo_version)
    "outdated"
  else if (installed_version == repo_version)
    "up_to_date"
  else
    "ahead"


  fmt_currentness <- switch(currentness,
    unknown = cli::col_silver,
    outdated = cli::col_red,
    up_to_date = cli::col_green,
    ahead = cli::col_br_magenta
  )

  src_name <- available[["Source_Name"]]
  src_url  <- available[["Source_URL"]]

  extra_info <- if (new_version_found && currentness %in% c("outdated", "unknown")) {
    cli::col_grey(cli::style_italic(
      " (", repo_version, " now available from ",
      if (is.null(src_url))
        src_name
      else
        cli::format_inline("{.href [{src_name}]({src_url})}"),
      ")"
    ))
  }

  cli::cli_alert_info(paste0(
    "Loading ",
    cli::format_inline("{.pkg {pkg}}"), " ",
    fmt_currentness(installed_version),
    extra_info
  ))

  invisible(NULL)

}

available_packages <- function(repos = getOption("repos")) {
  # Do these one-by-one so we can cache packages for individual repos
  pkgs <- lapply(repos, available_packages_impl)
  do.call(rbind, c(pkgs, make.row.names = FALSE))
}


available_packages_impl <- function(repo) {
  available.packages(repos = repo) |>
    as.data.frame(row.names = FALSE) |>
    subset(select = c(Repository, Package, Version))
}

#' Figure out where a package was installed from
#'
#' Checks the following sources (in order):
#'
#'   1. Repository
#'      - Might be a URL, in which case is retrieved using
#'        [available_packages()], or an alias, in which case the URL is
#'        checked against the `repos` global option (if not found, skip to 5.)
#'   2. RemoteType
#'      - if 'github' then make a URL using GithubRepo and GithubUsername fields
#'   3. RemoteUrl (not yet implemented)
#'      - Use to ~ somehow ~ get pkg description? Not sure this can be done
#'      in a general way without downloading the whole repo, which isn't viable.
#'      Maybe just need to support alternative methods like butbucket, gitlab etc
#'      individually.
#'   4. BiocViews
#'      - Use to ~ somehow ~ get pkg description
#'   5. Just check [available_packages()] with default repos
#'   6. Return NULL
#'
#' @param pkg A package
#' @param lib.loc Optionally, the library location
#'
#' @return A named list, including at least the fields `Available_Sources`,
#'   `Package`, `Version_Installed`, and possibly additional fields
#'   `Repository`, `Github_Username`, `Github_Repository`, `Remote_URL`, and
#'   `Bioc_Views`
#' @noRd
package_installation_info <- function(pkg, lib.loc = NULL) {

  desc <- package_description(
    pkg,
    lib.loc = lib.loc,
    fields = c(
      "Version", "URL", "Repository", "RemoteType",
      "RemoteUsername", "RemoteRepo", "GithubUsername", "GithubRepo",
      "RemoteUrl", "biocViews"
    )
  )

  version     <- desc[["Version"]]
  repo        <- desc[["Repository"]]
  remote_type <- desc[["RemoteType"]]
  gh_username <- desc[["GithubUsername"]] %||% desc[["RemoteUsername"]]
  gh_repo     <- desc[["GithubRepo"]] %||% desc[["RemoteRepo"]]
  remote_url  <- desc[["RemoteUrl"]]
  bioc_views  <- desc[["biocViews"]]
  pkg_urls    <- desc[["URL"]]

  if (is.null(desc[["Version"]]))
    return(NULL)

  # If no github info set, try getting it from the URL field
  if ((is.null(gh_username) || is.null(gh_repo)) && !is.null(pkg_urls)) {
    pkg_urls   <- strsplit(pkg_urls, ",\\s*")[[1]]
    github_url <- pkg_urls[is_valid_github_url(pkg_urls)]
    if (length(github_url) > 0) {
      github_url  <- github_url[1]
      gh_username <- github_username_from_url(github_url)
      gh_repo     <- github_repo_from_url(github_url)
    }
  }

  available_sources <- c(
    if (!is.null(repo)) "repo",
    if (!is.null(gh_repo) && !is.null(gh_username)) "github",
    if (!is.null(remote_url)) "remote",
    if (!is.null(bioc_views)) "bioc"
  )

  list(
    Available_Sources = available_sources,
    Package           = pkg,
    Version_Installed = version,
    Repository        = repo,
    Github_Username   = gh_username,
    Github_Repository = gh_repo,
    Remote_URL        = remote_url,
    Bioc_Views        = bioc_views
  )

}



package_description <- function(pkg, lib.loc = NULL, fields = NULL) {
  tryCatch(
    pkg |>
      packageDescription(lib.loc = lib.loc, fields = fields, drop = FALSE) |>
      keep(\(x) !is.na(x) && x != ""),
    warning = function(w) {
      cli::cli_abort(c(
        "No DESCRIPTION file found for {.pkg {pkg}}",
        i = "Original warning: {w$message}"
      ))
    }
  )
}

maybe_as_version <- function(x) {
  try(x <- package_version(x), silent = TRUE)
  x
}

parse_description <- function(x) {
  x <- paste(x, collapse = "\n")
  out <- as.list(read.dcf(textConnection(x))[1, ])
  out[["Version"]] <- maybe_as_version(out[["Version"]])
  out
}


