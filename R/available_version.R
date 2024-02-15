#' Get the available version of a package
#'
#' @param desc_src Output from [package_installation_info()], or a package name
#'   (for convenience/testing purposes)
#'
#' @return Either `NULL` or a list with fields `Source_Name`, `Source_Version`,
#'   and possibly `Source_URL`
#'
#' @noRd
available_version <- function(desc_src) {

  if (!is_online()) {
    return(NULL)
  }

  # Handle cases where `x` is just a package name - for convenience/testing only
  if (is.character(desc_src) && length(desc_src) == 1L)
    desc_src <- package_installation_info(desc_src)

  preferred_srcs <- preferred_sources(desc_src[["Package"]])

  # Check each of the sources specified in getOption("updateme.sources")
  for (src in preferred_srcs) {
    vn <- available_version_impl(
      list_replace(desc_src, !!!compact(src)),
      type = src[["Preferred_Source"]]
    )
    if (!is.null(vn))
      return(vn)
  }

  # Names: source type
  # Values: required fields to be able to search this kind of repo
  # ! Order is important
  src_type_defaults <- list(
    "repo"   = c("Package", "Repository"),
    "github" = c("Package", "Github_Username", "Github_Repository"),
    "remote" = c(), # Not implemented yet
    "bioc"   = c()  # Not implemented yet
  )

  for (src_type in names(src_type_defaults)) {
    required_fields <- src_type_defaults[[src_type]]

    # If we've already checked this (because it's specified in one of
    # getOption("updateme.sources")), skip so we don't check it again
    already_checked <- FALSE
    for (preferred_src in preferred_srcs)
      already_checked <- list_is_subset(desc_src[required_fields], preferred_src)
    if (already_checked)
      next

    vn <- available_version_impl(desc_src, src_type)
    if (!is.null(vn))
      return(vn)
  }

  NULL
}

available_version_impl <- function(x, type = c("repo", "github", "remote", "bioc")) {
  type <- match.arg(type)
  pkg <- x[["Package"]]

  if (!type %in% x[["Available_Sources"]])
    return(NULL)

  # Packages from CRAN/CRAN-like sources such as r-universe
  if (identical(type, "repo"))
    return(available_version_impl_repo(pkg, x[["Repository"]]))

  # Packages from GitHub
  if (identical(type, "github"))
    return(available_version_impl_github(
      x[["Package"]],
      x[["Github_Username"]],
      x[["Github_Repository"]]
    ))

  # TODO: Implement remote and bioc
  NULL
}

available_version_impl_repo <- function(pkg, repo = NULL) {
  repos_option <- getOption("repos")

  if (is.null(repo)) {
    repo <- repos_option
  } else if (repo %in% names(repos_option)) {
    repo <- as.list(repos_option)[[repo]]
  }

  pkg_data <- available_packages(repos = repo) |>
    subset(Package == pkg) |>
    subset(Version == max(Version)) # E.g. if there are multiple repos

  if (nrow(pkg_data) == 0L) {
    return(NULL)
  } else if (nrow(pkg_data) > 1L) {
    pkg_data <- pkg_data[1, , drop = FALSE]
  }

  # The last bit of the repo URL can contain some extra stuff - remove this so
  # we can detect whether it's the same as a repo from the `repos` global option
  pattern <- pkg_data[["Repository"]] |>
    sub(x = _, "/R/.+$", "") |>
    sub(x = _, "/src/contrib$", "")

  repo_alias <- names(repos_option)[grepl(pattern, repos_option, fixed = TRUE)]
  if (length(repo_alias) == 0L || identical(repo_alias, ""))
    repo_alias <- repo

  list(
    Source_Name = repo_alias,
    Source_URL = repo,
    Source_Version = maybe_as_version(pkg_data[["Version"]])
  )
}


available_version_impl_github <- function(pkg, username, repo) {
  response <- desc_from_github(username, repo, pkg)

  if (is.null(response))
    return(response)

  desc <- parse_description(response)
  github_name <- desc[["Package"]] %||% pkg

  # Handle cases when the returned DESCRIPTION isn't for the correct package
  if (!identical(github_name, pkg)) {
    cli::cli_warn(c(
      "Incorrect repo specification for package {.pkg {pkg}}",
      i = "Found DESCRIPTION file for package {.pkg {github_name}}",
      i = "Check the file at {.url {make_github_url(username, repo)}}"
    ))
    return(NULL)
  }

  github_version <- desc[["Version"]]

  list(
    Source_Name = "GitHub",
    Source_URL = paste0("https://github.com/", username, "/", repo),
    Source_Version = github_version
  )

}

desc_from_github <- function(username, repo, pkg = repo, use_curl = TRUE) {
  file_url <- paste0(
    "https://raw.githubusercontent.com/",
    username, "/", repo,
    "/HEAD/DESCRIPTION"
  )

  handle <- curl::new_handle()
  pat <- get_github_pat()

  if (!is.null(pat))
    curl::handle_setheaders(handle, Authorization = paste("token", get_github_pat()))

  con <- curl::curl(file_url, handle = handle)

  tryCatch(
    readLines(con, warn = FALSE),
    error = function(e) {
      msg <- e$message

      private_repo_msg <- if (is.null(get_github_pat()))
        "If the repo is private, consider setting a PAT using {.fun gitcreds::gitcreds_set}"

      warning_msg <- if (grepl("404", msg)) {
        c(
          i = "{.val 404} error: DESCRIPTION not found",
          i = private_repo_msg,
          i = paste(
            "Is the repo private? Perhaps you need to configure",
            "an {.topic [access token](updateme::`private-repos`)}."
          )
        )
      } else if (grepl("302", msg)) {
        c(i = "{.val 302} response: not yet implemented") # TODO
      } else if (grepl("403", msg)) {
        c(i = "{.val 403} error: access forbidden")
      } else {
        c(i = "DESCRIPTION file not accessible: {msg}")
      }

      cli::cli_warn(c(
        "Failed attempting to get a package version for {.pkg {pkg}} from GitHub",
        warning_msg,
        i = "Error occurred accessing URL {.url {file_url}}"
      ))

      NULL
    }
  )
}

get_github_pat <- function() {
  # 1. check special updateme env var
  updateme_github_pat <- env_var("UPDATEME_GITHUB_PAT")
  if (!is.null(updateme_github_pat))
    return(updateme_github_pat)

  # 2. check w/{gitcreds} pkg
  if (is_installed("gitcreds")) {
    # gitcreds may error if no git installed, no creds set, etc
    try(silent = TRUE, {
      pat <- gitcreds::gitcreds_get()[["password"]]
      return(pat)
    })
  }

  # 3. Check standard env vars
  env_var("GITHUB_PAT") %||% env_var("GITHUB_TOKEN")
}
