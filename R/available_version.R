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
    if (identical(src[["Preferred_Source"]], .updateme_skip))
      return(invisible(NULL))

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
    "bioc"   = c("Package"),
    "github" = c("Package", "Github_Username", "Github_Repository"),
    "gitlab" = c("Package", "Gitlab_Username", "Gitlab_Repository"),
    "remote" = c() # Not implemented yet
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

available_version_impl <- function(x, type = c("repo", "github", "gitlab", "remote", "bioc")) {
  type <- match.arg(type)
  pkg <- x[["Package"]]

  if (!type %in% x[["Available_Sources"]])
    return(NULL)

  # Packages from CRAN/CRAN-like sources such as r-universe
  if (identical(type, "repo"))
    return(available_version_impl_repo(pkg, x[["Repository"]]))

  # Packages from Bioconductor
  if (identical(type, "bioc"))
    return(available_version_impl_bioc(pkg, version = bioc_version()))

  # Packages from GitHub
  if (identical(type, "github"))
    return(available_version_impl_github(
      x[["Package"]],
      x[["Github_Username"]],
      x[["Github_Repository"]]
    ))

  # Packages from GitLab
  if (identical(type, "gitlab"))
    return(available_version_impl_gitlab(
      x[["Package"]],
      x[["Gitlab_Username"]],
      x[["Gitlab_Repository"]]
    ))

  NULL
}

available_version_impl_repo <- function(pkg, repo = NULL, repo_alias = NULL) {
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

  if (is.null(repo_alias)) {
    # The last bit of the repo URL can contain some extra stuff - remove this so
    # we can detect whether it's the same as a repo from the `repos` global option
    pattern <- pkg_data[["Repository"]] |>
      sub(x = _, "/R/.+$", "") |>
      sub(x = _, "/src/contrib$", "")

    repo_alias <- names(repos_option)[grepl(pattern, repos_option, fixed = TRUE)][1]
    if (is.null(repo_alias) || any(is.na(repo_alias)) || identical(repo_alias, ""))
      repo_alias <- repo
  }

  list(
    Source_Name = repo_alias,
    Source_URL = repo,
    Source_Version = maybe_as_version(pkg_data[["Version"]])
  )
}

available_version_impl_bioc <- function(pkg, version = bioc_version()) {
  if (!inherits(version, "numeric_version"))
    return(NULL)

  available_version_impl_repo(
    pkg,
    bioc_repo(version),
    paste("Bioconductor", version)
  )
}

available_version_impl_github <- function(pkg, username, repo) {
  available_version_impl_git(pkg, username, repo, "github")
}

available_version_impl_gitlab <- function(pkg, username, repo) {
  available_version_impl_git(pkg, username, repo, "gitlab")
}

available_version_impl_git <- function(pkg, username, repo, type = c("github", "gitlab")) {
  response <- desc_from_git(username, repo, pkg, type = type)

  if (is.null(response))
    return(response)

  desc <- parse_description(response)
  returned_name <- desc[["Package"]] %||% pkg

  # Handle cases when the returned DESCRIPTION isn't for the correct package
  if (!identical(returned_name, pkg)) {
    cli::cli_warn(c(
      "Incorrect repo specification for package {.pkg {pkg}}",
      i = "Found DESCRIPTION file for package {.pkg {returned_name}}",
      i = "Check the file at {.url {github_url_make(username, repo)}}"
    ))
    return(NULL)
  }

  returned_version <- desc[["Version"]]

  list(
    Source_Name = switch(type, github = "GitHub", gitlab = "GitLab"),
    Source_URL = switch(type,
      github = github_url_make(username, repo),
      gitlab = gitlab_url_make(username, repo)
    ),
    Source_Version = returned_version
  )

}

desc_from_git <- function(username, repo, pkg = repo, type = c("github", "gitlab")) {
  type <- match.arg(type)

  pat <- get_git_pat(type)

  switch(type,
    github = {
      git_name <- "GitHub"
      file_url <- paste0(
        "https://raw.githubusercontent.com/",
        username, "/", repo, "/HEAD/DESCRIPTION"
      )
      auth_header <- list(Authorization = paste("token", pat))
    },
    gitlab = {
      git_name <- "GitLab"
      file_url <- paste0(
        "https://gitlab.com/api/v4/projects/",
        utils::URLencode(paste0(username, "/", repo), reserved = TRUE),
        "/repository/files/DESCRIPTION/raw"
      )
      auth_header <- list(`PRIVATE-TOKEN` = pat)
    }
  )

  handle <- curl::new_handle()

  if (!is.null(pat))
    curl::handle_setheaders(handle, .list = auth_header)

  con <- curl::curl(file_url, handle = handle)
  on.exit(try(close(con), silent = TRUE))

  tryCatch(
    readLines(con, warn = FALSE),
    error = function(e) {
      msg <- e$message

      private_repo_msg <- if (is.null(pat))
        "If the repo is private, consider setting a PAT using {.fun gitcreds::gitcreds_set}"

      warning_msg <- if (grepl("404", msg)) {
        c(
          i = "{.val 404} error: DESCRIPTION not found",
          i = private_repo_msg,
          i = paste(
            "Is the repo private? Perhaps you need to configure",
            "an {.topic [access token](updateme::updateme_sources_set)}."
          )
        )
      } else if (grepl("302", msg)) {
        c(i = "{.val 302} response: not yet implemented") # TODO: not exactly sure what this means
      } else if (grepl("403", msg)) {
        c(i = "{.val 403} error: access forbidden")
      } else {
        c(i = "DESCRIPTION file not accessible: {msg}")
      }

      cli::cli_warn(c(
        "Failed attempting to get a package version for {.pkg {pkg}} from {git_name}",
        warning_msg,
        i = "Error occurred accessing URL {.url {file_url}}"
      ))

      NULL
    }
  )
}

get_git_pat <- function(type = c("github", "gitlab")) {
  type <- match.arg(type)

  switch(type,
    github = {
      envvars  <- c("GITHUB_PAT", "GITHUB_TOKEN")
      git_name <- "GitHub"
      git_url  <- "https://github.com"
    },
    gitlab = {
      envvars  <- c("GITLAB_PAT", "GITLAB_TOKEN")
      git_name <- "GitLab"
      git_url  <- "https://gitlab.com"
    }
  )

  # Check standard env vars
  for (envvar in envvars) {
    pat <- env_var(envvar)
    if (!is.null(pat))
      return(pat)
  }

  # Check using {gitcreds}
  if (is_installed("gitcreds")) {
    # gitcreds may error if no git installed, no creds set, etc
    try(silent = TRUE, {
      pat <- gitcreds::gitcreds_get(git_url)[["password"]]
      return(pat)
    })
  }

  NULL
}
