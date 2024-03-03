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

  version      <- desc[["Version"]]

  if (is.null(desc[["Version"]]))
    return(NULL)

  repo         <- desc[["Repository"]]
  remote_type  <- desc[["RemoteType"]]
  is_github    <- identical(remote_type, "github")
  is_gitlab    <- identical(remote_type, "gitlab")
  rmt_username <- desc[["RemoteUsername"]]
  rmt_repo     <- desc[["RemoteRepo"]]
  gl_username  <- desc[["GitlabUsername"]] %||% (if (is_gitlab) rmt_username)
  gl_repo      <- desc[["GitlabRepo"]]     %||% (if (is_gitlab) rmt_repo)
  gh_username  <- desc[["GithubUsername"]] %||% (if (is_github) rmt_username)
  gh_repo      <- desc[["GithubRepo"]]     %||% (if (is_github) rmt_repo)
  remote_url   <- desc[["RemoteUrl"]]
  bioc_views   <- desc[["biocViews"]]
  pkg_urls     <- desc[["URL"]]

  url_repo_details <- repo_details_from_urls(pkg_urls)

  # If no github info set, try getting it from the URL field
  if ((is.null(gh_username) || is.null(gh_repo)) && !is.null(pkg_urls)) {
    gh_username <- url_repo_details[["Github_Username"]]
    gh_repo     <- url_repo_details[["Github_Repository"]]
  }

  # If no gitlab info set, try getting it from the URL field
  if ((is.null(gl_username) || is.null(gl_repo)) && !is.null(pkg_urls)) {
    gl_username <- url_repo_details[["Gitlab_Username"]]
    gl_repo     <- url_repo_details[["Gitlab_Repository"]]
  }

  available_sources <- c(
    if (!is.null(repo)) "repo",
    if (!is.null(gh_repo) && !is.null(gh_username)) "github",
    if (!is.null(gl_repo) && !is.null(gl_username)) "gitlab",
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
    Gitlab_Username   = gl_username,
    Gitlab_Repository = gl_repo,
    Remote_URL        = remote_url,
    Bioc_Views        = bioc_views
  )

}

repo_details_from_urls <- function(x) {
  if (is.null(x))
    return(NULL)

  pkg_urls <- strsplit(x, ",\\s*")[[1]]

  out <- list(
    Github_Username   = NULL,
    Github_Repository = NULL,
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL
  )

  github_urls <- pkg_urls[is_github_url(pkg_urls)]
  if (length(github_urls) > 0L) {
    github_url <- github_urls[1]
    out[["Github_Username"]]   <- github_username_from_url(github_url)
    out[["Github_Repository"]] <- github_repo_from_url(github_url)
  }

  gitlab_urls <- pkg_urls[is_gitlab_url(pkg_urls)]
  if (length(gitlab_urls) > 0L) {
    gitlab_url <- gitlab_urls[1]
    out[["Gitlab_Username"]]   <- gitlab_username_from_url(gitlab_url)
    out[["Gitlab_Repository"]] <- gitlab_repo_from_url(gitlab_url)
  }

  out
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
