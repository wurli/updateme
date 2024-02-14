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