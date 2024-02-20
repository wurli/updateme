inform_load <- function(pkg) {

  if (!is_interactive() || !updateme_is_on()) {
    return(invisible(NULL))
  }

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


