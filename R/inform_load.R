inform_load <- function(pkg, extra_attachments = NULL) {

  if (!is_interactive() || !updateme_is_on()) {
    return(invisible(NULL))
  }

  if (identical(pkg, "tidyverse")) {
    cli::cat_line(tidyverse_attach_message(extra_attachments))

    if (!is_attached("conflicted"))
      print(get("tidyverse_conflicts", asNamespace("tidyverse"))())

    return(invisible(NULL))
  }

  is_loaded <- paste0("package:", pkg) %in% search()

  cli::cli_alert_info(paste0(
    if (is_loaded) "Using " else "Loading ",
    "{.pkg {pkg}} ",
    package_version_describe(pkg)
  ))

  invisible(NULL)

}

package_version_describe <- function(pkg, inform_if_ahead = NULL, template = NULL) {

  installation_info <- package_installation_info(pkg)
  current           <- packageVersion(pkg)
  remote_vn_info    <- available_version(installation_info)
  available         <- remote_vn_info[["Source_Version"]]
  source            <- remote_vn_info[["Source_Name"]]
  source_url        <- remote_vn_info[["Source_URL"]]

  inform_if_ahead <- inform_if_ahead %||% grepl("^Bioc", source) %or% FALSE

  new_version_found <- !is.null(available)

  currentness_is_unknown <- !new_version_found ||
    !is.package_version(available) ||
    !is.package_version(current)

  currentness <- if (currentness_is_unknown)
    "unknown"
  else if (current < available)
    "outdated"
  else if (current == available)
    "up_to_date"
  else
    "ahead"

  fmt_currentness <- switch(currentness,
    unknown = cli::col_silver,
    outdated = cli::col_red,
    up_to_date = cli::col_green,
    ahead = cli::col_br_magenta
  )

  show_extra_info <- currentness %in% c(
    "outdated", "unknown", if (inform_if_ahead) "ahead"
  )

  extra_info <- if (!new_version_found || !show_extra_info) {
    ""
  } else {
    src <- if (is.null(source_url))
      source
    else
      cli::format_inline("{.href [{source}]({source_url})}")

    vn <- available

    cli::col_grey(cli::style_italic(cli::format_inline(switch(currentness,
      outdated = ,
      unknown  = template %||% "({vn} now available from {src})",
      ahead    = "({vn} is the latest version on {src})"
    ))))
  }

  paste(fmt_currentness(style_version(current)), extra_info)
}

style_version <- function(x) {
  x <- as.character(x)
  is_dev <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    !is.na(x) & x >= 9000
  }
  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(x) ifelse(is_dev(x), cli::style_italic(x), x))
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
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


