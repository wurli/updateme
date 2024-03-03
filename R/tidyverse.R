tidyverse_core_packages <- function() {
  c(
    "ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr", "forcats",
    if (installed_version("tidyverse") >= package_version("2.0.0")) "lubridate"
  )
}

tidyverse_extra_attachments <- function(pkg) {
  if (pkg != "tidyverse" || !is_installed("tidyverse"))
    return(NULL)

  tidyverse_pkgs <- tidyverse_core_packages()
  search <- paste0("package:", tidyverse_pkgs)
  tidyverse_pkgs[!search %in% search()]
}

tidyverse_attach_message <- function(to_load = tidyverse_core_packages()) {

  if (length(to_load) == 0)
    return(NULL)

  header <- cli::rule(
    left = cli::style_bold("Attaching core tidyverse packages"),
    right = paste0(
      "tidyverse ",
      package_version_describe("tidyverse", template = "({src} = {vn})")
    )
  )

  to_load <- sort(to_load)
  versions <- vapply(to_load, package_version_describe, character(1), template = "{src} = {vn}")

  col1 <- seq_len(ceiling(length(to_load) / 2))

  col1_text <- paste0(
    cli::col_green(cli::symbol$tick), " ", cli::col_blue(format(to_load[col1])), " ",
    cli::ansi_align(style_version(versions[col1]), max(cli::ansi_nchar(versions[col1])))
  )

  col2_text <- paste0(
    cli::col_green(cli::symbol$tick), " ", cli::col_blue(format(to_load[-col1])), " ",
    cli::ansi_align(style_version(versions[-col1]), max(cli::ansi_nchar(versions[-col1])))
  )

  if (length(to_load) %% 2 == 1)
    col2_text <- append(col2_text, "")

  info <- paste0(col1_text, "     ", col2_text)

  paste0(header, "\n", paste(info, collapse = "\n"))
}
