the <- new_environment()


shims_bind <- function(env = caller_env()) {
  # Args to library() and require() changed after 3.6
  if (getRversion() >= "3.6.0") {
    shim_library <- shim_library_3_6
    shim_require <- shim_require_3_6
  } else {
    shim_library <- shim_library_3_1
    shim_require <- shim_require_3_1
  }

  env_bind(
    env,
    library = shim_library,
    require = shim_require
  )

  NULL

}

# library -----------------------------------------------------------------

shim_library_3_1 <- function(package,
                             help,
                             pos = 2,
                             lib.loc = NULL,
                             character.only = FALSE,
                             logical.return = FALSE,
                             warn.conflicts = TRUE,
                             quietly = FALSE,
                             verbose = getOption("verbose")) {

  library <- the$conflicted_shims$library %||% library

  if (!missing(package)) {
    package <- package_name(enquo(package), character.only = character.only)
    extra_attachments <- tidyverse_extra_attachments(package)

    out <- maybe_muffle(package)(library(
      package,
      pos = pos,
      lib.loc = lib.loc,
      character.only = TRUE,
      logical.return = logical.return,
      warn.conflicts = FALSE,
      quietly = quietly,
      verbose = verbose
    ))

    inform_load(package, extra_attachments)
    handle_conflicted()
    invisible(out)

  } else if (!missing(help)) {
    help <- package_name(enquo(help), character.only = character.only)
    library(
      help = help,
      character.only = TRUE
    )
  } else {
    library(
      lib.loc = lib.loc,
      logical.return = logical.return
    )
  }
}

shim_library_3_6 <- function(package,
                             help,
                             pos = 2,
                             lib.loc = NULL,
                             character.only = FALSE,
                             logical.return = FALSE,
                             warn.conflicts,
                             quietly = FALSE,
                             verbose = getOption("verbose"),
                             mask.ok,
                             exclude,
                             include.only,
                             attach.required = missing(include.only)) {

  library <- the$conflicted_shims$library %||% library

  if (!missing(package)) {
    package <- package_name(enquo(package), character.only = character.only)
    extra_attachments <- tidyverse_extra_attachments(package)

    out <- maybe_muffle(package)(library(
      package,
      pos = pos,
      lib.loc = lib.loc,
      character.only = TRUE,
      logical.return = logical.return,
      warn.conflicts = FALSE,
      quietly = quietly,
      verbose = verbose,
      mask.ok = mask.ok,
      exclude = exclude,
      include.only = include.only,
      attach.required = attach.required
    ))

    inform_load(package, extra_attachments)
    handle_conflicted()
    invisible(out)

  } else if (!missing(help)) {
    help <- package_name(enquo(help), character.only = character.only)
    library(
      help = help,
      character.only = TRUE
    )
  } else {
    library(
      lib.loc = lib.loc,
      logical.return = logical.return
    )
  }
}

# require -----------------------------------------------------------------
shim_require_3_1 <- function(package,
                             lib.loc = NULL,
                             quietly = FALSE,
                             warn.conflicts = TRUE,
                             character.only = FALSE) {

  require <- the$conflicted_shims$require %||% require

  package <- package_name(enquo(package), character.only = character.only)
  extra_attachments <- tidyverse_extra_attachments(package)

  out <- maybe_muffle(package)(require(
    package,
    lib.loc = lib.loc,
    quietly = quietly,
    warn.conflicts = FALSE,
    character.only = TRUE
  ))

  inform_load(package, extra_attachments)
  handle_conflicted()
  invisible(out)
}

shim_require_3_6 <- function(package,
                             lib.loc = NULL,
                             quietly = FALSE,
                             warn.conflicts,
                             character.only = FALSE,
                             mask.ok,
                             exclude,
                             include.only,
                             attach.required = missing(include.only)) {

  require <- the$conflicted_shims$require %||% require

  package <- package_name(enquo(package), character.only = character.only)
  extra_attachments <- tidyverse_extra_attachments(package)

  out <- maybe_muffle(package)(require(
    package,
    lib.loc = lib.loc,
    quietly = quietly,
    warn.conflicts = FALSE,
    character.only = TRUE,
    mask.ok = mask.ok,
    exclude = exclude,
    include.only = include.only,
    attach.required = attach.required
  ))

  inform_load(package, extra_attachments)
  handle_conflicted()
  invisible(out)
}


package_name <- function(package, character.only = FALSE) {
  if (!character.only) {
    package <- as.character(quo_squash(package))
  } else {
    package <- eval_tidy(package)
  }

  if (!is.character(package) || length(package) != 1L) {
    cli::cli_abort("{.arg package} must be character vector of length 1.")
  }
  if (is.na(package) || (package == "")) {
    cli::cli_abort("{.arg package} must not be NA or ''.")
  }

  package
}

# Adds compatibility with the {conflicted} package, which also shims library
# and require. Does so by:
# * Saving the conflicted shims in the$conflicted_shims
# * Calling these shims in the updateme shims for library and require from
#   then on
# * Deleting the shims from the conflicted shim environment, to handle cases
#   where updateme gets loaded after conflicted
# This gets called in library(), require(), and .onLoad()
handle_conflicted <- function() {
  if (conflicted_loaded()) {
    the$conflicted_shims <- list(
      library = get("library", ".conflicts"),
      require = get("require", ".conflicts")
    )
    conflicted_maybe_remove("library")
    conflicted_maybe_remove("require")
  }
}

maybe_muffle <- function(pkg) {
  if (pkg == "tidyverse" && updateme_is_on())
    suppressPackageStartupMessages
  else
    identity
}

conflicted_loaded <- function() {
  all(c("package:conflicted", ".conflicts") %in% search())
}

conflicted_maybe_remove <- function(x) {
  if (conflicted_loaded())
    assign(x, NULL, ".conflicts")
}

get_shim <- function(x) {
  if (x %in% ls(".updateme"))
    get(x, ".updateme")
}
