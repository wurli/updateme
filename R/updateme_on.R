#' Temporarily turn updateme off for all packages
#'
#' These functions may be useful if you find the slight delay caused by
#' updateme looking up new package versions to be slowing down your workflow.
#'
#' @return The old `updateme.on` option, as returned by `options()`
#' @export
#'
#' @seealso [updateme_sources_set()] to turn updateme off for individual
#'   packages
#'
updateme_on <- function() {
  options(updateme.on = TRUE)
}

#' @rdname updateme_on
#' @export
updateme_off <- function() {
  options(updateme.on = FALSE)
}

updateme_is_on <- function() {
  getOption("updateme.on", TRUE)
}
