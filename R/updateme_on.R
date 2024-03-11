#' Temporarily turn updateme off for all packages
#'
#' These functions may be useful if you find the slight delay caused by
#' updateme looking up new package versions to be slowing down your workflow.
#'
#' @return `NULL`, invisibly
#' @export
#'
#' @seealso [updateme_sources_set()] to turn updateme off for individual
#'   packages
#'
#' @examples
#' if (FALSE) {
#'   updateme_off()
#'   updateme_on()
#' }
updateme_on <- function() {
  options(updateme.on = TRUE)
  invisible(NULL)
}

#' @rdname updateme_on
#' @export
updateme_off <- function() {
  options(updateme.on = FALSE)
  invisible(NULL)
}

updateme_is_on <- function() {
  getOption("updateme.on", TRUE)
}
