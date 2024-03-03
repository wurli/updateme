`%or%` <- function(x, y) {
  if (length(x) == 0L) y else x
}

env_var <- function(x) {
  out <- Sys.getenv(x)
  if (identical(out, "")) NULL else out
}

# See testthat::skip_if_offline
is_online <- function(host = "captive.apple.com") {
  !is.null(curl::nslookup(host, error = FALSE))
}

list_replace <- function(x, ...) {
  dots <- list2(...)
  imap(x, function(el, nm) if (nm %in% names(dots)) dots[[nm]] else el)
}

list_is_subset <- function(x, y) {
  for (i in seq_along(x)) {
    nm <- names(x)[i]
    el <- x[[i]]
    if (is.null(el))
      next
    if (!nm %in% names(y) || !identical(el, y[[nm]]))
      return(FALSE)
  }
  TRUE
}

with_timeout <- function(expr, time = Inf){
  on.exit(setTimeLimit())
  setTimeLimit(elapsed = time, transient = TRUE)
  tryCatch(
    eval(substitute(expr), envir = caller_env()),
    error = function(e) {
      if (grepl("reached elapsed time limit", e$message))
        return(NULL)
      stop(e)
    }
  )
}

str_extract_all <- function(x, pattern, invert = FALSE, perl = TRUE) {
  regmatches(x, gregexpr(pattern, x, perl = perl), invert)[[1]]
}

installed_version <- function(pkg) {
  tryCatch(packageVersion(pkg), error = function(e) NULL)
}
