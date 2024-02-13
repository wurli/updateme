`%or%` <- function(x, y) {
  if (length(x) == 0L) y else x
}

env_var <- function(x) {
  out <- Sys.getenv(x)
  if (identical(out, "")) NULL else out
}

is_online <- function() {
  # curl is faster than pingr, but has hefty system requirements
  if (is_installed("curl"))
    return(!is.null(curl::nslookup("google.com", error = FALSE)))
  if (is_installed("pingr"))
    return(pingr::is_online(timeout = 1))
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

