# TODO:
#
# ### Before CRAN
# - bioconductor support
# - If curl not installed, don't bother pinging - just try and handle the error.
#   This should be slightly faster.
# - reasonable unit testing
# - handle tidyverse loading message
#
# ### One day
# - add timeouts, and option to control
# - check for internet connection more intelligently. In some cases, could still
#   use the `available.packages()` cache, even if internet is down. Currently,
#   this doesn't happen.

#' @keywords internal
"_PACKAGE"


globalVariables(c(
  "Package", "Repository", "Version"
))

#' @import rlang
#' @importFrom utils available.packages packageDescription packageVersion

## usethis namespace: start
## usethis namespace: end
NULL
