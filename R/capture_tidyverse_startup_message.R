# # For reference later
# capture_tidyverse_startup_message <- function() {
#   out <- ""
#   withCallingHandlers(
#     library(tidyverse),
#     packageStartupMessage = function(x) {
#       out <<- paste0(out, "\n", x$message)
#       tryInvokeRestart("muffleMessage")
#     }
#   )
#   out
# }
#
