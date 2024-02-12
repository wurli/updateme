# To do:
# * Gracefully handle failures:
#   * If no internet, wait until available
#   * If anything fails, e.g. because CRAN is down, try again after 4 hours
#     and inform the user
# * Check GitHub for pkgs installed from GitHub
# * Check r-universe for pkgs installed from GitHub
# * Inform about source in 'new available' message
# * Use global option to control where packages are checked from


# General process:
# * At start, get list of installed packages, with sources
# * Get current versions for as many of these as can be done in one go
# * For others, get them at load time
#

.onLoad <- function(...) {

  available_packages <<- memoise::memoise(
    available_packages,
    # reset available packages either on reload or once every 8 hours
    cache = cachem::cache_mem(max_age = 60 * 60 * 8)
  )

  env <- get("attach")(env(), name = ".updateme")

  shims_bind(env)

}
