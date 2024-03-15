.onLoad <- function(...) {

  # Caches last 1 hour
  updateme_cache <- cachem::cache_mem(
    max_age = 60 * getOption("updateme.cache_expiry", 60)
  )

  # Note: <<- does not modify the global environment, it simply memoises
  # existing functions included in this package. This is the approach
  # recommended by the {memoise} package itself - see ?memoise::memoise
  available_packages <<- memoise::memoise(
    available_packages, cache = updateme_cache
  )

  available_version_impl_git <<- memoise::memoise(
    available_version_impl_git, cache = updateme_cache
  )

  env <- get("attach")(env(), name = ".updateme")
  shims_bind(env)
  handle_conflicted()
}
