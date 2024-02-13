.onLoad <- function(...) {

  # Caches last 1 hour
  updateme_cache <- cachem::cache_mem(
    max_age = getOption("updateme.cache_age", 60 * 60)
  )

  available_packages <<- memoise::memoise(
    available_packages, cache = updateme_cache
  )

  available_version_impl_github <<- memoise::memoise(
    available_version_impl_github, cache = updateme_cache
  )

  env <- get("attach")(env(), name = ".updateme")
  shims_bind(env)
  handle_conflicted()
}
