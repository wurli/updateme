test_that("available_version() respects updateme.sources", {

  skip_if_offline()

  old_opts <- options()
  withr::defer(options(old_opts))

  options(repos = c(
    CRAN = "https://cloud.r-project.org",
    tidyverse = "https://tidyverse.r-universe.dev",
    rlib = "https://r-lib.r-universe.dev"
  ))


  info <- list(
    Available_Sources = c("github", "gitlab"),
    Package = "updateme.testpkg",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username = "wurli",
    Github_Repository = "updateme.testpkg",
    Gitlab_Username = "wurl1",
    Gitlab_Repository = "updateme.testpkg",
    Remote_URL = NULL,
    Bioc_Views = NULL
  )

  updateme_sources_set("github")
  expect_identical(available_version(info), list(
    Source_Name = "GitHub",
    Source_URL = "https://github.com/wurli/updateme.testpkg",
    Source_Version = package_version("0.2.0")
  ))

  updateme_sources_set(updateme.testpkg = "gitlab")
  expect_identical(available_version(info), list(
    Source_Name = "GitLab",
    Source_URL = "https://gitlab.com/wurl1/updateme.testpkg",
    Source_Version = package_version("0.2.0")
  ))

  updateme_sources_set(updateme.testpkg = NA)
  expect_identical(available_version(info), NULL)

})
