test_that("updateme_sources_set() fails correctly", {
  old_opts <- options()
  withr::defer(options(old_opts))

  options(repos = c(
    CRAN = "https://cloud.r-project.org",
    tidyverse = "https://tidyverse.r-universe.dev",
    rlib = "https://r-lib.r-universe.dev"
  ))

  expect_error(updateme_sources_set_impl("foo"),         'Invalid package source "foo"')
  expect_error(updateme_sources_set_impl("CRAN", "foo"), 'Invalid package source "foo"')
  expect_error(updateme_sources_set_impl("http://github.com/wurli/updateme", 'Invalid package source "http'))
  expect_error(updateme_sources_set_impl(), "At least one argument must be supplied")

})

test_that("updateme_sources_set() sets options correctly", {
  old_opts <- options()
  withr::defer(options(old_opts))

  options(repos = c(
    CRAN = "https://cloud.r-project.org",
    tidyverse = "https://tidyverse.r-universe.dev",
    rlib = "https://r-lib.r-universe.dev"
  ))

  expect_equal(
    updateme_sources_set_impl("CRAN"),
                    dots_list("CRAN")
  )
  expect_equal(
    updateme_sources_set_impl(dplyr = "CRAN"),
                         list(dplyr = "CRAN")
  )
  expect_equal(
    updateme_sources_set_impl("CRAN", dplyr = "github", "rlib"),
                         list("CRAN", dplyr = "github", "rlib")
  )
  expect_equal(
    updateme_sources_set_impl("CRAN", dplyr = "gitlab", "rlib"),
                         list("CRAN", dplyr = "gitlab", "rlib")
  )
  expect_equal(
    updateme_sources_set_impl("rlib", "CRAN"),
                    dots_list("rlib", "CRAN")
  )
  expect_equal(
    updateme_sources_set_impl("bioc"),
                    dots_list("bioc")
  )
  expect_equal(
    updateme_sources_set_impl(dplyr = "https://github.com/tidyverse/dplyr"),
                         list(dplyr = "https://github.com/tidyverse/dplyr")
  )
  expect_equal(
    updateme_sources_set_impl(dplyr = NA),
                         list(dplyr = NA)
  )
  expect_equal(
    updateme_sources_set_impl(dplyr = NULL),
                              dots_list()
  )

  # Note that the output is named!
  expect_equal(
    updateme_sources_set_impl("https://github.com/tidyverse/dplyr"),
                         list(dplyr = "https://github.com/tidyverse/dplyr")
  )

})

test_that("updateme_sources_get() works", {

  old_opts <- options()
  withr::defer(options(old_opts))

  options(
    repos = c(
      CRAN = "https://cloud.r-project.org",
      tidyverse = "https://tidyverse.r-universe.dev",
      rlib = "https://r-lib.r-universe.dev"
    )
  )

  options(updateme.sources = NULL)
  expect_identical(updateme_sources_get(), NULL)

  options(updateme.sources = list())
  expect_identical(updateme_sources_get(), NULL)

  options(updateme.sources = "tidyverse")
  expect_identical(updateme_sources_get(), list(
    list(
      Available_Sources = "repo",
      Preferred_Source = "repo",
      Package = NULL,
      Source_Name = "tidyverse",
      Repository = "tidyverse",
      Github_Username = NULL,
      Github_Repository = NULL,
      Remote_URL = NULL,
      Bioc_Views = NULL
    )
  ))

  # Same as above, but using list()
  options(updateme.sources = list("tidyverse"))
  expect_identical(updateme_sources_get(), list(
    list(
      Available_Sources = "repo",
      Preferred_Source = "repo",
      Package = NULL,
      Source_Name = "tidyverse",
      Repository = "tidyverse",
      Github_Username = NULL,
      Github_Repository = NULL,
      Remote_URL = NULL,
      Bioc_Views = NULL
    )
  ))

  options(updateme.sources = list("github"))
  expect_identical(updateme_sources_get(), list(
    list(
      Available_Sources = NULL,
      Preferred_Source = "github",
      Package = NULL,
      Source_Name = "github",
      Repository = NULL,
      Github_Username = NULL,
      Github_Repository = NULL,
      Remote_URL = NULL,
      Bioc_Views = NULL
    )
  ))

  options(updateme.sources = list("gitlab"))
  expect_identical(updateme_sources_get(), list(
    list(
      Available_Sources = NULL,
      Preferred_Source = "gitlab",
      Package = NULL,
      Source_Name = "gitlab",
      Repository = NULL,
      Github_Username = NULL,
      Github_Repository = NULL,
      Remote_URL = NULL,
      Bioc_Views = NULL
    )
  ))

  options(updateme.sources = list(dplyr = "https://github.com/tidyverse/dplyr"))
  expect_identical(updateme_sources_get(), list(
    list(
      Available_Sources = NULL,
      Preferred_Source = "github",
      Package = "dplyr",
      Source_Name = "https://github.com/tidyverse/dplyr",
      Repository = NULL,
      Github_Username = "tidyverse",
      Github_Repository = "dplyr",
      Remote_URL = NULL,
      Bioc_Views = NULL
    )
  ))

  options(updateme.sources = list("CRAN", "https://github.com/tidyverse/dplyr"))
  expect_identical(updateme_sources_get(), list(
    list(
      Available_Sources = "repo",
      Preferred_Source = "repo",
      Package = NULL,
      Source_Name = "CRAN",
      Repository = "CRAN",
      Github_Username = NULL,
      Github_Repository = NULL,
      Remote_URL = NULL,
      Bioc_Views = NULL
    ),
    list(
      Available_Sources = NULL,
      Preferred_Source = "github",
      Package = "dplyr",
      Source_Name = "https://github.com/tidyverse/dplyr",
      Repository = NULL,
      Github_Username = "tidyverse",
      Github_Repository = "dplyr",
      Remote_URL = NULL,
      Bioc_Views = NULL
    )
  ))

})
