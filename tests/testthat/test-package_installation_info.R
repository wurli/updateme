test_that("package_installation_info works with no available sources", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0'
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = NULL,
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username = NULL,
    Github_Repository = NULL,
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL,
    Remote_URL = NULL,
    Bioc_Views = NULL
  ))

})

test_that("package_installation_info works with repos packages", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      "Repository: CRAN" # This is the important bit
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = "repo",
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = "CRAN",
    Github_Username = NULL,
    Github_Repository = NULL,
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL,
    Remote_URL = NULL,
    Bioc_Views = NULL
  ))

})

test_that("package_installation_info works with desc URL field", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      'URL: https://github.com/foofyfooson/foo' # This is the important bit
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = "github",
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username = "foofyfooson",
    Github_Repository = "foo",
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL,
    Remote_URL = NULL,
    Bioc_Views = NULL
  ))

})

test_that("package_installation_info works with desc GithubRepo/GithubUsername fields", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      'GithubUsername: foofyfooson',
      'GithubRepo: foo'
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = "github",
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username = "foofyfooson",
    Github_Repository = "foo",
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL,
    Remote_URL = NULL,
    Bioc_Views = NULL
  ))

})


test_that("package_installation_info works with desc GitLab fields", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      'RemoteType: gitlab',
      'RemoteUsername: foofyfooson',
      'RemoteRepo: foo'
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = "gitlab",
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username = NULL,
    Github_Repository = NULL,
    Gitlab_Username   = "foofyfooson",
    Gitlab_Repository = "foo",
    Remote_URL = NULL,
    Bioc_Views = NULL
  ))

})

test_that("package_installation_info can pare URL field", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      'URL: https://github.com/foofyfooson/foo, https://gitlab.com/foofyfooson/foo'
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = c("github", "gitlab"),
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username   = "foofyfooson",
    Github_Repository = "foo",
    Gitlab_Username   = "foofyfooson",
    Gitlab_Repository = "foo",
    Remote_URL = NULL,
    Bioc_Views = NULL
  ))

})

test_that("package_installation_info works with desc RemoteRepo/RemoteUsername fields", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      'RemoteType: github',
      'RemoteUsername: foofyfooson',
      'RemoteRepo: foo'
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = "github",
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username = "foofyfooson",
    Github_Repository = "foo",
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL,
    Remote_URL = NULL,
    Bioc_Views = NULL
  ))

})

test_that("package_installation_info works with desc biocViews fields", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      "biocViews: Infrastructure"
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = "bioc",
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = NULL,
    Github_Username = NULL,
    Github_Repository = NULL,
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL,
    Remote_URL = NULL,
    Bioc_Views = "Infrastructure"
  ))

})

test_that("package_installation_info works with multiple sources", {

  withr::local_temp_libpaths()
  libpath <- .libPaths()[1]

  pkg_dir <- file.path(libpath, "foo")
  dir.create(pkg_dir)

  writeLines(
    con = file.path(pkg_dir, "DESCRIPTION"),
    paste(
      sep = "\n",
      'Package: foo',
      'Type: Package',
      'Version: 0.1.0',
      "Repository: CRAN",
      "biocViews: Infrastructure",
      'URL: https://github.com/foofyfooson/foo'
    )
  )

  info <- package_installation_info("foo")

  expect_identical(info, list(
    Available_Sources = c("repo", "github", "bioc"),
    Package = "foo",
    Version_Installed = "0.1.0",
    Repository = "CRAN",
    Github_Username = "foofyfooson",
    Github_Repository = "foo",
    Gitlab_Username   = NULL,
    Gitlab_Repository = NULL,
    Remote_URL = NULL,
    Bioc_Views = "Infrastructure"
  ))

})
