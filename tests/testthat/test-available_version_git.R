test_that("Description files can be read from GitHub public repos", {

  skip_if_offline()

  desc <- desc_from_git("wurli", "updateme.testpkg")

  expect_identical(desc, c(
    "Package: updateme.testpkg",
    "Title: For 'updateme' Testing",
    "Version: 0.2.0",
    "Authors@R: person(\"Jacob\", \"Scott\", email = \"jscott2718@gmail.com\", role = c(\"aut\", \"cre\"))",
    "Description: This package is used in the {updateme} test suite.",
    "License: MIT + file LICENSE", "Encoding: UTF-8", "Roxygen: list(markdown = TRUE)",
    "RoxygenNote: 7.2.3"
  ))

})

test_that("Description files can be read from GitHub private repos", {

  skip_if_offline()
  skip_on_ci()

  # NOTE: This PAT is 'fine-grained' and gives read-only access for a single
  # repo containing an empty package {updateme.testpkg.private}.
  withr::local_envvar(list(
    GITHUB_PAT = paste0(
      "github_pat_11AEFKREY0bCtmUEn37oc7_MhIhcuvThFPzQ9Nmd74Bf",
      "kydKkvUgKx2NvYw9DEYtw7WUJ7VPW6UnvQVppv"
    )
  ))

  desc <- desc_from_git("wurli", "updateme.testpkg.private", type = "github")

  expect_identical(desc, c(
    "Package: updateme.testpkg.private",
    "Title: For 'updateme' Testing",
    "Version: 0.2.0",
    "Authors@R: person(\"Jacob\", \"Scott\", email = \"jscott2718@gmail.com\", role = c(\"aut\", \"cre\"))",
    "Description: This package is used in the {updateme} test suite.",
    "License: MIT + file LICENSE", "Encoding: UTF-8", "Roxygen: list(markdown = TRUE)",
    "RoxygenNote: 7.2.3"
  ))

})

test_that("Description files can be read from GitLab public repos", {

  skip_if_offline()

  desc <- desc_from_git("wurl1", "updateme.testpkg", type = "gitlab")

  expect_identical(desc, c(
    "Package: updateme.testpkg",
    "Title: For 'updateme' Testing",
    "Version: 0.2.0",
    "Authors@R: person(\"Jacob\", \"Scott\", email = \"jscott2718@gmail.com\", role = c(\"aut\", \"cre\"))",
    "Description: This package is used in the {updateme} test suite.",
    "License: MIT + file LICENSE", "Encoding: UTF-8", "Roxygen: list(markdown = TRUE)",
    "RoxygenNote: 7.2.3"
  ))

})

test_that("Description files can be read from GitLab private repos", {

  skip_if_offline()
  skip_on_ci()

  # NOTE: This PAT is 'fine-grained' and gives read-only access for a single
  # repo containing an empty package {updateme.testpkg.private}.
  withr::local_envvar(list(
    GITLAB_PAT = "glpat-idE4upwYGmHjsiH-YYzw"
  ))

  desc <- desc_from_git("wurli", "updateme.testpkg.private")

  expect_identical(desc, c(
    "Package: updateme.testpkg.private",
    "Title: For 'updateme' Testing",
    "Version: 0.2.0",
    "Authors@R: person(\"Jacob\", \"Scott\", email = \"jscott2718@gmail.com\", role = c(\"aut\", \"cre\"))",
    "Description: This package is used in the {updateme} test suite.",
    "License: MIT + file LICENSE", "Encoding: UTF-8", "Roxygen: list(markdown = TRUE)",
    "RoxygenNote: 7.2.3"
  ))

})
