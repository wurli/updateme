test_that("Description files can be read from GitHub", {

  skip_if_offline()

  desc <- desc_from_github("wurli", "updateme.testpkg")

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
