test_that("list_replace works", {

  expect_identical(list_replace(list(a = 1), a = 2), list(a = 2))
  expect_identical(list_replace(list(a = 1), b = 2), list(a = 1))
  expect_identical(list_replace(list(), a = 1), list())
  expect_identical(list_replace(list(a = 1, b = 2), b = 1, a = 2, c = 3), list(a = 2, b = 1))

})
