test_that("list_is_subset works", {

  expect_true(list_is_subset(list(a = 1, b = 2), list(a = 1, b = 2)))
  expect_true(list_is_subset(list(a = 1, b = 2), list(a = 1, b = 2, c = 3)))
  expect_false(list_is_subset(list(a = 1, b = 2), list(a = 1)))
  expect_false(list_is_subset(list(a = 1, b = 2), list(a = 1, b = 3)))
  expect_false(list_is_subset(list(a = 1, b = 2, c = 2), list(a = 1, b = 2, c = 3)))

})
