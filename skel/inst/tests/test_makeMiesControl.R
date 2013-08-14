context("MiesControl")

test_that("budget works", {
  expect_equal(makeMiesControl()$maxit, 10)
  expect_error(makeMiesControl(budget=10, maxit=10), "Only one")
})
