context("mies")

library(cmaes)
ps = makeParamSet(
  makeNumericVectorParam("x", length=2, lower=-20, upper=20)
)
f = function(x) as.numeric(f_sphere(x$x))

test_that("mies converges on f_sphere", {
  maxit = 20L
  ctrl = makeMiesControl(maxit=maxit)
  z = mies(f, ps, ctrl)
  expect_true(z$value < 0.1)
  expect_equal(getOptPathLength(z$opt.path), ctrl$mu + maxit*ctrl$lambda)
})


test_that("mies budget works", {
  budgets = c(100, 167, 500)
  for (b in budgets) {
    ctrl = makeMiesControl(budget = b)
    z = mies(f, ps, ctrl)
    expect_true(getOptPathLength(z$opt.path) < b)
    expect_true(getOptPathLength(z$opt.path) >= b - ctrl$lambda)
  }
})


test_that("mies maximize works", {
  ctrl = makeMiesControl(minimize=FALSE)
  z = mies(f, ps, ctrl)
  expect_true(z$value >= 799)
  expect_equal(getOptPathLength(z$opt.path), ctrl$mu + ctrl$maxit*ctrl$lambda)
})


test_that("mies vectorized works", {
  f = function(xs) sapply(xs, function(x) sum(x$x^2))
  ctrl = makeMiesControl(vectorized=TRUE)
  z = mies(f, ps, ctrl)
  expect_true(z$value < 0.1)
})

test_that("mies with logical works", {
  ps = makeParamSet(
    makeNumericVectorParam("x", length=2, lower=-20, upper=20),
    makeLogicalParam("w")
  )
  f = function(x) if(x$w) sum(x$x^2) else sum(x$x^2)+1
  ctrl = makeMiesControl()
  z = mies(f, ps, ctrl)
  expect_true(z$value < 0.1)
})



