context("ops")

if (interactive()) {

test_that("mutate", {
  ps = makeParamSet(
    makeIntegerParam("u", lower=0, upper=10),
    makeDiscreteParam("v", values=list(a=1, b=list())),
    makeNumericVectorParam("w", len=2, lower=0, upper=10)
  )
  ctrl = makeMiesControl()
  i = makeInternal(ps, ctrl)
  ctrl = initTaus(ctrl, i)
  x1 = list(r=c(1, 2), z=c(5L), d=c("a"), sigma=c(0.1, 0.1), xi=c(0.5), p=c(0.5))
  x2 = mutate(x1, ps, ctrl, i)
  expect_true(0 <= x2$r[1] && x2$r[1] <= 10)
  expect_true(0 <= x2$r[2] && x2$r[2] <= 10)
  expect_true(0 <= x2$z[1] && x2$z[1] <= 10)
  expect_true(x2$d[1] %in% c("a", "b"))
  expect_true(is.numeric(x2$sigma) && length(x2$sigma) == 2)
  expect_true(is.numeric(x2$xi) && length(x2$xi) == 1)
  expect_true(is.numeric(x2$p) && length(x2$p) == 1)
})

test_that("mutate log", {
  ps = makeParamSet(
    makeLogicalParam("u")
  )
  ctrl = makeMiesControl()
  i = makeInternal(ps, ctrl)
  ctrl = initTaus(ctrl, i)
  x1 = list(r=numeric(0), z=integer(0), d=c("TRUE"), sigma=numeric(0), xi=numeric(0), p=c(1))
  x2s = replicate(100, mutate(x1, ps, ctrl, i), simplify=FALSE)
  expect_equal(unique(unlist(extractSubList(x2s, "d"))), "FALSE")
  x1 = list(r=numeric(0), z=integer(0), d=c("TRUE"), sigma=numeric(0), xi=numeric(0), p=c(0))
  x2s = replicate(100, mutate(x1, ps, ctrl, i), simplify=FALSE)
  expect_equal(unique(unlist(extractSubList(x2s, "d"))), "TRUE")
  x1 = list(r=numeric(0), z=integer(0), d=c("TRUE"), sigma=numeric(0), xi=numeric(0), p=c(0.5))
  x2s = replicate(100, mutate(x1, ps, ctrl, i), simplify=FALSE)
  tab = table(unlist(extractSubList(x2s, "d")))
  expect_true(40 < tab[1] && tab[1] < 60)
  expect_true(40 < tab[2] && tab[2] < 60)
})


test_that("recombine", {
  x1 = list(r=c(1, 2), z=c(5L), d=c("a"), sigma=c(0.1, 0.1), xi=c(1), p=c(0.5))
  x2 = list(r=c(1, 3), z=c(6L), d=c("a"), sigma=c(0.3, 0.1), xi=c(1), p=c(0.5))
  expect_equal(recombine(x1, x1), x1)
  expect_equal(recombine(x2, x2), x2)
  x3 = recombine(x1, x2)
  expect_true(x3$r[1] %in% c(1))
  expect_true(x3$r[2] %in% c(2, 3))
  expect_true(x3$z[1] %in% c(5,6))
  expect_true(x3$d[1] %in% c("a"))
  expect_equal(x3$sigma, c(0.2, 0.1))
  expect_equal(x3$xi, c(1))
  expect_equal(x3$p, c(0.5))
})


test_that("recombine num only", {
  x1 = list(r=c(1, 2), z=integer(0), d=character(0), sigma=c(0.1, 0.1), xi=numeric(0), p=numeric(0))
  x2 = list(r=c(1, 3), z=integer(0), d=character(0), sigma=c(0.3, 0.1), xi=numeric(0), p=numeric(0))
  expect_equal(recombine(x1, x1), x1)
  expect_equal(recombine(x2, x2), x2)
  x3 = recombine(x1, x2)
  expect_true(x3$r[1] %in% c(1))
  expect_true(x3$r[2] %in% c(2, 3))
  expect_equal(x3$z, integer(0))
  expect_equal(x3$d, character(0))
  expect_equal(x3$sigma, c(0.2, 0.1))
  expect_equal(x3$xi, numeric(0))
  expect_equal(x3$p, numeric(0))
})

}
