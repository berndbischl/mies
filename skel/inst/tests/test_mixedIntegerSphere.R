context("mixedIntegerSphere")

test_that("mixedIntegerSphere", {
  n.z = n.d = n.r = 5
  mu = 4
  lambda = 10

  ctrl = makeMiesControl(budget=500L, mu=mu, lambda=lambda, log.every=1L)
  ps = makeParamSet(
    makeNumericVectorParam("r", len=n.r, lower=0, upper=10),
    makeIntegerVectorParam("z", len=n.z, lower=0, upper=10),
    makeDiscreteVectorParam("d", len=n.d, values=c(0:9))
  )
  res = mies(mixedIntegerSphere, ps, control=ctrl)
  expect_true(res$value < 1)
}) 
