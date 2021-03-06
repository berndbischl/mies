context("barrierFunction")

test_that("barrierFunction", {
  nr = nz = nd = 5
  ruggedness = 20 
  mu = 4
  lambda = 28

  barrier = makeBarrierFunction(nr=nr, nz=nz, nd=nd, ruggedness=ruggedness)

  ctrl = makeMiesControl(maxit=20, mu=mu, lambda=lambda, log.every=10L)
  ps = makeParamSet(
    makeNumericVectorParam("r", len=nr, lower=1, upper=20),
    makeIntegerVectorParam("z", len=nz, lower=1, upper=20),
    makeDiscreteVectorParam("d", len=nd, values=c(1:20))
  )

  res = mies(barrier, ps, control=ctrl)
}) 

