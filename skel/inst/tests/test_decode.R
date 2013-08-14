context("decode")

if (interactive()) {

test_that("num/ int / discrete", {
  ps = makeParamSet(
    makeIntegerVectorParam("w", length=3, lower=7:9, upper=20),
    makeDiscreteParam("d1", values=list(a=1, b=list())),
    makeIntegerParam("x", lower=0, upper=6),
    makeNumericParam("u", lower=1, upper=4),
    makeNumericVectorParam("v", length=2, lower=1:2, upper=10:11),
    makeDiscreteVectorParam("d2", length=2, values=1:2)
  )
  ctrl = makeMiesControl()
  i = makeInternal(ps, ctrl)
  x1 = list(r=c(1,2,3), z=6:9, d=c("b", "2", "1"))
  x2 = decode(x1, i)
  d2.vals = list(2L, 1L)
  names(d2.vals) = c("2", "1")
  expect_equal(x2, list(w=6:8, d1=list(), x=9, u=1, v=2:3, d2=d2.vals))
  
  ps = makeParamSet(
    makeNumericParam("b"),
    makeNumericParam("a")
  )
  ctrl = makeMiesControl()
  i = makeInternal(ps, ctrl)
  x1 = list(r=c(1,2), z=integer(0), d=character(0))
  expect_equal(decode(x1, i), list(b=1, a=2))
})
  
}
  



