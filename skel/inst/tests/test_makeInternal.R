context("internal")

if (interactive()) {

test_that("simple num", {
  ps = makeParamSet(
    makeNumericParam("u", lower=1, upper=4),
    makeNumericVectorParam("v", len=2, lower=1:2, upper=10:11)
  )
  ctrl = makeMiesControl()
  i = makeInternal(ps, ctrl)
  expect_equal(i$ps.r, ps)
  expect_equal(i$ps.z, makeParamSet())
  expect_equal(i$ps.d, makeParamSet())
  expect_equal(i$n.r, 3)
  expect_equal(i$n.z, 0)
  expect_equal(i$n.d, 0)
  expect_equal(i$ids.r, c("u", "v", "v"))
  expect_equal(i$ids.z, character(0))
  expect_equal(i$ids.d, character(0))
  expect_equal(i$lower.r, c(u=1, v=1, v=2))
  expect_equal(i$upper.r, c(u=4, v=10, v=11))
  expect_equal(i$lower.z, numeric(0))
  expect_equal(i$upper.z, numeric(0))
  expect_equal(i$values, list())
  expect_equal(i$values.ns, list())
  expect_equal(i$decode.order, 1:2)
})


test_that("simple num / int", {
  ps = makeParamSet(
    makeIntegerVectorParam("w", length=3, lower=7:9, upper=20),
    makeIntegerParam("x", lower=0, upper=6),
    makeNumericParam("u", lower=1, upper=4),
    makeNumericVectorParam("v", length=2, lower=1:2, upper=10:11)
  )
  ctrl = makeMiesControl()
  i = makeInternal(ps, ctrl)
  expect_equal(i$n.r, 3)
  expect_equal(i$n.z, 4)
  expect_equal(i$n.d, 0)
  expect_equal(i$ids.r, c("u", "v", "v"))
  expect_equal(i$ids.z, c("w", "w", "w", "x"))
  expect_equal(i$ids.d, character(0))
  expect_equal(i$lower.r, c(u=1, v=1, v=2))
  expect_equal(i$upper.r, c(u=4, v=10, v=11))
  expect_equal(i$lower.z, c(w=7, w=8, w=9, x=0))
  expect_equal(i$upper.z, c(w=20, w=20, w=20, x=6))
  expect_equal(i$values, list())
  expect_equal(i$values.ns, list())
  expect_equal(i$decode.order, c(3,4,1,2))
})

test_that("num/ int / discrete", {
  ps = makeParamSet(
    makeIntegerVectorParam("w", len=3, lower=7:9, upper=20),
    makeDiscreteParam("d", values=list(a=1, b=list())),
    makeIntegerParam("x", lower=0, upper=6),
    makeNumericParam("u", lower=1, upper=4),
    makeNumericVectorParam("v", length=2, lower=1:2, upper=10:11)
  )
  ctrl = makeMiesControl()
  i = makeInternal(ps, ctrl)
  expect_equal(i$n.r, 3)
  expect_equal(i$n.z, 4)
  expect_equal(i$n.d, 1)
  expect_equal(i$ids.r, c("u", "v", "v"))
  expect_equal(i$ids.z, c("w", "w", "w", "x"))
  expect_equal(i$ids.d, "d")
  expect_equal(i$lower.r, c(u=1, v=1, v=2))
  expect_equal(i$upper.r, c(u=4, v=10, v=11))
  expect_equal(i$lower.z, c(w=7, w=8, w=9, x=0))
  expect_equal(i$upper.z, c(w=20, w=20, w=20, x=6))
  expect_equal(i$values, list(d=list(a=1, b=list())))
  expect_equal(i$values.ns, list(d=c("a", "b")))
  expect_equal(i$decode.order, c(3,5,4,1,2))
})
  
}
  

