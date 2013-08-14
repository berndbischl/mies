context("eval inds")
#
#if (interactive()) {
  
#  test_that("simple num / int", {
#      ps = makeParamSet(
#        makeIntegerVectorParam("w", length=3, lower=7:9, upper=20),
#        makeIntegerParam("x", lower=0, upper=6),
#        makeNumericParam("u", lower=1, upper=4),
#        makeNumericVectorParam("v", length=2, lower=1:2, upper=10:11)
#      