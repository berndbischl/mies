context("term conditions")

if (interactive()) {
  
test_that("term conds", {
  ps = makeParamSet(
    makeNumericParam("u", lower=0, upper=10)
  )
  ctrl = makeMiesControl(maxit = 10, stop.fitness=0.5)
  internal = makeInternal(ps, ctrl)
  y.pop = c(1,2,3)
  iter = 10
  expect_false(termBudgetExhausted(iter, ctrl, internal))
  expect_false(termStopFitness(y.pop, ctrl, internal))
  expect_equal(termConditionSatisfied(iter, y.pop, ctrl, internal), -1L)
  iter = 11
  expect_true(termBudgetExhausted(iter, ctrl, internal))
  expect_false(termStopFitness(y.pop, ctrl, internal))
  expect_equal(termConditionSatisfied(iter, y.pop, ctrl, internal), 1L)
  y.pop = c(1,0,3)
  expect_true(termBudgetExhausted(iter, ctrl, internal))
  expect_true(termStopFitness(y.pop, ctrl, internal))
  expect_equal(termConditionSatisfied(iter, y.pop, ctrl, internal), 0L)
})

}