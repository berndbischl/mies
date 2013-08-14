library(mlrTune)

inner = makeResampleDesc("CV", iters=2)
outer = makeResampleDesc("Subsample", iters=1, split=0.8)
tune.ctrl = makeTuneControlMies(budget=10, mu=2, lambda=4)

ps.rf = makeParamSet(
	#makeNumericParam("fw.threshold", lower=0.01, upper=1),
  #makeDiscreteParam("fw.method", values=c("correlation")),
  makeIntegerParam("ntree", lower=10, upper=10000),
	makeIntegerParam("mtry", lower=1, upper=10),
  makeLogicalParam("replace")
)

makeRFLearner = function(par.set, inner, tune.ctrl) {
  lrn = makeLearner("classif.randomForest")
  #lrn = makeFilterWrapper(lrn)
  #makeTuneWrapper(lrn, resampling=inner, control=tune.ctrl)
}

task = makeClassifTask(data=iris, target="Species")
lrn = makeRFLearner(ps.rf, inner, tune.control)
r = tune(lrn, task, inner, par.set=ps.rf, control=tune.ctrl)
#r = resample(lrn, task, outer)


