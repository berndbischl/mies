# Initialization of strategy parameters for self-adaptive mutation
# @param control [\code{\link{MiesControl}}]\cr
#   Control object.
# @param internal [\code{list}]\cr
#   Internal constants.
# @return initialized control object 
initTaus = function(control, internal) {
  if (is.na(control$tau.g.r))
    control$tau.g.r = 1 / sqrt(2*internal$n.r)
  if (is.na(control$tau.l.r))
    control$tau.l.r = 1 / sqrt(2*sqrt(internal$n.r))
  if (is.na(control$tau.g.z))
    control$tau.g.z = 1 / sqrt(2*internal$n.z)
  if (is.na(control$tau.l.z))
    control$tau.l.z = 1 / sqrt(2*sqrt(internal$n.z))
  if (is.na(control$tau.g.d))
    control$tau.g.d = 1 / sqrt(2*internal$n.d)
  if (is.na(control$tau.l.d))
    control$tau.l.d = 1 / sqrt(2*sqrt(internal$n.d))
  return(control)
}

# Initialization of population.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set that describes types and constraints.
# @param control [\code{\link{MiesControl}}]\cr
#   Control object.
# @param internal [\code{list}]\cr
#  Internal constants.
# @return [\code{list}] population: list of individuals.
initPopulation = function(par.set, control, internal) {
  i = internal
  width.r = i$upper.r - i$lower.r
  width.z = i$upper.z - i$lower.z
  
  if (i$n.r > 0) {
    p.r = makeNumericVectorParam("r", length=i$n.r, lower=i$lower.r, upper=i$upper.r)
    p.sigma = makeNumericVectorParam("sigma", length=i$n.r, lower=0, upper=width.r/5)
  }
  if (i$n.z > 0) {
    p.z = makeIntegerVectorParam("z", length=i$n.z, lower=i$lower.z, upper=i$upper.z)
    p.xi = makeNumericVectorParam("xi", length=i$n.z, lower=0, upper=width.z/5)
  }
  if (i$n.d > 0) {
    p.p = makeNumericVectorParam("p", length=i$n.d, lower=0, upper=0.1)
  }
  
  pop = list()
  for (j in 1:control$mu) {
    x = list()
    if (i$n.r > 0) {
      x$r = sampleValue(p.r)
      x$sigma = sampleValue(p.sigma)
    } else {
      x$r = numeric(0)
      x$sigma = numeric(0)
    }
    if (i$n.z > 0) {
      x$z = sampleValue(p.z)
      x$xi = sampleValue(p.xi)
    } else {
      x$z = integer(0)
      x$xi = numeric(0)
    }
    if (i$n.d > 0) {
      x$d = sapply(i$values.ns, sample, size=1)
      x$p = sampleValue(p.p)
    } else {
      x$d = character(0)
      x$p = numeric(0)
    }
    pop[[length(pop)+1]] = x
  }
  return(pop)
}

