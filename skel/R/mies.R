# Evaluation of list of individuals.
# @param inds [\code{\link{Individual}}]\cr
#   List of individuals to evaluate.
# @param fitn [\code{function}]\cr
#   Fitness function. Takes individual as input and returns numeric vector of fitness values.
# @param opt.path [\code{\link[ParamHelpers]{OptPath}}]\cr
#   Optimization Path.
# @param control [\code{\link{MiesControl}}]\cr
#   Control object.
# @param internal [\code{list}]\cr
#   Internal constants.
evalIndividuals = function(inds, fitn, opt.path, dob, control, internal) {
  decoded = lapply(inds, decode, internal=internal) 
  if (control$vectorized)
    ys = fitn(decoded) 
  else
    ys = sapply(decoded, fitn)
  Map(function(x,y) addOptPathEl(opt.path, x=x, y=y, dob=dob), decoded, ys)
  ys * internal$fitn.scale
}


# Recombination function.
# Recombines parents from population 'pop' so that 'lambda' offspring are created
# @param pop\cr
#   Two parents are randomly selected from the population 'pop' and recombine is used on them, only
#   one offspring is created
# @return [\code{list}]\cr
#   List of individuals.
recombineParents = function(pop, control) {
  inds = seq_len(control$mu)
  replicate(control$lambda, {
    j = sample(inds, 2)
    do.call(recombine, pop[j])
  }, simplify=FALSE)
}

#' Mixed integer evolution strategy.
#' @param fitn [\code{function}]\cr
#'   Fitness function to optimize.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set that describes types and constraints.
#' @param control [\code{\link{MiesControl}}]\cr
#'   Control object.
#' @param log.fun [\code{function(iter, pop, y, opt.path)}]\cr
#'   Logger function that is called every \code{control$log.every} iteration.
#'   It is always called at the of the iteration, when the offspring have already
#'   been added to the optimization path, and the new population has been determined
#'   from the parents / offspring. 
#'   It is passed: the current iteration, the current, new population, their fitness 
#'   values and the optimization path.
#' @return [\code{list}]: 
#'   \describe{
#'   \item{par [\code{list}]}{Best ever encountered parameter value. In case of ties the last encountered individual is selected.}
#'   \item{value [\code{numeric(1)}]}{Its fitness value.}
#'   \item{convergence [\code{integer(1)}]}{0 means \code{stop.fitness} has been reached, 1 means that the iteration limit \code{maxit} has been reached.}
#'   \item{opt.path [\code{\link{OptPath}}]}{Optimization path.}
#'   }
#' @references
#'   Li, R. and Emmerich, M. and Bovenkamp, E. and Eggermont, J. and Baeck, T. and Dijkstra, J. and Reiber, J. (2006)
#'   Mixed-integer evolution strategies and their application to intravascular ultrasound image analysis.
#'   In: Applications of Evolutionary Computing, 
#'   415-26, Springer.
#'
#'   Schwefel, H.-P. (1987) 
#'   Collective phenomena in evolutionary systems.
#'   In: Preprints of the 31st Annual Meeting of the International Society for General System Research, 
#'   Budapest, 2: 1025-33.
#' @examples
#'  n.z = n.d = n.r = 5
#'  mu = 4 
#'  lambda = 28  
#'  ctrl = makeMiesControl(maxit=10, mu=mu, lambda=lambda, log.every=10L)
#'  ps = makeParamSet(
#'    makeNumericVectorParam("r", len=n.r, lower=0, upper=10),
#'    makeIntegerVectorParam("z", len=n.z, lower=0, upper=10),
#'    makeDiscreteVectorParam("d", len=n.d, values=c(0:9))
#'  )
#'  
#'  y = mies(mixedIntegerSphere, ps, control=ctrl)
#'  print(y)
#'  p = as.data.frame(y$opt.path)
#'  print(head(p))
#'
#' @export
mies = function(fitn, par.set, control, log.fun) {
  checkArg(fitn, "function")
  checkArg(par.set, "ParamSet")
  checkArg(control, "MiesControl")
  ok = c("numeric", "integer", "numericvector", "integervector", "discrete", "discretevector", "logical")
  if(length(par.set$pars) > length(filterParams(par.set, ok)$pars))
    stop("mies can currently only be used for: ", paste(ok, collapse=","))
  low = getLower(par.set)
  upp = getLower(par.set)
  if (!all(is.finite(c(low, upp))))
    stop("mies requires finite box constraints for all parameters!")      
  if (missing(log.fun)) {
    log.fun = function(iter, pop, y, opt.path) {
        message(sprintf("Iteration %i of %i: Best fitn in pop: %f.",
          iter, control$maxit, min(y)))
    }
  }
  checkArg(log.fun, formals=c("iter", "pop", "y", "opt.path"))
  internal = makeInternal(par.set, control)
  control = initTaus(control, internal)  
  opt.path = makeOptPathDF(par.set, control$opt.path.yname, minimize=control$minimize)
  iter = 1L
  pop = initPopulation(par.set, control, internal)
  y.pop = evalIndividuals(pop, fitn, opt.path, 0L, control, internal)
  # indices of current pop in opt.path / archiv
  pop.inds.opt.path = 1:control$mu

  repeat {
    offspring = recombineParents(pop, control)
    offspring = lapply(offspring, mutate, par.set=par.set, control=control, internal=internal)
    pop = c(pop, offspring)
    # add indices here before opt.path gets longer   
    opt.path.len = getOptPathLength(opt.path)
    pop.inds.opt.path = c(pop.inds.opt.path, (opt.path.len+1):(opt.path.len+control$lambda))
    y.off = evalIndividuals(offspring, fitn, opt.path, iter, control, internal)
    y = c(y.pop, y.off)
    ord = order(y)
    # take mu best from current pop + offspring 
    # this is mu + lambda strategy
    # FIXME: is this correct? check paper
    best = ord[1:control$mu]
    dead = ord[(control$mu+1):length(ord)]
    setOptPathElEOL(opt.path, index=pop.inds.opt.path[dead], eol=iter)
    pop = pop[best]
    pop.inds.opt.path = pop.inds.opt.path[best]
    y.pop = y[best]
    if (iter %% control$log.every == 0)
      # multiply so we see real fitness in logger
      log.fun(iter, pop, y.pop * internal$fitn.scale, opt.path)
    iter = iter + 1L
    term = termConditionSatisfied(iter, y.pop, control, internal)
    if (term >= 0L) {
      break;
    }
  }
  j = getOptPathBestIndex(opt.path, ties="last")
  el = getOptPathEl(opt.path, j)
  messagef("Terminated. Iter: %i. Best fitn in pop: %f. Best fitn overall: %f.", 
    iter-1, min(y.pop)*internal$fitn.scale, el$y[1])
  list(par=el$x, value=as.numeric(el$y), convergence=term, opt.path=opt.path)
}

