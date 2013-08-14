#' Construct control object.
#' @title Construct control object.
#' @param maxit [\code{integer(1)}]\cr
#'   Number of allowed generations of the ES.
#'   Missing means \code{budget} is used. If \code{budget} is also missing a default of 10 iterations is used.
#' @param budget [\code{integer(1)}]\cr
#'   Maximal number of allowed fitness function evaluations for the ES.
#'   Then \code{maxit = floor((budget - mu) / lambda)}.
#'   Missing means \code{maxit} is used. 
#' @param mu [\code{integer(1)}]\cr
#'   Number of individuals in population.
#'   Default is 10. 
#' @param lambda [\code{integer(1)}]\cr
#'   Number of offspring created in each generation.
#'   Default is 100. 
#' @param tau.g.r [\code{numeric(1)}]\cr
#'   Global tau for real parameters.
#'   Default is \code{NA}.
#' @param tau.l.r [\code{numeric(1)}]\cr
#'   Local tau for real parameters.
#'   Default is \code{NA}.
#' @param tau.g.z [\code{numeric(1)}]\cr
#'   Global tau for integer parameters.
#'   Default is \code{NA}.
#' @param tau.l.z [\code{numeric(1)}]\cr
#'   Local tau for integer parameters.
#'   Default is \code{NA}.
#' @param tau.g.d [\code{numeric(1)}]\cr
#'   Global tau for for discrete parameters.
#'   Default is \code{NA}.
#' @param tau.l.d [\code{numeric(1)}]\cr
#'   Local tau for discrete parameters.
#'   Default is \code{NA}.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the objective function be minimized?
#'   If you maximize, the fitness values are internally scaled with -1, but
#'   the real, unscaled fitness values are passed to the optimization path
#'   and the logger.  
#'   Default is \code{TRUE}.
#' @param vectorized [\code{logical(1)}]\cr
#'   When set to true the fitness function will be called given a list of 
#'   individuals instead of a single individual.
#'   Default is \code{FALSE}.
#' @param stop.fitness [\code{numeric(1)}]\cr
#'   When value of stop.fitness is reached the ES stops automatically.
#'   Default is \code{NA}, which means \code{-Inf} in case of minimization and \code{Inf} in case of maximization. 
#' @param log.every [\code{integer(1)}]\cr
#'   After how many iterations / generations should the logger be called?
#'   Default is 10.
#' @param opt.path.yname [\code{character(1)}]\cr
#'   Name of fitness column in optimization path.
#'   Default is \dQuote{y}.
#' @return [\code{\link{MiesControl}}].
#' @aliases MiesControl
#' @export
makeMiesControl = function(maxit=10, budget, mu=10L, lambda=50L, 
  tau.l.r=as.numeric(NA), tau.g.r=as.numeric(NA), tau.l.z=as.numeric(NA), 
  tau.g.z=as.numeric(NA), tau.l.d=as.numeric(NA), tau.g.d=as.numeric(NA),
  minimize=TRUE, stop.fitness=as.numeric(NA), vectorized=FALSE, log.every=10L,
  opt.path.yname="y") {
  
  if (missing(maxit) && !missing(budget)) {
    budget = convertInteger(budget)
    checkArg(budget, "integer", len=1, na.ok=FALSE)
    maxit = as.integer(floor((budget - mu) / lambda))
    if (maxit == 0L)
      stop("Budget too small, cannot do 1 iteration!")
    message("Setting maxit to: ", maxit)
  } else if (!missing(maxit) && missing(budget)) {
    maxit = convertInteger(maxit)
    checkArg(maxit, "integer", len=1, na.ok=FALSE)
  } else if (!missing(maxit) && !missing(budget)) {
    stop("Only one of maxit / budget must be defined!")
  } 
  
  mu = convertInteger(mu)
  checkArg(mu, "integer", len=1, na.ok=FALSE)
  lambda = convertInteger(lambda)
  checkArg(tau.l.r, "numeric", len=1, na.ok=TRUE)
  checkArg(tau.g.r, "numeric", len=1, na.ok=TRUE)
  checkArg(tau.l.z, "numeric", len=1, na.ok=TRUE)
  checkArg(tau.g.z, "numeric", len=1, na.ok=TRUE)
  checkArg(tau.l.d, "numeric", len=1, na.ok=TRUE)
  checkArg(tau.g.d, "numeric", len=1, na.ok=TRUE)
  checkArg(minimize, "logical", len=1, na.ok=FALSE)
  checkArg(stop.fitness, "numeric", len=1, na.ok=TRUE)
  if(is.na(stop.fitness))
    stop.fitness = c(Inf, -Inf)[minimize+1]
  checkArg(vectorized, "logical", len=1, na.ok=TRUE)
  log.every = convertInteger(log.every)
  checkArg(log.every, "integer", len=1, na.ok=FALSE)
  ctrl = list(maxit=maxit, mu=mu, lambda=lambda, 
    tau.l.r=tau.l.r, tau.g.r=tau.g.r, 
    tau.l.z=tau.l.z, tau.g.z=tau.g.z, 
    tau.l.d=tau.l.d, tau.g.d=tau.g.d,
    minimize=minimize, stop.fitness=stop.fitness, 
    vectorized=vectorized, log.every=log.every,
    opt.path.yname=opt.path.yname)
  class(ctrl) = "MiesControl"
  return(ctrl)      
}
