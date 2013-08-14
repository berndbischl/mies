# Mixed Integer Mutation Operator
# @param x [\code{\link{Individual}}]\cr
#   Individual to mutate.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set that describes types and constraints.
# @param control [\code{\link{MiesControl}}]\cr
#   Control object.
# @param internal [\code{list}]\cr
#   Internal constants.
# @return Mutated individual.
mutate = function(x, par.set, control, internal) {
  x2 = x
  N.g = rnorm(1)
  if (internal$n.r > 0)   {
    if (internal$n.r == 1)
      x2$sigma = x$sigma * exp(control$tau.g.r * N.g)
    else  
      x2$sigma = x$sigma * exp(control$tau.g.r * N.g + control$tau.l.r * rnorm(internal$n.r))
    x2$r = x$r + rnorm(internal$n.r, sd=x2$sigma)
    x2$r = pmax(internal$lower.r, x2$r)
    x2$r = pmin(internal$upper.r, x2$r)
  }
  if (internal$n.z > 0) {
    if (internal$n.z == 1)
      x2$xi = max(1, x$xi * exp(control$tau.g.z * N.g))
    else  
      x2$xi = pmax(1, x$xi * exp(control$tau.g.z * N.g + control$tau.l.z * rnorm(internal$n.z)))
    p.geom = 1 - ((x2$xi / internal$n.z) / (1 + sqrt(1 + (x2$xi/internal$n.z)^2)))
    g1 = rgeom(internal$n.z, prob=p.geom) 
    g2 = rgeom(internal$n.z, prob=p.geom) 
    x2$z = x$z + g1 - g2
    x2$z = pmax(internal$lower.z, x2$z)
    x2$z = pmin(internal$upper.z, x2$z)
  }
  if (internal$n.d > 0) {
    x2$p = 1 / (1 + (1-x$p) * exp(-control$tau.l.d * rnorm(internal$n.d)) / x$p)
    x2$p = pmin(0.5, x2$p)
    flip = as.logical(rbinom(internal$n.d, size=1, prob=x$p))
    x2$d = x$d
    vals = Map(setdiff, internal$values.ns, x$d)
    d2 = sapply(vals, sample, size=1)
    x2$d[flip] = d2[flip]
  }
  return(x2)
}

# Mixed Integer Recombination Operator
# Individuals 'x1' and 'x2' are recombined and single offspring is returned.
# @param x1 [\code{\link{Individual}}]\cr
#   First individual to recombine.
# @param x2 [\code{\link{Individual}}]\cr
#   Second individual to recombine.
# @return Recombined offspring.
recombine = function(x1, x2) {
  mix = function(a,b) {
    j = as.logical(rbinom(length(a), 1, 0.5))
    a[j] = b[j]
    a
  }
  x = list()
  x$r = mix(x1$r, x2$r)
  x$z = mix(x1$z, x2$z)
  x$d = mix(x1$d, x2$d)
  x$sigma = (x1$sigma + x2$sigma) / 2
  x$xi = (x1$xi + x2$xi) / 2
  x$p = (x1$p + x2$p) / 2
  return(x)
}
