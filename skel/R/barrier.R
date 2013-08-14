# Helper function for constructing a permutation \code{a} for real-valued and integer parameters
# This function is called during function construction.
# @param c [\code{integer(1)}]\cr 
#   Control parameter for ruggedness of search space.
# @return [\code{integervector}]\cr. A permuted vector in range [0,19] is returned.
makeBarrierAi = function(ruggedness=0){
  a = c(0:19)
  if(ruggedness > 1){
    for (k in 1:ruggedness){
      j = floor(runif(1,1,19)) # compared to the original code we replaced 0:18 by 1:19 here, since R assigns array from 1:x
      # swapping
      tmp = a[j]
      a[j] = a[j+1]
      a[j+1] = tmp
    }
  }
  return(a)
}

# Helper function for constructing a set of permutations for the discrete pararameters of a barrier function.
# The result is returned as matrix, where each row contains a single permutation.
# This function is called during function construction.
# @param nd [\code{integer(1)}]\cr 
#   Size of discrete parameter vector.
# @return Bi [\code{nd}] times [\code{20}]. Matrix containing nd permutations of the interval [0,19].
makeBarrierBi = function(nd){
  Bi <- matrix(sapply(1:nd, FUN=function(i) sample(0:19)), nrow=nd, byrow=TRUE)
  return(Bi)
}

#' Barrier objective function generator.
#' @param nr [\code{integer(1)}]\cr 
#'   Number of numeric parameters.
#' @param nz [\code{integer(1)}]\cr 
#'   Number of integer parameters.
#' @param nd [\code{integer(1)}]\cr 
#'   Number of discrete parameters.
#' @param ruggedness [\code{integer(1)}]\cr 
#'   Ruggedness of the search space (higher values for more ruggedness).
#' @return [\code{function(x)}]. Barrier objective function.
#' @export
makeBarrierFunction = function(nr, nz, nd, ruggedness){
  a = makeBarrierAi(ruggedness)
  Bi = makeBarrierBi(nd)
  barrier = list(a=a, Bi=Bi)
  function(x) {
    sum((barrier$a[floor(x$r)])^2) + sum((barrier$a[x$z])^2) + 
      sum(sapply(1:length(x$d), FUN=function(i) (barrier$Bi[i,x$d[[i]]]^2)))
  }	
}
