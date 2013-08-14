#' Mixed integer sphere test function.
#' @param x [\code{list}]\cr 
#'   Discrete parameter vector consisting of numeric parameters [\code{x$d}], 
#'   integer parameters [\code{x$r}] and discrete parameters [\code{x$d}].
#' @return [\code{numeric(1)}]. Objective function value.
#' @export
mixedIntegerSphere <- function(x){
  sum(x$r^2)+sum(x$z^2)+sum(x$d1^2)+sum(x$d2^2)+sum(x$d3^2)+sum(x$d4^2)+sum(x$d5^2)
}


