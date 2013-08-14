# Stop criterion based on fitness.
# Returns true if best value is better than or equal to control$stop.fitness 
termStopFitness = function(y.pop, control, internal){
  min(y.pop) <= internal$fitn.scale * control$stop.fitness
}

# Stop criterion based on number of iterations / fevals.
# Returns true if current iteration exceeds \code{control$maxit}.
# Note that if the user set a budget in control this is converted at once 
# in a maximal number of iterations, so we only have to check the iters
termBudgetExhausted = function(iter, control, internal){
  iter > control$maxit
}

# Main stop function.
# @param iter [\code{integer(1)}]\cr
#   Next iteration.
# @param y.pop [\code{numeric}]\cr
#   Fitness values of next generation.
# @param control [\code{\link{MiesControl}}]\cr
#   Control object.
# @param internal [\code{list}]\cr
#   Internal constants.
# @return [\code{integer(1)}].
#   Negative if should not terminate, 0 means fitness reached, 1 means maxit hit. 
termConditionSatisfied = function(iter, y.pop, control, internal){
  if (termStopFitness(y.pop, control, internal))
    return(0L)
  else if (termBudgetExhausted(iter, control, internal))
    return(1L) 
  else
    return(-1L)
}
