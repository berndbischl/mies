library(devtools)
library(ParamHelpers)

load_all("skel")

ctrl = makeMiesControl(maxit=100, mu=4, lambda=4)
ps = makeParamSet(
  makeNumericParam("GAMMA", lower=0, upper=1),
  makeNumericParam("COST", lower=0, upper=10)
)

f <- function(x){
  y = runif(1,0,1)*(x$COST**2 + x$GAMMA**2)
  cat(x$COST, ",", x$GAMMA, "/", y, "\n")
  return(y)
}

set.seed(1)
z = mies(f, ps, control=ctrl)
print(z)
