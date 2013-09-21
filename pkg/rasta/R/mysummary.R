## a demo function

# function example - get measures of central tendency
# and spread for a numeric vector x. The user has a
# choice of measures and whether the results are printed.


mysummary <- function(x,npar=TRUE,print=TRUE) {
  if (!npar) {
    center <- mean(x); spread <- sd(x) 
  } else {
    center <- median(x); spread <- mad(x) 
  }
  if (print & !npar) {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & npar) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  result <- list(center=center,spread=spread)
  return(result)
}

