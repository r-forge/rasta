# Author: Loic Dutrieux
# September 2013
QA2cloud <- function(x, bitpos=0xC000) {
  fun <- function(x=x, bitpos=bitpos) {
    if (bitAnd(x, bitpos) == bitpos) {
      cloud <- 1
    } else {
      cloud <- 0
    }
    return(cloud) 	
  }
  # Just to make it vectorized
  out <- sapply(X=x, FUN=fun, bitpos=bitpos)
  return(out)
}
