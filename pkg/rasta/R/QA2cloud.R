# Author: Loic Dutrieux
# September 2013

QA2cloud <- function(x, bitpos=0xC000) {
  if (bitAnd(x, bitpos) == bitpos) {
    cloud <- 1
  } else {
    cloud <- 0
  }
  return(cloud)
	
}