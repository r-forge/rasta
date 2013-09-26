# Author: Loic Dutrieux
# September 2013

QA2cloud <- function(x, bitpos=0x4000) {
  if (bitAnd(x, bitpos) != 0) {
    cloud <- 1
  } else {
    cloud <- 0
  }
  return(cloud)
	
}