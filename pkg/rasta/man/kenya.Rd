\name{kenya}
\alias{kenya}
\docType{data}
\title{
Kenya shape file
}
\description{
A shape file containing the administrative boundaries of Kenya.
}
\usage{data(kenya)}
\format{
  The format is:
Formal class 'Spatial Vector Object' [resulting from readOGR() function package "rgdal"] 
 
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
}
\examples{
# Visualise the data
library(raster)
data(kenya)
plot(kenya)
}
\keyword{Kenya}
