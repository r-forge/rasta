\name{taravao2}
\alias{taravao2}
\docType{data}
\title{
Subset of a Landsat 8 image of Taravao village, on Tahiti island.
}
\description{
Subset of a LAndsat 8 multispectral image. The dataset is in the form of a RasterBrick and contains 9 layers. See \url{http://landsat.usgs.gov/landsat8.php} for more information.
}
\usage{data(taravao2)}
\format{
  The format is:
Formal class 'RasterBrick' [package "raster"] with 12 slots
 
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
\url{http://landsat.usgs.gov/landsat8.php}
}
\examples{
# Visualise the data
library(raster)
data(taravao2)
plotRGB(taravao2, 5, 3, 4)
}
\keyword{Landsat}
