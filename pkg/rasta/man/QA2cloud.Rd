\name{QA2cloud}
\alias{QA2cloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to generate a cloud mask from the QA layer of Landsat 8 data.
}
\description{
Landsat 8 data come with a QA layer, providing reliability information of each individual pixels. This information is stored bitwise on 16 bits. The function targets the relevant bit for cloud information and generates a cloud mask from it.
}
\usage{
QA2cloud(x, bitpos = 0xC000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric. A 16 bits integer.
}
  \item{bitpos}{
numeric. The bit mask used to target the desired bits in \code{x} The default (0xC000) targets bit #15 so that the resulting cloud mask generated has a cloud detection confidence comprised between 67 and 100 \%
}
}
\details{
More information can be found at \url{http://landsat.usgs.gov/landsat8.php}
}
\value{
numeric. Either 0 or 1. 1 corresponding to presence of cloud and 0 absence.
}
\references{
\url{http://landsat.usgs.gov/landsat8.php}
}
\author{
Loic Dutrieux
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Visualise the data
library(raster)
data(taravao) # import taravao RasterBrick
cloud <- calc(taravao[[9]], fun=QA2cloud) # Generate the cloud mask
cloud[cloud == 0] <- NA # Replace 0s by NAs (to improve visual display)

# Display data and the cloud mask 
plotRGB(taravao, 5, 3, 4) 
plot(cloud, add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Landsat }
\keyword{ Cloud mask }% __ONLY ONE__ keyword per line
