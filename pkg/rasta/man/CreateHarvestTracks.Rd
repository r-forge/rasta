\name{CreateHarvestTracks}
\alias{CreateHarvestTracks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create a SpatialLinesDataFrame (sp) representing continuous tracks of a harvester from a data frame with temporally ordered point data. 
}
\description{
The trajectory of a harvester can be represented by a series of continuous tracks, which correspond to different load numbers, working widths and harvest dates, while points within a track are separated by a maximum distance. The function creates such tracks from a data frame with  temporally ordered input records.
}
\usage{
CreateHarvestTracks(x, CRSobj, maxdist=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
data frame. See details
}
  \item{CRSobj}{
object of class CRS of coordinate reference system arguments.
}
  \item{maxdist}{
numeric. The maximum distance (same unit as input coordinates) of consecutive points belonging to the same track.
}
}
\details{
The input data frame should at least hold the following columns: "x" [numeric], "y" [numeric], "loadnr" [integer], "datetime" [POSIXct], "workwidth.m." [numeric].

}

\value{
SpatialLinesDataFrame (sp).
}

\author{
Sytze de Bruin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# download and read data
download.file('http://rasta.r-forge.r-project.org/kroonven.csv.zip',
              'kroonven.csv.zip')
unzip('kroonven.csv.zip')
borne_data = read.table("kroonven.csv", sep = ",", header = T)

# make spatial
coordinates(borne_data) <- c("lon.degr.","lat.degr.")

# transform to Dutch grid (Note: this is an approximate transformation).
prj_string_WSG <- CRS("+proj=longlat +datum=WGS84")
borne_data@proj4string <- prj_string_WSG

prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 
+k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,
-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")

all_rd <- spTransform(borne_data, prj_string_RD)
dimnames(all_rd@coords)[[2]] <- c("x", "y")

# add attributes 'datetime' and 'date' 
all_rd$datetime <- as.POSIXct(paste(paste(all_rd$year, all_rd$month, all_rd$day, sep="-"),
                          paste(all_rd$hr, all_rd$min, all_rd$sec, sep=":")),
                              tz="Europe/Andorra")

# convert to data frame
all_rd <- as.data.frame(all_rd)

# make sure points are temporally ordered
all_rd <- all_rd[order(all_rd$datetime),]

# call function to create tracks
sp_lines_df <- (all_rd, prj_string_RD)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

