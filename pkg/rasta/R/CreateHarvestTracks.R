CreateHarvestTracks <- function(x, CRSobj, maxdist = 10.0){
  
  # create SpatialLinesDataFrame (sp) from consecutive points which are 
  # in an temporally ordered data frame, with several required column names.
  # Checks the input data frame for required attributes.
  
  require(sp)
  if (nargs() < 2) 
    stop("\nCreateLinesDataFrame expects at least two arguments: a data frame and an object of class CRS")
  required.names <- c("x", "y", "loadnr", "datetime", "workwidth.m.", "date")
  if (!all(required.names %in% names(x))){
    missing <- required.names[which(!required.names %in% names(x))]
    stop(paste("\nAttribute '", missing, "' missing in input data frame.", sep = ""))
  }
  if (class(CRSobj)[1] != "CRS") stop("\nWrong class 'CRSobj'.")
  current_id <- x[1,][["loadnr"]]
  current_ww <- x[1,][["workwidth.m."]]
  current_dt <- x[1,][["date"]]
  
  Point2PointDistance <- function(point1, point2)
    sqrt((point2[1] - point1[1])**2 + (point2[2] - point1[2])**2)
  
  i <- 1
  line_list = list()
  sp_lines = list()
  Ids = character(); loads=numeric(); datim <- vector("raw"); width=numeric()
  imax = nrow(x)
  while (i <= imax){
    coords <- matrix(nrow = 0, ncol=2)
    colnames(coords) <- c("x","y")
    np <- 0
    while (i <= imax && x[i,][["loadnr"]] == current_id && 
             x[i,][["workwidth.m."]] == current_ww && 
             x[i,][["date"]] == current_dt)
    {
      # distOK is TRUE if two points are likely to belong to the same work line
      distOK = i == 1 | np == 0 || 
        Point2PointDistance(x[i,][c("x","y")], 
                            x[i-1,][c("x","y")]) < maxdist
      i <- i + 1
      if (distOK) {
        coords <- rbind(coords, x[(i-1),][c("x","y")])
        np <- np + 1
      }
      else break
    }
    # prepare for SpatialLinesDataFrame
    line_list <- append(line_list, Line(coords))
    if (distOK){
      sp_lines <- append(sp_lines, Lines(line_list, ID=paste(current_dt, 
                                                             current_id,sep="_")))
      Ids <- append(Ids, paste(current_dt, current_id,sep="_"))
      loads <- append(loads, x[(i-1),][["loadweight.ton."]])
      datim <- append(datim, x[(i-1),][["datetime"]])
      width <- append(width, current_ww)
      rm(line_list)
      line_list = list()
    }
    if (i < imax){
      current_id = x[i,][["loadnr"]]
      current_ww = x[i,][["workwidth.m."]]
      current_dt = x[i,][["date"]]}
    current_tm <- x[i,][["datetime"]]
  }
  dfr <- data.frame(ID=Ids,loads, width, datim)
  row.names(dfr) <- dfr$ID
  SpatialLinesDataFrame(SpatialLines(sp_lines, proj4string=CRSobj), dfr)
}
