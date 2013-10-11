CreateLinesDataFrame <- function(OrderedPointRecords, prj_string){
  
  # create SpatialLinesDataFrame (sp) from consecutive points which are 
  # in an temporally ordered data frame, with several required column names.
  # Checks the input data frame for required attributes.
  
  require(sp)
  if (nargs() != 2) 
    stop("\nCreateLinesDataFrame expects two arguments: a data frame and a proj4string")
  required.names <- c("x", "y", "loadnr", "datetime", "workwidth.m.", "date")
  if (!all(required.names %in% names(OrderedPointRecords))){
    missing <- required.names[which(!required.names %in% names(OrderedPointRecords))]
    stop(paste("\nAttribute '", missing, "' missing in input data frame.", sep = ""))
  }
  if (class(prj_string)[1] != "CRS") stop("\nWrong class 'prj_string'.")
  current_id <- OrderedPointRecords[1,][["loadnr"]]
  current_ww <- OrderedPointRecords[1,][["workwidth.m."]]
  current_dt <- OrderedPointRecords[1,][["date"]]
  
  Point2PointDistance <- function(point1, point2)
    sqrt((point2[1] - point1[1])**2 + (point2[2] - point1[2])**2)
  
  i <- 1
  line_list = list()
  sp_lines = list()
  Ids = character(); loads=numeric(); datim <- vector("raw"); width=numeric()
  imax = nrow(OrderedPointRecords)
  while (i <= imax){
    coords <- matrix(nrow = 0, ncol=2)
    colnames(coords) <- c("x","y")
    np <- 0
    while (i <= imax && OrderedPointRecords[i,][["loadnr"]] == current_id && 
             OrderedPointRecords[i,][["workwidth.m."]] == current_ww && 
             OrderedPointRecords[i,][["date"]] == current_dt)
    {
      # distOK is TRUE if two points are likely to belong to the same work line
      distOK = i == 1 | np == 0 || 
        Point2PointDistance(OrderedPointRecords[i,][c("x","y")], 
                            OrderedPointRecords[i-1,][c("x","y")]) < 10.0
      i <- i + 1
      if (distOK) {
        coords <- rbind(coords, OrderedPointRecords[(i-1),][c("x","y")])
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
      loads <- append(loads, OrderedPointRecords[(i-1),][["loadweight.ton."]])
      datim <- append(datim, OrderedPointRecords[(i-1),][["datetime"]])
      width <- append(width, current_ww)
      rm(line_list)
      line_list = list()
    }
    if (i < imax){
      current_id = OrderedPointRecords[i,][["loadnr"]]
      current_ww = OrderedPointRecords[i,][["workwidth.m."]]
      current_dt = OrderedPointRecords[i,][["date"]]}
    current_tm <- OrderedPointRecords[i,][["datetime"]]
  }
  dfr <- data.frame(ID=Ids,loads, width, datim)
  row.names(dfr) <- dfr$ID
  SpatialLinesDataFrame(SpatialLines(sp_lines, proj4string=prj_string), dfr)
}
