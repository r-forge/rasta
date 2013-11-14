### R code from vignette source 'pkg/rasta/vignettes/Lesson_6.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: packages
###################################################
# load the necessary packages
library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)


###################################################
### code chunk number 2: explore_data
###################################################
# load in the data
data(GewataB2)
data(GewataB3)
data(GewataB4)
# check out the attributes
GewataB2
# some basic statistics using cellStats()
cellStats(GewataB2, stat=max)
cellStats(GewataB2, stat=mean)
# This is equivalent to:
maxValue(GewataB2)
# what is the maximum value of all three bands?
max(c(maxValue(GewataB2), maxValue(GewataB3), maxValue(GewataB4)))
# summary() is useful function for a quick overview
summary(GewataB2)
# plot the histograms of all bands
hist(GewataB2)
hist(GewataB3)
hist(GewataB4)
# put the 3 bands into a rasterBrick object to summarize together
gewata <- brick(GewataB2, GewataB3, GewataB4)
# 3 histograms in one window (automatic, if a rasterBrick is supplied)
hist(gewata)


###################################################
### code chunk number 3: histogram
###################################################
# view all histograms together with rasterVis
histogram(gewata)


###################################################
### code chunk number 4: spplom
###################################################
splom(gewata)


###################################################
### code chunk number 5: ndvi
###################################################
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})


###################################################
### code chunk number 6: drawextent (eval = FALSE)
###################################################
## # first, plot the raster
## plot(ndvi)
## # call drawExtent() to activate interactive mode
## # and assign the result to an extent object e
## e <- drawExtent()
## # now click 2 points on the plot window
## # these represent the top-right and bottom-left corner of your extent
## # this creates an object of the 'extent' class
## class(e)
## print(e)
## # an extent object can be used for a variety of functions,
## # like crop() and plot()
## # now plot ndvi again, but only the extent you defined interactively
## plot(ndvi, ext=e)
## # alternatively, we can create a new raster with this revised extent
## ndviCrop <- crop(ndvi, e)
## # look at the attributes of the new raster
## ndviCrop
## plot(ndviCrop)
## # remove from the workspace
## rm(ndviCrop)


###################################################
### code chunk number 7: ndviFocalMean (eval = FALSE)
###################################################
## # to save time, try on a small subset
## # first, define a small extent using drawExtent()
## plot(ndvi)
## e <- drawExtent()
## # make a test raster by cropping
## ndviTest <- crop(ndvi, e)
## # define a 3x3 matrix of equal weights
## w <- matrix(1/9, nc=3, nr=3)
## print(w)
## sum(w) # should be 1
## # pass this matrix to focal()
## ndviMean <- focal(ndviTest, w=w)
## # plot the result compared to original
## opar <- par(mfrow=c(1,2))
## plot(ndviTest, main="NDVI")
## plot(ndviMean, main="NDVI with 3x3 mean filter")
## par(opar)


###################################################
### code chunk number 8: vcfGewata
###################################################
# load the data and check it out
data(vcfGewata)
vcfGewata
plot(vcfGewata)
summary(vcfGewata)
hist(vcfGewata)


###################################################
### code chunk number 9: vcfGewata2
###################################################
vcfGewata[vcfGewata > 100] <- NA
# look at revised summary stats
summary(vcfGewata)
plot(vcfGewata)
hist(vcfGewata)


###################################################
### code chunk number 10: covariants
###################################################
# multiply all values by 10000
ndvi <- calc(ndvi, fun = function(x) floor(x*10000))
# change the data type
# see ?dataType for more info
dataType(ndvi) <- "INT2U"
# name this layer to make plots interpretable
names(ndvi) <- "NDVI"
# make the covariate rasterBrick
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)


###################################################
### code chunk number 11: trainingPoly
###################################################
# load the training polygons
data(trainingPoly)
# superimpose training polygons onto ndvi plot
plot(ndvi)
plot(trainingPoly, add = TRUE)


###################################################
### code chunk number 12: reclassify
###################################################
# inspect the data slot of the trainingPoly object
trainingPoly@data
# the 'Class' column is actually an ordered factor type
trainingPoly@data$Class
str(trainingPoly@data$Class)

# define a reclassification function which substitutes
# the character label for the factor level (between 1 and 3)
reclass <- function(x){
  which(x==levels(trainingPoly@data$Class))
}

# use sapply() to apply this function over each element of the 'Class' column
# and assign to a new column called 'Code'
trainingPoly@data$Code <- sapply(trainingPoly@data$Class, FUN=reclass)


###################################################
### code chunk number 13: training_plot
###################################################
# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field='Code')
# set the dataType of the raster to INT1U
# see ?dataType for more information
dataType(classes) <- "INT1U"
# define a colour scale for the classes
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue")
# plot without a legend
plot(classes, col=cols, legend=FALSE)
# add a customized legend
legend("topright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")


###################################################
### code chunk number 14: masked_covariates
###################################################
covmasked <- mask(covs, classes)
plot(covmasked)
# add the classes layer to this new brick
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
plot(trainingbrick)
# Note that in this plot, the 'class' legend is not meaningful
# This plot is useful simply to check the available layers


###################################################
### code chunk number 15: valuetable
###################################################
# extract all values into a matrix
valuetable <- getValues(trainingbrick)
# convert to a data.frame and inspect the first and last rows
valuetable <- as.data.frame(valuetable)
head(valuetable)
tail(valuetable)


###################################################
### code chunk number 16: valuetable2
###################################################
# keep only rows where valuetable$classes has a value
valuetable <- valuetable[!is.na(valuetable$class),]
head(valuetable)
tail(valuetable)

# convert values in the class column to factors
valuetable$class <- factor(valuetable$class, levels = c(1:3))


###################################################
### code chunk number 17: prep_ggplots
###################################################
# add a label column to valuetable
valuetable$label <- with(valuetable, ifelse(class==1, "cropland", 
                                            ifelse(class==2, "forest", "wetland")))
# see ?ifelse() for more information


###################################################
### code chunk number 18: NDVIVCFB3B4_ggplots (eval = FALSE)
###################################################
## # Now make the ggplots using valuetable$label to split the data into facets
## 
## # 1. NDVI
## p1 <- ggplot(data=valuetable, aes(x=NDVI)) + 
##   geom_histogram(binwidth=300) + 
##   facet_wrap(~ label) +
##   theme_bw()
## p1
## 
## # 2. VCF
## p2 <- ggplot(data=valuetable, aes(x=vcf2000Gewata)) +
##   geom_histogram(binwidth=5) +
##   labs(x="% Tree Cover") +
##   facet_wrap(~ label) +
##   theme_bw()
## p2
## # 4. Bands 3 and 4
## p3 <- ggplot(data=valuetable, aes(x=gewataB3, y=gewataB4)) +
##   stat_bin2d() +
##   facet_wrap(~ label) +
##   theme_bw()
## p3
## 


###################################################
### code chunk number 19: B2B3_ggplots
###################################################
# 4. Bands 2 and 3
p4 <- ggplot(data = valuetable, aes(x=gewataB2, y=gewataB3)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
p4



###################################################
### code chunk number 20: clean_valuetable
###################################################
# NA values are not permitted in the covariates/predictor columns
# keep only the rows with containing no NA's
valuetable <- na.omit(valuetable)


###################################################
### code chunk number 21: randomforest (eval = FALSE)
###################################################
## # construct a random forest model
## # covariates (x) are found in columns 1 to 5 of valuetable
## # training classes (y) are found in the 'class' column of valuetable
## # caution: this step takes fairly long!
## # but can be shortened by setting importance=FALSE
## library(randomForest)
## modelRF <- randomForest(x=valuetable[,c(1:5)], y=valuetable$class,
##                         importance = TRUE)


###################################################
### code chunk number 22: RF_background
###################################################
library(randomForest)
if(!file.exists("data/modelRF.rda")){
  modelRF <- randomForest(x = valuetable[,c(1:5)], y = valuetable$class,
                        importance = TRUE)
  save(modelRF, file='data/modelRF.rda', compress="bzip2", ascii=FALSE)
} else {
  load('data/modelRF.rda')
}


###################################################
### code chunk number 23: inspect_RFmodel
###################################################
# inspect the structure and element names of the resulting model
modelRF
class(modelRF)
str(modelRF)
names(modelRF)
# inspect the confusion matrix of the OOB error assessment
modelRF$confusion
# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("cropland", "forest", "wetland", "class.error")
rownames(modelRF$confusion) <- c("cropland", "forest", "wetland")
modelRF$confusion


###################################################
### code chunk number 24: varImpPlot
###################################################
varImpPlot(modelRF)


###################################################
### code chunk number 25: predictRaster
###################################################
# check layer and column names
names(covs)
names(valuetable)
# predict land cover using the RF model
predLC <- predict(covs, model=modelRF, na.rm=TRUE)
# plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("orange", "dark green", "light blue")
plot(predLC, col=cols, legend=FALSE)
legend("bottomright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")


###################################################
### code chunk number 26: kmeans_valuetable
###################################################
valuetable <- getValues(covs)
head(valuetable)


###################################################
### code chunk number 27: kmeans (eval = FALSE)
###################################################
## km <- kmeans(na.omit(valuetable), centers = 3, iter.max = 100, nstart = 10)


###################################################
### code chunk number 28: kmeans_background
###################################################
if(!file.exists('data/km.rda')){
  km <- kmeans(na.omit(valuetable), centers = 3, iter.max = 100, nstart = 10)
  save(km, file='data/km.rda', compres='bzip2', ascii=FALSE)
} else {
  load('data/km.rda')
}


###################################################
### code chunk number 29: inspect_km
###################################################
# km contains the clusters (classes) assigned to the cells
head(km$cluster)
unique(km$cluster) # displays unique values


###################################################
### code chunk number 30: clean_km
###################################################
# create a blank raster with default values of 0
rNA <- setValues(raster(covs), 0)
# loop through layers of covs
# assign a 1 to rNA wherever an NA is enountered in covs
for(i in 1:nlayers(covs)){
  rNA[is.na(covs[[i]])] <- 1
}
# convert rNA to an integer vector
rNA <- getValues(rNA)


###################################################
### code chunk number 31: assign_classes
###################################################
# convert valuetable to a data.frame
valuetable <- as.data.frame(valuetable)
# assign the cluster values (where rNA != 1)
valuetable$class[rNA==0] <- km$cluster
# assign NA to this column elsewhere
valuetable$class[rNA==1] <- NA


###################################################
### code chunk number 32: plot_kmeans
###################################################
# create a blank raster
classes <- raster(covs)
# assign values from the 'class' column of valuetable
classes <- setValues(classes, valuetable$class)
plot(classes, legend=FALSE, col=c("dark green", "orange", "light blue"))


###################################################
### code chunk number 33: formask
###################################################
# Make an NA-value raster based on the LC raster attributes
formask <- setValues(raster(predLC), NA)
# assign 1 to all cells corresponding to the forest class
formask[predLC==2] <- 1
plot(formask, col="dark green", legend = FALSE)


###################################################
### code chunk number 34: clump
###################################################
# Group raster cells into clumps based on the Queen's Case
forestclumps <- clump(formask, directions=8)
plot(forestclumps)


###################################################
### code chunk number 35: clumpCount
###################################################
# assign freqency table to a matrix
clumpFreq <- freq(forestclumps)
head(clumpFreq)
tail(clumpFreq)


###################################################
### code chunk number 36: final_sieve
###################################################
# Coerce freq table to data.frame
clumpFreq <- as.data.frame(clumpFreq)
# which rows of the data.frame are only represented by one cell?
which(clumpFreq$count==1)
# which values do these correspond to?
clumpFreq$value[which(clumpFreq$count==1)]
# put these into a vector of clump ID's to be removed
excludeID <- clumpFreq$value[which(clumpFreq$count==1)]
# make a new forest mask to be sieved
formaskSieve <- formask
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[forestclumps %in% excludeID] <- NA
# zoom in to a small extent to check the results
# Note: you can define your own zoom by using e <- drawExtent()
e <- extent(c(811744.8, 812764.3, 849997.8, 850920.3))
opar <- par(mfrow=c(1, 2)) # allow 2 plots side-by-side
plot(formask, ext=e, col="dark green", legend=FALSE)
plot(formaskSieve, ext=e, col="dark green", legend=FALSE)
par(opar) # reset plotting window


###################################################
### code chunk number 37: final_sieve_nodiag
###################################################
# Group raster cells into clumps based on the Queen's Case
forestclumps <- clump(formask, directions=4)
clumpFreq <- freq(forestclumps)
# Coerce freq table to data.frame
clumpFreq <- as.data.frame(clumpFreq)
excludeID <- clumpFreq$value[which(clumpFreq$count==1)]
# make a new forest mask to be sieved
formaskSieve <- formask
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[forestclumps %in% excludeID] <- NA
e <- extent(c(811744.8, 812764.3, 849997.8, 850920.3))
opar <- par(mfrow = c(1, 2))
plot(formask, ext=e, col="dark green", legend=FALSE)
plot(formaskSieve, ext=e, col="dark green", legend=FALSE)
par(opar)


###################################################
### code chunk number 38: lulcGewata
###################################################
data(lulcGewata)
# check out the distribution of the values
freq(lulcGewata)
hist(lulcGewata)


###################################################
### code chunk number 39: LUTGewata
###################################################
data(LUTGewata)
LUTGewata


###################################################
### code chunk number 40: lulc_factor
###################################################
lulc <- as.factor(lulcGewata)


###################################################
### code chunk number 41: lulc_RAT
###################################################
# assign a raster attribute table (RAT)
levels(lulc) <- LUTGewata
lulc


###################################################
### code chunk number 42: layerize
###################################################
classes <- layerize(lulc)
plot(classes)
# layer names follow the order of classes in the LUT
names(classes) <- LUTGewata$Class
plot(classes, legend=FALSE)


###################################################
### code chunk number 43: layerize_formask
###################################################
forest <- raster(classes, 5)
# is equivalent to:
forest <- classes[[5]]
# or (since the layers are named):
forest <- classes$forest
# replace 0's (non-forest) with NA's
forest[forest==0] <- NA
plot(forest, col="dark green", legend=FALSE)


###################################################
### code chunk number 44: exercise_guide (eval = FALSE)
###################################################
## # load in the training classes and Look-Up Table (LUT)
## data(lulcGewata)
## data(LUTGewata)


###################################################
### code chunk number 45: Lesson_6.Rnw:741-746 (eval = FALSE)
###################################################
## # plot lulcGewata with a meaningful legend (see LUTGewata)
## # make sure the classes correspond correctly!
## cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
## plot(lulcGewata, col=cols, legend=FALSE)
## legend("topright", legend=LUTGewata$Class, fill=cols)


###################################################
### code chunk number 46: Lesson_6.Rnw:748-758 (eval = FALSE)
###################################################
## # draw a SpatialPolygons object in area purely represented by cropland
## # Note that drawPoly() doesn't work once you have added a legend() to the plot
## # so, first plot the raster again without the legend
## plot(lulcGewata, col=cols, legend=FALSE)
## cropland <- drawPoly(sp=TRUE)
## # click on "Finish" in the top-right corner (if using Rstudio) when finished
## # this outputs an sp (SpatialPolygons) object with 1 row (feature)
## # if you want more training data for a particular class:
## # append another polygon onto the same object using gUnion() of the rgeos package
## cropland <- gUnion(cropland, drawPoly(sp=TRUE))


###################################################
### code chunk number 47: Lesson_6.Rnw:760-767 (eval = FALSE)
###################################################
## # when you are finished with this class, 
## # be sure to set the coordinate reference system (CRS)
## projection(cropland) <- projection(lulcGewata)
## # and check
## projection(cropland)
## plot(lulcGewata)
## plot(cropland, add=TRUE)


###################################################
### code chunk number 48: Lesson_6.Rnw:769-772 (eval = FALSE)
###################################################
## # convert it to a SpatialPolygonsDataFrame (ie. add a @data slot)
## cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(
##   class="cropland"), match.ID=FALSE)


###################################################
### code chunk number 49: Lesson_6.Rnw:774-788 (eval = FALSE)
###################################################
## # do the same for the other 5 classes...
## # you may have to zoom in first for (e.g.) coffee investment areas
## plot(lulcGewata, col=cols, legend=FALSE)
## e <- drawExtent() # zoom into a coffee area
## plot(lulcGewata, col=cols, legend=FALSE, ext=e)
## # now define a training polygon
## coffee <- drawPoly(sp=TRUE)
## projection(coffee) <- projection(lulcGewata)
## # check
## plot(lulcGewata)
## plot(coffee, add=TRUE)
## # convert to SpatialPolygonsDataFrame
## coffee <- SpatialPolygonsDataFrame(coffee, data=data.frame(
##   class="coffee investment area"), match.ID=FALSE)


###################################################
### code chunk number 50: Lesson_6.Rnw:790-799 (eval = FALSE)
###################################################
## # once all polygons have been drawn, 
## # fusing them into one SpatialPolygons object is problematic,
## # because the Polygon ID's are not unique
## # use spChFIDs() from the sp package to solve this
## # e.g. for the cropland features
## cropland <- spChFIDs(cropland, "cropland")
## forest <- spChFIDs(forest, "forest")
## coffee <- spChFIDs(coffee, "coffee")
## # etc...


###################################################
### code chunk number 51: Lesson_6.Rnw:801-811 (eval = FALSE)
###################################################
## # now they can be bound (2 at a time) as one object using spRbind (maptools)
## trainingPoly <- spRbind(cropland, forest)
## trainingPoly <- spRbind(trainingPoly, coffee)
## # etc...
## # check
## trainingPoly@data
## plot(lulcGewata)
## plot(trainingPoly, add=TRUE)
## ### now proceed with the Random Forest classification 
## ### using the Gewata covariates as done earlier in the exercise


