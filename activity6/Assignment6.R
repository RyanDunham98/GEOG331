#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("/Volumes/class/Geog331/Data/activities/a06/GNPglaciers/GNPglaciers_1966.shp")

g1998 <- readOGR("/Volumes/class/Geog331/Data/activities/a06/GNPglaciers/GNPglaciers_1998.shp")

g2005 <- readOGR("/Volumes/class/Geog331/Data/activities/a06/GNPglaciers/GNPglaciers_2005.shp")

g2015 <- readOGR("/Volumes/class/Geog331/Data/activities/a06/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

g1966@proj4string

spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME


#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#read in rgb imagery from landsat
redL <- raster("/Volumes/class/Geog331/Data/activities/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Volumes/class/Geog331/Data/activities/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Volumes/class/Geog331/Data/activities/a06/glacier_09_05_14/l08_blue.tif")


#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)


#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)


plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("/Volumes/class/Geog331/Data/activities/a06/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

plot(NDVIraster[[1]])

#Q3
par(mfrow=c(1,2))
plot(NDVIraster[[1]], pch = 1)
plot(g1966, pch =2)

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#Q4
#plot the maximum NDVI and glacier polygons for 2015
plot(NDVIraster[[14]], axes=FALSE, box=FALSE)
plot(g2015p,col="black", border=NA,add=TRUE)

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   

#Q5
#creates new variable in g2015p data called pct_change which represents the % change in glacial area from 1966 to 2015
g2015p@data$pct_change = ((g1966p@data$a1966m.sq-g2015p@data$a2015m.sq)/(g1966p@data$a1966m.sq))*100
#ssplot of glaciers and pct_change
spplot(g2015p, "pct_change")

#gDifference does not work on mac
#diffPoly <- gDifference(g1966p, g2015p)
#plot(diffPoly)

#plot with NDVI
#plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
#plot(diffPoly,col="black", border=NA,add=TRUE)

#Q6
print(g2015p@data$GLACNAME[g2015p@data$pct_change > 84.7])
rgbL <- brick(redL, greenL, blueL)
par(mai=c(1,1,1,1))
pct = toString(max(g2015p@data$pct_change))
t = paste("Boulder Glacier", pct, "% loss", sep = " ")
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin", main = t)
plot(g1966p[g1966p@data$GLACNAME == "Boulder Glacier"], col="palegreen2", border=NA, add=TRUE)
plot(g1998p[g1998p@data$GLACNAME == "Boulder Glacier"], col="royalblue3", add=TRUE, border=NA)
plot(g2005p[g2005p@data$GLACNAME == "Boulder Glacier"], col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015p[g2015p@data$GLACNAME == "Boulder Glacier"], col="tomato3", add=TRUE, border=NA)


#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)


meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)


#Q9
#Get all mean NDVI changes excluding zone 0 
mean_change <- meanChange[-c(1), ] 
#create new data field called NDVI mean change in g2015p
g2015p@data$NDVI_mean_change <- mean_change[,"mean"]
#ssplot of glaciers and pct_change
spplot(g2015p, "NDVI_mean_change")

#Q11



#Q11