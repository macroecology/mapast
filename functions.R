###########################libraries#############################
library(raster)
library(maptools)
library (paleobioDB)

###########################get_paleomap#############################

#' get_paleomap
#' 
#' gets the shapefile for the map of a choosen time interval (e.g. "triassic")
#' 
#' @usage get_paleomap (interval)
#' 
#' @param interval time interval of interest
#' @return a shape file for the choosen time interval
#' @export 
#' @examples \dontrun{
#' shapefile_quat <- get_paleomap(interval="Quaternary") 
#' plot(shapefile_quat)
#'}
#'

get_paleomap <- function (interval){
  wd <- getwd()
  file <- paste(gsub("paleoMap", "paleoMap/data/", wd), paste(interval, ".shp", sep=""), sep="")
  shape <- readShapePoly(file, IDvar=NULL, proj4string=CRS(as.character(NA)), 
  verbose=FALSE, repair=TRUE, force_ring=TRUE)
  shape
}

################getdata_paleomap##############################

#' getdata_paleomap
#' 
#' gets paleontological data from paleobiodb
#' 
#' @usage getdata_paleomap (base_name, interval)
#' 
#' @param base_name name of the base (e.g reptiles) you want to get data from
#' @param interval time interval of interest (e.g. jurassic)
#' @return a shape file for the choosen time interval
#' @export 
#' @examples \dontrun{
#' getdata_paleomap (base_name="Canis", interval="Quaternary", database=??)
#'}
#'

getdata_paleomap <- function(base_name, interval){
  data <- c()
    occ <- pbdb_occurrences (base_name=base_name, interval=interval, 
                      show=c("paleoloc"), 
                      vocab="pbdb")
    data <- data.frame(occ)
  return (data)
}

####################plot_paleomap#################################

#' plot_paleomap
#' 
#' plots the wanted base_name from paleobioDB directly on the map of the time interval
#' if you do not want to save and process the data and shape file before plotting it
#' 
#' @usage plot_paleomap (base_name, interval)
#' 
#' @param base_name name of the base (e.g reptiles) you want to get data from
#' @param interval time interval of interest (e.g. jurassic)
#' @return plot with map of the time intervall and the fossil occurences
#' @export 
#' @examples \dontrun{
#' plot_paleomap (base_name= "Canis", interval="Quaternary")
#'}
#'

plot_paleomap <- function(base_name, interval){
  title <-paste("Time interval: ", interval, sep="")
  subtitle <- paste("Base name: ",base_name, sep="")
  shape <- get_paleomap(interval=interval)
  data <- getdata_paleomap(base_name=base_name, interval=interval)

  
  x11()
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="longitude", ylab="latitude"
       , main=title)
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col="lightblue")
  mtext(subtitle)
  plot(shape, col="lightgrey", border="grey", add=T)
  points (data$paleolng, data$paleolat, pch=19, col=rgb(0,0.3,0, alpha=0.4))
}

#####################raster_paleomap##############################

#' raster_paleomap
#' 
#' creates a raster of the fossil occurences (sampling effort)
#' also makes a plot of the map, raster and occurences
#' 
#' @usage raster_paleomap (data, shape)
#' 
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng, can be created with getdata_paleomap
#' @param the shape file from the time interval of interest. Can be created with get_paleomap
#' @return plot with map of the time intervall, the fossil occurences and the raster file. And the raster file itself
#' @export 
#' @examples \dontrun{
#' myraster <- raster_paleomap (data, shape)
#' plot(myraster)
#'}

raster_paleomap <- function(shape, data){
    ras <- raster(shape)
    r<-rasterize(data[,14:15],ras,fun=sum)
    x11()
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="longitude", ylab="latitude"
         , main="Raster of occurences - sampling effort")
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col="lightblue")
    plot(shape, col="lightgrey", border="grey", add=TRUE)
    plot(r,col=c(mycols(100)), add=T)
    r
}

######################spraster_paleomap####################

#do not know if correct?
spraster_paleomap <- function(shape, data){
  ras <- raster(shape)
  data <- subset(data, taxon_rank=="species")
  r<-rasterize(data[,14:15],ras,fun=sum)
  x11()
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="longitude", ylab="latitude"
       , main="Raster - richness of species")
  plot(shape, col="lightgrey", border="grey", add=TRUE)
  plot(r, col= mycols(100), add=T)
  r
}


##############################color palette ########################################
mycols <- colorRampPalette(colors=c(rgb(0,1,0,0.5), rgb(0,1,1,0.5), rgb(0,0,1,0.5)), alpha=TRUE)
