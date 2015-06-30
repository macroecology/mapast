###########################libraries#############################
library(raster)
library(maptools)
library (paleobioDB)

###########################get_paleomap#############################

#' get_paleomap
#' 
#' gets the shapefile for the map of a choosen time interval (e.g. "triassic")
#' 
#' @usage get_paleomap (interval, plot)
#' 
#' @param interval time interval of interest
#' @param plot is by default TRUE, if TRUE you get plot of the map directly
#' @return a shape file for the choosen time interval
#' @export 
#' @examples \dontrun{
#' shapefile_quat <- get_paleomap(interval="Quaternary") 
#'}
#'

get_paleomap <- function (interval, plot=TRUE){
  wd <- getwd()
  file <- paste(gsub("paleoMap", "paleoMap/data/", wd), paste(interval, ".shp", sep=""), sep="")
  shape <- readShapePoly(file, IDvar=NULL, proj4string=CRS(as.character(NA)), 
  verbose=FALSE, repair=TRUE, force_ring=TRUE)
  if(plot==TRUE){
    x11()
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="longitude", ylab="latitude"
         , main=interval)
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col="lightblue")
    plot(shape, col="lightgrey", border="grey", add=TRUE)
  }
  shape
}

################getdata_paleomap##############################

#' getdata_paleomap
#' 
#' gets paleontological data from paleobiodb
#' 
#' @usage getdata_paleomap (interval, base_name)
#' 
#' @param interval time interval of interest (e.g. jurassic)
#' #' @param base_name name of the base (e.g reptiles) you want to get data from
#' @return a shape file for the choosen time interval
#' @export 
#' @examples \dontrun{
#' getdata_paleomap (base_name="Canis", interval="Quaternary", database=??)
#'}
#'

getdata_paleomap <- function(interval,base_name){
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
#' @usage plot_paleomap (interval, base_name)
#' 
#' @param interval time interval of interest (e.g. jurassic)
#'  @param base_name name of the base (e.g reptiles) you want to get data from
#' @return plot with map of the time intervall and the fossil occurences
#' @export 
#' @examples \dontrun{
#' plot_paleomap (base_name= "Canis", interval="Quaternary")
#'}
#'

plot_paleomap <- function(interval, base_name){
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
#' also makes a plot of the map and raster
#' 
#' @usage raster_paleomap (shape, data)
#' 
#' @param the shape file from the time interval of interest. Can be created with get_paleomap
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng, can be created with getdata_paleomap
#' @return plot with map of the time intervall, the fossil occurences and the raster file. And the raster file itself
#' @export 
#' @examples \dontrun{
#' myraster <- raster_paleomap (shape, data)
#' plot(myraster)
#'}

raster_paleomap <- function(shape, data){
    ras <- raster(shape)
    r<-rasterize(data[,14:15],ras
                 ,fun=sum)
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

#' spraster_paleomap
#' 
#' creates a raster of species richness
#' and makes a plot of the map and raster
#' 
#' @usage raster_paleomap (shape, data)
#' 
#' @param the shape file from the time interval of interest. Can be created with get_paleomap
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng, can be created with getdata_paleomap
#' @return plot with map of the time intervall, the fossil occurences and the raster file. And the raster file itself
#' @export 
#' @examples \dontrun{
#' myraster <- raster_paleomap (shape, data)
#' plot(myraster)
#'}
spraster_paleomap <- function(shape, data){
  ras <- raster(shape)
  data <- subset(data, taxon_rank=="species")
  fdata <- species_filter(data)
  r<-rasterize(fdata[,14:15],ras,fun=sum)
  x11()
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="longitude", ylab="latitude"
       , main="Raster - species richness")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col="lightblue")
  plot(shape, col="lightgrey", border="grey", add=TRUE)
  plot(r, col= mycols(100), add=T)
  r
}


##############################color palette ########################################
mycols <- colorRampPalette(colors=c(rgb(0.255*3,0.255*3,0,0.5), rgb(0.144*3,0.238*3,0.144,0.5), rgb(0,1,0,0.5)), alpha=TRUE)


#################filter#######################
species_filter <- function(data){
  filter <- data[0,]
  data <- subset(data, taxon_rank=="species")
  for(i in seq(-180,180,10)){
    frame <- data[0,]
    for (j in seq(-90,90,10)){
      for (k in 1:length(data$paleolng)){
        if(data$paleolng>=i && data$paleolng <=(i+10) && data$paleolat>=j && data$paleolat <=(j+10)){
          frame <- rbind(frame,data[k,])
        }
      }
      
    }
    filter <- rbind(filter, subset(frame, !duplicated(frame$taxon_name)))
  }
  filter
}
