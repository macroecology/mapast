library(raster)
library(maptools)
library(RCurl)
library(rgdal)
library (paleobioDB)
library(rgbif)

? get_paleomap


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
#' shapefile <- get_paleomap(interval) 
#' plot(shapefile)
#'}
#'


? getdata_paleomap


#' getdata_paleomap
#' 
#' gets paleontological data from paleobiodb and gbif
#' 
#' @usage getdata_paleomap (base_name, interval, database)
#' 
#' @param base_name name of the base (e.g reptiles) you want to get data from
#' @param interval time interval of interest (e.g. jurassic)
#' @param database database which you want to get you data from (either paleobiodb or gbif)
#' @return a shape file for the choosen time interval
#' @export 
#' @examples \dontrun{
#' data <- getdata_paleomap (base_name, interval, database)
#' show(data)
#'}
#'
get_paleomap <- function (interval){
  wd <- getwd()
  file <- paste(gsub("paleoMap", "paleoMap/data/", wd), paste(interval, ".shp", sep=""), sep="")
  shape <- readShapePoly(file, IDvar=NULL, proj4string=CRS(as.character(NA)), 
  verbose=FALSE, repair=TRUE, force_ring=TRUE)
  shape
}

getdata_paleomap <- function(base_name, interval){
  data <- c()
#   if(database=="paleobiodb"){
    occ <- pbdb_occurrences (base_name=base_name, interval=interval, 
                      show=c("paleoloc"), 
                      vocab="pbdb")
    data <- data.frame(occ)
#   }
#   #not sure
#   if(database=="gbif"){
#     occ <-occ_search(scientificName = base_name, hasCoordinate=TRUE, 
#     fields = c("family","genus","species","decimalLatitude","decimalLongitude", 
#                "earliestEpochOrLowestSeries"), 
#                return = "all")
#     df <- data.frame(occ[3])
#     if(length(df$data.earliestEpochOrLowestSeries)!=0){
#       for(i in 1:length(df$data.earliestEpochOrLowestSeries)){
#         if(!is.na(df$data.earliestEpochOrLowestSeries[i])){
#           if(grep(interval, df$data.earliestEpochOrLowestSeries[i])==1){
#             data <- rbind(data,df[i,])
#           }
#         }
#       }
#     }
#     if(length(df$data.earliestEpochOrLowestSeries)==0 || length(data)==0){
#       print("No occurences found!")
#     }
#   }
  data
}

plot_paleomap <- function(base_name, interval){
  shape <- get_paleomap(interval=interval)
  data <- getdata_paleomap(base_name=base_name, interval=interval)
  
  x11()
  plot(shape)
  points (data$paleolng, data$paleolat, pch=19, col="red")
}

raster_paleomap <- function(shape, data){
    ras <- raster(shape)
    r<-rasterize(data[,14:15],ras,fun=sum)
    x11()
    plot(r)
    plot(shape, add=TRUE)
    points(data$paleolng, data$paleolat, pch=19, col="red")
    r
}
