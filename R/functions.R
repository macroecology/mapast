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
  #getting working directory & directory for shape file
  wd <- getwd()
  file <- paste(gsub("paleoMap/R", "paleoMap/data/", wd), paste(interval, ".shp", sep=""), sep="")
  #read in shape file and save it
  shape <- readShapePoly(file, IDvar=NULL, proj4string=CRS(as.character(NA)), 
  verbose=FALSE, repair=TRUE, force_ring=TRUE)
  #if user does not set plot=FALSE plot the shape file
  if(plot==TRUE){
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="longitude", ylab="latitude"
         , main=interval)
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col="lightblue")
    plot(shape, col="lightgrey", border="grey", add=TRUE)
  }
  #return the shape file
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
  #create an empty data variable for storing occurences
    data <- c()
    #get data from paleobioDB
    occ <- pbdb_occurrences (base_name=base_name, interval=interval, 
                      show=c("paleoloc"), 
                      vocab="pbdb")
    #save data from paleobiodb as data frae
    data <- data.frame(occ)
    #return data frame
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
  #create variables for labeling the plot
  title <-paste("Time interval: ", interval, sep="")
  subtitle <- paste("Base name: ",base_name, sep="")
  #getting the shape file for the map and the data for plotting it on the map
  shape <- get_paleomap(interval=interval)
  data <- getdata_paleomap(base_name=base_name, interval=interval)
  #plotting the map and the data
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
#' @param the shape file from the time interval of interest. Can be created with get_paleomap
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng, can be created with getdata_paleomap
#' @return plot with map of the time intervall, the fossil occurences and the raster file. And the raster file itself
#' @export 
#' @examples \dontrun{
#' myraster <- raster_paleomap (shape, data)
#' plot(myraster)
#'}

raster_paleomap <- function(shape, data){
    #creating a raster in the size of the shape
    ras <- raster(shape)
    #raster of the occurences (sampling effort)
    r<-rasterize(data[,14:15],ras
                 ,fun=sum)
    #plotting the map and the raster on the map
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="longitude", ylab="latitude"
         , main="Raster of occurences - sampling effort")
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col="lightblue")
    plot(shape, col="lightgrey", border="grey", add=TRUE)
    plot(r,col=c(mycols(100)), add=T)
    #returning the raster
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
  #creating a raster in size of the shape file
  ras <- raster(shape)
  #getting only species data and no duplictaed in a raster field
  fdata <- species_filter(data)
  #getting the raster of the species richness
  r<-rasterize(fdata[,14:15],ras,fun=sum)
  #plotting the map and the raster
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="longitude", ylab="latitude"
       , main="Raster - species richness")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col="lightblue")
  plot(shape, col="lightgrey", border="grey", add=TRUE)
  plot(r, col= mycols(100), add=T)
  #return the raster
  r
}


##############################color palette ########################################

#creating a color palette for the raster
mycols <- colorRampPalette(colors=c(rgb(0.255*3,0.255*3,0,0.5), rgb(0.144*3,0.238*3,0.144,0.5), rgb(0,1,0,0.5)), alpha=TRUE)


#################filter#######################
#filters all occurences which are not species and duplicates in raster cells


#' species_filter
#' 
#' filters the data frame so tehre are only species left 
#' and for each raster every species only once
#' 
#' @usage species_filer (data)
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng, can be created with getdata_paleomap
#' @return filtered data frame with only species
#' @examples \dontrun{
#' filtered_data <- filter_data (data)
#' show(data)
#'}
species_filter <- function(data){
  #gets colnames for new data frame
  filter <- data[0,]
  #filters only species
  data <- subset(data, taxon_rank=="species")
  #getting each species only once in 10*10 raster cell
  for(i in seq(-180,180,10)){
    frame <- data[0,]
    for (j in seq(-90,90,10)){
      for (k in 1:length(data$paleolng)){
        if(data$paleolng>=i && data$paleolng <=(i+10) && data$paleolat>=j && data$paleolat <=(j+10)){
          frame <- rbind(frame,data[k,])
        }
      }
      
    }
    #add for each raster the filtered data
    filter <- rbind(filter, subset(frame, !duplicated(frame$taxon_name)))
  }
  #return filtered data frame
  filter
}
