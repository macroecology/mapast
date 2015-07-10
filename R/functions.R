###########################libraries#############################
library(raster)
# install.packages("maptools")
library(maptools)
# install.packages("devtools")
# library(devtools)
# install_github ("ropensci/paleobioDB")
# install.packages("paleobioDB")
library(paleobioDB)

###########################pm_getmap#############################

#' pm_getmap
#' 
#' gets the shapefile for the map of a choosen time interval (e.g. "triassic")
#' 
#' @usage pm_getmap (interval, plot, colsea, colland, colborder)
#' 
#' @param interval time interval of interest
#' @param plot is by default TRUE, if TRUE you get plot of the map directly
#' @param colsea color of the ocean in the plot
#' @param colland color of the land masses
#' @param colborder border color of the land masses
#' @return a shape file for the choosen time interval
#' @export 
#' @examples \dontrun{
#' shapefile_quat <- pm_getmap(interval="Quaternary") 
#'}
#'

pm_getmap <- function (interval, plot=TRUE, colsea="#E5E5E520", colland="#66666680", colborder="#2B2B2B30"){
  #getting working directory & directory for shape file
  wd <- getwd()
  file <- paste(gsub("paleoMap/R", "paleoMap/data/", wd), 
                paste(interval, ".shp", sep=""), sep="")
  #read in shape file and save it
  shape <- readShapePoly(file, IDvar=NULL, proj4string=CRS(as.character(NA)), 
  verbose=FALSE, repair=TRUE, force_ring=TRUE)
  #if user does not set plot=FALSE plot the shape file
  if(plot==TRUE){
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="Longitude", ylab="Latitude"
         , main=interval, xaxs="i", yaxs="i")
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, border=FALSE)
    plot(shape, col=colland, border=colborder, add=TRUE)
    box(which="plot")
  }
  #return the shape file
  shape
}

################pm_getdata##############################

#' pm_getdata
#' 
#' gets paleontological data from paleobiodb
#' 
#' @usage pm_getdata (interval, base_name, limit)
#' 
#' @param interval time interval of interest (e.g. jurassic)
#' @param base_name name of the base (e.g reptiles) you want to get data from
#' @param limit how many entrances from pbdb you want to have, e.g. 500 or "all" 
#' @return a shape file for the choosen time interval
#' @export 
#' @examples \dontrun{
#' pm_getdata (base_name="Canis", interval="Quaternary", database=??)
#'}
#'

pm_getdata <- function(interval, base_name, limit="all"){
  #create an empty data variable for storing occurences
    data <- c()
    #get data from paleobioDB
    occ <- data.frame(pbdb_occurrences (base_name=base_name, interval=interval, 
                      show=c("paleoloc", "phylo"), 
                      vocab="pbdb", limit=limit))
    #save data from paleobiodb as data frame
#     data <- data.frame(occ)
    data <- data.frame(occ$matched_name, occ$matched_rank,
                       occ$early_interval, occ$late_interval,
                       occ$paleolng, occ$paleolat, occ$geoplate,
                       occ$genus, occ$family, occ$order, occ$class, occ$phylum)
    colnames(data) <- c("matched_name", "matched_rank",
                        "early_interval", "late_interval",
                        "paleolng", "paleolat", "geoplate",
                        "genus", "family", "order", "class", "phylum")
    #return data frame
  return (data)
}

####################pm_plot#################################

#' pm_plot
#' 
#' plots the wanted base_name from paleobioDB directly on the map of the time interval
#' if you do not want to save and process the data and shape file before plotting it
#' 
#' @usage pm_plot (interval, base_name, limit, colsea, colland, colborder, colpoints, colpointborder)
#' 
#' @param interval time interval of interest (e.g. jurassic)
#' @param base_name name of the base (e.g reptiles) you want to get data from
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colborder color of the landmass borders
#' @param colpoints color of the points of the occurences
#' @param colpointborder color of the border of the points
#' @return plot with map of the time intervall and the fossil occurences
#' @export 
#' @examples \dontrun{
#' pm_plot (base_name= "Canis", interval="Quaternary")
#'}
#'

pm_plot <- function(interval, base_name,
                    limit="all",
                    colsea="#E5E5E520", colland="#66666680", colborder="#2B2B2B30", 
                    colpoints="#9ACD3250",
                    colpointborder="black"){
  #create variables for labeling the plot
  title <-paste("Time interval: ", interval, sep="")
  subtitle <- paste("Base name: ",base_name, sep="")
  #getting the shape file for the map and the data for plotting it on the map
  shape <- pm_getmap(interval=interval, plot=FALSE)
  data <- pm_getdata(base_name=base_name, interval=interval, limit)
  #plotting the map and the data
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="Longitude", ylab="Latitude"
       , main=interval, xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, border=FALSE)
  mtext(subtitle)
  plot(shape, col=colland, border=colborder, add=TRUE)
  points (data$paleolng, data$paleolat, pch=21, col=colpointborder, bg=colpoints)
  box(which="plot")
}

#####################pm_occraster##############################

#' pm_occraster
#' 
#' creates a raster of the fossil occurences (sampling effort)
#' also makes a plot of the map and raster
#' 
#' @usage pm_occraster (shape, data, colsea, colland, colborder)
#' @param the shape file from the time interval of interest. Can be created with get_paleomap
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng, can be created with getdata_paleomap
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colborder color of the landmass borders
#' @return plot with map of the time intervall, the fossil occurences and the raster file. And the raster file itself
#' @export 
#' @examples \dontrun{
#' myraster <-  pm_occraster (shape, data, colsea, colland, colborder)
#'}

pm_occraster <- function(shape,
                         data,
                         rank,
                         res=10,
                         colsea="#E5E5E520", colland="#66666680", colborder="#2B2B2B30"){
    
  #flter data for rank
  fdata <- rfilter(data, rank)
  #creating a raster in the size of the shape
    ras <- raster(shape, res=res)
    #raster of the occurences (sampling effort)
    r<-rasterize(fdata[,5:6],ras
                 ,fun=sum)
    #plotting the map and the raster on the map
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="longitude", ylab="latitude"
         , main=paste("Raster - sampling effort of ", rank), xaxs="i", yaxs="i")
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea)
    plot(shape, col=colland, border=colborder, add=TRUE)
    plot(r,col=c(mycols(res*res)), add=T)
    box(which="plot")
    #returning the raster
    r
}

#####################pm_richraster####################

#' pm_richraster
#' 
#' creates a raster of species richness
#' and makes a plot of the map and raster
#' 
#' @usage pm_richraster (shape, data, res, rank, colsea, colland, colborder)
#' 
#' @param the shape file from the time interval of interest. Can be created with get_paleomap
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng, can be created with getdata_paleomap
#' @param res resollution of the raster/ size of the grid cell
#' @param rank gives the rank for the richness raster (e.g. species, genus,...)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colborder color of the landmass borders
#' @return plot with map of the time intervall, the fossil occurences and the raster file. And the raster file itself
#' @export 
#' @examples \dontrun{
#' myraster <- pm_richraster (shape, data)
#' plot(myraster)
#'}
pm_richraster <- function(shape,
                          data,
                          res=10,
                          rank,
                          colsea="#E5E5E520", colland="#66666680", colborder="#2B2B2B30"){
  #creating a raster in size of the shape file
  ras <- raster(shape, res=res)
  #getting only species data and no duplictaed in a raster field
  fdata <- rank_filter(data, res=res, rank)
  #getting the raster of the species richness
  r<-rasterize(fdata[,5:6],ras,fun=sum)
  #plotting the map and the raster
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="longitude", ylab="latitude"
       , main=paste("Raster - richness of ", rank), xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea)
  plot(shape, col=colland, border=colborder, add=TRUE)
  plot(r, col= mycols(res*res), add=T)
  box(which="plot")
  #return the raster
  r
}


##############################color palette ########################################

#creating a color palette for the raster
mycols <- colorRampPalette(colors=c(rgb(0.255*3,0.255*3,0,0.5), rgb(0.144*3,0.238*3,0.144,0.5), rgb(0,1,0,0.5)), alpha=TRUE)


#################filter#######################
#filters all occurences which are not species and duplicates in raster cells


#' rank_filter
#' 
#' filters the data frame so tehre are only species left 
#' and for each raster every species only once
#' 
#' @usage rank_filer (data, res, rank)
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng,
#'  can be created with getdata_paleomap
#' @param res resolution of the raster file
#' @param rank rank of interest
#' @return filtered data frame with only species
#' @examples \dontrun{
#' filtered_data <- rank_filter (data, res, rank)
#' show(data)
#'}
#'

rank_filter <- function(data, res, rank){
  #gets colnames for new data frame
  filter <- data[0,]
  #filters only species
  if(rank=="species"){
    data <- subset(data, matched_rank=="species")
  }
  if(rank=="genus"){
    data <- subset(data, genus!="NA")
  }
  if(rank=="family"){
    data <- subset(data, family!="NA")
  }
  if(rank=="order"){
    data <- subset(data, order!="NA")
  }
  #getting each species only once in 10*10 raster cell
  for(i in seq(-180,180,res)){
    frame <- data[0,]
    for (j in seq(-90,90,res)){
      for (k in 1:length(data$paleolng)){
        if(data$paleolng>=i && data$paleolng <=(i+res) && data$paleolat>=j &&
             data$paleolat <=(j+res)){
          frame <- rbind(frame,data[k,])
        }
      }
      
    }
    #add for each raster the filtered data
    if(rank=="species"){ 
      filter <- rbind(filter, subset(frame, !duplicated(frame$matched_name)))
    }
    if(rank=="genus"){
      filter <- rbind(filter, subset(frame, !duplicated(frame$genus)))
    }
    if(rank=="family"){
      filter <- rbind(filter, subset(frame, !duplicated(frame$family)))
    }
    if(rank=="order"){
      filter <- rbind(filter, subset(frame, !duplicated(frame$order)))
    }
  }
  #return filtered data frame
  filter
}

rfilter <- function(data, rank){
  if(rank=="species"){
    data <- subset(data, matched_rank=="species")
  }
  if(rank=="genus"){
    data <- subset(data, genus!="NA")
  }
  if(rank=="family"){
    data <- subset(data, family!="NA")
  }
  if(rank=="order"){
    data <- subset(data, order!="NA")
  }
  data
}
