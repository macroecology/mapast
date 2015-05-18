library(raster)
library(maptools)
library(RCurl)
library(rgdal)
library (paleobioDB)
library(rgbif)


get_paleomap <- function (interval){
  wd <- getwd()
  file <- paste(gsub("paleoMap", "paleoMap/data/", wd), paste(interval, ".shp", sep=""), sep="")
  shape <- readShapePoly(file, IDvar=NULL, proj4string=CRS(as.character(NA)), verbose=FALSE, repair=TRUE, force_ring=TRUE)
  shape
}

getdata_paleomap <- function(base_name, interval, database){
  data <- c()
  if(database=="paleobiodb"){
    occ <- pbdb_occurrences (base_name=base_name, interval=interval, 
                      show=c("coords", "phylo", "ident", "paleoloc"), 
                      vocab="pbdb")
    data <- data.frame(occ)
  }
  if(database=="gbif"){
    occ <-occ_search(scientificName = base_name, hasCoordinate=TRUE, fields = c("family","genus","species","decimalLatitude","decimalLongitude", "earliestEpochOrLowestSeries"), return = "all")
    df <- data.frame(occ[3])
    if(length(df$data.earliestEpochOrLowestSeries)!=0){
      for(i in 1:length(df$data.earliestEpochOrLowestSeries)){
        if(!is.na(df$data.earliestEpochOrLowestSeries[i])){
          if(grep(interval, df$data.earliestEpochOrLowestSeries[i])==1){
            data <- rbind(data,df[i,])
          }
        }
      }
    }
    if(length(df$data.earliestEpochOrLowestSeries)==0 || length(data)==0){
      print("No occurences found!")
    }
  }
  data
}

