# private functions
# the following functions are used by our package to check the input given by the user
# but users can not change their parameters.

######.checkRank#####
#.checkRank
#
#checks if the chosen rank is correct and if column with this rank exists
#
#@keywords internal
#@param rank string with the rank of interest
#@param df data frame of fossil data
#@return boolean if the rank is possible to be chosen
#@examples
#\dontrun{
#rank <- "species"
#.checkRank(rank)
#}

.checkRank <- function(rank, df){
  coln <- colnames(df)
  possible_ranks <- c("species", "genus", "family", "order", "class", "phylum")
  
  return(rank %in% possible_ranks && rank %in% coln)
}

#####.checkLatLng#####
#.checkLatLng
#
#checks if the input data frame has 
#paleolatitude, paleolongitude and recon_age as columns
#
#@keywords internal
#@param data a data frame with fossil occurences
#@return boolean if paleolat and paleolng are columns of the data frame
#@examples
#\dontrun{
#data <- getdata(base_name="Canis", interval="Quaternary")
#.checkLatLng(data)
#}

.checkLatLng <- function(data){
  coln <- base::colnames(data)
  
  return("paleolat" %in% coln && "paleolng" %in% coln && "recon_age" %in% coln)
}

######.checkDataRank#####
#.checkDataRank
#
#checks if the column corresponding to the rank is inside the data frame
#
#
#@param data a data frame with fossil occurences
#@param rank a string defning the rank of interest
#@return boolean of correct columns are inside the data frame
#@examples 
#\dontrun{
#data <- getdata(base_name="Canis", interval="Quaternary")
#rank <- "species"
#.checkDataRank(data, rank)
#} 

.checkDataRank <- function(data, rank){
  coln <- base::colnames(data)
  return(rank %in% coln)
}


######.checkShape#####
#.checkShape
#
#checks if the input shape is a SpatialPolygonsDataFrame
#
#@keywords internal
#@param shape a SpatialPolygonsDataFrame
#@return a boolean if the shape is a SpatialPolygonsDataFrame
#@examples
#\dontrun{
#shape <- getmap(interval="Quaternary", model="GPlates")
#.checkShape(shape)
#}

.checkShape <- function(shape){
  if(class(shape)=="list"){
    shapebool <- TRUE
    for(i in 1:length(shape)){
      if(! base::class(shape[[i]])=="SpatialPolygonsDataFrame"){
        shapebool <- FALSE
      }
      return(shapebool)
    }
  }else{
    return(base::class(shape)=="SpatialPolygonsDataFrame")
  }
  
}


######.checkRange#####
#.checkRange
#
#checks if paleolat and paleolng are in range 
#
#@param df data frame including at leat paleolat and paleolng
#@return boolean if lat and lng are in range
#@examples
#\dontrun{
#.checkRange(df)
#}

.checkRange <- function(df){
  lat <- df$paleolat
  lng <- df$paleolng
  return(base::min(lat, na.rm=TRUE) >= -90 && base::max(lat, na.rm=TRUE) <= 90 && base::min(lng, na.rm=TRUE) >= -180 && base::max(lng, na.rm=TRUE) <= 180)
}


######.checkSaveAs#####
#.checkSaveAs
#
# check if the input for save.as is correct
#
#@param save.as
#@return boolean if format is implemented to be saved
#@examples
#\dontrun{
#.checkSaveAs(save.as)
#}

.checkSaveAS <- function(save.as){
  formats <- c("pdf", "tiff", "jpeg", "png")
  if(!is.null(save.as)){
    return(save.as %in% formats)
  }else{
    return(TRUE)
  }
  
}


######.checkUnity#####
#.checkUnity
#
# check if unity is fossilsite or cell
#
#@param unity
#@return boolean if unity is fossilsite or cell
#@examples
#\dontrun{
#.checkUnity(unity)
#}

.checkUnity <- function(unity){
  unities <- c("fossilsite", "cell")
  return( unity %in% unities)
  
}


######.checkPaleocoords#####
#.checkPaleocoords
#
# check data frame for paleocoords
#
#@param df
#@return boolean if needed columns are in df
#@examples
#\dontrun{
#.checkPaleocoords(df)
#}

.checkPaleocoords <- function(df){
  dfcols <- colnames(df)
  columns <- c("recon_age", "lat", "lng", "early_age", "late_age", "avg_age")
  colbool <- TRUE
  for(i in 1:base::length(dfcols)){
    colbool <- colbool && (columns[i] %in% dfcols)
  }
  return(colbool)
}

######.checkFormat#####
#.checkFormat
#
# check if columns needed are inside the df
#
#@param df
#@return boolean if all columns needed are inside the df
#@examples
#\dontrun{
#.checkFormat(df)
#}

.checkFormat <- function(df){
  columns <- c("early_age", "late_age", "matched_rank")
  cols <- colnames(df)
  colbool <- TRUE
  for (i in 1:base::length(columns)){
    colbool <- colbool && (columns[i] %in% cols)
  }
  return(colbool)
}
