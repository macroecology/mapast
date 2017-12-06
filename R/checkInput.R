# private functions
# the following functions are used by our package to check the input given by the user
# but users can not change their parameters.

######.checkRank#####
#.checkRank
#
#checks if the chosen rank is correct
#
#@keywords internal
#@param rank string with the rank of interest
#@return boolean if the rank is possible to be chosen
#@examples
#\dontrun{
#rank <- "species"
#.checkRank(rank)
#}

.checkRank <- function(rank){
  possible_ranks <- c("species", "genus", "family", "order", "class", "phylum")
  
  return(rank %in% possible_ranks)
}

#####.checkLatLng#####
#.checkLatLng
#
#checks if the input data frame has 
#paleolatitude and paleolongitude as columns
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
  
  return("paleolat" %in% coln && "paleolng" %in% coln)
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
  if(rank=="species"){
    return("matched_name" %in% coln)
  }else{
    return(rank %in% coln)
  }
}

######.checkDataNo#####
#.checkDataNo
#
#checks if the column xy_no corresponding to the rank is inside the data frame
#
#
#@param data a data frame with fossil occurences
#@param rank a string defning the rank of interest
#@return boolean of correct columns are inside the data frame
#@examples 
#\dontrun{
#data <- getdata(base_name="Canis", interval="Quaternary")
#rank <- "species"
#.checkDataNo(data, rank)
#} 

.checkDataNo <- function(data, rank){
  coln <- base::colnames(data)
  col <- base::paste0(rank, "_no")
  if(rank=="species"){
    return("matched_no" %in% coln)
  }else{
    return(col %in% coln)
  }
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
  return(base::class(shape)=="SpatialPolygonsDataFrame")
}

######.checkFun#####
#.checkFun
#
#checks if chosen function is allowed for this function
#
#@param fun a string defining the function
#@param fct the fct that is calling this check. 
#Either pm_latdiv or pm_divraster
#@return boolean if function is allowed or not
#@examples
#\dontrun{
#fun1 <- mean
#fct1 <- "pm_latdiv"
#.checkFun(fun1, fct1)
#
#fun2 <- "count"
#fct2 <- "pm_divraster"
#.checkFun(fun2, fct2)
#}

.checkFun <- function(fun, fct){
  if(fct == "pm_latdiv"){
    return(base::grep("mean", base::list(fun))==1 || base::identical(fun,mean) || base::identical(fun,max))
  }else if(fct == "pm_divraster_loc"){
    return(base::grep("mean", base::list(fun))==1 || base::identical(fun, mean) || base::identical(fun,max) || base::identical(fun,min) || base::identical(fun,"count"))
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
  return(base::min(lat)>=-90 && base::max(lat)<=90 && base::min(lng)>=-180 && base::max(lng)<=180)
}
