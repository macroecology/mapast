# private functions
# the following functions are used by our package to check the input given by the user
# but users can not change their parameters.

######checkRank#####
#'checkRank
#'
#'checks if the chosen rank is correct
#'
#'@usage checkRank(rank)
#'@param rank string with the rank of interest
#'@return boolean if the rank is possible to be chosen
#'@examples
#'\dontrun{
#'rank <- "species"
#'checkRank(rank)
#'}

checkRank <- function(rank){
  possible_ranks <- c("species", "genus", "family", "order", "class", "phylum")
  
  return(rank %in% possible_ranks)
}

#####checkLatLng#####
#'checkLatLng
#'
#'checks if the input data frame has 
#'paleolatitude and paleolongitude as columns
#'
#'@usage checkLatLng(data)
#'@param data a data frame with fossil occurences
#'@return boolean if paleolat and paleolng are columns of the data frame
#'@examples
#'\dontrun{
#'data <- pm_getdata(base_name="Canis", interval="Quaternary")
#'checkLatLng(data)
#'}

checkLatLng <- function(data){
  coln <- colnames(data)
  
  return("paleolat" %in% coln && "paleolng" %in% coln)
}

######checkDataRank#####
#'checkDataRank
#'
#'checks if the column corresponding to the rank is inside the data frame
#'
#' @usage checkDataRank(data, rank)
#' @param data a data frame with fossil occurences
#' @param rank a string defning the rank of interest
#' @return boolean of correct columns are inside the data frame
#' @examples 
#' \dontrun{
#'data <- pm_getdata(base_name="Canis", interval="Quaternary")
#'rank <- "species"
#'checkDataRank(data, rank)
#' } 

checkDataRank <- function(data, rank){
  coln <- colnames(data)
  if(rank=="species"){
    return("matched_name" %in% coln)
  }else{
    return(rank %in% coln)
  }
}

######checkShape#####
#'checkShape
#'
#'checks if the input shape is a SpatialPolygonsDataFrame
#'
#'@usage checkShape(shape)
#'@param shape a SpatialPolygonsDataFrame
#'@return a boolean if the shape is a SpatialPolygonsDataFrame
#'@examples
#'\dontrun{
#'shape <- pm_getmap(interval="Quaternary", model="GPlates")
#'checkShape(shape)
#'}

checkShape <- function(shape){
  return(class(shape)=="SpatialPolygonsDataFrame")
}

######checkFun#####
#'checkFun
#'
#'checks if chosen function is allowed for this function
#'
#'@usage checkFun(fun, fct)
#'@param fun a string defining the function
#'@param fct the fct that is calling this check. 
#'Either pm_latdiv or pm_divraster
#'@return boolean if function is allowed or not
#'@examples
#'\dontrun{
#'fun1 <- mean
#'fct1 <- "pm_latdiv"
#'checkFun(fun1, fct1)
#'
#'fun2 <- "count"
#'fct2 <- "pm_divraster"
#'checkFun(fun2, fct2)
#'}

checkFun <- function(fun, fct){
  if(fct == "pm_latdiv"){
    funs <- c("mean", "max")
    return(fun %in% funs)
  }else if(fct == "pm_divraster_loc"){
    funs <- c("mean", "max", "min", "count")
    return(fun %in% funs)
  }
}