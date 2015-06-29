
#'We have developed paleoMap, an R-package designed to make palaeontolgical maps 
#'available and using them for plotting the fossil occurences from the paleobio 
#'database on the maps of the past.  
#'
#'
#'We programmed functions for getting the shape file of the maps in the past, 
#'functions to get fossil data from the paleobilogy database and plotting this 
#'data on the maps.
#'
#' @name paleoMap
#' @aliases paleoMap-package
#' @docType package
#' @title paleoMap: An R-package for getting and using paleontoligcal maps.
#' @author Sonja Rothkugel \email{rothkugelsonja@aol.de}
#' @references ?
#' 
#' @keywords package
#'
#' @details \tabular{ll}{
#' Package: \tab paleoMap\cr
#' Type: \tab Package\cr
#' Version: \tab ? \cr
#' Date: \tab ? \cr
#' License: \tab ? \cr
#' }
#' 
#' 
#' @import raster maptools paleobioDB 
#' 
#'@examples \dontrun{
#'
#'#save and display the shp-file of the Aeronian map
#'aalenian <- get_paleomap(interval="aalenian")
#'plot(aeronian)
#'
#'#get and save the fossil occurences of mammalia found in the Aalenian
#'reptilia_aalenian <- getdata_paleomap(base_name="reptilia", interval="aalenian")
#'
#'plot the fossil occurences on the map
#'plot(aalenian) #shape file saved before
#'points(reptilia_aalenian$paleolng, reptilia_aalenian$paleolat, pch=19, col="red")
#'
#'
#'#directly plotting reptilia on aalenian without saving them before
#'plot_paleomap(base_name="reptilia", interval="aalenian")
#'
#'#reating a raster file
#'jurassic <- get_paleomap(interval="jurassic")
#'reptilia_jurassic <- getdata_paleomap(base_name="reptilia", interval="jurassic")
#'myraster <- raster_paleomap(shape=jurassic, data=reptilia_jurassic)
#'show(myraster)
#'
#'
#' }
#'