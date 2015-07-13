
#'We have developed paleoMap, an R-package designed to make palaeontolgical maps 
#'available to plot the fossil occurences from paleontological databases onto the maps of the past.  
#'
#'
#'We programmed functions for getting the shape files of the paleo-maps, 
#'functions to get fossil data from the paleobilogy database and plotting this 
#'data on the maps.
#'
#' @name paleoMap
#' @aliases paleoMap-package
#' @docType package
#' @title paleoMap: An R-package for getting and using paleontoligcal maps.
#' @author Sonja Rothkugel \email{rothkugelsonja@@aol.de}
#' @author Sara Varela \email{svarela@@paleobiogeography.org}
#' # @references
#' 
#' @keywords package
#'
#' @details \tabular{ll}{
#' Package: \tab paleoMap\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2015-08-01\cr
#' License: \tab GPL-2\cr
#' }
#' 
#'@seealso {
#'\url{http://www.gplates.org/}
#'\url{http://paleobiodb.org}
#' }
#' 
#' @import raster maptools paleobioDB 
#' 
#'@examples \dontrun{
#'
#'#save and display the shp-file of the Aalenian map and creates plot is plot=TRUE
#'aalenian <- pm_getmap(interval="aalenian", plot=TRUE, colsea="#00005020", 
#'colland="#2B2B2B80", colborder="#2B2B2B30")
#'
#'#get and save the fossil occurences of mammalia found in the Aalenian
#'reptilia_aalenian <- pm_getdata(base_name="reptilia", interval="aalenian", limit=1000)
#'
#'#plot the fossil occurences on the map
#'plot(aalenian) #shape file saved before
#'points(reptilia_aalenian$paleolng, reptilia_aalenian$paleolat, pch=19, col="green")
#'
#'
#'#directly plotting reptilia on aalenian without saving them before
#'pm_plot(base_name="reptilia", interval="aalenian", limit=1000,
#'        colsea="#00005020",colland="#2B2B2B80", colborder="#2B2B2B30",
#'        colpoints="#9ACD3250",colpointborder="black")
#'
#'#creating a raster file for the sampling effort
#'jurassic <- pm_getmap(interval="jurassic", plot=FALSE, colsea="#00005020", colland="#2B2B2B80", colborder="#2B2B2B30")
#'reptilia_jurassic <- pm_getdata(interval="jurassic", base_name="reptilia", limit=1000)
#'myraster <- pm_occraster(shape=jurassic, data=reptilia_jurassic, res=10,colsea="#00005020",
#'                          colland="#2B2B2B80", colborder="#2B2B2B30")
#'show(myraster)
#'
#'#creating a raster file of the species richness
#'jurassic <- pm_getmap(interval="jurassic", plot=FALSE, colsea="#00005020", colland="#2B2B2B80", colborder="#2B2B2B30")
#'reptilia_jurassic <- pm_getdata(interval="jurassic", base_name="reptilia", limit=1000)
#'myraster <- pm_richraster(shape=jurassic, data=reptilia_jurassic, res=10,colsea="#00005020",
#'                          colland="#2B2B2B80", colborder="#2B2B2B30")
#'show(myraster)
#'
#' }
#'