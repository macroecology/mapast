#' @docType package
#' @name mapast R-package
#' @title mapast: MAp the PAST.
#' @description We have developed mapast (MAp the PAST), an R-package designed to make paleontolgical maps available to 
#' visualise the fossil occurrences from the Paleobiology Database onto their corresponding maps and analyse them.
#' We programmed functions for getting the SpatialPolygonsDataFrame of the paleontological maps, 
#' functions to modify the data.frame from the Paleobiology Database and getting the paleocoordinates, analysing the 
#' diversity of the past and plotting this data on the corresponding paleogeographical maps.
#' @aliases mapast-package
#' @author Sonja Rothkugel \email{rothkugelsonja@@aol.de}
#' @author Sara Varela \email{svarela@@paleobiogeography.org}
#' 
#' @keywords package
#' 
#' @details \tabular{ll}{
#' Package: \tab mapast\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2017-12-12\cr
#' License: \tab GPL-2\cr
#' }
#' 
#'@seealso GPlates
#'\url{http://www.gplates.org/}
#'@seealso Golonka & Smith Global Paleoshorelines
#'\url{https://github.com/chhei/Heine_AJES_15_GlobalPaleoshorelines}
#'@seealso PaleobioDB
#'\url{http://paleobiodb.org}
#'
#' 
#' @import devtools maptools paleobioDB raster roxygen2 rgdal rjson sp vegan
#' @importFrom stats na.omit
#' 
NULL
