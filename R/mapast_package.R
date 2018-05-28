#' @docType package
#' @name mapast R-package
#' @title mapast: MAp the PAST.
#' @description We have developed mapast (MAp the PAST), an R-package designed to make paleontolgical maps available to 
#' visualise the fossil occurrences from the Paleobiology Database onto their corresponding maps and analyse them.
#' We programmed functions for getting the SpatialPolygonsDataFrame of the paleontological maps, 
#' functions to modify the fossil occurrences data.frame from the Paleobiology Database and getting the paleocoordinates, functions for analysing the 
#' fossil diversity of the past and plotting the fossil occurrences data on the corresponding paleogeographical maps.
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
#' Date: \tab 2018-06-01\cr
#' License: \tab GPL-2\cr
#' }
#' 
#'@seealso GPlates API
#'\url{https://github.com/GPlates/gplates_web_service_doc/wiki}
#'@seealso Reconstruction Models
#'\url{https://github.com/GPlates/gplates_web_service_doc/wiki/Reconstruction-Models}
#'@seealso PaleobioDB
#'\url{http://paleobiodb.org}
#'
#' 
#' @import devtools maptools paleobioDB raster roxygen2 rgdal rjson sp vegan
#' @importFrom stats na.omit
#' 
NULL
