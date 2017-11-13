#' Fossil dataset for tests
#
#'A dataframe with the fossil occurences
#'
#' @keywords data
#' @format data.frame
#' @name data
#' @format A data frame with 1000 rows and 19 variables:
#' \itemize{
#'   \item occurence_no: occurence number of paleobioDB
#'   \item matche_name: name of the fossil
#'   \item matchd_rank: rank e.g. "genus", "species",...
#'   \item matched_no: matched number
#'   \item early_interval: early interval
#'   \item late_interval: late interval
#'   \item paleolng: paleolongitude
#'   \item paleolat: paleolatitude
#'   \item genus: the genus to which the fossil belongs to
#'   \item family: the familiy to which the fossil belongs to
#'   \item order: the order to which the fossil belongs to
#'   \item class: the class to which the fossil belongs to
#'   \item phylum: the phylum to which the fossil belongs to
#'   \item family_no: family number
#'   \item order_no: order number
#'   \item class_no: class number
#'   \item phylum_no: phylum number
#' }
#' 
NULL
#' Shape file
#'
#'
#' @format A Large SpatialPolygonsdataFrame with the shape of the landmasses at the specific interval for tests
#' @keywords shape
#' @format shapefile
#' @name shape
NULL
#' testdata
#' 
#' A shapefile from the Jurassic time interval and fossil data from reptiles from the Jurassic (limit=1000) 
#' 
#' @source http://www.gplates.org/
#' @docType data
#' @keywords datasets
#' @format shapefile, data.frame
#' @name testdata
NULL

#' df_maps
#' 
#' A data.frame containing Model, interval, fromage and toage of the maps provided by the package
#' 
#' @docType data
#' @keywords datasets
#' @format data.frame
#' @name df_maps
NULL