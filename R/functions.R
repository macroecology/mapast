###########################getmap#############################

#' getmap
#' 
#' Generates a SpatialPolygonsDataFrame with the paleogeographical map of the chosen 
#' time interval (e.g. "Cretaceous") and a plot of the map.
#' 
#' @usage getmap(interval, model, colland = "#66666660"
#'                           , colsea = "#00509010", do.plot = TRUE, ...)
#' 
#' @param interval character. Temporal paleoeographical interval of interest (e.g. "Cretaceous" for GPlates or "112.0" for Smith).
#' @param model character. Defining the model the map was created with. "GPlates", "Smith" or "Golonka".
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660"
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main = "my own title" or main.col = "red".
#' @return SpatialPolygonsDataFrame
#' @export
#' @examples
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #GPlates
#' getmap(interval = "Cretaceous", model = "GPlates")
#' 
#' #save plot as pdf file
#' pdf("getmap-GPlates_Cretaceous.pdf")
#' getmap(interval = "Cretaceous", model = "GPlates")
#' dev.off()
#'
#'#save plot as tiff image
#' tiff("getmap-GPlates_Cretaceous.tiff", 
#'       height = 10, width = 17, units = "cm", res = 300)
#' getmap(interval = "Cretaceous", model = "GPlates")
#' dev.off()
#' 
#' 
#' #Smith
#' getmap(interval = "112.0", model = "Smith")
#' 
#' 
#' #Golonka
#' getmap(interval = "123.0", model = "Golonka")
#' 
#' #for checking which maps are available including a specific age 
#' # or the complete list of maps provided by this package
#' # look at ??checkage
#' 
#'}

getmap <- function(interval, model, colland = "#66666660", 
                      colsea = "#00509010", 
                      do.plot = TRUE, ...) {
  #check if requested map is available
  mapavailable <- .mapAvailable(interval, model)
  if(mapavailable){
    # if map is available, get shape file from external database
    #get GPlates map
    if(model == "GPlates"){
      shape <- utils::data(list = base::paste(model,interval, sep = "_"), package = "paleogeoDB"
                           ,envir = base::environment())
      base::assign("shape",base::get(interval))
    }else{
      #get Smith or Golonka map
      shape <- utils::data(list = base::paste(model,interval, sep = "_"), package = "paleogeoDB"
                           ,envir = base::environment())
      base::assign("shape",base::get(base::paste(model,interval, sep = "_")))
    }
  }else{
    # throw error if map is not available
    stop(paste0("No map available for interval = \"",interval,"\", model = \"",
                model,"\". Please use checkage() to see which maps are available."))
  }
  #getting final parameter list for plot
  #default parameter list for plotting
  graphparams.def <- base::list(x = shape, col = "white", border = FALSE
                         , xlim = c(-180,180), ylim = c(-90,90)
                         , xaxs = "i", yaxs = "i")
  #list of user defined graphical parameter
  graphparams.user <- base::list(...)
  names_graphparams.user <- base::as.vector(base::names(graphparams.user))
  names_graphparams.def <- base::as.vector(base::names(graphparams.def))
  #remove default parameter if user specifies a different value
  for( param in names_graphparams.user){
    if(param %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == param)] 
  }
  #complete new list of plotting parameters, including default and user specified ones
  graphparams <- c(graphparams.def, graphparams.user)
  # if user does not set plot=FALSE plot the shape file
  if (do.plot) {
    #define the size of the margin of the plot and save the former definition
    def.mar <- graphics::par("mar")
    graphics::par(mar = c(2,2,2,2))
    #do a first plot with the graphical parameters set by the user
    base::do.call(sp::plot, graphparams)
    #draw a rectangle showing the sea
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)
    #add x-axis and x-axis label
    graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180,-180,4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
    graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels="Longitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
    #add y-axis and y-axis label
    graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6, las = 1)
    graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
    #get metadata from the SpatialPolygonsDataFrame
    shape.info <- .getShapeInfo(shape)
    #add name, model and age info top right of the plot
    graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = shape.info[1], col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 1)
    graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = shape.info[2], col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.5)
    graphics::axis(side = 3, pos = 81, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.7)
    #add the landmasses to the plot
    sp::plot(shape, col = colland, border = FALSE, add = TRUE)
    #restore the former graphical mar parameters
    graphics::par(mar = def.mar)
  }
  # return the shape file
  return(shape)
}



################getdata##############################

#' getdata
#' 
#' Uses the paleobioDB R package to extract data needed for other functions of this package
#' from the Paleobiology Database. Creates a data.frame with fossil occurrence data.
#'  
#' @usage getdata(interval, base_name, limit = "all")
#' 
#' @param interval character. Temporal paleoeographical interval of interest (e.g. "Cretaceous" for GPlates or "112.0" for Smith).
#' @param base_name character. The name of the taxon of interest (e.g. "Canis" or "reptilia").
#' @param limit numeric. Defining the maximum number of occurrences to be downloaded from paleobioDB. By default limit = "all".
#' @return data.frame
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(pasta)
#' 
#' getdata(interval = "Quaternary", base_name = "Canis")
#' 
#' #if you want to use FROMAGE and TOAGE to define the interval you can use the paleobioDB package
#' #max_ma defines the fromage parameter
#' #min_ma defines the toage parameter
#' myocc <- base::data.frame(paleobioDB::pbdb_occurrences(base_name = "mammalia", 
#'                     min_ma = 0, max_ma = 2.58, 
#'                     show = c("paleoloc", "phylo"), 
#'                     vocab = "pbdb", limit = 100))
#'                     
#'}


getdata <- function(interval, base_name, limit = "all") {
  #create an empty data frame for storing the fossil data
  occ <- base::data.frame()
  #try to get data from the Paleobiology Database with the given parameters, using R-package paleobioDB
  try(occ <- base::data.frame(paleobioDB::pbdb_occurrences(base_name = base_name, interval = interval, 
                                     show = c("paleoloc", "phylo"), 
                                     vocab = "pbdb", limit = limit))
                        , silent = TRUE)
  #if there were results, save them in a data frame called 'data' and remove entries which do not have a paleolatitude or paleolongitude
  if (base::nrow(occ) != 0) {
    data <- .checkPbdb(occ)
    data <- data[!base::is.na(data$paleolat), ]
    data <- data[!base::is.na(data$paleolng), ]
    #return data frame with the fossil occurrences
    return(data)
  } else { #catching error when there are no occurences for the request
      stop("There is no data that matches your query on the paleobioDB. 
          Check if the spelling, temporal intervals, etc. are correct")
    }
}



####################pastplot#################################

#' pastplot
#' 
#' Plots your fossil occurrence data from the paleobioDB onto the map of the selected time interval.
#' 
#' 
#' @usage pastplot(interval, model, data, colland = "#66666660",
#'                 colsea = "#00509010",colpoints = "#65432190", 
#'                 pch = 16, cex = 1, ...)
#' 
#' @param interval character. Temporal paleoeographical interval of interest. (e.g. "Cretaceous" for GPlates or "112.0" for Smith)
#' @param model character. Defining the model the map was created with. "GPlates", "Smith" or "Golonka".
#' @param data data.frame. Fossil occurrences data. Can be created with getdata(interval, base_name).
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param colpoints character. Defines the color of the occurrence-points. By default colpoints = "#65432190".
#' @param pch numeric. Point symbol for plotting the occurences. By default pch = 16 (filled circle).
#' @param cex numeric. Size of the points. By default cex = 1.
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return Plot
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #get data
#' data  <-  getdata (interval = "Cretaceous", base_name = "Mammalia")
#' #create a plot with fossils on the paleogeographical map
#' pastplot(interval = "Cretaceous", model = "GPlates", data)
#' 
#' #save plot as pdf file
#' pdf("pastplot-GPlates_Cretaceous-Mammalia.pdf")
#' pastplot(interval = "Cretaceous", model = "GPlates", data)
#' dev.off()
#' 
#' #save plot as tiff image
#' tiff("pastplot-GPlates_Cretaceous-Mammalia.tiff", 
#'       height = 10.5, width = 17, units = "cm", res = 300)
#' pastplot(interval = "Cretaceous", model = "GPlates", data)
#' dev.off()
#' 
#'}

pastplot <- function(interval, model, data,
                    colland = "#66666660",
                    colsea = "#00509010", 
                    colpoints = "#65432190", 
                    pch = 16, cex = 1, ...) {
  
  #check if inut data has needed columns (paleolat/paleolng)
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  #getting the shape file with getmap
  shape <- pasta::getmap(interval = interval, model = model, do.plot = FALSE)
  #default parameter list for plotting
  graphparams.def <- base::list(x = shape, col = "white", border = FALSE
                         , xlim = c(-180, 180), ylim = c(-90, 90)
                         , xaxs = "i", yaxs = "i")
  #list of user defined graphical parameter
  graphparams.user <- base::list(...)
  names_graphparams.user <- base::as.vector(base::names(graphparams.user))
  names_graphparams.def <- base::as.vector(base::names(graphparams.def))
  #remove default parameter from list if user specified the same parameter different
  for( param in names_graphparams.user){
    if(param %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == param)] 
  }
  #create graphparams with default and user parameter for plotting
  graphparams <- c(graphparams.def, graphparams.user)
  #plotting the map and the data
  #input data needs to be a data frame
  if (base::class(data) == "data.frame") {
    #save old mar settings and define plotting margins as we need
    def.mar <- graphics::par("mar")
    graphics::par(mar = c(1.5, 1.5, 2, 1.5))
    #plot with the parameter list which includes users graphical parameter
    #defines size and axes of the plot
    base::do.call(sp::plot, graphparams)
    #draw the rectangle showing the sea
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)
    #plot the landmasses on the sea
    sp::plot(shape, col = colland, border = FALSE, add = TRUE)
    # add x-axis and x-axis labels
    graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    # add y-axis and y-axis labels
    graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
    graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    #get metadata from the SpatialPolygonDataFrame
    shape.info <- .getShapeInfo(shape)
    #add name, model and age at the top right of the plot
    graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = shape.info[1], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
    graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = shape.info[2], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.5)
    graphics::axis(side = 3, pos = 81, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
    #add the fossil occurrences to the plot
    graphics::points(data$paleolng, 
           data$paleolat, 
           pch = pch, col = colpoints, 
           cex = cex)
    #restore the old margin values
    graphics::par(mar = def.mar)
  }
}

#####################mapocc##############################

#' mapocc
#' 
#' Creates a RasterLayer, containing the number of occurrences per cell, and a plot of the fossil 
#' occurences by taxonomic rank per cell (a proxy for the sampling effort).
#' 
#' @usage mapocc(shape, data, rank = "genus", res = 1,
#'                     colland = "#66666660", colsea = "#00509010", col.grid = mycols(100), 
#'                     do.plot = TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame. A map that can be created with getmap.
#' @param data data.frame. Fossil occurrences data. Can be created with 
#' getdata(interval, base_name).
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum".
#' @param res numeric. Defining the spatial resolution. Default res = 1. 
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param col.grid character. Defines the color of the raster.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main = "my own title" or main.col = "red".
#' @return RasterLayer
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #get map
#' shape <- getmap(interval = "Quaternary", model = "GPlates", do.plot = FALSE)
#' #get data from paleobioDB
#' data <- getdata(base_name = "Canis", interval = "Quaternary")
#' #creasre map with occurrence RasterLayer
#' mapocc(shape, data)
#' 
#' #save plot as pdf file
#' pdf("mapocc-GPlates_Quaternary-Canis.pdf")
#' mapocc(shape, data)
#' dev.off()
#' 
#' # save plot as tiff image
#' tiff("mapocc-GPlates_Quaternary-Canis.tiff", 
#'       height = 10.5, width = 19, units = "cm", res = 300)
#' mapocc(shape, data)
#' dev.off()
#' 
#'}

mapocc <- function(shape, data, 
                         rank = "genus", 
                         res=1, 
                         colland = "#66666660",
                         colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...) {
  #check user input
  #check if data has latitude and longitude columns
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  #check if the rank is allowed
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep = ""))
  }
  #check if the needed column for the chosen rank is in the data frame
  if(!.checkDataRank(data,rank)){
    if(rank == "species"){
      stop(base::paste("There is no column matched_name in the data frame.", sep = ""))
    }else{
      stop(base::paste("There is no column ", rank,  " in the data frame.", sep = ""))
    }
  }
  #check if the shape is a SpatialPolygonsDataFrame
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  #filter data for rank
  rankdata <- .rfilter(data, rank)
  #creating a raster in the size of the shape
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
  #create a raster of the occurences (sampling effort)
  if(rank == "species"){
    occraster <- raster::rasterize(rankdata[ , c("paleolng", "paleolat")], ras, field = rankdata[ , "matched_name"], fun = "count")
  }else{
    occraster <- raster::rasterize(rankdata[ , c("paleolng", "paleolat")], ras, field = rankdata[ , rank], fun = "count")
  }
  
  #default graphical parameter list
  graphparams.def <- base::list(x = shape, col = "white", border = FALSE, col.grid = col.grid
                         , xlim = c(-180, 180), ylim = c(-90, 90)
                         , xaxs = "i", yaxs = "i")
  #list user given graphical parameter
  graphparams.user <- base::list(...)
  names_graphparams.user <- base::as.vector(base::names(graphparams.user))
  names_graphparams.def <- base::as.vector(base::names(graphparams.def))
  #if user changes parameter defined with default values take users values
  for( param in names_graphparams.user){
    if(param %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == param)] 
  }
  #create a graphical parameter list including default and user parameter
  graphparams <- c(graphparams.def, graphparams.user)
  #if user changes grid color, save it in mycol and remove it from the list
  gridcol <- graphparams$col.grid
  graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
  #if do.plot is true, create a plot
  if(do.plot){
    #save old margin values and define needed margin values
    def.mar <- graphics::par("mar")
    graphics::par(mar = c(1.5, 1.5, 2, 4))
    #create a plot with the users parameters
    base::do.call(raster::plot, graphparams)
    #create a rectangle showing the sea
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                   border = FALSE)
    #plot the landmasses on the sea
    raster::plot(shape, col = colland, border = FALSE, add = T)
    #add x-axis and x-axis labels
    graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    #add y-axis and y-axis labels
    graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
    graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    #get metatdata from the SpatialPolygonsDataFrame
    shape.info <- .getShapeInfo(shape)
    #add name, model, age at the top right of the plot
    graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = shape.info[1], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
    graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = shape.info[2], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.5)
    graphics::axis(side = 3, pos = 81, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
    #add the raster to the plot without legend
    raster::plot(occraster, add = T,axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
    #allow the plot to expand the borders
    graphics::par(xpd = TRUE)
    graphics::par(bty = "n")
    #add the raster legend outside the plot
    raster::plot(occraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                  axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA),
                  legend.args = list(text = "occurrences", line = 1, side = 3, adj = 0.25, cex = 0.6, col = col.grid[length(col.grid) / 2]))
    raster::plot(occraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                  axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = col.grid[length(col.grid) / 2]))
    graphics::par(bty = "o")
    #restore default margin settings
    graphics::par(mar = def.mar)
  }
  #return the raster
  return(occraster)
}

#####################maprich####################

#' maprich
#' 
#' Creates a RasterLayer of taxon richness
#' and makes a plot of the map and richness raster.
#' 
#' @usage maprich(shape, data, rank = "genus", res = 1, 
#'                      colland = "#66666660", colsea = "#00509010", col.grid = mycols(100), 
#'                      do.plot = TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame. A map which can be created with getmap.
#' @param data data.frame. Fossil occurrences data. Can be created with 
#' getdata(interval, base_name).
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank="genus".
#' @param res numeric. Defining the spatial resolution. Default res = 1. 
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param col.grid character. Defines the color of the raster.
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main = "my own title" or main.col = "red".
#' @return RasterLayer
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #get map
#' shape <- getmap(interval = "Paleocene", model = "GPlates")
#' #get the data from the paleobioDB
#' data<- getdata(base_name = "Testudines", interval = "Paleocene")
#' #calculate the genus richness
#' richness<- maprich(shape, data, rank = "genus")
#' 
#' #save plot as pdf file
#' pdf("maprich-GPlates_Paleocene-Testudines.pdf")
#' maprich(shape, data, rank = "genus")
#' dev.off()
#' 
#' #save plot as tiff image
#' tiff("maprich-GPlates_Paleocene-Testudines.tiff", 
#'       height = 10.5, width = 19, units = "cm", res = 300)
#' maprich(shape, data, rank = "genus")
#' dev.off()
#' 
#'}
#'

maprich <- function (shape, data, rank = "genus", res = 1, 
                           colland = "#66666660",
                           colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...) {
  #check the users input data
  #check if the lat/lng column is in the data frame
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  #check if the rank is allowed
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep = ""))
  }
  #check if the columns according to the rank is inside the data frame
  if(!.checkDataRank(data, rank)){
    if(rank == "species"){
      stop(base::paste("There is no column matched_name in the data frame.", sep = ""))
    }else{
      stop(base::paste("There is no column ", rank, " in the data frame.", sep = ""))
    }
  }
  #check if the shape is a SpatialPolygonsDataFrame
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  #check if taxon_no is in data frame
  if(!.checkDataNo(data, rank)){
    stop(base::paste0("Column ", rank, "_no is missing in the data frame"))
  }
  #creating a raster in size of the shape file
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
  #getting the raster of the species richness
  richraster <- .rank_filter(ras, data, res = res, rank)
  #default graphical parameters
  graphparams.def <- base::list(x = shape, col = "white", border = FALSE, col.grid = col.grid
                                       , xlim = c(-180, 180), ylim = c(-90, 90)
                                       , xaxs = "i", yaxs = "i")
  #list of user defined graphical parameter
  graphparams.user <- base::list(...)
  names_graphparams.user <- base::as.vector(base::names(graphparams.user))
  names_graphparams.def <- base::as.vector(base::names(graphparams.def))
  #if user defines default value different only keep users value
  for( param in names_graphparams.user){
    if(param %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == param)] 
  }
  #komplete parameter list
  graphparams <- c(graphparams.def, graphparams.user)
  #save the color of the grid/raster and remove it from the parameter list (only needed later)
  gridcol <- graphparams$col.grid
  graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
  if(do.plot){
    #save current margin values and define it as needed
    def.mar <- graphics::par("mar")
    graphics::par(mar = c(1.5, 1.5, 2, 4))
    #plot with the default and user defined graphical parameter
    base::do.call(raster::plot, graphparams)
    #add a rectangle as the sea
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                   border = FALSE)
    #add the landmasses to the plot
    raster::plot(shape, col = colland, border = FALSE, add = T, bty = "L")
    #add x-axis and x-axis labels
    graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    #add y-axis and y-axis labels
    graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
    graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    #get metadata from the shape file
    shape.info <- .getShapeInfo(shape)
    #add name, model and age at the top rigt of the plot
    graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = shape.info[1], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
    graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = shape.info[2], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.5)
    graphics::axis(side = 3, pos = 81, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
    #add the raster without legend
    raster::plot(richraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
    #allow to draw outside the plot
    graphics::par(xpd = TRUE)
    graphics::par(bty = "n")
    #add raster legend outside the plot
    raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                  axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                  legend.args = list(text = "richness", line = 1, side = 3, adj = 0.25, cex = 0.6, col = col.grid[length(col.grid) / 2]))
    raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                 axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = col.grid[length(col.grid) / 2]))
    graphics::par(bty = "o")
    #restore prior margin values
    graphics::par(mar = def.mar)
  }
  #return the raster
  return(richraster)
}

########spsite###################
#' spsite
#' 
#' Generates a diversity data.frame, with the number occurrences of a taxon per locality.
#' 
#' @usage spsite(data, unity, res = 1, rank = "genus", pa = FALSE)
#' 
#' @param data data.frame. Fossil occurrences data. Can be created with 
#' getdata(interval, base_name).
#' @param unity character. unity = "fossilsite" or unity = "cell" defining if the user wants the occurrences per 
#' cell or per fossilsite.
#' @param res numeric. Defining the spatial resolution. By default res = 1. Only used if unity = "cell".
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus"
#' @param pa boolean. Defines if the user wants presence absence or counted data. By default pa = FALSE.
#' @return data.frame
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #get fossil occurrence data
#' data <- getdata(base_name = "Canis", interval = "Quaternary")
#' #get thediversity per locality
#' result <- spsite(data, unity = "fossilsite", rank = "genus")
#' #get the diversity per cell
#' result_cell <- spsite(data, unity="cell", rank = "genus")
#' 
#'}

spsite <- function(data, unity, res = 1, rank = "genus", pa = FALSE) {
  #check users input data
  #check if lat/lng columns are in the data frame
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  #check if the chosen rank is allowed
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep = ""))
  }
  #check if the column belonging to the rank is in the data frame
  if(!.checkDataRank(data,rank)){
    if(rank == "species"){
      stop(base::paste("There is no column matched_name in the data frame.", sep = ""))
    }else{
      stop(base::paste("There is no column ", rank, " in the data frame.", sep=""))
    }
  }
  
  if(unity == "fossilsite"){
    #filter data for the rank
    rankdata <- .rfilter(data, rank)
    #create a data. frame with all the locations once
    latlng <- base::data.frame(paleolng = rankdata$paleolng, paleolat = rankdata$paleolat)
    ulatlng <- base::unique(latlng)
    #getting list of unique taxa
    if(rank == "species"){
      urank <- base::as.vector(base::unique(rankdata[ , "matched_name"]))
    }else if(rank == "genus"){
      urank <- base::as.vector(base::unique(rankdata[ , "genus"]))
    }else if(rank == "family"){
      urank <- base::as.vector(base::unique(rankdata[ , "family"]))
    }else if(rank == "order"){
      urank <- base::as.vector(base::unique(rankdata[ , "order"]))
    }else if(rank == "class"){
      urank <- base::as.vector(base::unique(rankdata[ , "class"]))
    }else if(rank == "phylum"){
      urank <- base::as.vector(base::unique(rankdata[ , "phylum"]))
    }
    #save the unique locations in nsites
    occ <- ulatlng
    #fill matrix with default values -1 for each species/genus/.. and each locality
    def.values <- base::matrix(-1, nrow = base::nrow(occ), ncol = base::length(urank))
    #add the unique localities to the matrix
    occ <- base::cbind(occ, def.values)
    #set lat, lng and species/genus/.. names as column names
    base::colnames(occ) <- c(base::unlist(base::names(latlng)), urank)
    #getting the number of occurrences of a species/genus/... for each locality
    for (curloc in 1:base::nrow(occ)) {
      #get lat & lng
      lat_cur <- occ[curloc, "paleolat"]
      lng_cur <- occ[curloc, "paleolng"]
      #go through the list with unique species/genera/...
      for (curtaxon in 1:base::length(urank)) {
        #get current species/genus/...
        taxon_cur <- urank[curtaxon]
        #get all fossil occurrences at current locality
        curlatlng <- base::subset(rankdata, rankdata$paleolat == lat_cur)
        curlatlng <- base::subset(curlatlng, curlatlng$paleolng == lng_cur)
        #select only current species/genus/...
        if(rank == "species"){
          cur.taxon <- base::subset(curlatlng, curlatlng[ , "matched_name"] == taxon_cur)
        }else if(rank == "genus"){
          cur.taxon <- base::subset(curlatlng, curlatlng[ , "genus"] == taxon_cur)
        }else if(rank == "family"){
          cur.taxon <- base::subset(curlatlng, curlatlng[ , "family"] == taxon_cur)
        }else if(rank == "order"){
          cur.taxon <- base::subset(curlatlng, curlatlng[ , "order"] == taxon_cur)
        }else if(rank == "class"){
          cur.taxon <- base::subset(curlatlng, curlatlng[ , "class"] == taxon_cur)
        }else if(rank == "phylum"){
          cur.taxon <- base::subset(curlatlng, curlatlng[ , "phylum"] == taxon_cur)
        }
        #count the number of species/geners/.. in this locality and save it in the matrix
        count<- base::nrow(cur.taxon)
        occ[curloc, curtaxon + 2] <- count
      }
    }
    if(pa){
      occnoloc <- occ[ , 3:length(occ)]
      occnoloc[occnoloc > 0] <- 1
      occ <- base::cbind(paleolng = occ$paleolng, paleolat = occ$paleolat, occnoloc)
    }
    occ <- occ[with(occ, order(paleolng, -paleolat)), ]
    #return the data.frame
    return(occ)
  }
  if(unity == "cell"){
    if(rank == "species"){
      data <- data[which(data$matched_rank == "species"), ]
    }else if(rank == "genus"){
      data <- data[which(!is.na(data$genus)), ]
    }else if(rank == "order"){
      data <- data[which(!is.na(data$order)), ]
    }else if(rank == "family"){
      data <- data[which(!is.na(data$family)), ]
    }else if(rank == "class"){
      data <- data[which(!is.na(data$class)), ]
    }else{
      data <- data[which(!is.na(data$phylum)), ]
    }
    #remove NA columns that don't have the chosen rank
    if(rank == "species"){
      rankcol <- "matched_name"
    }else{
      rankcol <- rank
    }
    data <- data[!is.na(data[[rankcol]]), ]
    #only getting occurences with a known genus
    rankdata <- .rfilter(data, rank)
    #getting list of unique taxa
    if(rank == "species"){
      urank <- base::as.vector(base::unique(rankdata[ , "matched_name"]))
    }else if(rank == "genus"){
      urank <- base::as.vector(base::unique(rankdata[ , "genus"]))
    }else if(rank == "family"){
      urank <- base::as.vector(base::unique(rankdata[ , "family"]))
    }else if (rank == "order"){
      urank <- base::as.vector(base::unique(rankdata[ , "order"]))
    }
    else if(rank == "class"){
      urank <- base::as.vector(base::unique(rankdata[ , "class"]))
    }else{
      urank <- base::as.vector(base::unique(rankdata[ , "phylum"]))
    }
    #define lat/lng sequence using the resolution
    lat <- base::seq(-90 + (res / 2) , 90 - (res / 2), res)
    long <- base::seq(-180 + (res / 2), 180 - (res / 2), res)
    occ <- base::expand.grid (long, lat)
    base::colnames(occ) <- c ("paleolng", "paleolat")
    occ <- occ[with(occ, order(paleolng, -paleolat)), ]
    #fill with default values 0 and add lat, lng and column names
    def.values <- base::matrix(0, nrow = base::nrow (occ), ncol = base::length(urank))
    occ <- base::cbind(occ, def.values)
    base::colnames(occ) <- c ("paleolng", "paleolat", urank)
    #getting the number of occurrences of a taxa for each locality
    latbord <- seq(90, -90, -res)
    for(curocc in 1:length(data$paleolng)){
      curtaxon <- as.character(data[[rankcol]][curocc])
      curlat <- data$paleolat[curocc]
      curlng <- data$paleolng[curocc]
      if(!(curlat %in% latbord)){
        if(curlng == 180){
          if(curlat >= 90 - (res / 2)){
            row <- abs(ceiling((curlat - 90) / res) + 1) + abs(floor((curlng + 180) / res) - 1) * (180 / res)
          }else{
            row <- abs(ceiling((curlat - 90) / res)) + abs(floor((curlng + 180) / res) - 1) * (180 / res)
          }
          
        }else if(curlng == -180){
          if(curlat >= 90 - (res / 2)){
            row <- abs(ceiling((curlat - 90) / res) + 1) + abs(floor((curlng + 180) / res)) * (180 / res)
          }else{
            row <- abs(ceiling((curlat - 90) / res)) + abs(floor((curlng + 180) / res)) * (180 / res)
          }
          
        }else{
          row <- abs(ceiling((curlat - 90) / res)) + 1 + abs(floor((curlng + 180) / res))*(180 / res)
        }
      }else{
        if(curlng == 180){
          row <- abs(ceiling((curlat - 90) / res)) + 1 + abs(floor((curlng + 180) / res) - 1) * (180 / res)
        }else if(curlng == -180){
          if(curlat >= 90 - (res / 2)){
            row <- abs(ceiling((curlat - 90) / res)) + abs(floor((curlng + 180) / res) - 1) * (180 / res)
          }else{
            row <- abs(ceiling((curlat - 90) / res)) + abs(floor((curlng + 180) / res) - 1) * (180 / res)
          }
        }else{
          if(curlat <= -90 + (res / 2)){
            if(curlng <= -180 + (res / 2)){
              row <- abs(ceiling((curlat - 90) / res)) + abs(floor((curlng + 180) / res)) * (180 / res)
            }else{
              row <- abs(ceiling((curlat - 90) / res)) + abs(floor((curlng + 180) / res)) * (180 / res)
            } 
          }else if (curlat >= 90 - (res / 2)){
            row <- abs(ceiling((curlat - 90) / res)) + 1 + abs(floor((curlng + 180) / res)) * (180 / res)
          }else{
            row <- abs(ceiling((curlat - 90) / res)) + 1 + abs(floor((curlng + 180) / res)) * (180 / res)
          }
        }
      }
      
      if(row == 0){
        row <- 1
      }
      occ[row, curtaxon] <- (occ[row, curtaxon] + 1)
    }
    if(pa){
      occnoloc <- occ[ , 3:base::length(occ)]
      occnoloc[occnoloc > 0] <- 1
      occ <- base::cbind(paleolng = occ$paleolng, paleolat = occ$paleolat, occnoloc)
    }
    #return matrix as data frame
    return(base::as.data.frame(occ))
  }
  
}



#####################mapdiv####################

#' mapdiv
#' 
#' Calculates the Shannon diversity per cell or locality 
#' (taking into account relative abundances of all the fossil records 
#' whithin the cell) and creates a plot of the map with a RasterLayer of the diversity.
#' 
#' @usage mapdiv  (shape, data, unity, rank = "genus", res = 1, fun = mean,
#'                            colland = "#66666660", colsea = "#00509010", 
#'                            col.grid = mycols(100), do.plot = TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame. A map which can be created with getmap.
#' @param data data.frame. Fossil occurrences data.
#' @param unity character. Either "fossilsite" or "cell".
#' @param rank character. Taxnomic rank. By default rank = "genus".
#' @param res numeric. Defining the spatial resolution. Default res = 1. 
#' @param fun function or character. To determine what values to assign to cells that are covered by multiple spatial features. 
#' You can use functions such as min, max, or mean, or the character value: 'count'. 
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param col.grid character. Defines the color of the raster.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main = "my own title" or main.col = "red".
#' @return RasterLayer
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #get GPlates map
#' shape <- getmap(interval = "Quaternary", model = "GPlates", do.plot = FALSE)
#' #get data from the paleobiology DB
#' data <- getdata (base_name = "Canis", interval = "Quaternary")
#' #calculate diversity per locality
#' div_site <- mapdiv (shape, data, unity = "fossilsite", rank ="genus", res = 1)
#' #calculate diversity per cell
#' div_cell <- mapdiv (shape, data, unity = "cell", rank = "genus", res = 1)
#' 
#' #save plot as pdf file
#' pdf("mapdiv-GPlates_Quaternary-Canis.pdf")
#' mapdiv (shape, data, unity = "fossilsite", rank = "genus", res = 1)
#' dev.off()
#' 
#' #save plot as tiff image
#' tiff("mapdiv-GPlates_Quaternary-Canis.tiff", 
#'       height = 10.5, width = 19, units = "cm", res = 300)
#' mapdiv (shape, data, unity = "fossilsite", rank = "genus", res = 1)
#' dev.off()
#' 
#' }

mapdiv <- function(shape, data, unity, rank = "genus", res = 1, fun = mean,
                   colland = "#66666660", colsea = "#00509010", col.grid = mycols(100), 
                   do.plot = TRUE, ...) {
  #check user input data
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  #creating a raster in size of the shape file
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
 
  if(unity == "cell"){
    occ_df_cell <- pasta::spsite(data, unity = unity, res = res, rank = rank)
    
    #remove lat and lng from data frame
    drops <- c("paleolat", "paleolng")
    rawocc <- occ_df_cell[ , !(base::names(occ_df_cell) %in% drops)]
    rawocc <- base::data.frame(base::rep(0, base::length(occ_df_cell$paleolat)), rawocc)
    #calculate the diversity and save diversity, lat and lng in new data frame
    div <- vegan::diversity(rawocc)
    divlatlng <- base::data.frame(occ_df_cell$paleolat, 
                                  occ_df_cell$paleolng, div = div)
    base::colnames(divlatlng) <- c("paleolat", "paleolng", "div")
    #create a raster with the diversity
    divraster <- raster::rasterize(divlatlng[ , c("paleolng", "paleolat")], ras, 
                                   field = divlatlng$div, fun = fun)
    #declare all cells where diversity=0 as NA (otherwise everything  is colored)
    divraster[divraster == 0] <- NA
    #set the ones with a fossil occurrence but no diversity to 0
    if(base::length(occ_df_cell) > 3){
      occurrences <- base::rowSums(occ_df_cell[ , 3:base::length(occ_df_cell)])
    }else{
      occurrences <- occ_df_cell[ , 3]
    }
    occurrences[occurrences > 0] <- 1
    div.df <- base::data.frame(paleolat = occ_df_cell$paleolat, paleolng = occ_df_cell$paleolng, genus = occurrences)
    div_cell <- pasta::spsite(div.df, unity = "cell", res = res, rank = "genus")
    if(base::length(div_cell) > 3){
      div_cell$`0` <- NULL
    }
    #sort div_cell as r@data@values is sorted
    xyras <- base::data.frame(raster::xyFromCell(divraster, 1:base::length(divraster@data@values)))
    div_cell <- div_cell[base::order(base::match(
      base::paste(div_cell[ , 1], div_cell[ , 2]),
      base::paste(xyras[ , 1], xyras[ , 2]))), ]
    for(i in 1:base::length(div_cell[ , 3])){
      if(div_cell[i, 3] > 0 && base::is.na(divraster@data@values[i])){
        divraster@data@values[i] <- 0
      }
    }
  }
  if(unity == "fossilsite"){

    occ_df <- pasta::spsite(data, unity = unity, res = res, rank = rank)

    drops <- c("paleolat", "paleolng")
    rawocc <- base::data.frame(occ_df[ , !(base::names(occ_df) %in% drops)])
    #calculate the diversity from the fossil occurrences
    div <- c()
    if(nrow(rawocc > 1)){
      for(i in 1:nrow(rawocc)){
        div <- c(div, vegan::diversity(rawocc[i, ]))
      }
    }else{
      div <- vegan::diversity(rawocc)
    }
    #save paleolat, paleolng and diversity in a data frame and define column names
    divlatlng <- base::data.frame(occ_df$paleolat, 
                                  occ_df$paleolng, div = div)
    base::colnames(divlatlng) <- c("paleolat", "paleolng", "div")
    
    #getting the raster of the diversity, using the function as defined in the input parameter
    divraster<-raster::rasterize(divlatlng[ , c("paleolng", "paleolat")], ras, 
                                 field = divlatlng$div, fun = fun)
    
    #set all values that are 0 to NA (otherwise raster would fill the whole map)
    divraster[divraster == 0] <- NA
    #set the ones with a fossil occurrence but no diversity to 0
    div.df <-base::cbind(divlatlng, base::rep(1, base::length(divlatlng[ , 1])))
    base::colnames(div.df)[4] <- "genus"
    div_cell <- pasta::spsite(div.df, unity = "cell", res = res, rank = "genus")
    #sort div_cell as r@data@values is sorted
    xyras <- base::data.frame(raster::xyFromCell(divraster, 1:base::length(divraster@data@values)))
    div_cell <- div_cell[base::order(base::match(
      paste(div_cell[ , 1], div_cell[ , 2]),
      paste(xyras[ , 1], xyras[ , 2]))), ]
    for(i in 1:base::length(div_cell[ , 3])){
      if(div_cell[i, 3] > 0 && base::is.na(divraster@data@values[i])){
        divraster@data@values[i] <- 0
      }
    }
  }
  
  
  #default graphical parameter list
  graphparams.def <- base::list(x = shape, col = "white", border = FALSE
                                , xlim = c(-180, 180), ylim = c(-90, 90)
                                , xlab = "Longitude", ylab = "Latitude"
                                , xaxs = "i", yaxs = "i", col.grid = col.grid)
  #user defined grapical parameter
  graphparams.user <- base::list(...)
  names_graphparams.user <- base::as.vector(base::names(graphparams.user))
  names_graphparams.def <- base::as.vector(base::names(graphparams.def))
  #only keep user defined graphical parameters if defined before as default
  for( i in names_graphparams.user){
    if(i %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == i)] 
  }
  #create default & user graphical parameters list
  graphparams <- c(graphparams.def, graphparams.user)
  #save color of the raster and remove it from parameter list
  gridcol <- graphparams$col.grid
  graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
  #if do.plot is true create a plot
  if(do.plot){
    #save current margin settings and define margin as needed
    def.mar <- graphics::par("mar")
    graphics::par(mar = c(1.5, 1.5, 2, 4))
    #plot with parameter list
    base::do.call(raster::plot, graphparams)
    #add a rectangle defining the sea
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                   border = FALSE)
    #add the landmasses
    raster::plot(shape, col = colland, border = FALSE, add = T)
    #add x-axis and x-axis labels
    graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    #add y-axis and y-axis labels
    graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
    graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
    #get metadata from SpatialPolygonsDataFrame
    shape.info <- .getShapeInfo(shape)
    #add name, model, age at the top right of the plot
    graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = shape.info[1], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
    graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = shape.info[2], col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.5)
    graphics::axis(side = 3, pos = 81, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
    #add the raster without legend
    raster::plot(divraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE)
    #allow to expand the plot
    graphics::par(xpd = TRUE)
    graphics::par(bty = "n")
    #add legend outside the plot
    raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                  axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), legend.args = list(text = "diversity", line = 1, side = 3, adj = 0.25, cex = 0.6, col = gridcol[length(gridcol) / 2]))
    raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                  axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = gridcol[length(gridcol) / 2]))
    graphics::par(bty = "o")
    #restore prior margin settings
    graphics::par(mar = def.mar)
  }
  #return the raster
  return(divraster)
}


###################################latdivgrad###################
#' latdivgrad
#' 
#' Calculates the latitudinal diversity of taxa (species, genera, families, orders) and creates a plot of the continental masses with the fossil occurrences and the latitudinal diversity.
#' 
#' @usage latdivgrad (shape, data, method, rank = "genus", res = 1,
#'                    colland ="#66666680", colsea = "#00509010", 
#'                    colpoints = "#65432190", 
#'                    rich.col = "#654321", pch = 21, do.plot = TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame. A map, which can be created with getmap.
#' @param data data.frame. Fossil occurrence data, which can be created with
#'  getdata(interval, base_name).
#' @param method character. Defining the method of diversity measure, method = "shannon" or method = "richness".
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus".
#' @param res numeric. Defining the spatial resolution. Default res = 1. 
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param colpoints character. Defines the color of the occurrence-points. By default colpoints = "#65432190". 
#' @param rich.col character. Defines the color of the richness curve. By default rich.col = "#654321".
#' @param pch numeric. Point symbol for plotting the occurences. By default pch = 21.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, 
#' such as main = "my own title" or main.col = "red".
#' @return data.frame 
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #get the map and the fossil data
#' shape <- getmap(interval = "Quaternary", model = "GPlates", do.plot = FALSE)
#' data <- getdata (base_name = "Canis", interval = "Quaternary")
#' #calculate the shannon diversity
#' shannon <- latdivgrad (shape, data, method = "shannon", rank = "species", res = 1)
#' #calculate the richness
#' rich <- latdivgrad (shape, data, method = "richness", rank = "genus", res = 1)
#' 
#' #save plot as pdf file
#' pdf("latdivgrad-GPlates_Quaternary-Canis.pdf")
#' latdivgrad (shape, data, method = "richness", rank = "genus", res = 1)
#' dev.off()
#' 
#' #save plot as tiff image
#' tiff("latdivgrad-GPlates_Quaternary-Canis.tiff", 
#'       height = 9, width = 17.5, units = "cm", res = 300)
#' latdivgrad (shape, data, method = "richness", rank = "genus", res = 1)
#' dev.off()
#' 
#'}


latdivgrad <- function(shape, data, method, rank = "genus",
                       res = 1, 
                       colland = "#66666680", colsea = "#00509010", 
                       colpoints = "#65432190",
                       rich.col = "#654321", pch = 21, do.plot = TRUE, ...) {
  #check the users input data
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank."))
  }
  if(!.checkDataRank(data,rank)){
    if(rank=="species"){
      stop(base::paste("There is no column matched_name in the data frame.", sep=""))
    }else{
      stop(base::paste("There is no column ", rank, " in the data frame.", sep=""))
    }
  }
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  #define default graphical parameters
  graphparams.def <- base::list(x = shape, col = "white", border = FALSE
                                , xlim = c(-180,180), ylim = c(-90,90)
                                , xaxs = "i", yaxs = "i")
  #get users input parameters
  graphparams.user <- base::list(...)
  names_graphparams.user <- base::as.vector(base::names(graphparams.user))
  names_graphparams.def <- base::as.vector(base::names(graphparams.def))
  #if user defines sth. with default value only keep users value
  for( i in names_graphparams.user){
    if(i %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == i)] 
  }
  #create argument list with default and user values
  graphparams <- c(graphparams.def, graphparams.user)
  
  if(method == "richness"){
    #filter the data for the taxonomic rank
    rankdata <-.rfilter(data, rank)
    #setting min and max value for lat
    #creating empty richness data frame
    richn <- NULL
    #going through lats
    for(lat in base::seq(-90, 90-res, res)) {
      if(lat == -90){
        latocc <- base::subset(rankdata, rankdata$paleolat >= lat)
        latocc <- base::subset(latocc, latocc$paleolat <= (lat+res))
        latocc <- base::unique(latocc[ ,3])
      }else{
        latocc <- base::subset(rankdata, rankdata$paleolat > lat)
        latocc <- base::subset(latocc, latocc$paleolat <= (lat + res))
        latocc <- base::unique(latocc[ ,3])
      }
      #count and save the number of different taxa at each latitude
      richn <- c(richn, base::length(latocc))
    }
    #define the magnitude of the richness graph
    magn <- 140/base::max(richn)
    #combine min,max lat and richness in a data frame
    latdiv <- base::data.frame(paleolat = c(base::seq(-90 + (res / 2), 90 - (res / 2), res)), div = richn)
  }
  if(method == "shannon"){
    rankdata <- pasta::spsite(data, unity = "fossilsite", res = res, rank = rank)
    
    div <- c()
    for(lat in base::seq(-90, 90 - res, res)){
      if(lat == -90){
        latocc <- base::subset(rankdata, rankdata$paleolat >= lat)
      }else{
        latocc <- base::subset(rankdata, rankdata$paleolat > lat)
      }
      latocc <- base::subset(latocc, latocc$paleolat <= lat + res)
      drops <- c("paleolat","paleolng")
      rawocc <- base::data.frame(latocc[, !(base::names(latocc) %in% drops)])
      sum_occ <- base::colSums(rawocc)
      div <- c(div , vegan::diversity(sum_occ))
    }
    #calculate the magnitude by taking the max of the diversity
    magn <- 140/base::max(div)
    #create data frame with paleolat and diversity
    latdiv <- base::data.frame(paleolat = c(base::seq(-90 + (res / 2), 90 - (res / 2), res)), div = div)
  }
  
  #calculate the center of each range
  centros<- (base::seq(-90, 90 - res, res) + (base::seq(-90, 90 - res, res) + res)) / 2
  #save the diversity, x and y value for plotting
  rich<- 180 + (latdiv$div*magn)
  yrich<- c(180, rich, 180)
  xrich<- c(-90, centros, 90)
  #if do.plot is true create a plot
  if(do.plot){
    #save current margin settings and define needed margin values 
    def.mar <- graphics::par("mar")
    graphics::par(mar = c(1.5,1.5,2,10), xpd = NA)
    #create a plot with the parameter list
    do.call(raster::plot, graphparams)
    #add a rectangle as the sea
    graphics::rect(xleft = -180, xright = 180, 
                   ybottom = -90, ytop = 90, col = colsea, 
                   border = FALSE)
    #add the landmasses to the plot
    raster::plot(shape, col = colland, border = FALSE, add = T)
    #add the fossil occurrences to the plot
    graphics::points(rankdata$paleolng, rankdata$paleolat, 
                     pch = pch, col = NA, bg = colpoints)
    #add x-axis and x-axis labels
    graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
    graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
    #add y-axis and y-axis labels
    graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6, las = 1)
    graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
    #get metadata from the SpatialPolygonsDataFrame
    shape.info <- .getShapeInfo(shape)
    #add name, model and age at the top right of the plot
    graphics::axis(side = 3, pos = 94, lwd = 0, at = 135, labels = shape.info[1], col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 1)
    graphics::axis(side = 3, pos = 86, lwd = 0, at = 135, labels = shape.info[2], col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.5)
    graphics::axis(side = 3, pos = 78, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.7)
    if(!is.nan(base::min(yrich))){
      #add the richness curve at the right side of the plot
      graphics::polygon (yrich, xrich, col = rich.col, border = F, xpd = T)
      #get the parameters for the richness axis
      ax_seq <- base::seq(base::min(yrich), base::max(yrich), ((base::max(yrich) - base::min(yrich)) / 2))
      ax_lab <- ax_seq - 180
      ax_lab <- base::round(ax_lab / magn, 2)
      #add the richness axes
      graphics::axis(side = 3, pos = 90, lwd = 1, xpd = TRUE, at = ax_seq, labels = FALSE , col.ticks = rich.col ,col.axis = rich.col , col = rich.col , cex.axis = 0.6, tck = -0.01)
      graphics::axis(side = 3, pos = 80, lwd = 0, xpd = TRUE, at = ax_seq, labels = ax_lab , col.ticks = rich.col ,col.axis = rich.col , col = rich.col , cex.axis = 0.6, tck = -0.01)
      graphics::axis(side = 3, pos = 90, lwd = 0, xpd = TRUE, at = ax_seq[round(length(ax_seq) / 2)], labels = method, col.ticks = rich.col, col.axis = rich.col, col = rich.col, cex.axis = 0.8, tck = -0.01, cex.lab = 0.8)  
    }
    
    #restore the prior margin settings
    graphics::par(mar = def.mar)
  }
  #return latitudinal richness
  latdiv <- latdiv[base::length(latdiv$paleolat):1, ]
  return(latdiv)
}


###########################checkage#############################
#' checkage
#' 
#' Creates a data.frame with all maps that contain the requested age.
#' 
#' @usage checkage(age = "all")
#' 
#' @param age character. Defining the age of interest in mya. By default age = "all", which gives the complete list.
#' @return data.frame
#' @export
#' @examples
#' \dontrun{
#' 
#' library(pasta)
#' 
#' #get a data.frame with all maps including the age 112 mya
#' checkage(age = "112")
#' 
#' #get complete data.frame with all available ages, from all models
#' checkage()
#' 
#'}

checkage <- function(age = "all"){
  #define data frame df_maps
  df_maps <- NULL
  #load df_maps from the package
  utils::data(df_maps,envir = base::environment())
  #if age is a defined age get all maps including this age
  if(age != "all"){
    maps <- df_maps[base::as.numeric(df_maps$fromage) >= base::as.numeric(age),]
    maps <- maps[base::as.numeric(maps$toage) <= base::as.numeric(age),]
  }else{
    #else if age is "all" return complete list
    maps <- df_maps
  }
  #return the list with the maps
  maps
}



##########################GPlates API functions#############################

###########################modelmap#############################

#' modelmap
#' 
#' Gets a map of a specific age from a model specified by the user.
#' Available models and ages at https://github.com/GPlates/gplates_web_service_doc/wiki/Reconstruction-Models .
#' 
#' @usage modelmap(ma, model = 'SETON2012', show.plates = FALSE, colland, colsea, do.plot, ...)
#' 
#' @param ma numeric. Age in Ma.
#' @param model character. Defining the model the map should be created with. 'SETON2012' (default), 
#' 'MULLER2016', 'GOLONKA', 'PALEOMAP' or 'MATTHEWS2016'.
#' @param show.plates boolean. Defines if the user wants to get the continental plate borders. By default. show.plates = FALSE.
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return SpatialPolygonsDataFrame
#' @export
#' @examples
#' \dontrun{
#' 
#' #with continental plates
#' map <- modelmap(100, model = 'SETON2012', show.plates = T)
#' coastlines <- map[[1]]
#' plates <- map[[2]]
#' 
#' #without continental plates
#' coastlines <- modelmap(100, model = 'SETON2012')
#' 
#'#save map as tiff image
#' tiff("getmap-GPlates_Cretaceous.tiff", 
#'       height = 10, width = 17, units = "cm", res = 300)
#' modelmap(100, model = 'SETON2012', show.plates = T)
#' dev.off()
#' 
#'}

modelmap <- function(ma, model = 'SETON2012', show.plates = FALSE, colland = "#66666660", 
                     colsea = "#00509010", 
                     do.plot = TRUE, ...) {
  url <- paste0("http://gws.gplates.org/reconstruct/coastlines/?time=",ma,"&model=",model)
  err <- FALSE
  shape <- tryCatch(
    {
      rgdal::readOGR(url)
    }, error = function(e) {
      err <- TRUE
      message(paste0("There is no map for ", ma, " mya in ", model, " model."))
      stop()
    }
  )
  
  errplate <- FALSE
  if(show.plates){
    #no plate bounds for paleomap
    plateurl <- paste0("http://gws.gplates.org/topology/plate_boundaries/?time=", ma, "&model=", model)
    platebounds <- tryCatch(
      {
        rgdal::readOGR(plateurl)
      }, error = function(e){
        errplate <- TRUE
        message(paste0("No Plate Boundaries available for ", ma, " mya in ", model, "model."))
        stop()
      }
    )
  }
  
  if(!err){
    #getting final parameter list for plot
    #default parameter list for plotting
    graphparams.def <- base::list(x = shape, col = "white", border = FALSE
                                  , xlim = c(-180, 180), ylim = c(-90, 90)
                                  , xaxs = "i", yaxs = "i")
    #list of user defined graphical parameter
    graphparams.user <- base::list(...)
    names_graphparams.user <- base::as.vector(base::names(graphparams.user))
    names_graphparams.def <- base::as.vector(base::names(graphparams.def))
    #remove default parameter if user specifies a different value
    for( param in names_graphparams.user){
      if(param %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == param)] 
    }
    #complete new list of plotting parameters, including default and user specified ones
    graphparams <- c(graphparams.def, graphparams.user)
    # if user does not set plot=FALSE plot the shape file
    if (do.plot) {
      #define the size of the margin of the plot and save the former definition
      def.mar <- graphics::par("mar")
      graphics::par(mar = c(2, 2, 2, 2))
      #do a first plot with the graphical parameters set by the user
      base::do.call(sp::plot, graphparams)
      if(!errplate && show.plates){
        sp::plot(platebounds, add = T, col = "#66666680")
      }
      #draw a rectangle showing the sea
      graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
                     ytop = 90, col = colsea, 
                     border = FALSE)
      #add x-axis and x-axis label
      graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
      graphics::axis(side = 1, pos = -89, lwd = 0, at = 0 , labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
      #add y-axis and y-axis label
      graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
      graphics::axis(side = 2, pos = -178, lwd = 0, at = 0 , labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
      #get metadata from the SpatialPolygonsDataFrame
      # shape.info <- .getShapeInfo(shape)
      #add name, model and age info top right of the plot
      # graphics::axis(side = 3, pos=97, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
      graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
      graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(ma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
      #add the landmasses to the plot
      sp::plot(shape, col = colland, border = FALSE, add = TRUE)
      #restore the former graphical mar parameters
      graphics::par(mar = def.mar)
    }
    # return the shape file
    if(!errplate && show.plates){
      return(list(shape, platebounds))
    }else{
      return(shape)
    }
  }
}

################madata##############################

#' madata
#' 
#' Uses the paleobioDB R package to extract data used in for other functions of this package
#' from the Paleobiology Database.
#'  
#' @usage madata(minma, maxma, base_name, limit="all", paleocoords=F, 
#' recontime=NULL, model='SETON2012')
#' 
#' @param minma numeric. Minimal age in Ma.
#' @param maxma numeric. Maximal age in Ma.
#' @param base_name character. The name of the taxon of interest. (e.g. "Canis" or "reptilia")
#' @param limit integer. Defining the max. number of occurrences to be downloaded from paleobioDB. By default limit="all"
#' @param paleocoords boolean. Defines if the user wants to get the paleocoordinates.
#' @param recontime numeric. Time of reconstruction in ma. If recontime is null the recnstruction time will be 
#' the midpoint of the interval the fossil occurred.
#' @param model character. Defining the model the map should be created with. 'SETON2012' (default), 
#' 'MULLER2016', 'GOLONKA', 'PALEOMAP' or 'MATTHEWS2016'.
#' @return Returns a data frame with fossil occurrences
#' @export 
#' @examples 
#' \dontrun{
#' 
#' #get data without paleocoordinates
#' occ <- madata(minma=0, maxma=10,base_name="Canis", limit=100)
#' 
#' #get data with paleocoordinates taking the midpoint of the occurrence time
#' occma <- madata(minma=0, maxma=10,base_name="Canis", 
#'                     limit=100, paleocoords=TRUE, model='SETON2012')
#' 
#' #get data with paleocoordinates for a chosen time in ma
#' occmatime <- madata(minma=0, maxma=10,base_name="Canis", limit=100, 
#'                          paleocoords=TRUE, recontime=5,  model='SETON2012')
#'                     
#'                     
#'}


madata <- function(minma, maxma, base_name, limit="all", paleocoords=F, recontime=NULL, model='SETON2012') {
  #create an empty data frame for storing the fossil data
  occ <- base::data.frame()
  #try to get data from the Paleobiology Database with the given parameters, using R-package paleobioDB
  try(occ <- base::data.frame(paleobioDB::pbdb_occurrences(base_name=base_name, min_ma=minma, max_ma=maxma, 
                                                           show=c("coords", "phylo"), 
                                                           vocab="pbdb", limit=limit))
      , silent=TRUE)
  #if there were results, save them in a data frame called 'data' and remove entries which do not have a paleolatitude or paleolongitude
  
  if (base::nrow(occ) != 0) {
    if(paleocoords){
      if(is.null(recontime)){
        occ <- paleocoords(occ, NULL)
      }else{
        occ <- paleocoords(occ, recontime)
      }
      
    }
    
    return(occ)
  } else { #catching error when there are no occurences for the request
    stop("There is no data that matches your query on the paleobioDB. 
         Check if the spelling, temporal intervals, etc. are correct")
  }
}

################paleocoords##############################

#' paleocoords
#' 
#' Descr.
#'  
#' @usage paleocoords(occ, recontime=NULL, model='SETON2012')
#' 
#' @param occ data.frame. Fossil data, which should at least have the columns lng and lat (if recontime=NULL also early_age and late_age).
#' @param recontime numeric. Time of reconstruction in ma. If recontime is null the recnstruction time will be 
#' the midpoint of the interval the fossil occurred.
#' @param model character. Defining the model the map should be created with. 'SETON2012' (default), 
#' 'MULLER2016', 'GOLONKA', 'PALEOMAP' or 'MATTHEWS2016'.
#' @return data.frame
#' @export 
#' @examples 
#' \dontrun{
#' 
#' occ <- madata(minma=0, maxma=10,base_name="Canis", limit=100)
#' 
#' #reconstruct paleocoordinates with midpoint of appearance time
#' occ_ma <- paleoocoords(occ, recontime=NULL, model='SETON2012')
#' 
#' #reconstruct paleocoordinates with specific time
#' occ_matime <- paleoocoords(occ, recontime=5, model='SETON2012')
#' 
#'                     
#'}


paleocoords <- function(occ, recontime=NULL, model='SETON2012') {
  if(!.checkRecon(occ, recontime)){
    if(is.null(recontime)){
      stop("Your data.frame needs to have columns called lng, lat, early_age and late_age.")
    }else{
      stop("Your data.frame needs to have columns called lng and lat.")
    }
    
  }
  paleolng <- c()
  paleolat <- c()
  ma <-c()
  if(is.null(recontime)){
    ma <- (occ$early_age-occ$late_age)/2
    occ <- cbind(occ, ma)
    uma <- unique(ma)
    
    for( i in 1:length(uma)){
      part <- occ[occ$ma==uma[i],]
      
      pts <- ""
      for( j in 1:length(part$ma)){
        pts <- paste0(pts,",", part$lng[j], ",", part$lat[j])
      }
      
      pts <- substring(pts, 2)
      
      url <- paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=",pts,"&time=",uma[i], "&model=",model)
      
      paleopts <- rjson::fromJSON(file=url)
      
      for (k in 1:length(paleopts$coordinates)){
        paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
        paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
      }
    }
  }else{
    ma <- rep(recontime, length(occ$lng))
    occ <- cbind(occ, ma)
    
    
    pts <- ""
    for( j in 1:length(occ$lng)){
      pts <- paste0(pts,",", occ$lng[j], ",", occ$lat[j])
    }
    
    pts <- substring(pts, 2)
    
    url <- paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=",pts,"&time=",recontime, "&model=",model)
    
    paleopts <- rjson::fromJSON(file=url)
    
    for (k in 1:length(paleopts$coordinates)){
      paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
      paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
    }
  }
  
  
  occ <- cbind(occ, paleolng, paleolat)
  
  return(occ)
}