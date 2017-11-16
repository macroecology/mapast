###########################pm_getmap#############################

#' pm_getmap
#' 
#' Generates a shapefile with the paleomap of the chosen 
#' time interval (e.g. "Cretaceous") and a plot.
#' 
#' @usage pm_getmap(interval, model, colland = "#66666660"
#'                           , colsea = "#00509010", do.plot = TRUE, ...)
#' 
#' @param interval character. Temporal paleoeographical interval of interest. (e.g. "Cretaceous" for GPlates or "112.0" for Smith)
#' @param model character. Defining the model the map was created with. "GPlates", "Smith" or "Golonka"
#' @param colland define the color of the land masses. By default colland = "#66666660"
#' @param colsea define the color of the sea. By default colsea = "#00509010"
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return Returns a SpatialPolygonsDataFrame and creates a plot (if do.plot=TRUE).
#' @export
#' @examples
#' \dontrun{
#' 
#' #GPlates
#' pm_getmap(interval="Cretaceous", model="GPlates")
#' #save map as pdf file
#' pdf("pm_getmap-GPlates_Cretaceous.pdf")
#' pm_getmap(interval="Cretaceous", model="GPlates")
#' dev.off()
#'#save map as tiff image
#' tiff("pm_getmap-GPlates_Cretaceous.tiff", 
#'       height = 10, width = 17, units = 'cm', res=300)
#' pm_getmap(interval="Cretaceous", model="GPlates")
#' dev.off()
#' 
#' 
#' #Smith
#' pm_getmap(interval="112", model="Smith")
#' 
#' 
#' #Golonka
#' pm_getmap(interval="123", model="Golonka")
#' 
#' #for checking which maps are available including a specific age look at ??pm_checkAge
#' 
#'}

pm_getmap <- function(interval, model, colland = "#66666660", 
                      colsea = "#00509010", 
                      do.plot = TRUE, ...) {
  
  #get shape file from external database
  if(model=="GPlates"){
    
    shape <- utils::data(list=base::paste(model,interval, sep="_"), package="paleogeoDB"
                         ,envir = base::environment())
    base::assign("shape",base::get(interval))
    
  }else{
    
    shape <- utils::data(list=base::paste(model,interval, sep="_"), package="paleogeoDB"
                         ,envir = base::environment())
    base::assign("shape",base::get(base::paste(model,interval, sep="_")))
  }
  
  #getting final parameter list for plot
  int_args <- base::list(x=shape, col = "white", border = FALSE
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xaxs="i", yaxs="i")
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
 
  
  arglist <- c(int_args, params)
  # if user does not set plot=FALSE plot the shape file
  if (do.plot) {
    def.mar <- graphics::par("mar")
    def.oma <- graphics::par("oma")
    graphics::par(oma=c(0,0,0,0))
    graphics::par(mar=c(2,2,2,2))
    base::do.call(sp::plot, arglist)
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)

    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=97, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=89, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=81, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)

    sp::plot(shape, col = colland, border = FALSE, add = TRUE)
    
    graphics::par(mar=def.mar)
    graphics::par(oma=def.oma)
    
  }

  # return the shape file
  return(shape)
}



################pm_getdata##############################

#' pm_getdata
#' 
#' Uses the paleobioDB R package to extract data used in for other functions of this package
#' from the Paleobiology Database.
#'  
#' @usage pm_getdata(interval, base_name, limit="all")
#' 
#' @param interval character. Temporal paleoeographical interval of interest. (e.g. "Cretaceous" for GPlates or "112.0" for Smith)
#' @param base_name character. The name of the taxon of interest. (e.g. "Canis" or "reptilia")
#' @param limit integer. Defining the max. number of occurrences to be downloaded from paleobioDB. By default limit="all"
#' @return Returns a data frame with fossil occurrences
#' @export 
#' @examples 
#' \dontrun{
#' 
#' pm_getdata(interval="Quaternary",base_name="Canis")
#' 
#' #if you want to use FROMAGE and TOAGE to define the interval you can use the paleobioDB package
#' myocc <- base::data.frame(paleobioDB::pbdb_occurrences(base_name="mammalia", 
#'                     min_ma=0, max_ma=2.58, 
#'                     show=c("paleoloc", "phylo"), 
#'                     vocab="pbdb", limit=100))
#'                     
#'}


pm_getdata <- function(interval, base_name, limit="all") {
  # get data from paleobioDB
  # save data from paleobiodb as data frame
  occ <- base::data.frame()
  try(occ <- base::data.frame(paleobioDB::pbdb_occurrences(base_name=base_name, interval=interval, 
                                     show=c("paleoloc", "phylo"), 
                                     vocab="pbdb", limit=limit))
                        , silent=TRUE)
  if (base::nrow(occ) != 0) {
    data <- .checkPbdb(occ)
    data <- data[!is.na(data$paleolat),]
    data <- data[!is.na(data$paleolng),]
    #return data frame
    return(data)
  } else { #catching error when there are no occurences for the request
      
      stop("There is no data that matches your query on the paleobioDB. 
          Check if the spelling, temporal intervals, etc. are correct")
    }
}



####################pm_plot#################################

#' pm_plot
#' 
#' Plots your query from the paleobioDB onto the map of the selected time interval.
#' 
#' 
#' @usage pm_plot(interval, model, data, colland = "#66666660",
#'                 colsea = "#00509010",colpoints = "#65432190", 
#'                 pch =16, cex = 1, ...)
#' 
#' @param interval character. Temporal paleoeographical interval of interest. (e.g. "Cretaceous" for GPlates or "112.0" for Smith)
#' @param model character. Defining the model the map was created with. "GPlates", "Smith" or "Golonka"
#' @param data data.frame with fossil occurrences. Can be created with pm_getdata(interval, base_name)
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param colpoints define the color of the occurrence-points. By default colpoints = "#65432190".
#' @param pch point symbol for plotting the occurences. By default pch=16 (filled circle).
#' @param cex numeric. size of the points. By default cex=1.
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return Creates a plot with the configuration of the continents at the selected 
#' time interval and the fossil occurrences.
#' @export 
#' @examples 
#' \dontrun{
#' 
#' data  <-  pm_getdata (interval="Cretaceous", base_name="Mammalia")
#' pm_plot(interval="Cretaceous", model="GPlates", data)
#' 
#' #save as pdf file
#' pdf("pm_plot-GPlates_Cretaceous-Mammalia.pdf")
#' pm_plot(interval="Cretaceous", model="GPlates", data)
#' dev.off()
#' #save as tiff image
#' tiff("pm_plot-GPlates_Cretaceous-Mammalia.tiff", 
#'       height = 10.5, width = 17, units = 'cm', res=300)
#' pm_plot(interval="Cretaceous", model="GPlates", data)
#' dev.off()
#' 
#'}

pm_plot <- function(interval, model, data,
                    colland = "#66666660",
                    colsea = "#00509010", 
                    colpoints = "#65432190", 
                    pch =16, cex = 1, ...) {
  
  #check user input
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }

  int_args <- base::list(x=shape, col = "white", border = FALSE
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xaxs="i", yaxs="i")
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
  arglist <- c(int_args, params)
  
  #getting the shape file for the map and the data for plotting it on the map
  shape <- paleoMap::pm_getmap(interval = interval, model = model, do.plot = FALSE)
  #plotting the map and the data
  if (base::class(data) == "data.frame") {
    def.mar <- graphics::par("mar")
    def.oma <- graphics::par("oma")
    graphics::par(oma=c(0,0,0,0))
    graphics::par(mar=c(1.5,1.5,2,1.5))
    #defines size and axes of the plot
    base::do.call(sp::plot, arglist)
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)
    sp::plot(shape, col = colland, border = FALSE, add = TRUE)
    
    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=97, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=89, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=81, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)
    
    graphics::points(data$paleolng, 
           data$paleolat, 
           pch = pch, col = colpoints, 
           cex = cex)
    
    graphics::par(mar=def.mar)
    graphics::par(oma=def.oma)
  }
}

#####################pm_occraster##############################

#' pm_occraster
#' 
#' Creates a raster and a plot of the fossil occurences by taxonomic rank per cell 
#' (a proxy for the sampling effort).
#' 
#' @usage pm_occraster(shape, data, rank = "genus", res = 10,
#'                     colland = "#66666660", colsea = "#00509010", col.grid=mycols(100), 
#'                     do.plot=TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum".
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param col.grid define the color of the raster.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @return Returns a RasterLayer with the number of occurrences.
#' @export 
#' @examples 
#' \dontrun{
#' 
#' shape <- pm_getmap(interval="Quaternary", model="GPlates", do.plot = FALSE)
#' data <- pm_getdata(base_name="Canis", interval="Quaternary")
#' pm_occraster(shape, data)
#' 
#' #save as pdf file
#' pdf("pm_occraster-GPlates_Quaternary-Canis.pdf")
#' occras <- pm_occraster(shape, data)
#' dev.off()
#' save as tiff image
#' tiff("pm_occraster-GPlates_Quaternary-Canis.tiff", 
#'       height = 10.5, width = 19, units = 'cm', res=300)
#' pm_occraster(shape, data)
#' dev.off()
#' 
#'}

pm_occraster <- function(shape, data, 
                         rank = "genus", 
                         res = 10, 
                         colland = "#66666660",
                         colsea = "#00509010", col.grid = mycols(100), do.plot=TRUE, ...) {
  
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep=""))
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
  
  raster <- NULL
  #filter data for rank
  fdata <- .rfilter(data, rank)
  #creating a raster in the size of the shape
  ras <- raster::raster(shape, res = res)
  #raster of the occurences (sampling effort)
  r <- raster::rasterize(fdata[, c("paleolng","paleolat")], ras , fun = "count")
  int_args <- base::list(x=r, axes=F, box=F, col="white", col.grid=col.grid, legend=FALSE
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xaxs="i", yaxs="i", bty='L')
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
  arglist <- c(int_args, params)
  mycol <- arglist$col.grid
  arglist$col <- arglist$col.grid
  arglist <- arglist[- base::which(base::names(arglist)=="col.grid")]
  if(do.plot){
    def.mar <- graphics::par("mar")
    def.oma <- graphics::par("oma")
    graphics::par(oma=c(0,0,0,2))
    graphics::par(mar=c(1.5,1.5,2,3))
    #plotting the map and the raster on the map
    raster::plot (shape, col="white", border=FALSE
                  , xlim=c(-180,180), ylim=c(-90,90)
                  , xaxs="i", yaxs="i", bty='L')
    graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                   border=FALSE)
    raster::plot (shape, col=colland, border=FALSE, add=T, bty='L')
    
    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=97, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=89, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=81, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)
    
    raster::plot (r, add=T,axes=F, box=F, col=mycol, legend=FALSE, bty='L')
    graphics::par(xpd = TRUE)
    graphics::par(bty= "n")
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(tck=-0.2, col=NA, col.ticks="darkgrey", col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=NA), legend.args=list(text='occurrences', line=1, side=3, adj=0.25, cex=0.6, col=col.grid[length(col.grid)/2]))
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(line=-0.5, col=NA, col.ticks=NA, col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=col.grid[length(col.grid)/2]))
    graphics::par(bty= "o")
    
    graphics::par(mar=def.mar)
    graphics::par(oma=def.oma)
  }
 
  #returning the raster
  return(r)
}

#####################pm_richraster####################

#' pm_richraster
#' 
#' Creates a raster of species richness
#' and makes a plot of the map and raster.
#' 
#' @usage pm_richraster(shape, data, rank, res = 10, 
#'                      colland = "#66666660", colsea = "#00509010", col.grid= mycols(100), 
#'                      do.plot=TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum".
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param col.grid define the color of the raster.
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @return Creates a plot with map of the time intervall, the fossil occurences and the 
#' RasterLayer. And returns the RasterLayer itself
#' @export 
#' @examples 
#' \dontrun{
#' 
#' shape<- pm_getmap(interval="Paleocene", model="GPlates")
#' data<- pm_getdata(base_name="Testudines", interval="Paleocene")
#' richness<- pm_richraster(shape, data, rank="genus")
#' 
#' #save as pdf file
#' pdf("pm_richraster-GPlates_Paleocene-Testudines.pdf")
#' richness<- pm_richraster(shape, data, rank="genus")
#' dev.off()
#' #save as tiff image
#' tiff("pm_richraster-GPlates_Paleocene-Testudines.tiff", 
#'       height = 10.5, width = 19, units = 'cm', res=300)
#' richness<- pm_richraster(shape, data, rank="genus")
#' dev.off()
#' 
#'}
#'

pm_richraster <- function (shape, data, rank, res = 10, 
                           colland = "#66666660",
                           colsea = "#00509010", col.grid=mycols(100), do.plot=TRUE, ...) {
  
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep=""))
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
  
  #creating a raster in size of the shape file
  ras <- raster::raster(shape, res = res)
  #getting the raster of the species richness
  r <- .rank_filter(ras, data, res = res, rank)
  
  mycol <- mycols(100)
  int_args <- base::list(x=r, axes=F, box=F, col="white", col.grid=col.grid, legend=TRUE
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xaxs="i", yaxs="i", bty='L')
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
  arglist <- c(int_args, params)
  mycol <- arglist$col.grid
  arglist$col <- arglist$col.grid
  arglist <- arglist[- base::which(base::names(arglist)=="col.grid")]
  if(do.plot){
    def.mar <- graphics::par("mar")
    def.oma <- graphics::par("oma")
    graphics::par(oma=c(0,0,0,2))
    graphics::par(mar=c(1.5,1.5,2,3))
    #plotting the map and the raster
    # base::do.call(raster::plot, arglist)
    #adding axes
    raster::plot (shape, col="white", border=FALSE
                  , xlim=c(-180,180), ylim=c(-90,90)
                  , xaxs="i", yaxs="i", bty='L')
    graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                   border=FALSE)
    raster::plot (shape, col=colland, border=FALSE, add=T, bty='L')
    
    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=97, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=89, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=81, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)
    
    raster::plot (r, add=T,axes=F, box=F, col=mycol, legend=FALSE, bty='L')
    graphics::par(xpd = TRUE)
    graphics::par(bty= "n")
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(tck=-0.2, col=NA, col.ticks="darkgrey", col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=NA), legend.args=list(text='richness', line=1, side=3, adj=0.25, cex=0.6, col=col.grid[length(col.grid)/2]))
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(line=-0.5, col=NA, col.ticks=NA, col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=col.grid[length(col.grid)/2]))
    graphics::par(bty= "o")
    
    graphics::par(mar=def.mar)
    graphics::par(oma=def.oma)
  }
  
  #return the raster
  return(r)
}




########pm_occ###################
#' pm_occ
#' 
#' Generates a diversity matrix, with the number occurrences of each species, 
#' genus, family or order per locality.
#' 
#' @usage pm_occ(data, rank = "genus")
#' 
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus"
#' @return Returns a data frame with number of species, genera, families or orders 
#' per locality.
#' @export 
#' @examples 
#' \dontrun{
#' 
#' data <- pm_getdata(base_name = "Canis", interval = "Quaternary")
#' result <- pm_occ(data, rank = "genus")
#' 
#'}


pm_occ <- function(data, rank = "genus") {
  
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep=""))
  }
  if(!.checkDataRank(data,rank)){
    if(rank=="species"){
      stop(base::paste("There is no column matched_name in the data frame.", sep=""))
    }else{
      stop(base::paste("There is no column ", rank, " in the data frame.", sep=""))
    }
  }
  
  #only getting occurences with a known genus
  genus_data <- .rfilter(data, rank)
  
  #getting locations
  loc <- base::data.frame(paleolat = genus_data$paleolat, 
                    paleolng = genus_data$paleolng)
  #getting unique locations
  uloc <- base::unique(loc)
  
  #getting list of unique taxa
  if(rank=="species"){
    ugenus <- base::as.vector(base::unique(genus_data[, "matched_name"]))
  }else if(rank=="genus"){
    ugenus <- base::as.vector(base::unique(genus_data[, "genus"]))
  }else if(rank=="family"){
    ugenus <- base::as.vector(base::unique(genus_data[, "family"]))
  }else if(rank=="order"){
    ugenus <- base::as.vector(base::unique(genus_data[, "order"]))
  }else if(rank=="class"){
    ugenus <- base::as.vector(base::unique(genus_data[, "class"]))
  }else if(rank=="phylum"){
    ugenus <- base::as.vector(base::unique(genus_data[, "phylum"]))
  }
  
  nsites <- uloc
  
  #fill with default values -1
  blank <- base::matrix(-1, nrow = base::nrow(nsites), ncol = base::length(ugenus))
  nsites <- base::cbind(nsites, blank)
  base::colnames(nsites) <- c(base::unlist(base::names(loc)), ugenus)
  
  #getting the number of occurrences of a genus for each locality
  for (i in 1:base::nrow(nsites)) {
    #get lat & lng
    lat_i <- nsites[i, "paleolat"]
    lng_i <- nsites[i, "paleolng"]
    for (j in 1:base::length(ugenus)) {
      #get current genus
      genus_j <- ugenus[j]
      #get all genus at locality
      flat <- base::subset(genus_data, genus_data$paleolat == lat_i)
      flatlng <- base::subset(flat, flat$paleolng == lng_i)
      #select only current genus
      if(rank=="species"){
        fgen <- base::subset(flatlng, flatlng[,"matched_name"] == genus_j)
      }else if(rank=="genus"){
        fgen <- base::subset(flatlng, flatlng[,"genus"] == genus_j)
      }else if(rank=="family"){
        fgen <- base::subset(flatlng, flatlng[,"family"] == genus_j)
      }else if(rank=="order"){
        fgen <- base::subset(flatlng, flatlng[,"order"] == genus_j)
      }else if(rank=="class"){
        fgen <- base::subset(flatlng, flatlng[,"class"] == genus_j)
      }else if(rank=="phylum"){
        fgen <- base::subset(flatlng, flatlng[,"phylum"] == genus_j)
      }
      
      count<- base::nrow(fgen)
      nsites[i, j + 2] <- count
    }
  }
  return(nsites)
}





########pm_occ_cell###################
#' pm_occ_cell
#' 
#' Generates a diversity matrix, with the number occurrences of each species, 
#' genus, family or order per cell.
#' 
#' @usage pm_occ_cell(data, rank = "genus", res = 10)
#' 
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus"
#' @param res numeric. Defining the spatial resolution. By default res=10. 
#' @return Returns a data frame with number of species, genera, families or orders 
#' per cell.
#' @export 
#' @examples 
#' \dontrun{
#' 
#' data <- pm_getdata(base_name = "Canis", interval = "Quaternary")
#' result <- pm_occ_cell(data, rank = "genus", res = 10)
#' 
#'}



pm_occ_cell <- function(data, rank = "genus", res = 10) {
  
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRank(rank)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep=""))
  }
  if(!.checkDataRank(data,rank)){
    if(rank=="species"){
      stop(base::paste("There is no column matched_name in the data frame.", sep=""))
    }else{
      stop(base::paste("There is no column ", rank, " in the data frame.", sep=""))
    }
  }
  
  #only getting occurences with a known genus
  genus_data <-.rfilter(data, rank)
  #getting list of unique taxa
  if(rank=="species"){
    ugenus <- base::as.vector(base::unique(genus_data[, "matched_name"]))
  }else if(rank=="genus"){
    ugenus <- base::as.vector(base::unique(genus_data[, "genus"]))
  }else if(rank=="family"){
    ugenus <- base::as.vector(base::unique(genus_data[, "family"]))
  }else if (rank=="order"){
    ugenus <- base::as.vector(base::unique(genus_data[, "order"]))
  }
  else if(rank=="class"){
    ugenus <- base::as.vector(base::unique(genus_data[, "class"]))
  }else{
    ugenus <- base::as.vector(base::unique(genus_data[, "phylum"]))
  }
  lat <- base::seq(-90 , 90, res)
  long <- base::seq(-180, 180, res)
  nsites <- base::expand.grid (long, lat)
  #fill with default values -1
  blank <- base::matrix(-1, nrow = base::nrow (nsites), ncol = base::length(ugenus))
  nsites <- base::cbind(nsites, blank)
  base::colnames(nsites) <- c ("paleolng", "paleolat", ugenus)
  
  #getting the number of occurrences of a genus for each locality
  for (i in 1:base::nrow(nsites)) {
    #get lat & lng
    lng_i <- nsites[i, "paleolng"]
    lat_i <- nsites[i, "paleolat"]
    for (j in 1:base::length(ugenus)) {
      #get current genus
      genus_j <- ugenus[j]
      #get all genus at locality
      flat <- base::subset(genus_data, genus_data$paleolat >= lat_i - (res /2))
      flat <- base::subset(flat , flat$paleolat < lat_i + (res / 2))
      flatlng <- base::subset(flat, flat$paleolng >= lng_i - (res / 2))
      flatlng <- base::subset(flatlng , flatlng$paleolng < lng_i + (res / 2))
      
      #select only current genus
      if(rank=="species"){
        fgen <- base::subset(flatlng, flatlng[,"matched_name"] == genus_j)
      }else if(rank=="genus"){
        fgen <- base::subset(flatlng, flatlng[,"genus"] == genus_j)
      }else if(rank=="family"){
        fgen <- base::subset(flatlng, flatlng[,"family"] == genus_j)
      }else if(rank=="order"){
        fgen <- base::subset(flatlng, flatlng[,"order"] == genus_j)
      }else if(rank=="class"){
          fgen <- base::subset(flatlng, flatlng[,"class"] == genus_j)
      }else if(rank=="phylum"){
        fgen <- base::subset(flatlng, flatlng[,"phylum"] == genus_j)
      }
      count<- base::nrow (fgen)
      nsites[i, j + 2] <- count
    }
  }
  return(base::as.data.frame(nsites))
}


#####################pm_divraster_loc####################

#' pm_divraster_loc
#' 
#' Calculates the Shannon diversity per unique locality (based on 
#' its coordinates), makes a RasterLayer and a plot showing mean, 
#' max, min diversity per cell, or number of unique localities per cell.
#' 
#' @usage pm_divraster_loc  (shape, occ_df, res=10, fun=mean, colland = "#66666660"
#'                           , colsea = "#00509010", do.plot=TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param occ_df data.frame with fossil occurrences. Can be created with pm_occ(data).
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param fun function or character. To determine what values to assign to cells that are covered by multiple spatial features. 
#' You can use functions such as min, max, or mean, or the character value: 'count'. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @return Creates a plot with map of the time intervall, the fossil occurences and the 
#' RasterLayer. And returns the RasterLayer itself.
#' @export 
#' @examples 
#' \dontrun{
#' 
#' shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df <- pm_occ (data, rank = "genus")
#' pm_divraster_loc (shape, occ_df, fun=mean)
#' pm_divraster_loc (shape, occ_df, fun=max)
#' pm_divraster_loc (shape, occ_df, fun=min)
#' pm_divraster_loc (shape, occ_df, fun="count")
#' 
#' #save as pdf file
#' pdf("pm_divraster_loc-GPlates_Quaternary-Canis.pdf")
#' pm_divraster_loc (shape, occ_df, fun=mean)
#' dev.off()
#' #save as tiff image
#' tiff("pm_divraster_loc-GPlates_Quaternary-Canis.tiff", 
#'       height = 10.5, width = 19, units = 'cm', res=300)
#' pm_divraster_loc (shape, occ_df, fun=mean)
#' dev.off()
#' 
#'}

pm_divraster_loc <- function(shape, occ_df, res=10, fun = mean,
                             colland="#66666660", colsea="#00509010", do.plot=TRUE, ...) {
  
  if(!.checkLatLng(occ_df)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  if(!.checkFun(fun, "pm_divraster_loc")){
    stop("Chosen value for fun is not a valid input.")
  }
  
  #creating a raster in size of the shape file
  ras <- raster::raster(shape, res=res)
  #getting only species data and no duplictaed in a raster field
  drops <- c("paleolat","paleolng")
  drop_occ <- occ_df[, !(base::names(occ_df) %in% drops)]
  cordata1 <- vegan::diversity(drop_occ)
  cordata <- base::data.frame(occ_df$paleolat, 
                        occ_df$paleolng, div= cordata1)
  base::colnames(cordata) <- c("paleolat", "paleolng", "div")
  
  #getting the raster of the species richness
  r<-raster::rasterize(cordata[, c("paleolng", "paleolat")], ras, 
                       field= cordata$div, fun=fun)
  #getting the raster of the species richness
  r[r==0]<- NA
 
  mycol <- mycols(100)
  int_args <- base::list(x=r, axes=F, box=F, col=mycols(100), col.grid=mycol, legend=TRUE, xlim=c(-180,180), ylim=c(-90,90)
                         , xlab="Longitude", ylab="Latitude"
                         , xaxs="i", yaxs="i", legend.args=list(text='diversity', side=3, line=1, adj=0.25, cex=0.8, col="darkgrey"), bty='L')
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
  arglist <- c(int_args, params)
  mycol <- arglist$col.grid
  arglist$col <- arglist$col.grid
  arglist <- arglist[- base::which(base::names(arglist)=="col.grid")]
  
  if(do.plot){
    def.mar <- graphics::par("mar")
    def.oma <- graphics::par("oma")
    graphics::par(oma=c(0,0,0,2))
    graphics::par(mar=c(1.5,1.5,2,3))
    #plotting the map and the raster
    #adding axes
    raster::plot (shape, col="white", border=FALSE,  xlim=c(-180,180), ylim=c(-90,90)
                  , xaxs="i", yaxs="i", bty='L')
    graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                   border=FALSE,bty='L')
    raster::plot (shape, col=colland, border=FALSE, add=T, bty='L')
    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=97, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=89, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=81, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)
    
    raster::plot (r, add=T,axes=F, box=F, col=mycols(100), legend=FALSE, bty='L')
    graphics::par(xpd = TRUE)
    graphics::par(bty= "n")
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(tck=-0.2, col=NA, col.ticks="darkgrey", col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=NA), legend.args=list(text='diversity', line=1, side=3, adj=0.25, cex=0.6, col=mycol[length(mycol)/2]))
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(line=-0.5, col=NA, col.ticks=NA, col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=mycol[length(mycol)/2]))
    graphics::par(bty= "o")
    
    graphics::par(mar=def.mar)
    graphics::par(oma=def.oma)
  }
  #return the raster
  return(r)
}



#####################pm_divraster_cell####################

#' pm_divraster_cell
#' 
#' Calculates the Shannon diversity per cell 
#' (taking into account relative abundances of all the fossil records 
#' whithin the cell).
#' 
#' @usage pm_divraster_cell  (shape, occ_df_cell, res=10,
#'                            colland="#66666660", colsea="#00509010", do.plot=TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param occ_df_cell data.frame with fossil occurrences. Can be created with pm_occ_cell (data)
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @return Creates a plot with map of the time intervall, the fossil occurences and the 
#' RasterLayer. And returns the RasterLayer itself.
#' @export 
#' @examples 
#' \dontrun{
#' 
#'shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#'data<- pm_getdata (base_name="Canis", interval="Quaternary")
#'occ_df_cell <- pm_occ_cell (data, rank = "genus")
#'div_cell <- pm_divraster_cell (shape, occ_df_cell, res=10)
#' 
#' #save as pdf file
#' pdf("pm_divraster_loc-GPlates_Quaternary-Canis.pdf")
#' pm_divraster_cell (shape, occ_df_cell, res=10)
#' dev.off()
#' #save as tiff image
#' tiff("pm_divraster_loc-GPlates_Quaternary-Canis.tiff", 
#'       height = 10.5, width = 19, units = 'cm', res=300)
#' pm_divraster_cell (shape, occ_df_cell, res=10)
#' dev.off()
#' 
#' }

pm_divraster_cell <- function(shape, occ_df_cell, res=10,
                              colland="#66666660", colsea="#00509010", do.plot=TRUE, ...) {
  
  if(!.checkLatLng(occ_df_cell)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  
  
  #creating a raster in size of the shape file
  ras <- raster::raster(shape, res=res)
  
  #getting only species data and no duplictaed in a raster field
  drops <- c("paleolat","paleolng")
  drop_occ <- occ_df_cell[, !(base::names(occ_df_cell) %in% drops)]
  cordata1 <- vegan::diversity(drop_occ)
  cordata <- base::data.frame(occ_df_cell$paleolat, 
                        occ_df_cell$paleolng, div= cordata1)
  base::colnames(cordata) <- c("paleolat","paleolng","div")
  
  r<-raster::rasterize(cordata[, c("paleolng","paleolat")], ras, 
               field= cordata$div, fun=max)
  
  #getting the raster of the species richness
  r[r==0]<- NA
  
  mycol <- mycols(100)
  int_args <- base::list(x=r, axes=F, box=F, col=mycols(100), col.grid=mycol, legend=TRUE, main= 'Shannon diversity per cell'                
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xlab="Longitude", ylab="Latitude"
                         , xaxs="i", yaxs="i")
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
  arglist <- c(int_args, params)
  mycol <- arglist$col.grid
  arglist$col <- arglist$col.grid
  arglist <- arglist[- base::which(base::names(arglist)=="col.grid")]
  
  if(do.plot){
    def.mar <- graphics::par("mar")
    def.oma <- graphics::par("oma")
    graphics::par(oma=c(0,0,0,2))
    graphics::par(mar=c(1.5,1.5,2,3))
    #plotting the map and the raster
    # base::do.call(raster::plot, arglist)
    #adding axes
    raster::plot (shape, col="white", border=FALSE
                  , xlim=c(-180,180), ylim=c(-90,90)
                  , xlab="Longitude", ylab="Latitude"
                  , xaxs="i", yaxs="i", bty='L')
    graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                   border=FALSE)
    raster::plot (shape, col=colland, border=FALSE, add=T, bty='L')
    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=97, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=89, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=81, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)
    
    raster::plot (r, add=T,axes=F, box=F, col=mycol, legend=FALSE, bty='L')
    graphics::par(xpd = TRUE)
    graphics::par(bty= "n")
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(tck=-0.2, col=NA, col.ticks="darkgrey", col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=NA), legend.args=list(text='diversity', line=1, side=3, adj=0.25, cex=0.6, col=mycol[length(mycol)/2]))
    raster::plot (r, legend.only=TRUE, col=mycol, smallplot=c(0.95,0.99, 0.3,0.7), axis.args=list(line=-0.5, col=NA, col.ticks=NA, col.lab=NA, cex=0.5, cex.lab=0.5, cex.axis=0.5, col.axis=mycol[length(mycol)/2]))
    graphics::par(bty= "o")
    
    graphics::par(mar=def.mar)
    graphics::par(oma=def.oma)
  }
  
  #return the raster
  return(r)
}



###################################pm_latrich###################
#' pm_latrich
#' 
#' Calculates latitudinal diversity of taxa (species, genera, families, orders).
#' 
#' @usage pm_latrich (shape, data, rank = "genus", res=10,
#'                    colland="#66666680", colsea="#00509010", 
#'                    colpoints="#65432190", 
#'                    rich.col="#654321", pch=21, do.plot=TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param data data.frame with fossil occurrences. Can be created with
#'  pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus".
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param colpoints define the color of the occurrence-points. By default colpoints="#65432190". 
#' @param rich.col define the color of the richness curve. By default rich.col="#654321".
#' @param pch point symbol for plotting the occurences. By default pch=21.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, 
#' such as main="my own title", main.col="red".
#' @return Returns adata frame with richness of rank and a plot of the continental masses 
#' with the occurrences and the latitudinal richness.
#' @export 
#' @examples 
#' \dontrun{
#' 
#' shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' pm_latrich (shape, data, rank = "genus", res=10)
#' 
#' #save as pdf file
#' pdf("pm_latrich-GPlates_Quaternary-Canis.pdf")
#' pm_latrich (shape, data, rank = "genus", res=10)
#' dev.off()
#' #save as tiff image
#' tiff("pm_latrich-GPlates_Quaternary-Canis.tiff", 
#'       height = 9, width = 17.5, units = 'cm', res=300)
#' pm_latrich (shape, data, rank = "genus", res=10)
#' dev.off()
#' 
#'}


pm_latrich <- function(shape, data, rank = "genus",
                       res=10, 
                       colland="#66666680", colsea="#00509010", 
                       colpoints="#65432190",
                       rich.col="#654321", pch=21, do.plot=TRUE, ...) {
  
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
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
  
  int_args <- base::list(x=shape, col="white", border = FALSE
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xaxs="i", yaxs="i")
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
  arglist <- c(int_args, params)
  
  data2 <-.rfilter(data, rank)
  #setting min and max value for lat
  lr <- base::data.frame(lat_min= base::seq(-90,90-res,res), 
                   lat_max=base::seq(-90+res, 90, res))
  #creating empty richness data frame
  richn <- NULL
  #going through lats
  for(lat in base::seq(-90,90-res,res)) {
    sub1 <- base::subset(data2, data2$paleolat>=lat)
    sub2 <- base::subset(sub1, sub1$paleolat<(lat+res))
    sub3 <- base::unique(sub2[,3])
    #count and save the number of different genus at each latitude
    richn <- c(richn, base::length(sub3))
  }
  
  magn <- 140/max(richn)
  
  #combine min,max lat and richness
  lr <- base::cbind(lr, richn)
  centros<- (base::seq(-90,90-res,res)+(base::seq(-90,90-res,res) + res))/2
  rich<- 180 + (lr$richn*magn)
  yy<- c(180, rich, 180)
  xx<- c(-90, centros, 90)
  
  if(do.plot){
    def.mar <- graphics::par("mar")
    
    graphics::par(mar=c(1.5,1.5,2,10), xpd=NA)
    do.call(raster::plot, arglist)
    graphics::rect(xleft=-180, xright=180, 
                   ybottom=-90, ytop=90, col=colsea, 
                   border=FALSE)
    raster::plot(shape, col=colland, border=FALSE, add=T)
    graphics::points(data2$paleolng, data2$paleolat, 
                     pch=pch, col=NA, bg=colpoints)
    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=94, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=86, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=78, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)
    
    graphics::polygon (yy, xx, col=rich.col, border=F, xpd=T)
    
    maxi <- base::max(lr$richn)
    ax_seq <- base::seq(base::min(yy),base::max(yy), ((base::max(yy)-base::min(yy))/2))
    ax_lab <- base::round(ax_seq-180)
    ax_lab <- round(ax_lab/magn)
    graphics::axis(side=3, pos=90, lwd =1, xpd=TRUE, at=ax_seq, labels=FALSE , col.ticks = rich.col ,col.axis = rich.col , col= rich.col , cex.axis=0.6, tck=-0.01)
    graphics::axis(side=3, pos=80, lwd =0, xpd=TRUE, at=ax_seq, labels=ax_lab , col.ticks = rich.col ,col.axis = rich.col , col= rich.col , cex.axis=0.6, tck=-0.01)
    graphics::axis(side=3, pos=90, lwd = 0, xpd=TRUE, at=ax_seq[round(length(ax_seq)/2)], labels="richness", col.ticks=rich.col, col.axis=rich.col, col=rich.col, cex.axis=0.8, tck=-0.01, cex.lab=0.8)
  
    graphics::par(mar=def.mar)
  }
  
  #return latitudinal richness
  return(lr)
}

###################################pm_latdiv###################
#' pm_latdiv
#' 
#' Calculates the Shannon diversity along the latitudinal gradient based on 
#' the individual values of diverstiy the fossil localities of those latitudes.
#' The function returns the mean or max values of diversity of the sampled 
#' localities along the latitudinal gradient.
#' 
#' @usage pm_latdiv (shape, occ_df, res=10, fun= max,
#'                   colland="#66666680", colsea="#00509010",  
#'                   colpoints="#65432190",
#'                   div.col="#654321", pch=21, do.plot=TRUE, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param occ_df data.frame with fossil occurrences. Can be created with pm_occ(data)
#' @param res numeric. Defining the spatial resolution. By default res=10. 
#' @param fun function or character. To determine what values to assign to cells that are covered by multiple spatial features. 
#' You can use functions such as min, max, or mean, or the character value: 'count'. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param colpoints define the color of the occurrence-points. By default colpoints="#65432190".
#' @param div.col define the color od zje diversity curve. By default div.col="#654321".
#' @param pch point symbol for plotting the occurences. By default pch=21.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return Returns adata frame with shannon diversity,
#' a plot of the corresponding time map with the occurrences and their
#' latitudinal diversity
#' 
#' @export 
#' @examples 
#' \dontrun{
#' 
#' shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df <- pm_occ (data)
#' pm_latdiv (shape, occ_df, fun=mean)
#' pm_latdiv (shape, occ_df, fun=max)
#' 
#' #save as pdf file
#' pdf("pm_latrich-GPlates_Quaternary-Canis.pdf")
#' pm_latdiv (shape, occ_df, fun=mean)
#' dev.off()
#' #save as tiff image
#' tiff("pm_latrich-GPlates_Quaternary-Canis.tiff", 
#'       height = 9, width = 17.5, units = 'cm', res=300)
#' pm_latdiv (shape, occ_df, fun=mean)
#' dev.off()
#' 
#'}

pm_latdiv <- function(shape, occ_df, res=10, 
                      fun= max,
                      colland="#66666680", colsea="#00509010", 
                      colpoints="#65432190",
                      div.col="#654321", pch=21, do.plot=TRUE, ...) {
  
  if(!.checkLatLng(occ_df)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  if(!.checkFun(fun, "pm_latdiv")){
    stop(base::paste("\"", fun, "\" is not a valid input for parameter fun.", sep=""))
  }
  
  int_args <- base::list(x=shape, col="white", border = FALSE
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xaxs="i", yaxs="i")
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
  arglist <- c(int_args, params)
  
  
  #calculate the shannon diversity
  drops <- c("paleolat","paleolng")
  drop_occ <- occ_df[, !(base::names(occ_df) %in% drops)]
  H_data <- vegan::diversity(drop_occ)
  
  #get the localities
  locs <- base::cbind(occ_df[,"paleolat"],occ_df[,"paleolng"], H_data)
  base::colnames(locs) <- c("paleolat", "paleolng", "div")
  cornum <- NULL
  for(lat in base::seq(-90,80,res)) {
    slocs <- base::subset(locs, locs[,"paleolat"]>=lat)
    slocs <- base::subset(slocs, slocs[,"paleolat"]<lat+res)
    if (base::nrow (slocs) == 0) {
      cornum <- c(cornum, 0)  
    } else {
      cornum <- c(cornum, fun (slocs[,"div"])) 
    }
  }
  
  magn <- 140/max(cornum)
  
  latmin <- base::seq(-90,80,res)
  latmax <- base::seq(-80,90,res)
  #create data frame withh latmin latmax and corrected richness
  lr <- base::data.frame(latmin, latmax, cornum)
  base::colnames(lr) <- c("maxlat", "minlat", "div")
  
  centros<- (base::seq(-90,90-res,res)+(base::seq(-90,90-res,res) + res))/2
  rich<- 180 + (lr$div*magn)
  yy<- c(180, rich, 180)
  xx<- c(-90, centros, 90)
  
  if(do.plot){
    def.mar <- graphics::par("mar")
    
    graphics::par(mar=c(1.5,1.5,2,10), xpd=NA)
    base::do.call(raster::plot, arglist)
    graphics::rect(xleft=-180, xright=180, 
                   ybottom=-90, ytop=90, col=colsea, 
                   border=FALSE)
    raster::plot(shape, col=colland, border=FALSE, 
                 add=T)
    graphics::points(occ_df[, "paleolng"], occ_df[, "paleolat"], 
                     pch=pch, col=NA, 
                     bg=colpoints)
    graphics::axis(side = 1, pos=-84, lwd = 0, , xaxp=c(180,-180,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    graphics::axis(side = 1, pos=-89, lwd = 0, at=0 , labels="Longitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    graphics::axis(side = 2, pos=-175, lwd = 0, yaxp=c(90,-90,4), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6, las=1)
    graphics::axis(side = 2, pos=-178, lwd = 0, at=0 , labels="Latitude", col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.6)
    
    shape.info <- .getShapeInfo(shape)
    graphics::axis(side = 3, pos=94, lwd = 0, at=135 , labels=shape.info[1], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=1)
    graphics::axis(side = 3, pos=86, lwd = 0, at=135 , labels=shape.info[2], col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.5)
    graphics::axis(side = 3, pos=78, lwd = 0, at=135 , labels=paste(shape.info[3], " - ", shape.info[4], " mya", sep=""), col.ticks = "darkgrey",col.axis ="darkgrey", cex.axis=0.7)
    
    graphics::polygon(yy, xx, col=div.col, 
                      border=F, xpd=T)
    
    maxi <- base::max(lr$div)
    ax_seq <- base::seq(base::min(yy),base::max(yy), ((base::max(yy)-base::min(yy))/2))
    ax_lab <- base::round(ax_seq-180)
    ax_lab <- round(ax_lab/magn)
    graphics::axis(side=3, pos=90, lwd =1, xpd=TRUE, at=ax_seq, labels=FALSE , col.ticks = div.col ,col.axis = div.col , col= div.col , cex.axis=0.6, tck=-0.01)
    graphics::axis(side=3, pos=80, lwd =0, xpd=TRUE, at=ax_seq, labels=ax_lab , col.ticks = div.col ,col.axis = div.col , col= div.col , cex.axis=0.6, tck=-0.01)
    graphics::axis(side=3, pos=90, lwd = 0, xpd=TRUE, at=ax_seq[round(length(ax_seq)/2)], labels="diversity", col.ticks=div.col, col.axis=div.col, col=div.col, cex.axis=0.8, tck=-0.01)

    graphics::par(mar=def.mar) 
  }
  
  #return latitudinal richness
  return(lr)
}


###########################pm_checkAge#############################
#' pm_checkAge
#' 
#' Creates a data frame with all maps that contain the age requested.
#' 
#' @usage pm_checkAge(age="all")
#' 
#' @param age character. defining the age of interest. By default age="all", which gives complete list.
#' @return Returns a data.frame which maps are available
#' @export
#' @examples
#' \dontrun{
#' 
#' pm_checkAge(age="112")
#' 
#' #get complete list with all available maps
#' pm_checkAge()
#' 
#'}

pm_checkAge <- function(age="all"){
  df_maps <- NULL
  utils::data(df_maps,envir = base::environment())
  
  if(age!="all"){
    maps <- df_maps[as.numeric(df_maps$fromage) >= as.numeric(age),]
    maps <- maps[as.numeric(maps$toage) <= as.numeric(age),]
    
  }else{
    maps <- df_maps
  }
  
  maps
}
