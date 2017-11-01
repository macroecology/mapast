###########################pm_getmap#############################

#' pm_getmap
#' 
#' generates a shapefile with the paleomap of the chosen 
#' time interval (e.g. "Cretaceous") and a plot
#' 
#' @usage pm_getmap(interval, model, colland = "#66666660"
#'                           , colsea = "#00509010", do.plot = TRUE, ...)
#' 
#' @param interval character. Temporal paleoeographical interval of interest. (e.g. "Cretaceous" for GPlates or "112.0" for Smith)
#' @param model character. Defining the model the map was created with. "GPlates", "Smith" or "Golonka"
#' @param colland define the color of the land masses. By default colland = "#66666660"
#' @param colsea define the color of the sea. By default colsea = "#00509010"
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot=TRUE 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return a SpatialPolygonsDataFrame and a plot (if do.plot=TRUE)
#' @export
#' @examples
#' \dontrun{
#' pm_getmap(interval="Cretaceous", model="GPlates")
#' 
#' #for checking which maps are available please use
#' d <- data(package='paleogeoDB')
#' d$results[, "Item"]
#'}

pm_getmap <- function(interval, model, colland = "#66666660", 
                      colsea = "#00509010", 
                      do.plot = TRUE, ...) {
  
  #getting final parameter list for plot
  int_args <- base::list(x=shape, col = "white", border = FALSE, main=interval
                         , xlim=c(-180,180), ylim=c(-90,90)
                         , xlab="Longitude", ylab="Latitude"
                         , xaxs="i", yaxs="i")
  params <- base::list(...)
  names_params <- base::as.vector(base::names(params))
  names_intargs <- base::as.vector(base::names(int_args))
  for( i in names_params){
    if(i %in% names_intargs) int_args <- int_args[ - base::which(base::names(int_args)==i)] 
  }
 
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
  
  arglist <- c(int_args, params)
  # if user does not set plot=FALSE plot the shape file
  if (do.plot) {
    base::do.call(sp::plot, arglist)
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)
    graphics::axis(1, xaxp=c(180,-180,4))
    graphics::axis(2, yaxp=c(90,-90,4))
    sp::plot(shape, col = colland, border = FALSE, add = TRUE)
  }

  # return the shape file
  return(shape)
}



################pm_getdata##############################

#' pm_getdata
#' 
#' uses paleobioDB R package to get data from the Paleobiology Database
#'  
#' @usage pm_getdata(interval, base_name, limit="all")
#' 
#' @param interval character. Temporal paleoeographical interval of interest. (e.g. "Cretaceous" for GPlates or "112.0" for Smith)
#' @param base_name character. The name of the taxon of interest. (e.g. "Canis" or "reptilia")
#' @param limit integer. Defining the max. number of occurrences to be downloaded from paleobioDB. By default limit="all"
#' @return a data frame with fossil occurrences
#' @export 
#' @examples 
#' \dontrun{
#' pm_getdata(interval="Quaternary",base_name="Canis")
#' 
#' #if you want to use FROMAGE and TOAGE to define the interval you can use the paleobioDB package
#' myocc <- base::data.frame(paleobioDB::pbdb_occurrences(base_name="mammalia", 
#'                     min_ma=0, max_ma=2.58, 
#'                     show=c("paleoloc", "phylo"), 
#'                     vocab="pbdb", limit=100))
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
#' plots your query from the paleobioDB onto the map of the selected time interval
#' 
#' 
#' @usage pm_plot(interval, model, data, colland = "#66666660",
#'                 colsea = "#00509010",colpoints = "#99000020", 
#'                 pch =16, cex = 1, ...)
#' 
#' @param interval character. Temporal paleoeographical interval of interest. (e.g. "Cretaceous" for GPlates or "112.0" for Smith)
#' @param model character. Defining the model the map was created with. "GPlates", "Smith" or "Golonka"
#' @param data data.frame with fossil occurrences. Can be created with pm_getdata(interval, base_name)
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param colpoints define the color of the occurrence-points. By default colpoints = "#99000020".
#' @param pch point symbol for plotting the occurences. By default pch=16 (filled circle).
#' @param cex numeric. size of the points. By default cex=1.
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return a plot with the configuration of the continents at the selected 
#' time interval and the fossil occurrences
#' @export 
#' @examples 
#' \dontrun{
#' data  <-  pm_getdata (interval="Cretaceous", base_name="Mammalia")
#' pm_plot(interval="Cretaceous", model="GPlates", data)
#'}

pm_plot <- function(interval, model, data,
                    colland = "#66666660",
                    colsea = "#00509010", 
                    colpoints = "#99000020", 
                    pch =16, cex = 1, ...) {
  
  #check user input
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }

  int_args <- base::list(x=shape, col = "white", border = FALSE, main=interval
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
  
  #getting the shape file for the map and the data for plotting it on the map
  shape <- paleoMap::pm_getmap(interval = interval, model = model, do.plot = FALSE)
  #plotting the map and the data
  if (base::class(data) == "data.frame") {
    #defines size and axes of the plot
    base::do.call(sp::plot, arglist)
    graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)
    sp::plot(shape, col = colland, border = FALSE, add = TRUE)
    graphics::axis(1, xaxp=c(180,-180,4))
    graphics::axis(2, yaxp=c(90,-90,4))
    graphics::points(data$paleolng, 
           data$paleolat, 
           pch = pch, col = colpoints, 
           cex = cex)
  }
}

#####################pm_occraster##############################

#' pm_occraster
#' 
#' creates a raster and a plot of the fossil occurences by taxonomic rank per cell 
#' (a proxy for the sampling effort)
#' 
#' @usage pm_occraster(shape, data, rank = "species", res = 10,
#'                     colland = "#66666660", colsea = "#00509010", ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum".
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @export 
#' @examples 
#' \dontrun{
#' shape <- pm_getmap(interval="Quaternary", model="GPlates", do.plot = FALSE)
#' data <- pm_getdata(base_name="Canis", interval="Quaternary")
#' pm_occraster(shape, data)
#'}

pm_occraster <- function(shape, data, 
                         rank = "species", 
                         res = 10, 
                         colland = "#66666660",
                         colsea = "#00509010", ...) {
  
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
  mycol <- mycols(100)
  int_args <- base::list(x=r, axes=F, box=F, col=mycols(100), col.grid=mycol, legend=TRUE, main= "occurence raster"
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
  #plotting the map and the raster on the map
  do.call(raster::plot, arglist)
  #adding axes
  raster::plot (shape, col="white", border=FALSE
                , xlim=c(-180,180), ylim=c(-90,90)
                , xlab="Longitude", ylab="Latitude"
                , xaxs="i", yaxs="i", add=T)
  graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                 border=FALSE)
  raster::plot (shape, col=colland, border=FALSE, add=T)
  graphics::axis(1, xaxp=c(180,-180,4))
  graphics::axis(2, yaxp=c(90,-90,4))
  raster::plot (r, add=T,axes=F, box=F, col=mycol, legend=FALSE)
  
  #returning the raster
  return(r)
}

#####################pm_richraster####################

#' pm_richraster
#' 
#' Creates a raster of species richness
#' and makes a plot of the map and raster
#' 
#' @usage pm_richraster(shape, data, rank, res = 10, 
#'                      colland = "#66666660", colsea = "#00509010", ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum".
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Paleocene", model="GPlates")
#' data<- pm_getdata(base_name="Testudines", interval="Paleocene")
#' richness<- pm_richraster(shape, data, rank="genus")
#'}
#'

pm_richraster <- function (shape, data, rank, res = 10, 
                           colland = "#66666660",
                           colsea = "#00509010", ...) {
  
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
  int_args <- base::list(x=r, axes=F, box=F, col=mycols(100), col.grid=mycol, legend=TRUE, main= "richness raster"
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
  
  #plotting the map and the raster
  base::do.call(raster::plot, arglist)
  #adding axes
  raster::plot (shape, col="white", border=FALSE
                , xlim=c(-180,180), ylim=c(-90,90)
                , xlab="Longitude", ylab="Latitude"
                , xaxs="i", yaxs="i", add=T)
  graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                 border=FALSE)
  raster::plot (shape, col=colland, border=FALSE, add=T)
  graphics::axis(1, xaxp=c(180,-180,4))
  graphics::axis(2, yaxp=c(90,-90,4))
  raster::plot (r, add=T,axes=F, box=F, col=mycol, legend=FALSE)
  #return the raster
  return(r)
}




########pm_occ###################
#' pm_occ
#' 
#' generates a diversity matrix, with the number occurrences of each species, 
#' genus, family or order per locality
#' 
#' @usage pm_occ(data, rank = "species")
#' 
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank="species"
#' @return data frame with number of species, genera, families or orders per locality
#' @export 
#' @examples 
#' \dontrun{
#' data <- pm_getdata(base_name = "Canis", interval = "Quaternary")
#' result <- pm_occ(data, rank = "species")
#'}


pm_occ <- function(data, rank = "species") {
  
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
#' generates a diversity matrix, with the number occurrences of each species, 
#' genus, family or order per cell
#' 
#' @usage pm_occ_cell(data, rank = "species", res = 10)
#' 
#' @param data data.frame with fossil occurrences. Can be created with 
#' pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank="species"
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @return data frame with number of species, genera, families or orders per locality
#' @export 
#' @examples 
#' \dontrun{
#' data <- pm_getdata(base_name = "Canis", interval = "Quaternary")
#' result <- pm_occ_cell(data, rank = "species", res = 10)
#'}



pm_occ_cell <- function(data, rank = "species", res = 10) {
  
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
  lat <- base::seq(-90 + (res / 2), 90 -(res / 2), res)
  long <- base::seq(-180 + (res / 2), 180 -(res / 2), res)
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
#' calculates the Shannon diversity per unique locality (based on 
#' its coordinates), makes a raster file and a plot showing mean, 
#' max, min diversity per cell, or number of unique localities per cell
#' 
#' @usage pm_divraster_loc  (shape, occ_df, res=10, fun=mean,
#'                           colland="#66666680", colsea="#00509010", ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param occ_df data.frame with fossil occurrences. Can be created with pm_occ(data).
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param fun function or character. To determine what values to assign to cells that are covered by multiple spatial features. 
#' You can use functions such as min, max, or mean, or the character value: 'count'. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df <- pm_occ (data, rank="species")
#' pm_divraster_loc (shape, occ_df, fun=mean)
#' pm_divraster_loc (shape, occ_df, fun=max)
#' pm_divraster_loc (shape, occ_df, fun=min)
#' pm_divraster_loc (shape, occ_df, fun="count")
#'}

pm_divraster_loc <- function(shape, occ_df, res=10, fun = mean,
                             colland="#66666680", colsea="#00509010", ...) {
  
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
  int_args <- base::list(x=r, axes=F, box=F, col=mycols(100), col.grid=mycol, legend=TRUE, main= 'Shannon diversity per locality'                , xlim=c(-180,180), ylim=c(-90,90)
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
  
  #plotting the map and the raster
  base::do.call(raster::plot, arglist)
  
  #adding axes
  raster::plot (shape, col="white", border=FALSE,  xlim=c(-180,180), ylim=c(-90,90)
                , xlab="Longitude", ylab="Latitude"
                , xaxs="i", yaxs="i", add=T
  )
  graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                 border=FALSE)
  raster::plot (shape, col=colland, border=FALSE, add=T)
  graphics::axis(1, xaxp=c(180,-180,4))
  graphics::axis(2, yaxp=c(90,-90,4))
  raster::plot (r, add=T,axes=F, box=F, col=mycol, legend=FALSE)
  #return the raster
  return(r)
}



#####################pm_divraster_cell####################

#' pm_divraster_cell
#' 
#' calculates the Shannon diversity per cell 
#' (taking into account relative abundances of all the fossil records 
#' whithin the cell)
#' 
#' @usage pm_divraster_cell  (shape, occ_df_cell, res=10,
#'                            colland="#66666680", colsea="#00509010", ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param occ_df_cell data.frame with fossil occurrences. Can be created with pm_occ_cell (data)
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main="my own title", main.col="red".
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples 
#' \dontrun{
#'shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#'data<- pm_getdata (base_name="Canis", interval="Quaternary")
#'occ_df_cell <- pm_occ_cell (data, rank="species")
#'div_cell <- pm_divraster_cell (shape, occ_df_cell, res=10)
#' ## cells with diversity values = 0 (e.g., 1 species) are discarded.
#' }

pm_divraster_cell <- function(shape, occ_df_cell, res=10,
                              colland="#66666680", colsea="#00509010", ...) {
  
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
  
  #plotting the map and the raster
  base::do.call(raster::plot, arglist)
  #adding axes
  raster::plot (shape, col="white", border=FALSE
        , xlim=c(-180,180), ylim=c(-90,90)
        , xlab="Longitude", ylab="Latitude"
        , xaxs="i", yaxs="i", add=T)
  graphics::rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
                 border=FALSE)
  raster::plot (shape, col=colland, border=FALSE, add=T)
  graphics::axis(1, xaxp=c(180,-180,4))
  graphics::axis(2, yaxp=c(90,-90,4))
  raster::plot (r, add=T,axes=F, box=F, col=mycol, legend=FALSE)
  #return the raster
  return(r)
}



###################################pm_latrich###################
#' pm_latrich
#' 
#' calculates latitudinal diversity of taxa (species, genera, families, orders)
#' 
#' @usage pm_latrich (shape, data, rank="species", res=10, magn=1,
#'                    colland="#66666680", colsea="#00509010", 
#'                    colpoints="#FFC12530",colpointborder="black", 
#'                    rich.col="goldenrod1", pch=21, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param data data.frame with fossil occurrences. Can be created with
#'  pm_getdata(interval, base_name)
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank="species".
#' @param res numeric. Defining the spatial resolution. Default res=10. 
#' @param magn numeric. Defining the magnitude of the richness curve.
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param colpoints define the color of the occurrence-points. By default colpoints="#FFC12530". 
#' @param colpointborder define the color of the occurrence-points border. By default colpointborder="black".
#' @param rich.col define the color of the richness curve. By default rich.col="goldenrod1".
#' @param pch point symbol for plotting the occurences. By default pch=16 (filled circle).
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, 
#' such as main="my own title", main.col="red".
#' @return data frame with richness of rank and a plot of the continental masses 
#' with the occurrences and the latitudinal richness
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' pm_latrich (shape, data, rank="species", res=10)
#'}


pm_latrich <- function(shape, data, rank="species",
                       res=10, magn=1, 
                       colland="#66666680", colsea="#00509010", 
                       colpoints="#FFC12530",
                       colpointborder="black", rich.col="goldenrod1", pch=21, ...) {
  
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
  
  int_args <- base::list(x=shape, col="white", border = FALSE, main= "latitudinal richness" 
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
  #combine min,max lat and richness
  lr <- base::cbind(lr, richn)
  centros<- (base::seq(-90,90-res,res)+(base::seq(-90,90-res,res) + res))/2
  rich<- 185 + (lr$richn*magn)
  yy<- c(185, rich, 185)
  xx<- c(-90, centros, 90)
  
  do.call(raster::plot, arglist)
  graphics::rect(xleft=-180, xright=180, 
                 ybottom=-90, ytop=90, col=colsea, 
                 border=FALSE)
  raster::plot(shape, col=colland, border=FALSE, add=T)
  graphics::points(data2$paleolng, data2$paleolat, 
                   pch=pch, col=colpointborder, bg=colpoints)
  graphics::axis(1, xaxp=c(180,-180,4))
  graphics::axis(2, yaxp=c(90,-90,4))
  graphics::polygon (yy, xx, col=rich.col, border=F, xpd=T)
  #return latitudinal richness
  return(lr)
}

###################################pm_latdiv###################
#' pm_latdiv
#' 
#' calculates the Shannon diversity along the latitudinal gradient based on 
#' the individual values of diverstiy the fossil localities of those latitudes.
#' The function returns the mean or max values of diversity of the sampled 
#' localities along the latitudinal gradient.
#' 
#' @usage pm_latdiv (shape, occ_df, res=10, fun= max, magn=1,
#'                   colland="#66666680", colsea="#00509010",  
#'                   colpoints="#FFC12530",colpointborder="black", 
#'                   div.col="goldenrod1", pch=21, ...)
#' 
#' @param shape SpatialPolygonsDataFrame object containing a map.
#' @param occ_df data.frame with fossil occurrences. Can be created with pm_occ(data)
#' @param res numeric. Defining the spatial resolution. By default res=10. 
#' @param fun function or character. To determine what values to assign to cells that are covered by multiple spatial features. 
#' You can use functions such as min, max, or mean, or the character value: 'count'. 
#' @param magn numeric. Defining the magnitude of the diversity curve.
#' @param colland define the color of the land masses. By default colland = "#66666660".
#' @param colsea define the color of the sea. By default colsea = "#00509010".
#' @param colpoints define the color of the occurrence-points. By default colpoints="#FFC12530".
#' @param colpointborder define the color of the occurrence-points border. Bx default colpointborder="black".
#' @param div.col define the color od zje diversity curve. By default div.col="goldenrod1".
#' @param pch point symbol for plotting the occurences. By default pch=16 (filled circle).
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title", main.col="red"
#' @return data frame with shannon diversity,
#' a plot of the corresponding time map with the occurrences and their
#' latitudinal diversity
#' 
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df <- pm_occ (data)
#' pm_latdiv (shape, occ_df, fun=mean, magn=10)
#' pm_latdiv (shape, occ_df, fun=max, magn=5)
#'}

pm_latdiv <- function(shape, occ_df, res=10, 
                      fun= max, magn=1,
                      colland="#66666680", colsea="#00509010", 
                      colpoints="#FFC12530",
                      colpointborder="black", div.col="goldenrod1", pch=21, ...) {
  
  if(!.checkLatLng(occ_df)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkShape(shape)){
    stop("Shape is not a SpatialPolygonsDataFrame.")
  }
  if(!.checkFun(fun, "pm_latdiv")){
    stop(base::paste("\"", fun, "\" is not a valid input for parameter fun.", sep=""))
  }
  
  int_args <- base::list(x=shape, col="white", border = FALSE, main= "latitudinal diversity" 
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
  
  latmin <- base::seq(-90,80,res)
  latmax <- base::seq(-80,90,res)
  #create data frame withh latmin latmax and corrected richness
  lr <- base::data.frame(latmin, latmax, cornum)
  base::colnames(lr) <- c("maxlat", "minlat", "div")
  
  centros<- (base::seq(-90,90-res,res)+(base::seq(-90,90-res,res) + res))/2
  rich<- 185 + (lr$div*magn)
  yy<- c(185, rich, 185)
  xx<- c(-90, centros, 90)
  
  base::do.call(raster::plot, arglist)
  graphics::rect(xleft=-180, xright=180, 
       ybottom=-90, ytop=90, col=colsea, 
       border=FALSE)
  raster::plot(shape, col=colland, border=FALSE, 
        add=T)
  graphics::points(occ_df[, "paleolng"], occ_df[, "paleolat"], 
          pch=pch, col=colpointborder, 
          bg=colpoints)
  graphics::axis(1, xaxp=c(180,-180,4))
  graphics::axis(2, yaxp=c(90,-90,4))
  graphics::polygon(yy, xx, col=div.col, 
           border=F)
  #return latitudinal richness
  return(lr)
}
