####################formatdata#############################

#' formatdata
#' 
#' Changes columns names of your data.frame from a fossil database to the names needed for the functions in this package.
#' It also calculates the average age if not already given by the database.
#' 
#' @usage formatdata(df, db="pbdb")
#' 
#' @param df data.frame. Fossil data.
#' @param db character. Name of the database where the data is from.
#' @return data.frame
#' @export
#' @examples 
#' \dontrun{
#' 
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name="Canis", min_ma=0, max_ma=10, 
#'   show=c("coords", "phylo"), 
#'   vocab="pbdb", limit=100))
#' formatdata(data, db="pbdb")
#' 
#' }

formatdata <- function(df, db="pbdb"){
  
  if(db=="pbdb"){
    #####TODO: check if it has columns genus, family, order, class, phylum, early_age, late_age, matched_name, matched rank####
    
    avg_age <- (df$early_age + df$late_age) / 2
    
    species <- c()
    for(rank in 1:length(df$matched_rank)){
      if(df$matched_rank[rank]=="species"){
        species <- c(species, as.character(df$matched_name[rank][[1]]))
      }else{
        species <- c(species, NA)
      }
    }
    
    df <- cbind(df, species, avg_age)
    df <- df[order(-as.numeric(df$avg_age)), ]
    
  }
  
  return(df)
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
#' occ <- base::data.frame(paleobioDB::pbdb_occurrences(base_name="Canis", min_ma=0, max_ma=10, 
#' show=c("coords", "phylo"), 
#' vocab="pbdb", limit=100))
#' 
#' #reconstruct paleocoordinates with midpoint of appearance time
#' occ_ma <- paleoocoords(occ, recontime=NULL, model='SETON2012')
#' 
#' #reconstruct paleocoordinates with specific time
#' occ_matime <- paleoocoords(occ, recontime=5, model='SETON2012')
#' 
#'                     
#'}

####TODO: check if coordinates are inside range, if not add NA as paleocoords and not pass them to api####
paleocoords <- function(occ, time = "automatic", timevector=NULL, stepsize=10, model = 'SETON2012') {
  # if(!.checkRecon(occ, recontime)){
  #   if(is.null(recontime)){
  #     stop("Your data.frame needs to have columns called lng, lat, early_age and late_age.")
  #   }else{
  #     stop("Your data.frame needs to have columns called lng and lat.")
  #   }
  #   
  # }
  paleolng <- c()
  paleolat <- c()
  
  if(time=="average"){
    uma <- unique(round(occ$avg_age))

    
    for( i in 1:length(uma)){
      part <- occ[round(occ$avg_age)==uma[i], ]
      pts <- ""
      for( j in 1:length(part$avg_age)){
        pts <- paste0(pts,",", part$lng[j], ",", part$lat[j])
      }
      
      pts <- substring(pts, 2)

      url <- paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=",pts,"&time=",uma[i], "&model=",model, "&return_null_points")
      paleopts <- rjson::fromJSON(file=url)
      for (k in 1:length(paleopts$coordinates)){
        if(is.null(paleopts$coordinates[[k]])){
          paleolng <- c(paleolng, NA)
          paleolat <- c(paleolat, NA)
        }else{
          paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
          paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
        }
      }
    }
    recon_age <- round(occ$avg_age)
    occ <- cbind(occ, recon_age)
    
    
  }else if(time=="automatic"){
    #take min/max from early/late age as min and max from time bin (abgerundet/aufgerundet)
    #separate into bins of size stepsize
    #separate df into bins and take bin_age as request parameter.
    #set avg age to bin avg age
    min <- floor(min(occ$late_age))
    max <- ceiling(max(occ$early_age))
    ages <- seq(min, max, stepsize)
    #check if the stepsize is too big for min&max, if stepsize is not possible, just take min and max as borders and create one bin
    if(length(ages)==1){
      ages <- c(min, max)
    }
    #check if the max age is missing and add it to the ages for max border of last bin
    if((max-min)/stepsize != round((max-min)/stepsize)){
      ages <- c(ages, max)
    }
    bin_age <- ages[-length(ages)] + diff(ages)/2
    bin_ages <- c()
    for(i in 1:length(occ$avg_age)){
      for(j in 1:length(ages)-1){
        #check if >= ages j and <= ages j+1 --> then it is in bin_age[j] -> add bin_age[j] to bin_ages
        if(occ$avg_age[i] >= ages[j] && occ$avg_age[i]<= ages[j+1]){
          bin_ages <- c(bin_ages, bin_age[j])
        }
      }
    }

    recon_age <- bin_ages
    occ <- cbind(occ, recon_age)
    
    uma <- unique(recon_age)
    for( i in 1:length(uma)){
      part <- occ[occ$recon_age==uma[i], ]
      pts <- ""
      for( j in 1:length(part$recon_age)){
        pts <- paste0(pts,",", part$lng[j], ",", part$lat[j])
      }
      
      pts <- substring(pts, 2)
      
      url <- paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=",pts,"&time=",uma[i], "&model=",model, "&return_null_points")
      paleopts <- rjson::fromJSON(file=url)
      for (k in 1:length(paleopts$coordinates)){
        if(is.null(paleopts$coordinates[[k]])){
          paleolng <- c(paleolng, NA)
          paleolat <- c(paleolat, NA)
        }else{
          paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
          paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
        }
        
      }
    }
    
  }else if(time=="timevector"){
    #take midpoints of user specified bins for reconstruction
    #separate df into sets belonging to bins
    #calc for each bin
    
    bin_age <- timevector[-length(timevector)] + diff(timevector)/2
    bin_ages <- c()
    for(i in 1:length(occ$avg_age)){
      if(occ$avg_age[i] < min(timevector) || occ$avg_age[i] > max(timevector)){
        bin_ages <- c(bin_ages, NA)
      }else{
        for(j in 1:(length(timevector)-1)){
          #check if >= ages j and <= ages j+1 --> then it is in bin_age[j] -> add bin_age[j] to bin_ages
          if(occ$avg_age[i] >= timevector[j] && occ$avg_age[i]<= timevector[j+1]){
            bin_ages <- c(bin_ages, bin_age[j])
          }
        }
      }

    }
    recon_age <- bin_ages
    occ <- cbind(occ, recon_age)
    
    uma <- unique(recon_age)
    for( i in 1:length(uma)){
      if(is.na(uma[i])){

        part <- subset(occ, is.na(occ$recon_age))

        for( na in 1: length(part$recon_age)){
          paleolng <- c(paleolng, NA)
          paleolat <- c(paleolat, NA)
        }
      }else{

        part <- subset(occ, occ$recon_age==uma[i])
        pts <- ""
        for( j in 1:length(part$recon_age)){
          pts <- paste0(pts,",", part$lng[j], ",", part$lat[j])
        }

        pts <- substring(pts, 2)
        url <- paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=",pts,"&time=",uma[i], "&model=",model, "&return_null_points")
        paleopts <- rjson::fromJSON(file=url)
        for (k in 1:length(paleopts$coordinates)){
          if(is.null(paleopts$coordinates[[k]])){
            paleolng <- c(paleolng, NA)
            paleolat <- c(paleolat, NA)
          }else{
            paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
            paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
          }
        }
      }
      
    }
    
    
  }


  occ <- cbind(occ, paleolng, paleolat)
  num_reconage <- unique(na.omit(occ$recon_age))
  if(length(num_reconage)>1){
    ages <- c()
    for(i in 1:length(num_reconage)){
      ages <- paste0(ages, ", ", num_reconage[i], "mya ")
    }
    warning(paste0("You can not plot all of these points in a single map. You have ", length(num_reconage)," different maps, which are ", substring(ages, 2), "."))
  }
  
  return(occ)
}

###########################getmap#############################

#' getmap
#' 
#' Gets a map of a specific age from a model specified by the user.
#' Available models and ages at https://github.com/GPlates/gplates_web_service_doc/wiki/Reconstruction-Models .
#' 
#' @usage getmap(ma, model = 'SETON2012', show.plates = FALSE, colland, colsea, do.plot, ...)
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
#' map <- getmap(100, model = 'SETON2012', show.plates = T)
#' coastlines <- map[[1]]
#' plates <- map[[2]]
#' 
#' #without continental plates
#' coastlines <- getmap(100, model = 'SETON2012')
#' 
#'#save map as tiff image
#' tiff("getmap-GPlates_Cretaceous.tiff", 
#'       height = 10, width = 17, units = "cm", res = 300)
#' getmap(100, model = 'SETON2012', show.plates = T)
#' dev.off()
#' 
#'}

getmap <- function(ma, model = 'SETON2012', show.plates = FALSE, colland = "#66666660", 
                   colsea = "#00509010", 
                   do.plot = TRUE, ...) {
  url <- paste0("http://gws.gplates.org/reconstruct/coastlines/?time=",ma,"&model=",model)
  err <- FALSE
  shape <- tryCatch(
    {
      rgdal::readOGR(url, verbose = FALSE)
    }, error = function(e) {
      err <- TRUE
      message(paste0("There is no map for ", ma, " mya in ", model, " model. Please check the spelling, the age and the model you choose."))
      stop()
    }
  )
  
  errplate <- FALSE
  if(show.plates){
    #no plate bounds for paleomap
    plateurl <- paste0("http://gws.gplates.org/topology/plate_boundaries/?time=", ma, "&model=", model)
    platebounds <- tryCatch(
      {
        rgdal::readOGR(plateurl, verbose = FALSE)
      }, error = function(e){
        errplate <- TRUE
        message(paste0("No Plate Boundaries available for ", ma, " mya in ", model, "model. Please check the spelling, the age and the model you choose."))
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


####################mapast#################################
#####TODO: ??add parameter if take avg_age or other column#####
#' mapast
#' 
#' Plots your fossil occurrence data from the paleobioDB onto the map of the selected time interval.
#' 
#' 
#' 
#' @usage mapast(interval, model, data, colland = "#66666660",
#'                 colsea = "#00509010",colpoints = "#65432190", 
#'                 pch = 16, cex = 1, ...)
#' 
#' @param model character. Defining the model the map should be created with. 'SETON2012' (default), 
#' 'MULLER2016', 'GOLONKA', 'PALEOMAP' or 'MATTHEWS2016'.
#' @param data data.frame. Fossil occurrences data.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param save.as character. Defines the format the plots should be saved. "tiff" or "pdf".
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
#' library(mapast)
#' 
#' #get data
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name="Canis", min_ma=0, max_ma=10, 
#' show=c("coords", "phylo"), 
#' vocab="pbdb", limit=100))
#' #create a plot with fossils on the paleogeographical map
#' mapast(ma=5 , model = "SETON2012", data)
#' 
#' #save plot as pdf file
#' pdf("mapast-GPlates_Cretaceous-Mammalia.pdf")
#' mapast(interval = "Cretaceous", model = "GPlates", data)
#' dev.off()
#' 
#' #save plot as tiff image
#' tiff("mapast-GPlates_Cretaceous-Mammalia.tiff", 
#'       height = 10.5, width = 17, units = "cm", res = 300)
#' mapast(interval = "Cretaceous", model = "GPlates", data)
#' dev.off()
#' 
#'}

mapast <- function(model="SETON2012", data, do.plot=TRUE, save.as="tiff",
                    colland = "#66666660",
                    colsea = "#00509010", 
                    colpoints = "#65432190", 
                    pch = 16, cex = 1, ...) {
  
  #check if inut data has needed columns (paleolat/paleolng)
  # if(!.checkLatLng(data)){
  #   stop("Column/s paleolat and/or paleolng are missing in the input data.")
  # }
  # if(!.checkRange(data)){
  #   stop("Range of Latitude and/or Longitude is not allowed.")
  # }
  
  #count how many maps will be created
  #print warning
  num_recon <- length(unique(na.omit(data$recon_age)))
  print(paste0("You have ", num_recon," reconstruction times (meaning ", num_recon," maps). This is going to take about ",num_recon," minutes."))
  #through revcon_age -> for each one map and the corresponding points#
  #getting the shape file with getmap
  uage <- unique(data$recon_age)
  for(age in 1:length(uage)){
    curma <- uage[age]
    
    subdata <- subset(data, data$recon_age == curma)
    
    
    shape <- getmap(ma=curma, model = model, show.plates=FALSE, do.plot = FALSE)
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
    #save old mar settings and define plotting margins as we need
    def.mar <- graphics::par("mar")
    graphics::par(mar = c(1.5, 1.5, 2, 1.5))
    #plotting the map and the data
    #input data needs to be a data frame
    if (base::class(data) == "data.frame") {
      if(do.plot){
        if(save.as=="tiff"){
          tiff(paste0("mapast-",curma,"mya_",model,".tiff"), 
               height = 10.5, width = 17, units = "cm", res = 300)
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
          # shape.info <- .getShapeInfo(shape)
          #add name, model and age at the top right of the plot
          graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
          graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = paste0(curma, " mya"), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          # graphics::axis(side = 3, pos = 81, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          #add the fossil occurrences to the plot
          graphics::points(subdata$paleolng, 
                           subdata$paleolat, 
                           pch = pch, col = colpoints, 
                           cex = cex)
          
          dev.off()
        }
        #not working
        if(save.as=="pdf"){
          filename <- paste0("mapast-",curma,"mya_",model,".pdf")
          pdf(filename)
          #plot with the parameter list which includes users graphical parameter
          #defines size and axes of the plot
          map <- base::do.call(sp::plot, graphparams)
          #draw the rectangle showing the sea
          map <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
                         ytop = 90, col = colsea, 
                         border = FALSE)
          #plot the landmasses on the sea
          map <- sp::plot(shape, col = colland, border = FALSE, add = TRUE)
          # add x-axis and x-axis labels
          map <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
          map <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
          # add y-axis and y-axis labels
          map <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
          map <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
          #get metadata from the SpatialPolygonDataFrame
          # shape.info <- .getShapeInfo(shape)
          #add name, model and age at the top right of the plot
          map <- graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
          map <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = paste0(curma, " mya"), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          # graphics::axis(side = 3, pos = 81, lwd = 0, at = 135, labels = paste(shape.info[3], " - ", shape.info[4], " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          #add the fossil occurrences to the plot
          map <-graphics::points(subdata$paleolng, 
                           subdata$paleolat, 
                           pch = pch, col = colpoints, 
                           cex = cex)
          print(map)
          
          dev.off()
        }
      }
      


      #restore the old margin values
      graphics::par(mar = def.mar)

      

    }
  }
}

#####################mapocc##############################
#####TODO: if shape input, could be a list of shapes or a shape#####
#####shape_ages as parameter where age of maps is given (by the user), so functions takes this map if it is there, otherwise get map from db#####
#' mapocc
#' 
#' Creates a RasterLayer, containing the number of occurrences per cell, and a plot of the fossil 
#' occurences by taxonomic rank per cell (a proxy for the sampling effort).
#' 
#' @usage mapocc(data, rank = "genus", res = 1,
#'                     colland = "#66666660", colsea = "#00509010", col.grid = mycols(100), 
#'                     do.plot = TRUE, ...)
#' 
#' @param data data.frame. Fossil occurrences data. Can be created with 
#' getdata(interval, base_name).
#' @param model character. Defining the model the map should be created with. 'SETON2012' (default), 
#' 'MULLER2016', 'GOLONKA', 'PALEOMAP' or 'MATTHEWS2016'.
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum".
#' @param res numeric. Defining the spatial resolution. Default res = 1. 
#' @param save.as character. Defines the format the plots should be saved. "tiff" or "pdf".
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
#' library(mapast)
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

mapocc <- function(data, model="SETON2012",
                         rank = "genus", 
                         res=1, save.as="tiff",
                         colland = "#66666660",
                         colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...) {
  #check user input
  #check if data has latitude and longitude columns
  # if(!.checkLatLng(data)){
  #   stop("Column/s paleolat and/or paleolng are missing in the input data.")
  # }
  # if(!.checkRange(data)){
  #   stop("Range of Latitude and/or Longitude is not allowed.")
  # }
  # #check if the rank is allowed
  # if(!.checkRank(rank)){
  #   stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep = ""))
  # }
  # #check if the needed column for the chosen rank is in the data frame
  # if(!.checkDataRank(data,rank)){
  #   if(rank == "species"){
  #     stop(base::paste("There is no column matched_name in the data frame.", sep = ""))
  #   }else{
  #     stop(base::paste("There is no column ", rank,  " in the data frame.", sep = ""))
  #   }
  # }
  # #check if the shape is a SpatialPolygonsDataFrame
  # if(!.checkShape(shape)){
  #   stop("Shape is not a SpatialPolygonsDataFrame.")
  # }
  
  #count how many maps will be created
  #print warning
  num_recon <- length(unique(na.omit(data$recon_age)))
  print(paste0("You have ", num_recon," reconstruction times (meaning ", num_recon," maps). This is going to take about ",num_recon," minutes."))
  #through revcon_age -> for each one map and the corresponding points#
  #getting the shape file with getmap
  occraster <- c()
  uage <- unique(data$recon_age)
  for(age in 1:length(uage)){
    curma <- uage[age]
    
    subdata <- subset(data, data$recon_age == curma)
    #filter data for rank
    rankdata <- .rfilter(subdata, rank)
    #creating a raster in the size of the shape
    spatialext <- raster::extent(c(-180, 180, -90, 90))
    ras <- raster::raster(spatialext, res = res)
    #create a raster of the occurences (sampling effort)
    curoccraster <- raster::rasterize(rankdata[ , c("paleolng", "paleolat")], ras, field = rankdata[ , rank], fun = "count")
    occraster <- c(occraster, curoccraster)
    shape <- getmap(ma=curma, model = model, show.plates=FALSE, do.plot = FALSE)
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
      if(save.as=="tiff"){
        tiff(paste0("mapocc-",curma,"mya_",model,".tiff"), 
             height = 10.5, width = 17, units = "cm", res = 300)
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
        #
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the raster to the plot without legend
        raster::plot(curoccraster, add = T,axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
        #allow the plot to expand the borders
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add the raster legend outside the plot
        raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA),
                     legend.args = list(text = "occurrences", line = 1, side = 3, adj = 0.25, cex = 0.6, col = col.grid[length(col.grid) / 2]))
        raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = col.grid[length(col.grid) / 2]))
        graphics::par(bty = "o")
        dev.off()
      }
      if(save.as=="pdf"){
        filename <- paste0("mapocc-",curma,"mya_",model,".pdf")
        pdf(filename)
        #create a plot with the users parameters
        map <- base::do.call(raster::plot, graphparams)
        #create a rectangle showing the sea
        map <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #plot the landmasses on the sea
        map <- raster::plot(shape, col = colland, border = FALSE, add = T)
        #add x-axis and x-axis labels
        map <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        map <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        map <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        map <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        #
        map <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        map <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the raster to the plot without legend
        map <- raster::plot(curoccraster, add = T,axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
        #allow the plot to expand the borders
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add the raster legend outside the plot
        map <- raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA),
                     legend.args = list(text = "occurrences", line = 1, side = 3, adj = 0.25, cex = 0.6, col = col.grid[length(col.grid) / 2]))
        map <- raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = col.grid[length(col.grid) / 2]))
        graphics::par(bty = "o")
        print(map)
        dev.off()
      }
     
      #restore default margin settings
      graphics::par(mar = def.mar)
    }
  
  }
  occstack <- raster::stack(occraster)
  #return the raster
  return(occstack)
}

#####################maprich####################
#####TODO: Let user give shape, could be a list of shapes or a shape#####
#### also shape_ages as input, only getting the maps that are not in the list####
#' maprich
#' 
#' Creates a RasterLayer of taxon richness
#' and makes a plot of the map and richness raster.
#' 
#' @usage maprich(shape, data, rank = "genus", res = 1, 
#'                      colland = "#66666660", colsea = "#00509010", col.grid = mycols(100), 
#'                      do.plot = TRUE, ...)
#' 
#' @param data data.frame. Fossil occurrences data. Can be created with 
#' getdata(interval, base_name).
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank="genus".
#' @param res numeric. Defining the spatial resolution. Default res = 1. 
#' @param model character. Defining the model the map should be created with. 'SETON2012' (default), 
#' 'MULLER2016', 'GOLONKA', 'PALEOMAP' or 'MATTHEWS2016'.
#' @param save.as character. Defines the format the plots should be saved. "tiff" or "pdf".
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
#' library(mapast)
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

maprich <- function (data, rank = "genus", res = 1, model="SETON2012", save.as="tiff",
                           colland = "#66666660",
                           colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...) {
  #check the users input data
  #check if the lat/lng column is in the data frame
  # if(!.checkLatLng(data)){
  #   stop("Column/s paleolat and/or paleolng are missing in the input data.")
  # }
  # if(!.checkRange(data)){
  #   stop("Range of Latitude and/or Longitude is not allowed.")
  # }
  # #check if the rank is allowed
  # if(!.checkRank(rank)){
  #   stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep = ""))
  # }
  # #check if the columns according to the rank is inside the data frame
  # if(!.checkDataRank(data, rank)){
  #   if(rank == "species"){
  #     stop(base::paste("There is no column matched_name in the data frame.", sep = ""))
  #   }else{
  #     stop(base::paste("There is no column ", rank, " in the data frame.", sep = ""))
  #   }
  # }
  # #check if the shape is a SpatialPolygonsDataFrame
  # if(!.checkShape(shape)){
  #   stop("Shape is not a SpatialPolygonsDataFrame.")
  # }
  # #check if taxon_no is in data frame
  # if(!.checkDataNo(data, rank)){
  #   stop(base::paste0("Column ", rank, "_no is missing in the data frame"))
  # }
  
  #count how many maps will be created
  #print warning
  num_recon <- length(unique(na.omit(data$recon_age)))
  print(paste0("You have ", num_recon," reconstruction times (meaning ", num_recon," maps). This is going to take about ",num_recon," minutes."))
  
  #creating a raster in size of the shape file
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
  uage <- unique(data$recon_age)
  richlist <- c()
  for(age in 1:length(uage)){
    curma <- uage[age]
    
    subdata <- subset(data, data$recon_age == curma)
    shape <- getmap(ma=curma, model = model, show.plates=FALSE, do.plot = FALSE)
    #getting the raster of the species richness
    richraster <- .rank_filter(ras, subdata, res = res, rank)
    richlist <- c(richlist, richraster)
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
      if(save.as=="tiff"){
        tiff(paste0("maprich-",curma,"mya_",model,".tiff"), 
             height = 10.5, width = 17, units = "cm", res = 300)
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
        
        #add name, model and age at the top rigt of the plot
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        
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
        dev.off()
      }
      if(save.as=="pdf"){
        filename <- paste0("maprich-",curma,"mya_",model,".pdf")
        pdf(filename)
        #plot with the default and user defined graphical parameter
        map <- base::do.call(raster::plot, graphparams)
        #add a rectangle as the sea
        map <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #add the landmasses to the plot
        map <- raster::plot(shape, col = colland, border = FALSE, add = T, bty = "L")
        #add x-axis and x-axis labels
        map <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        map <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        map <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        map <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        #get metadata from the shape file
        
        #add name, model and age at the top rigt of the plot
        map <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        map <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        
        #add the raster without legend
        map <- raster::plot(richraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
        #allow to draw outside the plot
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add raster legend outside the plot
        map <- raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                     legend.args = list(text = "richness", line = 1, side = 3, adj = 0.25, cex = 0.6, col = col.grid[length(col.grid) / 2]))
        map <- raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = col.grid[length(col.grid) / 2]))
        graphics::par(bty = "o")
        print(map)
        dev.off()
      }
      
      #restore prior margin values
      graphics::par(mar = def.mar)
    }
  
  }
  rasterstack <- raster::stack(richlist)
  #return the raster
  return(rasterstack)
}

########spsite###################
####TODO: calc for eacht time bin and return list of df's####
#' spsite
#' 
#' Generates a diversity data.frame, with the number occurrences of a taxon per locality.
#' 
#' @usage spsite(data, unity, res = 1, rank = "genus", pa = FALSE)
#' 
#' @param data data.frame. Fossil occurrences data.
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
#' library(mapast)
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
  # #check users input data
  # #check if lat/lng columns are in the data frame
  # if(!.checkLatLng(data)){
  #   stop("Column/s paleolat and/or paleolng are missing in the input data.")
  # }
  # if(!.checkRange(data)){
  #   stop("Range of Latitude and/or Longitude is not allowed.")
  # }
  # #check if the chosen rank is allowed
  # if(!.checkRank(rank)){
  #   stop(base::paste("Rank: \"", rank, "\" is not a valid rank.", sep = ""))
  # }
  # #check if the column belonging to the rank is in the data frame
  # if(!.checkDataRank(data,rank)){
  #   if(rank == "species"){
  #     stop(base::paste("There is no column matched_name in the data frame.", sep = ""))
  #   }else{
  #     stop(base::paste("There is no column ", rank, " in the data frame.", sep=""))
  #   }
  # }
  
  #count how many maps will be created
  #print warning
  
  if(unity == "fossilsite"){
    numdf <- 1
    dflist <- list()
    uage <- unique(data$recon_age)
    for(age in 1:length(uage)){
      curma <- uage[age]
      
      subdata <- subset(data, data$recon_age == curma)
      
      #filter data for the rank
      rankdata <- .rfilter(data, rank)
      #create a data. frame with all the locations once
      latlng <- base::data.frame(paleolng = rankdata$paleolng, paleolat = rankdata$paleolat)
      ulatlng <- base::unique(latlng)
      #getting list of unique taxa
      if(rank == "species"){
        urank <- base::as.vector(base::unique(rankdata[ , "species"]))
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
            cur.taxon <- base::subset(curlatlng, curlatlng[ , "species"] == taxon_cur)
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
      dflist[[numdf]] <- occ
      numdf <- numdf + 1
    }
    
    #return the data.frame
    return(dflist)
  }
  if(unity == "cell"){
    dflist <- list()
    numdf <- 1
    uage <- unique(data$recon_age)
    for(age in 1:length(uage)){
      curma <- uage[age]
      
      subdata <- subset(data, data$recon_age == curma)
      if(rank == "species"){
        subdata <- subdata[which(!is.na(subdata$species)), ]
      }else if(rank == "genus"){
        subdata <- subdata[which(!is.na(subdata$genus)), ]
      }else if(rank == "order"){
        subdata <- subdata[which(!is.na(subdata$order)), ]
      }else if(rank == "family"){
        subdata <- subdata[which(!is.na(subdata$family)), ]
      }else if(rank == "class"){
        subdata <- subdata[which(!is.na(subdata$class)), ]
      }else{
        subdata <- subdata[which(!is.na(subdata$phylum)), ]
      }
      #remove NA columns that don't have the chosen rank
      subdata <- subdata[!is.na(subdata[[rank]]), ]
      #only getting occurences with a known genus
      rankdata <- .rfilter(subdata, rank)
      #getting list of unique taxa
      if(rank == "species"){
        urank <- base::as.vector(base::unique(rankdata[ , "species"]))
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
      rankcol <- rank
      #getting the number of occurrences of a taxa for each locality
      latbord <- seq(90, -90, -res)
      for(curocc in 1:length(subdata$paleolng)){
        curtaxon <- as.character(subdata[[rankcol]][curocc])
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
      dflist[[numdf]] <- (base::as.data.frame(occ))
      numdf <- numdf + 1
      
    }
    

    return(dflist)
  }
  
}



#####################mapdiv####################
####TODO: add Shape which can be a list of shapes or a single shape (should be same size as number of bins)####
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
#' library(mapast)
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

mapdiv <- function(data, unity, rank = "genus", res = 1, fun = mean, model="SETON2012",
                   colland = "#66666660", colsea = "#00509010", col.grid = mycols(100), 
                   do.plot = TRUE, save.as="tiff", ...) {
  #check user input data
  # if(!.checkShape(shape)){
  #   stop("Shape is not a SpatialPolygonsDataFrame.")
  # }
  
  
  #creating a raster in size of the shape file
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
 
  uage <- unique(data$recon_age)
  divlist <- c()
  for(age in 1:length(uage)){
    curma <- uage[age]
    
    subdata <- subset(data, data$recon_age == curma)
    
    
    if(unity == "cell"){
      # occ_df_cell <- mapast::spsite(subdata, unity = unity, res = res, rank = rank)
      occ_df_cell <- spsite(subdata, unity = unity, res = res, rank = rank)
      occ_df_cell <- occ_df_cell[[1]]
      print(occ_df_cell)
      #remove lat and lng from data frame
      # drops <- c("paleolat", "paleolng")
      # rawocc <- occ_df_cell[ , !(base::names(occ_df_cell) %in% drops)]
      rawocc <- subset(occ_df_cell, select = -c(paleolng,paleolat) )
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
      div.df <- base::data.frame(paleolat = occ_df_cell$paleolat, paleolng = occ_df_cell$paleolng, genus = occurrences, recon_age= base::rep(1, base::length(occ_df_cell$paleolng)))
      # base::colnames(div.df)[5] <- "recon_age"
      # div_cell <- mapast::spsite(div.df, unity = "cell", res = res, rank = "genus")
      div_cell <- spsite(div.df, unity = "cell", res = res, rank = "genus")
      div_cell <- div_cell[[1]]
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
      
      # occ_df <- mapast::spsite(subdata, unity = unity, res = res, rank = rank)
      occ_df <- spsite(subdata, unity = unity, res = res, rank = rank)
      occ_df <- occ_df[[1]]
      # drops <- c("paleolat", "paleolng")
      # rawocc <- base::data.frame(occ_df[ , !(base::names(occ_df) %in% drops)])
      rawocc <- subset(occ_df, select = -c(paleolng,paleolat) )
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
      div.df <-base::cbind(div.df, base::rep(1, base::length(divlatlng[ , 1])))
      base::colnames(div.df)[5] <- "recon_age"
      # div_cell <- mapast::spsite(div.df, unity = "cell", res = res, rank = "genus")
      div_cell <- spsite(div.df, unity = "cell", res = res, rank = "genus")
      div_cell <- div_cell[[1]]
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
    
    shape <- getmap(ma=curma, model = model, show.plates=FALSE, do.plot = FALSE)
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
      
      if(save.as=="tiff"){
        tiff(paste0("mapdiv-",curma,"mya_",model,".tiff"), 
             height = 10.5, width = 17, units = "cm", res = 300)
        
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
        
        #add name, model, age at the top right of the plot
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
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
        
        dev.off()
      }
      if(save.as=="pdf"){
        filename <- paste0("mapdiv-",curma,"mya_",model,".pdf")
        pdf(filename)
        
        map <- base::do.call(raster::plot, graphparams)
        #add a rectangle defining the sea
        map <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #add the landmasses
        map <- raster::plot(shape, col = colland, border = FALSE, add = T)
        #add x-axis and x-axis labels
        map <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        map <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        map <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        map <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        
        #add name, model, age at the top right of the plot
        map <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        map <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the raster without legend
        map <- raster::plot(divraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE)
        #allow to expand the plot
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add legend outside the plot
        map <- raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), legend.args = list(text = "diversity", line = 1, side = 3, adj = 0.25, cex = 0.6, col = gridcol[length(gridcol) / 2]))
        map <- raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = gridcol[length(gridcol) / 2]))
        graphics::par(bty = "o")
        print(map)
        
        dev.off()
        
      }
      
      #restore prior margin settings
      graphics::par(mar = def.mar)
    }
    
    divlist <- c(divlist, divraster)
    
  }
  divstack <- raster::stack(divlist)
  #return the raster
  return(divstack)
}


###################################latdivgrad###################
####TODO: go through time bins, calc for every time bin, create a df with column for each time bin####
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
#' library(mapast)
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


latdivgrad <- function(data, method, rank = "genus",
                       res = 1, model="SETON2012",
                       colland = "#66666680", colsea = "#00509010", 
                       colpoints = "#65432190",
                       rich.col = "#654321", pch = 21, do.plot = TRUE, save.as="tiff",...) {
  #check the users input data
  # if(!.checkLatLng(data)){
  #   stop("Column/s paleolat and/or paleolng are missing in the input data.")
  # }
  # if(!.checkRange(data)){
  #   stop("Range of Latitude and/or Longitude is not allowed.")
  # }
  # if(!.checkRank(rank)){
  #   stop(base::paste("Rank: \"", rank, "\" is not a valid rank."))
  # }
  # if(!.checkDataRank(data,rank)){
  #   if(rank=="species"){
  #     stop(base::paste("There is no column matched_name in the data frame.", sep=""))
  #   }else{
  #     stop(base::paste("There is no column ", rank, " in the data frame.", sep=""))
  #   }
  # }
  # if(!.checkShape(shape)){
  #   stop("Shape is not a SpatialPolygonsDataFrame.")
  # }
  uage <- unique(data$recon_age)
  latdivlist <- list()
  numlist <- 1
  for(age in 1:length(uage)){
    curma <- uage[age]
    
    subdata <- subset(data, data$recon_age == curma)
    
    shape <- getmap(ma=curma, model = model, show.plates=FALSE, do.plot = FALSE)
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
      rankdata <-.rfilter(subdata, rank)
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
      # rankdata <- mapast::spsite(data, unity = "fossilsite", res = res, rank = rank)
      rankdata <- spsite(data, unity = "fossilsite", res = res, rank = rank)
      rankdata <- rankdata[[1]]
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
      
      if(save.as=="tiff"){
        tiff(paste0("latdivgrad-",curma,"mya_",model,".tiff"), 
             height = 9, width = 17.5, units = "cm", res = 300)
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
        #add name, model and age at the top right of the plot
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
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
        dev.off()
      }
      if(save.as=="pdf"){
        filename <- paste0("latdivgrad-",curma,"mya_",model,".pdf")
        pdf(filename, height = 10, width = 25, paper='special')
        def.mar <- graphics::par("mar")
        graphics::par(mar = c(2,0,10,20), xpd = NA)
        
        #create a plot with the parameter list
        map <- do.call(raster::plot, graphparams)
        #add a rectangle as the sea
        map <- graphics::rect(xleft = -180, xright = 180, 
                       ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #add the landmasses to the plot
        map <- raster::plot(shape, col = colland, border = FALSE, add = T)
        #add the fossil occurrences to the plot
        map <- graphics::points(rankdata$paleolng, rankdata$paleolat, 
                         pch = pch, col = NA, bg = colpoints)
        #add x-axis and x-axis labels
        map <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
        map <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        map <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        map <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
        #add name, model and age at the top right of the plot
        map <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
        map <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        if(!is.nan(base::min(yrich))){
          #add the richness curve at the right side of the plot
          map <- graphics::polygon (yrich, xrich, col = rich.col, border = F, xpd = T)
          #get the parameters for the richness axis
          map <- ax_seq <- base::seq(base::min(yrich), base::max(yrich), ((base::max(yrich) - base::min(yrich)) / 2))
          map <- ax_lab <- ax_seq - 180
          map <- ax_lab <- base::round(ax_lab / magn, 2)
          #add the richness axes
          map <- graphics::axis(side = 3, pos = 90, lwd = 1, xpd = TRUE, at = ax_seq, labels = FALSE , col.ticks = rich.col ,col.axis = rich.col , col = rich.col , cex.axis = 0.6, tck = -0.01)
          map <- graphics::axis(side = 3, pos = 80, lwd = 0, xpd = TRUE, at = ax_seq, labels = ax_lab , col.ticks = rich.col ,col.axis = rich.col , col = rich.col , cex.axis = 0.6, tck = -0.01)
          map <- graphics::axis(side = 3, pos = 90, lwd = 0, xpd = TRUE, at = ax_seq[round(length(ax_seq) / 2)], labels = method, col.ticks = rich.col, col.axis = rich.col, col = rich.col, cex.axis = 0.8, tck = -0.01, cex.lab = 0.8)  
        }
        print(map)
        dev.off()
        
      }
      
      #restore the prior margin settings
      graphics::par(mar = def.mar)
    }
    #return latitudinal richness
    latdiv <- latdiv[base::length(latdiv$paleolat):1, ]
    latdivlist[[numlist]] <- latdiv
    numlist <- numlist + 1
    
  }
  

  return(latdivlist)
}





