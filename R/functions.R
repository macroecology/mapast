####################formatdata#############################
#' formatdata
#' 
#' Changes and adds columns to your fossil occurrences data.frame from the Paleobiology Database to the names needed for the functions in this package.
#' It also calculates the average age of the fossil occurrences from the data.frame columns early_age and late_age.
#' 
#' @usage formatdata(data, db = "pbdb")
#' 
#' @param data data.frame. Fossil occurrences data from the Paleoiology Database.
#' @param db character. Name of the database where the data is from. Only db = "pbdb" possible.
#' @return data.frame
#' @export
#' @examples 
#' \dontrun{
#' 
#' library(mapast)
#' 
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(limit = 100, base_name = "Canis", 
#'   min_ma = 0, max_ma = 10, 
#'   show = c("coords", "phylo"), 
#'   vocab = "pbdb"))
#'   
#' fdata <- formatdata(data, db = "pbdb")
#' 
#' }
formatdata <- function(data, db = "pbdb"){
  # check if paleobiodb is given as database
  # at the moment only pbdb possible
  if(db == "pbdb"){
    #calculate the average age from early_age and late_age of the fossils
    avg_age <- (data$early_age + data$late_age) / 2
    #create a species column (not given by paleobiodb)
    species <- c()
    #go through matched_rank, if it is species add the matched name to the species column
    #if mathced rank is not species, species of the fossil is not known -> add NA
    for(rank in 1:base::length(data$matched_rank)){
      if(data$matched_rank[rank] == "species"){
        species <- c(species, base::as.character(data$matched_name[rank][[1]]))
      }else{
        species <- c(species, NA)
      }
    }
    #add average_age of the fossil and species to the data.frame
    data <- base::cbind(data, species, avg_age)
    #sort data.frame by average age
    data <- data[base::order(-base::as.numeric(data$avg_age)), ]
  }else{
    # if user types in other db -> print message that at the moment only paleobiodb is possible
    base::print("Sorry, function is at the moment only available for db = \"pbdb\".")
  }
  #return the data.frame with species and average_age columns
  return(data)
}
################paleocoords##############################
#' paleocoords
#' 
#' Calculating the paleocoordinates of the fossil data. Either calculating timebins by looking at the early_age and late_age column,
#' taking the rounded average age (avg_age) of the fossils or by using user defined time bins.
#'  
#' @usage paleocoords(data, time = "automatic", timevector = NULL, 
#'                         stepsize = 10, model = "SETON2012")
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param time character. Defines how the reconstruction time is specified. Can be "automatic", "average" or "timevector". By default time = "automatic".
#' @param timevector vector. The borders of the time bins. Not allowed to be NULL if time = "timevector".
#' @param stepsize numeric. The stepsize of the time bins if time = "automatic". By default stepsize = 10.
#' @param model character. The model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @return data.frame
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(mapast)
#' 
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                        min_ma = 0, max_ma = 10, 
#'                                                        show = c("coords", "phylo"), 
#'                                                        vocab = "pbdb", limit = 100))
#' df <- formatdata(data = data)
#'
#' #reconstruct paleocoordinates with midpoint of appearance time
#' occ_ma <- paleocoords(data = df, time = "automatic", model = "SETON2012")
#'
#' #reconstruct paleocoordinates with specific time
#' occ_matime <- paleocoords(data = df, time = "timevector", timevector = c(0.10), 
#'                          model = "SETON2012")
#' 
#'                     
#'}
paleocoords <- function(data, time = "automatic", timevector=NULL, stepsize=10, model = "SETON2012") {
  #remove data with lat or lng outside of range
  #data2: data.frame for saving occurrences with lat/lng outside of range
  data2 <- NULL
  data2 <- base::rbind(data[base::which(data$lng < -180), ], data[base::which(data$lng > 180), ], data[base::which(data$lat < -90), ], data[base::which(data$lat > 90), ])
  # count number of fossils with incorrect lat/lng and print note for user
  if(base::nrow(data2) > 0){
    data2[ ,"recon_age"] <- NA
    data2[ ,"paleolng"] <- NA
    data2[ ,"paleolat"] <- NA
    base::print(base::paste0("There are ", base::length(data2$lng), " points out of the lat long boundaries (-180, 180, -90, 90). Those points can not be projected into the past. Please, check them and correct them in order to be able to project them correctly into the past."))
    #save only fossils with lat/lng inside range to go on with reconstructing the paleocoordinates
    data <- base::rbind(data[base::which(data$lng >= -180), ], data[base::which(data$lng < 180), ], data[base::which(data$lat >= -90), ], data[base::which(data$lat < 90), ])
  }
  #vectors for saving paleolat and paleolng
  paleolng <- c()
  paleolat <- c()
  #time = "average"
  #take the rounded average_age as reconstruction time
  #rounded to reduce number of maps
  if(time == "average"){
    #calculate the reconstruction age: rounded average age of the fossil
    recon_age <- base::round(data$avg_age)
    #add the reconstruction age to the data.frame
    data <- base::cbind(data, recon_age)
    #list of reconstruction ages
    uma <- base::unique(recon_age)
    #go through the reconstruction ages
    for( i in 1:base::length(uma)){
      lnglat <- .lnglat(data, uma[i], model,  paleolng, paleolat)
      paleolng <- c(lnglat$paleolng)
      paleolat <- c(lnglat$paleolat)
    }
  }else if(time == "automatic"){
    #take min/max from early/late age as min and max from time bin (abgerundet/aufgerundet)
    #separate into bins of size stepsize
    #separate df into bins and take bin_age as request parameter.
    #set avg age to bin avg age
    #get the latest age
    min <- base::floor(base::min(data$late_age))
    #get the earliest age
    max <- base::ceiling(base::max(data$early_age))
    #create a sequence from latest to earliest age with steps of the defined stepsize (default = 10)
    ages <- base::seq(min, max, stepsize)
    #check if the stepsize is too big for min&max, if stepsize is not possible, just take min and max as borders and create one bin
    if(base::length(ages) == 1){
      ages <- c(min, max)
    }
    #check if the max age is missing in the bin and add it to the age sequence for max border of last bin
    if((max - min) / stepsize != base::round((max - min) / stepsize)){
      ages <- c(ages, max)
    }
    #bin_age: vector of midpoints of the age sequence
    bin_age <- ages[-base::length(ages)] + diff(ages) / 2
    #bin_ages: vector to save for each average age in which bin it is
    bin_ages <- c()
    for(i in 1:base::length(data$avg_age)){
      for(j in 1:(base::length(ages) - 1)){
        #check if >= ages[j] and <= ages[j+1] --> then it is in bin_age[j] -> add bin_age[j] to bin_ages
        if(data$avg_age[i] >= ages[j] && data$avg_age[i] <= ages[j+1]){
          bin_ages <- c(bin_ages, bin_age[j])
        }
      }
    }
    #recon_age is the age of the bin where the avg_age is inside.
    recon_age <- bin_ages
    #add recon_age to the data.frame
    data <- base::cbind(data, recon_age)
    #uma: unique reconstruction ages
    uma <- base::unique(recon_age)
    #go through reconsruction ages
    for( i in 1:base::length(uma)){
      lnglat <- .lnglat(data, uma[i], model,  paleolng, paleolat)
      paleolng <- c(lnglat$paleolng)
      paleolat <- c(lnglat$paleolat)
    }
  }else if(time == "timevector"){
    #take midpoints of user specified bins for reconstruction
    #separate df into sets belonging to bins
    #calc for each bin
    #bin_age: midpoints of the time bins (reconstruction times)
    bin_age <- timevector[-base::length(timevector)] + diff(timevector) / 2
    #bin_ages: vector for saving the reconstruction age for the fossils
    bin_ages <- c()
    #go through the average ages of the fossils
    for(i in 1:base::length(data$avg_age)){
      #if average age is bigger than the given max age, or the given minimum age, put a NA in reconstruction age. This point will not be reconstructed
      if(data$avg_age[i] < min(timevector) || data$avg_age[i] > max(timevector)){
        bin_ages <- c(bin_ages, NA)
      }else{
        #look in which timebin the avg_age falls and save this age in bin_ages as reconstruction age
        for(j in 1:(base::length(timevector) - 1)){
          #check if >= ages j and <= ages j+1 --> then it is in bin_age[j] -> add bin_age[j] to bin_ages
          if(data$avg_age[i] >= timevector[j] && data$avg_age[i] <= timevector[j + 1]){
            bin_ages <- c(bin_ages, bin_age[j])
          }
        }
      }
    }
    #save the bin_ages as recon_age in the data.frame
    recon_age <- bin_ages
    data <- base::cbind(data, recon_age)
    #uma: unique reconstruction ages
    uma <- base::unique(recon_age)
    #go through the reconstruction ages
    for( i in 1:base::length(uma)){
      #for all fossils which are not in the time range of the reconstruction ages save NA as paleolat and paleolng
      if(base::is.na(uma[i])){
        part <- base::subset(data, base::is.na(data$recon_age))
        for(na in 1:base::length(part$recon_age)){
          paleolng <- c(paleolng, NA)
          paleolat <- c(paleolat, NA)
        }
      }else{
        lnglat <- .lnglat(data, uma[i], model,  paleolng, paleolat)
        paleolng <- c(lnglat$paleolng)
        paleolat <- c(lnglat$paleolat)
      }
    }
  }
  #add reconstructed paleocoordinates to the data.frame
  data <- base::cbind(data, paleolng, paleolat)
  #get the number of different reconstruction ages
  num_reconage <- base::unique(stats::na.omit(data$recon_age))
  #if there is more than one reconstruction age, save list of reconstruction ages and give the user a note that there are several ages
  if(base::length(num_reconage)>1){
    #ages: list of different reconstruction ages
    ages <- c()
    for(i in 1:base::length(num_reconage)){
      ages <- base::paste0(ages, ", ", num_reconage[i], "mya ")
    }
    #string for giving the user a note
    printstr <- base::paste0("[NOTE]: You can not plot all of these points in a single map. You have ", base::length(num_reconage), " different maps, which are ", base::substring(ages, 2), ".")
    base::print(printstr)
  }
  #if there where points that werent reconstructed because they where not in a time bin add them to the data.frame again
  if(base::nrow(data2) > 0){
    data <- base::rbind(data, data2)
  }
  #return the data frame with the reconstructed ages
  return(data)
}
###########################getmap#############################
#' getmap
#' 
#' Downloads a map of a specific age or a list of maps of specific ages from a model specified by the user.
#' Available models and ages can be found at 
#' https://github.com/GPlates/gplates_web_service_doc/wiki/Reconstruction-Models.
#' 
#' @usage getmap(ma, model = "SETON2012", show.plates = FALSE, 
#'                   save.as = NULL, colland = "#66666660", 
#'                   colsea = "#00509010", 
#'                   do.plot = TRUE, ...)
#' 
#' @param ma numeric. Age in ma(million years ago). Can also be a vector of ages (vector of numeric age values).
#' @param model character. The model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param show.plates boolean. If the user wants to get the continental plate borders or not. By default show.plates = FALSE.
#' @param save.as character. The format the plots should be saved. "tiff", "pdf", "jpeg" or "png". 
#' By default save.as = NULL, plots are only shown and are not automatically saved as a file.
#' @param colland character. The color of the land masses. By default colland = "#66666660".
#' @param colsea character. The color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. If a plot of the map is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main = "my own title" or main.col = "red".
#' @export
#' @examples
#' \dontrun{
#' 
#' library(mapast)
#' 
#' #with continental plates
#' map <- getmap(ma = 100, model = "SETON2012", show.plates = T)
#' coastlines <- map[[1]][[1]]
#' plates <- map[[2]][[1]]
#' 
#' #without continental plates
#' coastline <- getmap(ma = 100, model = "SETON2012")
#' 
#' #save map directly as pdf
#' getmap(ma = 100, model = "SETON2012", save.as="pdf")
#' 
#' #save multiple maps in one pdf
#' pdf("getmap_multi.pdf")
#' par(mfrow = c(2, 1))
#' getmap(ma = c(1, 2))
#' dev.off()
#' par(mfrow = c(1, 1))
#' 
#'}
getmap <- function(ma, model = "SETON2012", show.plates = FALSE, save.as = NULL, colland = "#66666660", 
                   colsea = "#00509010", 
                   do.plot = TRUE, ...) {
  #create empty list of shapes and plates
  shapes <- base::list()
  plates <- base::list()
  #go through the ages given by the user
  for(ages in 1:base::length(ma)){
    #url for api request
    url <- base::paste0("http://gws.gplates.org/reconstruct/coastlines/?time=", ma[ages], "&model=", model)
    #try to get the requested map of the model
    #print error message if map is not available
    #err: boolean if there was an error getting the map
    err <- FALSE
    shape <- tryCatch(
      {
        rgdal::readOGR(url, verbose = FALSE)
      }, error = function(e) {
        err <- TRUE
        message(base::paste0("There is no map for ", ma[ages], " mya in ", model, " model. Please check the spelling, the age and the model you chose."))
        stop()
      }
    )
    #add metadata to the shape file
    #age and model
    shape@data$age <- ma[ages]
    shape@data$model <- model
    #save shape in list of shapes
    shapes[[ages]] <- shape
    #errplate: boolean if there was an error getting the plates
    errplate <- FALSE
    #if user wants to get the plates
    if(show.plates){
      #check if model is golonka or paleomap: they dont have plates. Throw warning.
      if(model == "GOLONKA" || model == "PALEOMAP"){
        base::warning(base::paste0("No plate boundaries available for model ", model, "."))
        errplate <- TRUE
      }else{
        #create url for api request and try to get plates
        plateurl <- base::paste0("http://gws.gplates.org/topology/plate_boundaries/?time=", ma[ages], "&model=", model)
        platebounds <- tryCatch(
          {
            #load the SpatialPolygonsDataFrame
            rgdal::readOGR(plateurl, verbose = FALSE)
          }, error = function(e){
            errplate <- TRUE
            #if cannot be loaded throw a warning that there was a problem loading the map
            base::warning(base::paste0("No Plate Boundaries available for ", ma[ages], " mya in ", model, " model. Please check the spelling, the age and the model you chose."))
            stop()
          }
        )
        #add metadata to plates, the age and the model
        platebounds@data$age <- ma[ages]
        platebounds@data$model <- model
        #save plate in list of plates
        plates[[ages]] <- platebounds
      }
    }
    #if there was no error getting the map, create parameter for plotting and plot the map
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
        #if user specifies save.as, save the plot
        if(!base::is.null(save.as)){
          if(save.as == "tiff"){
            grDevices::tiff(base::paste0("getmap-", ma[ages], "mya_", model, ".tiff"), 
                            height = 10.5, width = 17, units = "cm", res = 300)
          }
          if(save.as == "jpeg"){
            grDevices::jpeg(base::paste0("getmap-", ma[ages], "mya_", model, ".jpeg"), 
                            height = 10.5, width = 17, units = "cm", res = 300)
          }
          if(save.as == "png"){
            grDevices::png(base::paste0("getmap-", ma[ages], "mya_", model, ".png"), 
                            height = 10.5, width = 17, units = "cm", res = 300)
          }
        }
        #define the size of the margin of the plot and save the former definition
        def.mar <- graphics::par("mar")
        #define the margin size
        graphics::par(mar = c(2, 2, 2, 2))
        #do a first plot with the graphical parameters set by the user
        base::do.call(sp::plot, graphparams)
        #if the user set show.plates true and there was no problem loading the plates add the plates to the plot
        if(!errplate && show.plates){
          sp::plot(platebounds, add = T, col = "#66666680")
        }
        #draw a rectangle showing the sea
        graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
                       ytop = 90, col = colsea, 
                       border = FALSE)
        #add x-axis and x-axis label
        graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        graphics::axis(side = 1, pos = -89, lwd = 0, at = 0 , labels = "Longitude", col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis label
        graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        graphics::axis(side = 2, pos = -178, lwd = 0, at = 0 , labels = "Latitude", col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        #add model and age info top right of the plot
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = base::paste(ma[ages], " mya", sep = ""), 
                       col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the landmasses to the plot
        sp::plot(shape, col = colland, border = FALSE, add = TRUE)
        #restore the former graphical mar parameters
        graphics::par(mar = def.mar)
        #if the user wants to have the maps as pdf
        if(!base::is.null(save.as) && save.as == "pdf"){
          filename <- base::paste0("getmap-", ma[ages], "mya_", model, ".pdf")
          grDevices::pdf(filename, width= 6.885417, height = 4.291667)
          #define the size of the margin of the plot and save the former definition
          graphics::par(mar = c(2, 2, 2, 2))
          #do a first plot with the graphical parameters set by the user
          plotmap <- base::do.call(sp::plot, graphparams)
          if(!errplate && show.plates){
            plotmap <- sp::plot(platebounds, add = T, col = "#66666680")
          }
          #draw a rectangle showing the sea
          plotmap <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
                                    ytop = 90, col = colsea, 
                                    border = FALSE)
          #add x-axis and x-axis label
          plotmap <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 0.6)
          plotmap <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0 , labels = "Longitude", col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 0.6)
          #add y-axis and y-axis label
          plotmap <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 0.6, las = 1)
          plotmap <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0 , labels = "Latitude", col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 0.6)
          #add model and age info top right of the plot
          plotmap <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 1)
          plotmap <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(ma[ages], " mya", sep = ""), 
                                    col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          #add the landmasses to the plot
          plotmap <- sp::plot(shape, col = colland, border = FALSE, add = TRUE)
          base::print(plotmap)
          #restore the former graphical mar parameters
          graphics::par(mar = def.mar)
        }
        #if the user wanted to save the plot close the device after plotting
        if(!base::is.null(save.as)){
          grDevices::dev.off()
        }
      }
    }
  }
  # return the shape file/ list of shapes
  # if there was/were plate/s also return plate/s
  if(!errplate && show.plates){
    return(base::list(shapes, plates))
  }else{
    #if there is only one map: return the SpatialPolygonDataFrame instead of a list of SpatialPolygonsDataFrames
    if(base::length(shapes)==1){
      return(shapes[[1]])
    }else{
      return(shapes)
    }
  }
}
####################mapast#################################
#' mapast
#' 
#' Plots your fossil occurrence data onto the corresponding MAp of the PAST.
#' 
#' 
#' 
#' @usage mapast(model = "SETON2012", data, map = NULL, do.plot = TRUE, save.as = NULL,
#'                          colland = "#66666660",
#'                          colsea = "#00509010", 
#'                          colpoints = "#65432190", 
#'                          pch = 16, cex = 1, ...)
#' 
#' @param model character. The model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param data data.frame. Fossil occurrences data.
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by getmap.
#' @param do.plot logical. If a plot is created or not. By default do.plot = TRUE. 
#' @param save.as character. The format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
#' @param colland character. The color of the land masses. By default colland = "#66666660".
#' @param colsea character. The color of the sea. By default colsea = "#00509010".
#' @param colpoints character. The color of the occurrence-points. By default colpoints = "#65432190".
#' @param pch numeric. Point symbol for plotting the occurences. By default pch = 16 (filled circle).
#' @param cex numeric. Size of the fossil occurrences points. By default cex = 1.
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title" or main.col="red".
#' @return Plot
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(mapast)
#' 
#' #get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data)
#' df_auto <- paleocoords(df, time = "automatic")
#'                                                         
#' #create a plot with fossils on the paleogeographical map
#' mapast(model = "SETON2012", data = df_auto)
#' 
#' #save the maps before so the function does not need to load them
#' maps <- getmap(ma = 2.5, model = "SETON2012", do.plot = FALSE)
#' mapast(model = "SETON2012", data = df_auto, map = maps)
#' 
#' #save maps as pdf
#' mapast(model = "SETON2012", data = df_auto, map = maps, save.as = "pdf")
#' 
#' 
#'}
mapast <- function(model = "SETON2012", data, map = NULL, do.plot = TRUE, save.as = NULL,
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
  #check if the shape is a SpatialPolygonsDataFrame
  if(!base::is.null(map)){
    if(!.checkShape(map)){
      stop("Maps need to be SpatialPolygonsDataFrames.")
    }
  }
  #get ages of input maps
  mapages <-c()
  if(!base::is.null(map)){
    if(class(map) == "list"){
      for(i in 1:base::length(map)){
        mapages <- c(mapages, map[[i]]@data$age[1])
      }
    }else{
      mapages <- c(map@data$age[1])
    }
  }
  #count how many maps will be created
  #print warning
  #num_recon: number of different reconstruction ages
  #uage: unique list of reconstructions ages
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  uage <- base::unique(data$recon_age)
  #toload: number of maps that need to get loaded
  toload <- 0
  #go through unique reconstruction ages and see if map of the age is available. If not add 1 to toload.
  for(a in 1:base::length(uage)){
    if(!base::as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  #if toload is not 0, give user a note how many maps need to get loaded.
  if(toload > 0){
    base::print(base::paste0("[NOTE]: You have ", num_recon," reconstruction time(s) (meaning ", num_recon," map(s)). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minute(s) for loading."))
  }
  #go through the reconstruction ages
  for(age in 1:base::length(uage)){
    #save all fossils with the current reconstruction age in subdata
    #curma: current reconstruction age
    curma <- uage[age]
    subdata <- base::subset(data, data$recon_age == curma)
    if(curma %in% mapages){
      #if the corresponding map of the reconstruction age is given save it in shape
      if(class(map) == "list"){
        shape <- map[[match(curma, mapages)]]
      }else{
        shape <- map
      }
    }else{
      #if map not given load map
      shape <- mapast::getmap(ma = curma, model = model, show.plates = FALSE, do.plot = FALSE)
    }
    #default parameter list for plotting
    graphparams.def <- base::list(x = shape, col = "white", border = FALSE
                                  , xlim = c(-180, 180), ylim = c(-90, 90)
                                  , xaxs = "i", yaxs = "i")
    #list of user defined graphical parameter
    graphparams.user <- base::list(...)
    names_graphparams.user <- base::as.vector(base::names(graphparams.user))
    names_graphparams.def <- base::as.vector(base::names(graphparams.def))
    #remove default parameter from list if user specified the same parameter different
    for(param in names_graphparams.user){
      if(param %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == param)] 
    }
    #create graphparams with default and user parameter for plotting
    graphparams <- c(graphparams.def, graphparams.user)
    #plotting the map and the data
    #input data needs to be a data frame
    if (base::class(data) == "data.frame") {
      #if the user wants to have the plot
      if(do.plot){
        #varname <- base::paste0("mapast-", curma, "mya_")
        plot.graphics(curma = curma, model = model, graphparams = graphparams, shape = shape,
                      colsea = colsea, colland = colland, colpoints = colpoints, 
                      varname = "mapast-",
                      curvelegend = FALSE, spplot = TRUE,
                      rankdata = subdata,
                      pch = 21, cex = 1, save.as = save.as, ...)
      }
    }
  }
}
#####################mapocc##############################
#' mapocc
#' 
#' Creates a RasterLayer, containing the number of occurrences per cell and a plot of the fossil 
#' occurrences by the taxonomic rank per cell (a proxy for the sampling effort).
#' 
#' @usage mapocc(data, model = "SETON2012",
#'                    rank = "genus", map = NULL,
#'                    res = 1, save.as = NULL,
#'                    colland = "#66666660",
#'                    colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...) 
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param model character. The model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param rank character. The taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus".
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by mapast::getmap(ma, model).
#' @param res numeric. The spatial resolution. By default res = 1. 
#' @param save.as character. The format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
#' @param colland character. The color of the land masses. By default colland = "#66666660".
#' @param colsea character. The color of the sea. By default colsea = "#00509010".
#' @param col.grid character. The color of the raster. By default col.grid = mycols(100),
#' @param do.plot logical. If a plot is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main = "my own title" or main.col = "red".
#' @return RasterLayer
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(mapast)
#' 
#' # get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data = data)
#' df_auto <- paleocoords(data = df, time = "automatic")
#'                                                         
#' # create a plot with fossils on the paleogeographical map
#' occras <- mapocc(data = df_auto, model = "SETON2012", rank = "species")
#' 
#' # save the maps before so the function does not need to load them
#' maps <- getmap(ma = 2.5, model = "SETON2012", do.plot = FALSE)
#' mapocc(data = df_auto, model = "SETON2012", rank = "species", map = maps)
#' 
#' # save maps as pdf
#' mapocc(data = df_auto, model = "SETON2012", rank = "species", map = maps, save.as = "pdf")
#' 
#'}
mapocc <- function(data, model = "SETON2012",
                   rank = "genus", map = NULL,
                   res = 1, save.as = NULL,
                   colland = "#66666660",
                   colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...) {
  # check user input
  # check if data has latitude and longitude columns
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  #check if the rank is allowed and column is in df
  if(!.checkRank(rank, data)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank or a column called \"", rank, "\" is missing in the data.", sep = ""))
  }
  #check if the needed column for the chosen rank is in the data frame
  if(!.checkDataRank(data,rank)){
    stop(base::paste("There is no column ", rank,  " in the data frame.", sep = ""))
  }
  #check if the shape is a SpatialPolygonsDataFrame
  if(!base::is.null(map)){
    if(!.checkShape(map)){
      stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
    } 
  }
  #get ages of input maps
  mapages <-c()
  if(!base::is.null(map)){
    if(class(map) == "list"){
      for(i in 1:base::length(map)){
        mapages <- c(mapages, map[[i]]@data$age[1])
      }
    }else{
      mapages <- c(map@data$age[1])
    }
  }
  #count how many maps need to get loaded
  #print warning
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  #getting the shape file with getmap
  #occraster: for saving the occurrences raster
  occraster <- c()
  #varname <- "mapocc-"
  #varlegend <- "occurrences"
  #uage: unique recnstruction ages
  uage <- base::unique(data$recon_age)
  #toload: count how many maps need to get loaded because there was no input
  toload <- 0
  for(a in 1:base::length(uage)){
    if(!base::as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  #if map/s need to get loaded print a note for the user how many maps need to get loaded.
  if(toload > 0){
    base::print(base::paste0("[NOTE]: You have ", num_recon," reconstruction time(s) (meaning ", num_recon," map(s)). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minute(s) for loading."))
  }
  #go through the reconstruction ages
  for(age in 1:base::length(uage)){
    #save the current reconstruction age in curma
    curma <- uage[age]
    #subdata: subset of the data with all fossils that have the current reconstrcution age
    subdata <- base::subset(data, data$recon_age == curma)
    #filter data for rank
    rankdata <- na.omit(subdata[ ,c("paleolng", "paleolat", rank)])
    #creating a raster in the size of the shape
    spatialext <- raster::extent(c(-180, 180, -90, 90))
    #create a raster with the dimensions of a map and the resolution (default res = 1)
    ras <- raster::raster(spatialext, res = res)
    #create a raster of the occurences (sampling effort)
    #curoccraster: occurrence raster of the current reconstruction age
    curoccraster <- raster::rasterize(rankdata[ , c("paleolng", "paleolat")], ras, field = rankdata[ ,rank], fun = "count")
    #save the current raster in occraster
    occraster <- c(occraster, curoccraster)
    #if the map corrsponding to the current reconstruction age is given save it in shape, otherwise load it with getmap
    if(curma %in% mapages){
      if(class(map) == "list"){
        shape <- map[[match(curma, mapages)]]
      }else{
        shape <- map
      }
    }else{
      shape <- getmap(ma = curma, model = model, show.plates = FALSE, do.plot = FALSE)
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
    graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
    #if do.plot is true, create a plot
    if(do.plot){
      plot.graphics(curma = curma, model = model, graphparams = graphparams, shape = shape,
                    colsea = colsea, colland = colland, col.grid = col.grid,
                    varname = "mapocc-", varlegend = "occurrences", varraster = curoccraster,
                    normallegend = TRUE, spplot = FALSE,
                    save.as = save.as, ...)
    }
  }
  #if there was more than was occurrence raster created save them as RasterStack, otherwise give back raster
  if(base::length(occraster) > 1){
    occstack <- raster::stack(occraster)
  }else{
    occstack <- occraster[[1]]
  }
  #return the raster or rasterstack
  return(occstack)
}
#####################maprich####################
#' maprich
#' 
#' Creates a RasterLayer of taxon richness
#' and makes a plot of the map with the richness raster.
#' 
#' @usage maprich(data, rank = "genus", res = 1, model = "SETON2012", map = NULL, save.as = NULL,
#'                     colland = "#66666660",
#'                     colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...)
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param rank character. The taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank="genus".
#' @param res numeric. The spatial resolution. By default res = 1. 
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by mapast::getmap(ma, model).
#' @param save.as character. The format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
#' @param colland character. The color of the land masses. By default colland = "#66666660".
#' @param colsea character. The color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. If a plot is created or not. By default do.plot = TRUE. 
#' @param col.grid character. The color of the raster. By default col.grid = mycols(100).
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main = "my own title" or main.col = "red".
#' @return RasterLayer
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(mapast)
#' 
#' #get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data = data)
#' df_auto <- paleocoords(data = df, time = "automatic")
#'                                                         
#' #create a plot with fossils on the paleogeographical map
#' rich <- maprich(data = df_auto, rank = "species", res = 10, model = "SETON2012")
#' 
#' #save the maps before so the function does not need to load them
#' maps <- getmap(ma = 2.5, model = "SETON2012", do.plot = FALSE)
#' maprich(data = df_auto, rank = "species", res = 10, model = "SETON2012", map = maps)
#' 
#' #save maps as pdf
#' maprich(data = df_auto, rank = "species", res = 10, model = "SETON2012", map = maps,
#'                       save.as = "pdf")
#' 
#'}
maprich <- function (data, rank = "genus", res = 1, model = "SETON2012", map = NULL, save.as = NULL,
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
  #check if the rank is allowed and column is in df
  if(!.checkRank(rank, data)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank or a column called \"", rank, "\" is missing in the data.", sep = ""))
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
  if(!base::is.null(map)){
    if(!.checkShape(map)){
      stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
    }
  }
  
  #get ages of maps
  mapages <-c()
  if(!base::is.null(map)){
    if(class(map) == "list"){
      for(i in 1:base::length(map)){
        mapages <- c(mapages, map[[i]]@data$age[1])
      }
    }else{
      mapages <- c(map@data$age[1])
    }
  }
  
  #count how many maps will be created
  #print warning
  #num_recon: number of different reconstruction ages
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  #uage: unique reconstruction ages
  uage <- base::unique(data$recon_age)
  #toload: count how many maps need to get loaded
  toload <- 0
  #go through reconstruction ages, if not given as input count toload +1
  for(a in 1:base::length(uage)){
    if(!base::as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  #if at least one map needs to get loaded -> print note for the user
  if(toload > 0){
    base::print(base::paste0("[NOTE]: You have ", num_recon, " reconstruction time(s) (meaning ", num_recon, " map(s)). ", toload, " map(s) need(s) to get loaded. This is going to take about ", toload, " minute(s) for loading."))
  }
  #creating a raster in size of a map
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
  #richlist: for saving the richness raster
  richlist <- c()
  #go through reconstruction ages and get all fossil data of the current reconstruction age
  for(age in 1:base::length(uage)){
    #curma: current reconstruction age
    curma <- uage[age]
    #subdata: subset of data which have the current reconstruction age
    subdata <- base::subset(data, data$recon_age == curma)
    #if a map with the current reconstruction age is given save it in shape, otherwise load it with getmap
    if(curma %in% mapages){
      if(class(map) == "list"){
        shape <- map[[match(curma, mapages)]]
      }else{
        shape <- map
      }
    }else{
      shape <- getmap(ma = curma, model = model, show.plates = FALSE, do.plot = FALSE)
    }
    #getting the raster of the taxon richnes
    subdata <- na.omit(subdata[ ,c("paleolng", "paleolat", rank)])
    richraster <- raster::rasterize(subdata[ , c("paleolng", "paleolat")], ras, field = subdata[ ,rank], fun = function(x, ...) {length(unique(na.omit(x)))})
    #save richness raster as richlist
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
    #col.grid <- graphparams$col.grid
    graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
    if(do.plot){
      plot.graphics(curma = curma, model = model, graphparams = graphparams, shape = shape,
                    colsea = colsea, colland = colland, col.grid = col.grid, 
                    varname = "maprich-", varlegend = "richness", varraster = richraster,
                    normallegend = TRUE, spplot = FALSE,
                    save.as = save.as, ...)
    }
  }
  #if there is more than one raster create a RasterStack, otherwise oly return raster
  if(base::length(richlist) > 1){
    rasterstack <- raster::stack(richlist)
  }else{
    rasterstack <- richlist[[1]]
  }
  #return the raster
  return(rasterstack)
}

########spsite###################
#' spsite
#' 
#' Generates a diversity data.frame, with the number occurrences of taxa per locality or a presence/absence data.frame.
#' 
#' @usage spsite(data, unity, res = 1, rank = "genus", pa = FALSE)
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param unity character. If the user wants the occurrences per 
#' cell (unity = "cell") or per fossilsite (unity = "fossilsite). 
#' @param res numeric. The spatial resolution. Only used if unity = "cell". By default res = 1.
#' @param rank character. The taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus"
#' @param pa boolean. Defines if the user wants presence/absence or counted data. By default pa = FALSE, which means the function returns counted data.
#' @return data.frame
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(mapast)
#' 
#' #get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data = data)
#' df_auto <- paleocoords(data = df, time = "automatic")
#'                                                         
#' #create a plot with fossils on the paleogeographical map
#' sp_fossilsite <- spsite(data = df_auto, unity = "fossilsite", rank = "species", res = 10)
#' sp_cell <- spsite(data = df_auto, unity = "cell", rank = "species", res = 10)
#' 
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
  #check if the rank is allowed and column is in df
  if(!.checkRank(rank, data)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank or a column called \"", rank, "\" is missing in the data.", sep = ""))
  }
  #check if the column belonging to the rank is in the data frame
  if(!.checkDataRank(data, rank)){
    stop(base::paste("There is no column ", rank, " in the data frame.", sep=""))
  }
  #if unity fossilsite calculate the diversity for each fossilsite
  if(unity == "fossilsite"){
    #numdf: help variable which is counting in which round we are for saving the data.frames in the list dflist
    numdf <- 1
    dflist <- base::list()
    #uage: reconstruction ages
    uage <- base::unique(data$recon_age)
    #go through reconstruction ages
    for(age in 1:base::length(uage)){
      #curma: current reconstruction age
      curma <- uage[age]
      #subdata: fossil data with the current reconstruction age
      subdata <- base::subset(data, data$recon_age == curma)
      #filter data for the rank
      rankdata <- na.omit(subdata[ ,c("paleolng", "paleolat", rank)])
      #create a data. frame with all the locations once
      latlng <- base::data.frame(paleolng = rankdata$paleolng, paleolat = rankdata$paleolat)
      #ulatlng: unique fossilsites
      ulatlng <- base::unique(latlng)
      #getting list of unique taxa
      urank <- base::as.vector(base::unique(rankdata[ , rank]))
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
          cur.taxon <- base::subset(curlatlng, curlatlng[ , rank] == taxon_cur)
          #count the number of species/geners/.. in this locality and save it in the matrix
          count<- base::nrow(cur.taxon)
          occ[curloc, curtaxon + 2] <- count
        }
      }
      #if user wants to have a presence/absence matrix change numbers to 0 or 1
      if(pa){
        #cnames: column names of the occ (lat, lng, taxa)
        cnames <- colnames(occ)
        #save data without coordinates
        occnoloc <- occ[ , 3:base::length(occ)]
        #put a 1 where occurrences are
        occnoloc[occnoloc > 0] <- 1
        #bind lat & lng to it
        occ <- base::cbind(paleolng = occ$paleolng, paleolat = occ$paleolat, occnoloc)
        #save correct column names
        colnames(occ) <- cnames
      }
      #save counted occurrences in dflist
      dflist[[numdf]] <- occ
      numdf <- numdf + 1
    }
    #return the data.frame/list
    if(base::length(dflist) > 1){
      return(dflist)
    }else{
      return(dflist[[1]])
    }
  }
  #if unity is cell calculate the number of fossils per cell
  if(unity == "cell"){
    #dflist: list for savong the occurrences
    #numdf: help counting variable
    dflist <- base::list()
    numdf <- 1
    #uage: unique reconstruction ages
    uage <- base::unique(data$recon_age)
    #go through reconstruction ages
    for(age in 1:base::length(uage)){
      #curma: current reconstruction age
      curma <- uage[age]
      #subdata: fossils with the current reconstruction age
      subdata <- base::subset(data, data$recon_age == curma)
      #filter the data for the rank
      rankdata <- na.omit(subdata[ ,c("paleolng", "paleolat", rank)])
      #getting list of unique taxa
      urank <- base::as.vector(base::unique(rankdata[ , rank]))
      #define lat/lng sequence using the resolution
      lat <- base::seq(-90 + (res / 2) , 90 - (res / 2), res)
      long <- base::seq(-180 + (res / 2), 180 - (res / 2), res)
      #occ: occurrence raster
      occ <- base::expand.grid(long, lat)
      base::colnames(occ) <- c("paleolng", "paleolat")
      #order by paleolat and paleolng
      occ <- occ[with(occ, order(paleolng, -paleolat)), ]
      #fill with default values 0 and add lat, lng and column names
      def.values <- base::matrix(0, nrow = base::nrow(occ), ncol = base::length(urank))
      #add default values to lat and lng
      occ <- base::cbind(occ, def.values)
      #add the different rank names
      base::colnames(occ) <- c("paleolng", "paleolat", urank)
      rankcol <- rank
      latbord <- base::seq(90, -90, -res)
      #getting the number of occurrences of a taxa for each locality
      for(curocc in 1:base::length(rankdata$paleolng)){
        #curtaxon: taxa in the current locality
        curtaxon <- base::as.character(rankdata[[rankcol]][curocc])
        #current lat and lng
        curlat <- data$paleolat[curocc]
        curlng <- data$paleolng[curocc]
        #calculate in which cell we are at the moment
        if(!base::is.na(curlng) && !base::is.na(curlat)){
          if(!(curlat %in% latbord)){
            if(curlng == 180){
              if(curlat >= (90 - (res / 2))){
                row <- base::abs(base::ceiling((curlat - 90) / res) + 1) + base::abs(base::floor((curlng + 180) / res) - 1) * (180 / res)
              }else{
                row <- base::abs(base::ceiling((curlat - 90) / res)) + base::abs(base::floor((curlng + 180) / res) - 1) * (180 / res)
              }
            }else if(curlng == -180){
              if(curlat >= (90 - (res / 2))){
                row <- base::abs(base::ceiling((curlat - 90) / res) + 1) + base::abs(base::floor((curlng + 180) / res)) * (180 / res)
              }else{
                row <- base::abs(base::ceiling((curlat - 90) / res)) + base::abs(base::floor((curlng + 180) / res)) * (180 / res)
              }
            }else{
              row <- base::abs(base::ceiling((curlat - 90) / res)) + 1 + base::abs(base::floor((curlng + 180) / res))*(180 / res)
            }
          }else{
            if(curlng == 180){
              row <- base::abs(base::ceiling((curlat - 90) / res)) + 1 + base::abs(base::floor((curlng + 180) / res) - 1) * (180 / res)
            }else if(curlng == -180){
              if(curlat >= 90 - (res / 2)){
                row <- base::abs(base::ceiling((curlat - 90) / res)) + base::abs(base::floor((curlng + 180) / res) - 1) * (180 / res)
              }else{
                row <- base::abs(base::ceiling((curlat - 90) / res)) + base::abs(base::floor((curlng + 180) / res) - 1) * (180 / res)
              }
            }else{
              if(curlat <= -90 + (res / 2)){
                if(curlng <= -180 + (res / 2)){
                  row <- base::abs(base::ceiling((curlat - 90) / res)) + base::abs(base::floor((curlng + 180) / res)) * (180 / res)
                }else{
                  row <- base::abs(base::ceiling((curlat - 90) / res)) + base::abs(base::floor((curlng + 180) / res)) * (180 / res)
                } 
              }else if (curlat >= 90 - (res / 2)){
                row <- base::abs(base::ceiling((curlat - 90) / res)) + 1 + base::abs(base::floor((curlng + 180) / res)) * (180 / res)
              }else{
                row <- base::abs(base::ceiling((curlat - 90) / res)) + 1 + base::abs(base::floor((curlng + 180) / res)) * (180 / res)
              }
            }
          }
          #if row is 0 set row to 1
          if(row == 0){
            row <- 1
          }
          #increase the value in the row and the column of the current taxon by one
          occ[row, curtaxon] <- (occ[row, curtaxon] + 1)
        }
      }
      #if user wants presence absence data set all values bigger than zero to one
      if(pa){
        cnames <- colnames(occ)
        occnoloc <- occ[ , 3:base::length(occ)]
        occnoloc[occnoloc > 0] <- 1
        occ <- base::cbind(paleolng = occ$paleolng, paleolat = occ$paleolat, occnoloc)
        colnames(occ) <- cnames
      }
      #add the matrix to the list
      dflist[[numdf]] <- (base::as.data.frame(occ))
      numdf <- numdf + 1
    }
    #if more than one return list of matrices, otherwise return only the one matrix
    if(base::length(dflist) > 1){
      return(dflist)
    }else{
      return(dflist[[1]])
    }
  }
}

#####################mapdiv####################
#' mapdiv
#' 
#' Calculates the Shannon diversity per cell or fossilsite 
#' (taking into account relative abundances of all fossil records 
#' whithin the cell) and creates a plot of the map with a RasterLayer of the diversity.
#' 
#' @usage mapdiv  (data, unity, rank = "genus", res = 1, map = NULL, 
#'                       fun = mean, model = "SETON2012",
#'                       colland = "#66666660", colsea = "#00509010", 
#'                       col.grid = mycols(100), 
#'                       do.plot = TRUE, save.as = NULL, ...)
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param unity character. Either "fossilsite" or "cell".
#' @param rank character. The taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus".
#' @param res numeric. The spatial resolution. By default res = 1. 
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by mapast::getmap(ma, model).
#' @param fun function or character. To determine what values to assign to cells that are covered by multiple spatial features. 
#' You can use functions such as min, max, or mean, or the character value 'count'. By default fun = mean.
#' @param model character. The model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param colland character. The color of the land masses. By default colland = "#66666660".
#' @param colsea character. The color of the sea. By default colsea = "#00509010".
#' @param col.grid character. The color of the raster. By default col.grid = mycols(100).
#' @param do.plot logical. If a plot is created or not. By default do.plot = TRUE. 
#' @param save.as character. The format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to
#' plot, such as main = "my own title" or main.col = "red".
#' @return RasterLayer
#' @export 
#' @examples 
#' \dontrun{
#' 
#' library(mapast)
#' 
#' #get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data = data)
#' df_auto <- paleocoords(data = df, time = "automatic")
#'                                                         
#' #create a plot with fossils on the paleogeographical map
#' div_fossilsite <- mapdiv(data = df_auto, unity = "fossilsite", rank = "species", res = 10,
#'                       fun = mean, model = "SETON2012")
#' div_cell <- mapdiv(data = df_auto, unity = "cell", rank = "species", res = 10, fun = mean,
#'                       model = "SETON2012")
#' 
#' #save the maps before so the function does not need to load them
#' maps <- getmap(ma = 2.5, model = "SETON2012", do.plot = FALSE)
#' mapdiv(data = df_auto, unity = "fossilsite", rank = "species", res = 10, map = maps,
#'                       fun = mean, model = "SETON2012")
#' mapdiv(data = df_auto, unity = "cell", rank = "species", res = 10, map = maps, fun = mean,
#'                       model = "SETON2012")
#' 
#' #save maps as pdf
#' mapdiv(data = df_auto, unity = "fossilsite", rank = "species", res = 10, map = maps, fun = mean,
#'                       model = "SETON2012", save.as = "pdf")
#' mapdiv(data = df_auto, unity = "cell", rank = "species", res = 10, map = maps, fun = mean,
#'                       model = "SETON2012", save.as = "pdf")
#' 
#' }
mapdiv <- function(data, unity, rank = "genus", res = 1, map = NULL, fun = mean, model = "SETON2012",
                   colland = "#66666660", colsea = "#00509010", 
                   col.grid = mycols(100), 
                   do.plot = TRUE, save.as = NULL, ...) {
  # check user input data
  if(!base::is.null(map)){
    if(!.checkShape(map)){
      stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
    }
  }
  #get ages of maps
  mapages <-c()
  if(!base::is.null(map)){
    if(class(map) == "list"){
      for(i in 1:base::length(map)){
        mapages <- c(mapages, map[[i]]@data$age[1])
      }
    }else{
      mapages <- c(map@data$age[1])
    }
  }
  #count how many maps need to get loaded
  #print warning
  #num_recon: number of different reconstruction ages
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  #uage: unique reconstruction ages
  uage <- base::unique(data$recon_age)
  #toload: count number of maps that need to get loaded
  toload <- 0
  #go through input maps and check if the reconstruction ages are given as map, count how many we need to load
  for(a in 1:base::length(uage)){
    if(!base::as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  #if maps need to get loaded print note
  if(toload > 0){
    base::print(base::paste0("[NOTE]: You have ", num_recon," reconstruction time(s) (meaning ", num_recon," map(s)). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minute(s) for loading."))
  }
  #creating a raster in size of the shape file
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
  #divlist: list for saving  the diversity values
  divlist <- c()
  #go through reconstruction ages
  for(age in 1:base::length(uage)){
    #curma: current reconstruction age
    curma <- uage[age]
    #subdata: fossils with the current reconstruction age
    subdata <- base::subset(data, data$recon_age == curma)
    #if unity is cell calculate diversity per cell
    if(unity == "cell"){
      #get the number of fossils per cell
      occ_df_cell <- mapast::spsite(subdata, unity = unity, res = res, rank = rank)
      #remove lat and lng from data frame
      drops <- c("paleolat", "paleolng")
      rawocc <- occ_df_cell[ , !(base::names(occ_df_cell) %in% drops)]
      rawocc <- base::data.frame(base::rep(0, base::length(occ_df_cell$paleolat)), rawocc)
      #calculate the diversity and save diversity, lat and lng in new data frame
      div <- vegan::diversity(rawocc)
      #create a data.frame with paleolat, paleolng and diversity
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
      #set al cells with more than one fossil occurrence to one
      occurrences[occurrences > 0] <- 1
      #create a diversity data.frame where the columns called genus is a presence absence column and get the diversity of it
      div.df <- base::data.frame(paleolat = occ_df_cell$paleolat, paleolng = occ_df_cell$paleolng, 
                                 genus = occurrences, recon_age= base::rep(1, base::length(occ_df_cell$paleolng)))
      div_cell <- mapast::spsite(div.df, unity = "cell", res = res, rank = "genus")
      #set all values that are 0 to NULL
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
    #create diversity per fossilsite
    if(unity == "fossilsite"){
      #get number of occurrences per fossilsite
      occ_df <- mapast::spsite(subdata, unity = unity, res = res, rank = rank)
      #remove lat and lng from data.frame
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
      div.df <-base::cbind(div.df, base::rep(1, base::length(divlatlng[ , 1])))
      base::colnames(div.df)[5] <- "recon_age"
      # div_cell <- mapast::spsite(div.df, unity = "cell", res = res, rank = "genus")
      div_cell <- spsite(div.df, unity = "cell", res = res, rank = "genus")
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
    #check if the map of the current reconstruction age is given, if not load the map
    if(curma %in% mapages){
      if(class(map) == "list"){
        shape <- map[[match(curma, mapages)]]
      }else{
        shape <- map
      }
    }else{
      shape <- getmap(ma = curma, model = model, show.plates = FALSE, do.plot = FALSE)
    }
    #default graphical parameter list
    graphparams.def <- base::list(x = shape, col = "white", border = FALSE
                                  , xlim = c(-180, 180), ylim = c(-90, 90)
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
    graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
    #if do.plot is true create a plot
    varname <- base::paste0("mapdiv-", unity, "_")
    if(do.plot){
      plot.graphics(curma = curma, model = model, graphparams = graphparams, shape = shape,
                    colsea = colsea, colland = colland, col.grid = col.grid,
                    varname = varname, varlegend = "diversity", varraster = divraster,
                    normallegend = TRUE, spplot = FALSE,
                    save.as = save.as, ...)
      #plot.graphics(save.as, curma, model, graphparams, colsea, shape, colland, col.grid, varname, "diversity", divraster)
    }
    #save diversity raster in list
    divlist <- c(divlist, divraster)
  }
  #check if there is more than one diversity raster and create rasterstack or only raster
  if(base::length(divlist) > 1){
    divstack <- raster::stack(divlist)
  }else{
    divstack <- divlist[[1]]
  }
  #return the raster
  return(divstack)
}

###################################latdivgrad###################
#' latdivgrad
#' 
#' Calculates the latitudinal diversity of taxa (species, genera, ...) and creates a plot of the paleogeographical continental masses with the fossil occurrences and the latitudinal diversity.
#' 
#' @usage latdivgrad (data, method, rank = "genus",
#'                         res = 1, map = NULL, model = "SETON2012",
#'                         colland = "#66666680", colsea = "#00509010", 
#'                         colpoints = "#65432190",
#'                         rich.col = "#654321", pch = 21, 
#'                         do.plot = TRUE, save.as = NULL,...)
#' 
#' @param data data.frame. Fossil occurrence data.
#' @param method character. The method of diversity measure, method = "shannon" or method = "richness".
#' Must be defined by the user.
#' @param rank character. The taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus".
#' @param res numeric. The spatial resolution. By default res = 1. 
#' @param map (list of) SpatialPolygonDataFrame(s). Containing map(s) which can be created with the function mapast::getmap(ma, model).
#' @param model character. The model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param colland character. The color of the land masses. By default colland = "#66666660".
#' @param colsea character. The color of the sea. By default colsea = "#00509010".
#' @param colpoints character. The color of the fossil occurrence-points. By default colpoints = "#65432190". 
#' @param rich.col character. The color of the richness curve. By default rich.col = "#654321".
#' @param pch numeric. Point symbol for plotting the occurences. By default pch = 21.
#' @param do.plot logical. If a plot is created or not. By default do.plot = TRUE. 
#' @param save.as character. The format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, 
#' such as main = "my own title" or main.col = "red".
#' @return data.frame 
#' @export
#' @examples
#' \dontrun{
#' 
#' library(mapast)
#' 
#' #get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data = data)
#' df_auto <- paleocoords(data = df, time = "automatic")
#'                                                         
#' #create a plot with fossils on the paleogeographical map
#' latrich <- latdivgrad(data = df_auto, method = "richness", rank = "species", res = 1,
#'                       model = "SETON2012")
#' latdiv <- latdivgrad(data = df_auto, method = "shannon", rank = "species", res = 1,
#'                       model = "SETON2012")
#' 
#' #save the maps before so the function does not need to load them
#' maps <- getmap(ma = 2.5, model = "SETON2012", do.plot = FALSE)
#' latdivgrad(data = df_auto, method = "richness", rank = "species", res = 1, map = maps,
#'                       model = "SETON2012")
#' latdivgrad(data = df_auto, method = "shannon", rank = "species", res = 1, map = maps,
#'                       model = "SETON2012")
#' 
#' #save maps with latitudinal diversity as pdf
#' latdivgrad(data = df_auto, method = "richness", rank = "species", res = 1, map = maps,
#'                       model = "SETON2012", save.as = "pdf")
#' latdivgrad(data = df_auto, method = "shannon", rank = "species", res = 1, map = maps,
#'                       model = "SETON2012", save.as = "pdf")
#'}
latdivgrad <- function(data, method, rank = "genus",
                       res = 1, map=NULL, model="SETON2012",
                       colland = "#66666680", colsea = "#00509010", 
                       colpoints = "#65432190",
                       rich.col = "#654321", pch = 21, 
                       do.plot = TRUE, save.as=NULL,...) {
  #check the users input data
  if(!.checkLatLng(data)){
    stop("Column/s paleolat and/or paleolng are missing in the input data.")
  }
  #check if lat is in -90/90 ans lng is in -180/180
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  #check if the rank is allowed and column is in df
  if(!.checkRank(rank, data)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank or a column called \"", rank, "\" is missing in the data.", sep = ""))
  }
  #check when there is an imput maps/are input maps if the format is a SpatialPolygonsDataFrame
  if(!base::is.null(map)){
    if(!.checkShape(map)){
      stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
    }
  }
  #get ages of given input maps and save them in mapages
  mapages <-c()
  if(!base::is.null(map)){
    if(class(map)=="list"){
      for(i in 1:base::length(map)){
        mapages <- c(mapages, map[[i]]@data$age[1])
      }
    }else{
      mapages <- c(map@data$age[1])
    }
  }
  #count how many maps will be created
  #print warning
  #num_recon: number of different reconstruction ages
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  #uage: unique reconstruction ages
  uage <- base::unique(data$recon_age)
  #toload: count how many maps need to get loaded
  toload <- 0
  for(a in 1:base::length(uage)){
    if(!base::as.character(uage[a]) %in% mapages){
      toload <- toload+1
    }
  }
  #if at least one map needs to get loaded print note for the user
  if(toload > 0){
    base::print(base::paste0("[NOTE]: You have ", num_recon," reconstruction time(s) (meaning ", num_recon," map(s)). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minute(s) for loading."))
  }
  #latdivlist: save latitudinal diversity dfs
  latdivlist <- base::list()
  #numlist: which round we are
  numlist <- 1
  #go through different reconstruction ages and calculate the latitudinal diversity for each age
  for(age in 1:base::length(uage)){
    #curma: current reconstruction age
    curma <- uage[age]
    #subdata: fossils with the current reconstruction age
    subdata <- base::subset(data, data$recon_age == curma)
    #check if the map with the current reconstruction age is given or if it needs to get loaded
    #shape: current map (SpatialPolygonsDataFrame)
    if(curma %in% mapages){
      if(class(map)=="list"){
        shape <- map[[match(curma, mapages)]]
      }else{
        shape <- map
      }
    }else{
      shape <- getmap(ma=curma, model = model, show.plates=FALSE, do.plot = FALSE)
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
    #graphparams: argument list with default and user values
    graphparams <- c(graphparams.def, graphparams.user)
    #calculate richness
    if(method == "richness"){
      #filter the data for the taxonomic rank
      #rankdata: all fossils that have a defined chosen rank (e.g. all fossils where the species is known)
      rankdata <- na.omit(subdata[ ,c("paleolng", "paleolat", rank)])
      #setting min and max value for lat
      #creating empty richness data frame
      #richn: save latitudinal richness
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
      #magn: define the magnitude of the richness graph
      magn <- 140/base::max(richn)
      #latdiv: combine min,max lat and richness in a data frame
      latdiv <- base::data.frame(paleolat = c(base::seq(-90 + (res / 2), 90 - (res / 2), res)), div = richn)
    }
    #calulating shannon diversity
    if(method == "shannon"){
      #rankdata: number of fossils of given rank per fossilsite
      rankdata <- mapast::spsite(subdata, unity = "fossilsite", res = res, rank = rank)
      #div: create empty diversity vector
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
      #magn: calculate the magnitude by taking the max of the diversity
      magn <- 140/base::max(div)
      #latdiv: create data frame with paleolat and diversity
      latdiv <- base::data.frame(paleolat = c(base::seq(-90 + (res / 2), 90 - (res / 2), res)), div = div)
    }
    #centros: calculate the center of each range
    centros<- (base::seq(-90, 90 - res, res) + (base::seq(-90, 90 - res, res) + res)) / 2
    #save the diversity, x and y value for plotting
    rich<- 180 + (latdiv$div*magn)
    yrich<- c(180, rich, 180)
    xrich<- c(-90, centros, 90)
    #save default marginvalues
    def.mar <- graphics::par("mar")
    #if do.plot is true create a plot
    varname <- base::paste0("latdivgrad_", method, "_")
    if(do.plot){
      plot.graphics(method = method, curma = curma, model = model, graphparams = graphparams, shape = shape,
                    colsea = colsea, colland = colland, col.grid = rich.col, colpoints = colpoints, 
                    magn = magn, varname = varname,
                    curvelegend = TRUE, spplot = TRUE,
                    yrich = yrich, xrich = xrich, rankdata = rankdata,
                    pch = pch, save.as = save.as, ...)
    }
    #return latitudinal richness
    latdiv <- latdiv[base::length(latdiv$paleolat):1, ]
    latdivlist[[numlist]] <- latdiv
    numlist <- numlist + 1
    
  }
  #if there is only one diversity return it, otherwise return the list of diversities
  if(base::length(latdivlist) > 1){
    return(latdivlist)
  }else{
    return(latdivlist[[1]])
  }
}
