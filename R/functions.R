####################formatdata#############################

#' formatdata
#' 
#' Changes and adds columns of your data.frame from the Paleobiology Database to the names needed for the functions in this package.
#' It also calculates the average age of the fossil occurrences from given early_age and late_age parameter.
#' 
#' @usage formatdata(data, db = "pbdb")
#' 
#' @param data data.frame. Fossil occurrences dat, from the Paleoiology Database.
#' @param db character. Name of the database where the data is from. Only db = "pbdb" possible.
#' @return data.frame
#' @export
#' @examples 
#' \dontrun{
#' 
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(limit = 100, base_name = "Canis", 
#'   min_ma = 0, max_ma = 10, 
#'   show = c("coords", "phylo"), 
#'   vocab = "pbdb"))
#' fdata <- formatdata(data, db = "pbdb")
#' 
#' }

formatdata <- function(data, db = "pbdb"){
  
  if(db == "pbdb"){
  
    avg_age <- (data$early_age + data$late_age) / 2
    
    species <- c()
    for(rank in 1:base::length(data$matched_rank)){
      if(data$matched_rank[rank] == "species"){
        species <- c(species, as.character(data$matched_name[rank][[1]]))
      }else{
        species <- c(species, NA)
      }
    }
    
    data <- base::cbind(data, species, avg_age)
    data <- data[base::order(-base::as.numeric(data$avg_age)), ]
    
  }
  
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
#' @param timevector vector. Defining the borders of the time bins. Not allowed to be NULL if time = "timevector".
#' @param stepsize numeric. Defining the stepsize of the time bins if time = "automatic".
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @return data.frame
#' @export 
#' @examples 
#' \dontrun{
#' 
#' occ <- base::data.frame(paleobioDB::pbdb_occurrences(base_name="Canis", min_ma = 0, max_ma = 10, 
#'                                                      show = c("coords", "phylo"), 
#'                                                      vocab = "pbdb", limit = 100))
#' 
#' #reconstruct paleocoordinates with midpoint of appearance time
#' occ_ma <- paleoocoords(occ, time="automatic", model = "SETON2012")
#' 
#' #reconstruct paleocoordinates with specific time
#' occ_matime <- paleoocoords(occ, time = "timevector", timevector = c(0.10), model = "SETON2012")
#' 
#'                     
#'}

paleocoords <- function(data, time = "automatic", timevector=NULL, stepsize=10, model = "SETON2012") {
  

  #remove data with lat or lng outside of range
  data2 <- NULL
  data2 <- base::rbind(data[base::which(data$lng < -180), ], data[base::which(data$lng > 180), ], data[base::which(data$lat < -90), ], data[base::which(data$lat > 90), ])
  if(base::nrow(data2) > 0){
    data2[ ,"recon_age"] <- NA
    data2[ ,"paleolng"] <- NA
    data2[ ,"paleolat"] <- NA
    print(paste0("There are ", base::length(data2$lng), " points out of the lat long boundaries (-180, 180, -90, 90). Those points can not be projected into the past. Please, check them and correct them in order to be able to project them correctly into the past."))
    data <- base::rbind(data[base::which(data$lng >= -180), ], data[base::which(data$lng < 180), ], data[base::which(data$lat >= -90), ], data[base::which(data$lat < 90), ])
  }
  
  paleolng <- c()
  paleolat <- c()
  
  if(time == "average"){
    recon_age <- base::round(data$avg_age)
    data <- base::cbind(data, recon_age)
    uma <- base::unique(recon_age)
    
    for( curma in 1:base::length(uma)){
      part <- base::subset(data, data$recon_age == uma[curma])
      pts <- ""
      if(base::length(part$recon_age) > 200){
        
        num <- base::ceiling(base::length(part$recon_age) / 200)
        round <- 1
        while(round <= num){
          pts <- ""
          if(round < num){
            pts <- ""
            part2 <- part[((round - 1) * 200 + 1):(round * 200), ]
            for( j in 1:base::length(part2$recon_age)){
              pts <- base::paste0(pts, ",", part2$lng[j], ",", part2$lat[j])
            }
            
            pts <- base::substring(pts, 2)
            url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[curma], "&model=", model, "&return_null_points")
            paleopts <- rjson::fromJSON(file = url)
            for (k in 1:base::length(paleopts$coordinates)){
              if(base::is.null(paleopts$coordinates[[k]])){
                paleolng <- c(paleolng, NA)
                paleolat <- c(paleolat, NA)
              }else{
                paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
                paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
              }
            }
            
          }else{
            pts <- ""
            part2 <- part[((round - 1) * 200 + 1):base::length(part$recon_age), ]
            for( j in 1:base::length(part2$recon_age)){
              pts <- paste0(pts, ",", part2$lng[j], ",", part2$lat[j])
            }
            
            pts <- base::substring(pts, 2)
            url <- paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[curma], "&model=", model, "&return_null_points")
            paleopts <- rjson::fromJSON(file = url)
            for (k in 1:base::length(paleopts$coordinates)){
              if(base::is.null(paleopts$coordinates[[k]])){
                paleolng <- c(paleolng, NA)
                paleolat <- c(paleolat, NA)
              }else{
                paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
                paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
              }
            }
            
          }
          round <- round + 1
          
        }
        
      }else{
        
        for( j in 1:base::length(part$recon_age)){
          pts <- base::paste0(pts, ",", part$lng[j], ",", part$lat[j])
        }
        
        pts <- base::substring(pts, 2)
        url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[curma], "&model=", model, "&return_null_points")
        paleopts <- rjson::fromJSON(file = url)
        for (k in 1:base::length(paleopts$coordinates)){
          if(base::is.null(paleopts$coordinates[[k]])){
            paleolng <- c(paleolng, NA)
            paleolat <- c(paleolat, NA)
          }else{
            paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
            paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
          }
        }
        
      }
      
    }
    
    
  }else if(time == "automatic"){
    #take min/max from early/late age as min and max from time bin (abgerundet/aufgerundet)
    #separate into bins of size stepsize
    #separate df into bins and take bin_age as request parameter.
    #set avg age to bin avg age
    min <- base::floor(base::min(data$late_age))
    max <- base::ceiling(base::max(data$early_age))
    ages <- base::seq(min, max, stepsize)
    #check if the stepsize is too big for min&max, if stepsize is not possible, just take min and max as borders and create one bin
    if(base::length(ages) == 1){
      ages <- c(min, max)
    }
    #check if the max age is missing and add it to the ages for max border of last bin
    if((max - min) / stepsize != base::round((max - min) / stepsize)){
      ages <- c(ages, max)
    }
    bin_age <- ages[-base::length(ages)] + diff(ages) / 2
    bin_ages <- c()
    for(i in 1:base::length(data$avg_age)){
      for(j in 1:(base::length(ages) - 1)){
        #check if >= ages j and <= ages j+1 --> then it is in bin_age[j] -> add bin_age[j] to bin_ages
        if(data$avg_age[i] >= ages[j] && data$avg_age[i] <= ages[j+1]){
          bin_ages <- c(bin_ages, bin_age[j])
        }
      }
    }

    recon_age <- bin_ages
    data <- base::cbind(data, recon_age)
    
    uma <- base::unique(recon_age)
    for( i in 1:base::length(uma)){
      part <- base::subset(data, data$recon_age == uma[i])
      pts <- ""
      if(base::length(part$recon_age) > 200){
        
        num <- base::ceiling(base::length(part$recon_age) / 200)
        round <- 1
        while(round <= num){
          pts <- ""
          if(round < num){
            pts <-""
            part2 <- part[((round - 1) * 200 + 1):(round * 200), ]
            for( j in 1:base::length(part2$recon_age)){
              pts <- base::paste0(pts, ",", part2$lng[j], ",", part2$lat[j])
            }
            
            pts <- base::substring(pts, 2)
            url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[i], "&model=", model, "&return_null_points")
            paleopts <- rjson::fromJSON(file = url)
            for (k in 1:base::length(paleopts$coordinates)){
              if(base::is.null(paleopts$coordinates[[k]])){
                paleolng <- c(paleolng, NA)
                paleolat <- c(paleolat, NA)
              }else{
                paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
                paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
              }
            }
            
          }else{
            pts <-""
            part2 <- part[((round - 1) * 200 + 1):base::length(part$recon_age), ]
            for( j in 1:base::length(part2$recon_age)){
              pts <- base::paste0(pts, ",", part2$lng[j], ",", part2$lat[j])
            }
            
            pts <- base::substring(pts, 2)
            url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[i], "&model=", model, "&return_null_points")
            paleopts <- rjson::fromJSON(file = url)
            for (k in 1:base::length(paleopts$coordinates)){
              if(base::is.null(paleopts$coordinates[[k]])){
                paleolng <- c(paleolng, NA)
                paleolat <- c(paleolat, NA)
              }else{
                paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
                paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
              }
            }
            
          }
          round <- round + 1
          
        }
        
        
      }else{
        
        for( j in 1:base::length(part$recon_age)){
          pts <- paste0(pts, ",", part$lng[j], ",", part$lat[j])
        }
        
        pts <- base::substring(pts, 2)
        url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[i], "&model=", model, "&return_null_points")
        paleopts <- rjson::fromJSON(file = url)
        for (k in 1:base::length(paleopts$coordinates)){
          if(base::is.null(paleopts$coordinates[[k]])){
            paleolng <- c(paleolng, NA)
            paleolat <- c(paleolat, NA)
          }else{
            paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
            paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
          }
        }
        
      }
      
    }
    
  }else if(time == "timevector"){
    #take midpoints of user specified bins for reconstruction
    #separate df into sets belonging to bins
    #calc for each bin
    
    bin_age <- timevector[-base::length(timevector)] + diff(timevector) / 2
    bin_ages <- c()
    for(i in 1:base::length(data$avg_age)){
      if(data$avg_age[i] < min(timevector) || data$avg_age[i] > max(timevector)){
        bin_ages <- c(bin_ages, NA)
      }else{
        for(j in 1:(base::length(timevector) - 1)){
          #check if >= ages j and <= ages j+1 --> then it is in bin_age[j] -> add bin_age[j] to bin_ages
          if(data$avg_age[i] >= timevector[j] && data$avg_age[i] <= timevector[j + 1]){
            bin_ages <- c(bin_ages, bin_age[j])
          }
        }
      }

    }
    recon_age <- bin_ages
    data <- base::cbind(data, recon_age)
    
    uma <- base::unique(recon_age)
    for( i in 1:base::length(uma)){
      if(base::is.na(uma[i])){

        part <- base::subset(data, base::is.na(data$recon_age))

        for( na in 1:base::length(part$recon_age)){
          paleolng <- c(paleolng, NA)
          paleolat <- c(paleolat, NA)
        }
      }else{
        
        part <- base::subset(data, data$recon_age == uma[i])
        pts <- ""
        if(base::length(part$recon_age) > 200){

          num <- base::ceiling(base::length(part$recon_age) / 200)
          round <- 1
          while(round <= num){
            pts <- ""
            if(round < num){
              pts <- ""
              part2 <- part[((round - 1) * 200 + 1):(round * 200), ]
              for( j in 1:base::length(part2$recon_age)){
                pts <- paste0(pts, ",", part2$lng[j], ",", part2$lat[j])
              }
              
              pts <- base::substring(pts, 2)
              url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[i], "&model=", model, "&return_null_points")
              paleopts <- rjson::fromJSON(file = url)
              for (k in 1:base::length(paleopts$coordinates)){
                if(base::is.null(paleopts$coordinates[[k]])){
                  paleolng <- c(paleolng, NA)
                  paleolat <- c(paleolat, NA)
                }else{
                  paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
                  paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
                }
              }
              
            }else{
              pts <- ""
              part2 <- part[((round - 1) * 200 + 1):base::length(part$recon_age), ]
              for( j in 1:base::length(part2$recon_age)){
                pts <- base::paste0(pts, ",", part2$lng[j], ",", part2$lat[j])
              }
              
              pts <- base::substring(pts, 2)
              url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[i], "&model=", model, "&return_null_points")
              paleopts <- rjson::fromJSON(file = url)
              for (k in 1:base::length(paleopts$coordinates)){
                if(base::is.null(paleopts$coordinates[[k]])){
                  paleolng <- c(paleolng, NA)
                  paleolat <- c(paleolat, NA)
                }else{
                  paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
                  paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
                }
              }
              
            }
            round <- round + 1
            
          }
          
          
        }else{
          
          for( j in 1:base::length(part$recon_age)){
            pts <- base::paste0(pts, ",", part$lng[j], ",", part$lat[j])
          }
          
          pts <- base::substring(pts, 2)
          url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma[i], "&model=", model, "&return_null_points")
          paleopts <- rjson::fromJSON(file = url)
          for (k in 1:base::length(paleopts$coordinates)){
            if(base::is.null(paleopts$coordinates[[k]])){
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
    
    
  }


  data <- base::cbind(data, paleolng, paleolat)
  num_reconage <- unique(stats::na.omit(data$recon_age))
  if(base::length(num_reconage)>1){
    ages <- c()
    for(i in 1:base::length(num_reconage)){
      ages <- base::paste0(ages, ", ", num_reconage[i], "mya ")
    }
    print(base::paste0("You can not plot all of these points in a single map. You have ", base::length(num_reconage), " different maps, which are ", base::substring(ages, 2), "."))
  }
  if(base::nrow(data2) > 0){
    data <- base::rbind(data, data2)
  }

  return(data)
}

###########################getmap#############################

#' getmap
#' 
#' Downloads a map of a specific age from a model specified by the user.
#' Available models and ages can be found at https://github.com/GPlates/gplates_web_service_doc/wiki/Reconstruction-Models .
#' 
#' @usage getmap(ma, model = "SETON2012", show.plates = FALSE, 
#'                   save.as = NULL, colland = "#66666660", 
#'                   colsea = "#00509010", 
#'                   do.plot = TRUE, ...)
#' 
#' @param ma numeric. Age in ma(million years ago). Can also be a vector of ages.
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param show.plates boolean. Defines if the user wants to get the continental plate borders. By default show.plates = FALSE.
#' @param save.as character. Defines the format the plots should be saved. "tiff", "pdf", "jpeg" or "png". 
#' By default save.as = NULL, plots are only shown not saved automatically as a file.
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param ... Graphical parameters. Any argument that can be passed to image.plot and to plot, such as main="my own title" or main.col="red".
#' @return SpatialPolygonsDataFrame
#' @export
#' @examples
#' \dontrun{
#' 
#' #with continental plates
#' map <- getmap(ma = 100, model = "SETON2012", show.plates = T)
#' coastlines <- map[[1]]
#' plates <- map[[2]]
#' 
#' #without continental plates
#' coastlines <- getmap(ma = 100, model = "SETON2012")
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
#' 
#'}

getmap <- function(ma, model = "SETON2012", show.plates = FALSE, save.as = NULL, colland = "#66666660", 
                   colsea = "#00509010", 
                   do.plot = TRUE, ...) {
  shapes <- list()
  plates <- list()
  for(ages in 1:base::length(ma)){
    url <- base::paste0("http://gws.gplates.org/reconstruct/coastlines/?time=", ma[ages], "&model=", model)
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
    shape@data$age <- ma[ages]
    shape@data$model <- model
    shapes[[ages]] <- shape
    
    errplate <- FALSE
    if(show.plates){
      if(model == "GOLONKA" || model == "PALEOMAP"){
        base::warning(base::paste0("No plate boundaries available for model ", model, "."))
        errplate <- TRUE
      }else{
        #no plate bounds for paleomap
        plateurl <- base::paste0("http://gws.gplates.org/topology/plate_boundaries/?time=", ma[ages], "&model=", model)
        platebounds <- tryCatch(
          {
            rgdal::readOGR(plateurl, verbose = FALSE)
          }, error = function(e){
            errplate <- TRUE
            base::warning(base::paste0("No Plate Boundaries available for ", ma[ages], " mya in ", model, " model. Please check the spelling, the age and the model you chose."))
            stop()
          }
        )
        platebounds@data$age <- ma[ages]
        platebounds@data$model <- model
        plates[[ages]] <- platebounds
      }
      
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
          print(plotmap)
          #restore the former graphical mar parameters
          graphics::par(mar = def.mar)
        }
        
        if(!base::is.null(save.as)){
          grDevices::dev.off()
        }
        
      }
    
  }

  }
  # return the shape file
  if(!errplate && show.plates){
    return(list(shapes, plates))
  }else{
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
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param data data.frame. Fossil occurrences data.
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by getmap.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param save.as character. Defines the format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param colpoints character. Defines the color of the occurrence-points. By default colpoints = "#65432190".
#' @param pch numeric. Point symbol for plotting the occurences. By default pch = 16 (filled circle).
#' @param cex numeric. Size of the points. By default cex = 1.
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
#' maps <- getmap(ma = 2.5, model = "SETON2012, do.plot = FALSE)
#' mapast(model = "SETON2012", data = df_auto, map = maps)
#' 
#' #save maps as pdf
#' mapast(model = "SETON2012", data = df_auto, map = maps, save.as = "pdf)
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
  if(!.checkShape(map)){
    stop("Maps need to be SpatialPolygonsDataFrames.")
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
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  #getting the shape file with getmap
  uage <- base::unique(data$recon_age)
  toload <- 0
  for(a in 1:base::length(uage)){
      if(!as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  if(toload > 0){
    print(paste0("You have ", num_recon," reconstruction times (meaning ", num_recon," maps). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minutes for loading."))
  }
  
   for(age in 1:base::length(uage)){
    curma <- uage[age]
    
    subdata <- base::subset(data, data$recon_age == curma)
    
    if(curma %in% mapages){
      
      if(class(map) == "list"){
        shape <- map[[match(curma, mapages)]]
      }else{
        shape <- map
      }
      
    }else{
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
    for( param in names_graphparams.user){
      if(param %in% names_graphparams.def) graphparams.def <- graphparams.def[ - base::which(base::names(graphparams.def) == param)] 
    }
    #create graphparams with default and user parameter for plotting
    graphparams <- c(graphparams.def, graphparams.user)
    
    
    #save old mar settings and define plotting margins as we need
    def.mar <- graphics::par("mar")
    #plotting the map and the data
    #input data needs to be a data frame
    if (base::class(data) == "data.frame") {
      if(do.plot){
        if(!base::is.null(save.as)){
          if(save.as == "tiff"){
            grDevices::tiff(base::paste0("mapast-", curma, "mya_", model, ".tiff"), 
                 height = 10.5, width = 17, units = "cm", res = 300)
          }
          if(save.as == "jpeg"){
            grDevices::jpeg(base::paste0("mapast-", curma, "mya_", model, ".jpeg"), 
                 height = 10.5, width = 17, units = "cm", res = 300)
          }
          if(save.as == "png"){
            grDevices::png(base::paste0("mapast-", curma, "mya_", model, ".png"), 
                 height = 10.5, width = 17, units = "cm", res = 300)
          }
        }
        graphics::par(mar = c(1.5, 2, 2, 1.5))
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
          graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                         col.axis = "darkgrey", cex.axis = 0.6)
          graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                         col.axis = "darkgrey", cex.axis = 0.6)
          # add y-axis and y-axis labels
          graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", 
                         col.axis = "darkgrey", cex.axis = 0.6, las = 1)
          graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", 
                         col.axis = "darkgrey", cex.axis = 0.6)
          #add model and age at the top right of the plot
          graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = model, col.ticks = "darkgrey", 
                         col.axis = "darkgrey", cex.axis = 1)
          graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = base::paste0(curma, " mya"), 
                         col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          #add the fossil occurrences to the plot
          graphics::points(subdata$paleolng, 
                           subdata$paleolat, 
                           pch = pch, col = colpoints, 
                           cex = cex)
        if(!base::is.null(save.as) && save.as == "pdf"){
          filename <- paste0("mapast-", curma, "mya_", model, ".pdf")
          grDevices::pdf(filename, width = 6.885417, height = 4.291667)
          graphics::par(mar = c(1.5, 2, 2, 1.5))
          #plot with the parameter list which includes users graphical parameter
          #defines size and axes of the plot
          plotmap <- base::do.call(sp::plot, graphparams)
          #draw the rectangle showing the sea
          plotmap <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, 
                         ytop = 90, col = colsea, 
                         border = FALSE)
          #plot the landmasses on the sea
          plotmap <- sp::plot(shape, col = colland, border = FALSE, add = TRUE)
          # add x-axis and x-axis labels
          plotmap <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 0.6)
          plotmap <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 0.6)
          # add y-axis and y-axis labels
          plotmap <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 0.6, las = 1)
          plotmap <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
          #add model and age at the top right of the plot
          plotmap <- graphics::axis(side = 3, pos = 97, lwd = 0, at = 135, labels = model, col.ticks = "darkgrey", 
                                    col.axis = "darkgrey", cex.axis = 1)
          plotmap <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135, labels = base::paste0(curma, " mya"), 
                                    col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          #add the fossil occurrences to the plot
          plotmap <-graphics::points(subdata$paleolng, 
                           subdata$paleolat, 
                           pch = pch, col = colpoints, 
                           cex = cex)
          print(plotmap)
        }
          
          if(!base::is.null(save.as)){
            grDevices::dev.off()
          }
          
      }
      #restore the old margin values
      graphics::par(mar = def.mar)

    }
   
  }
}

#####################mapocc##############################
#' mapocc
#' 
#' Creates a RasterLayer, containing the number of occurrences per cell, and a plot of the fossil 
#' occurrences by taxonomic rank per cell (a proxy for the sampling effort).
#' 
#' @usage mapocc(data, model = "SETON2012",
#'                    rank = "genus", map = NULL,
#'                    res = 1, save.as = NULL,
#'                    colland = "#66666660",
#'                    colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...) 
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum".
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by getmap.
#' @param res numeric. Defining the spatial resolution. By default res = 1. 
#' @param save.as character. Defines the format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
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
#' # get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data)
#' df_auto <- paleocoords(df, time = "automatic")
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
  if(!.checkShape(map)){
    stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
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
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
    #through revcon_age -> for each one map and the corresponding points#
  #getting the shape file with getmap
  occraster <- c()
  uage <- base::unique(data$recon_age)
  toload <- 0
  for(a in 1:base::length(uage)){
      if(!as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  if(toload > 0){
    print(paste0("You have ", num_recon," reconstruction times (meaning ", num_recon," maps). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minutes for loading."))
  }
  
  for(age in 1:base::length(uage)){
    curma <- uage[age]
    
    subdata <- base::subset(data, data$recon_age == curma)
    #filter data for rank
    rankdata <- .rfilter(subdata, rank)
    #creating a raster in the size of the shape
    spatialext <- raster::extent(c(-180, 180, -90, 90))
    ras <- raster::raster(spatialext, res = res)
    #create a raster of the occurences (sampling effort)
    curoccraster <- raster::rasterize(rankdata[ , c("paleolng", "paleolat")], ras, field = rankdata[ ,rank], fun = "count")
    occraster <- c(occraster, curoccraster)
    
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
    gridcol <- graphparams$col.grid
    graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
    #if do.plot is true, create a plot
    if(do.plot){
      #save old margin values and define needed margin values
      def.mar <- graphics::par("mar")
      if(!base::is.null(save.as)){
        if(save.as == "tiff"){
          grDevices::tiff(base::paste0("mapocc-", curma, "mya_", model, ".tiff"), 
               height = 10.5, width = 17, units = "cm", res = 300)
        }
        if(save.as == "jpeg"){
          grDevices::jpeg(base::paste0("mapocc-", curma, "mya_", model, ".jpeg"), 
               height = 10.5, width = 17, units = "cm", res = 300)
        }
        if(save.as == "png"){
          grDevices::png(base::paste0("mapocc-", curma,"mya_",model,".png"), 
               height = 10.5, width = 17, units = "cm", res = 300)
        }
      }
        graphics::par(mar = c(1.5, 2, 2, 4))
        #create a plot with the users parameters
        base::do.call(raster::plot, graphparams)
        #create a rectangle showing the sea
        graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #plot the landmasses on the sea
        raster::plot(shape, col = colland, border = FALSE, add = T)
        #add x-axis and x-axis labels
        graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.6)
        #
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = base::paste(curma, " mya", sep = ""), 
                       col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the raster to the plot without legend
        raster::plot(curoccraster, add = T,axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
        #allow the plot to expand the borders
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add the raster legend outside the plot
        raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, 
                                      cex.lab = 0.5, cex.axis = 0.5, col.axis = NA),
                     legend.args = list(text = "occurrences", line = 1, side = 3, adj = 0.25, cex = 0.6, 
                                        col = col.grid[base::length(col.grid) / 2]))
        raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, 
                                      cex.axis = 0.5, col.axis = col.grid[base::length(col.grid) / 2]))
        graphics::par(bty = "o")

      if(!base::is.null(save.as) && save.as == "pdf"){
        filename <- base::paste0("mapocc-", curma, "mya_", model, ".pdf")
        grDevices::pdf(filename, width= 8.385417, height = 4.791667)
        graphics::par(mar = c(1.5, 2, 2, 4))
        #create a plot with the users parameters
        plotmap <- base::do.call(raster::plot, graphparams)
        #create a rectangle showing the sea
        plotmap <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #plot the landmasses on the sea
        plotmap <- raster::plot(shape, col = colland, border = FALSE, add = T)
        #add x-axis and x-axis labels
        plotmap <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey",
                                  col.axis = "darkgrey", cex.axis = 0.6)
        plotmap <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        plotmap <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        plotmap <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6)
        plotmap <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 1)
        plotmap <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = base::paste(curma, " mya", sep = ""), 
                                  col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the raster to the plot without legend
        plotmap <- raster::plot(curoccraster, add = T,axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
        #allow the plot to expand the borders
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add the raster legend outside the plot
        plotmap <- raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, cex.lab = 0.5, 
                                      cex.axis = 0.5, col.axis = NA),
                     legend.args = list(text = "occurrences", line = 1, side = 3, adj = 0.25, cex = 0.6, 
                                        col = col.grid[base::length(col.grid) / 2]))
        plotmap <- raster::plot(curoccraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, 
                                      cex.axis = 0.5, col.axis = col.grid[base::length(col.grid) / 2]))
        graphics::par(bty = "o")
        print(plotmap)
      }
        
        if(!base::is.null(save.as)){
          grDevices::dev.off()
        }
     
      #restore default margin settings
      graphics::par(mar = def.mar)
    }
  
  }
  if(base::length(occraster) > 1){
    occstack <- raster::stack(occraster)
  }else{
    occstack <- occraster[[1]]
  }
  
  #return the raster
  return(occstack)
}

#####################maprich####################
#' maprich
#' 
#' Creates a RasterLayer of taxon richness
#' and makes a plot of the map and the richness raster.
#' 
#' @usage maprich(data, rank = "genus", res = 1, model = "SETON2012", map = NULL, save.as = NULL,
#'                     colland = "#66666660",
#'                     colsea = "#00509010", col.grid = mycols(100), do.plot = TRUE, ...)
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank="genus".
#' @param res numeric. Defining the spatial resolution. By default res = 1. 
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by getmap.
#' @param save.as character. Defines the format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
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
#' #get data and preprocess it
#' data  <-  base::data.frame(paleobioDB::pbdb_occurrences(base_name = "Canis", 
#'                                                         min_ma = 0, max_ma = 10, 
#'                                                         show = c("coords", "phylo"), 
#'                                                         vocab = "pbdb", limit = 100))
#' df <- formatdata(data)
#' df_auto <- paleocoords(df, time = "automatic")
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
  if(!.checkShape(map)){
    stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
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
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  #through revcon_age -> for each one map and the corresponding points#
  #getting the shape file with getmap
  uage <- base::unique(data$recon_age)
  toload <- 0
  for(a in 1:base::length(uage)){
      if(!as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  if(toload > 0){
    print(base::paste0("You have ", num_recon, " reconstruction times (meaning ", num_recon, " maps). ", toload, " map(s) need(s) to get loaded. This is going to take about ", toload, " minutes for loading."))
  }
  
  #creating a raster in size of the shape file
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
  uage <- base::unique(data$recon_age)
  richlist <- c()
  for(age in 1:base::length(uage)){
    curma <- uage[age]
    
    subdata <- subset(data, data$recon_age == curma)
    
    
    if(curma %in% mapages){
      if(class(map) == "list"){
        shape <- map[[match(curma, mapages)]]
      }else{
        shape <- map
      }
    }else{
      shape <- getmap(ma = curma, model = model, show.plates = FALSE, do.plot = FALSE)
    }
    
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
      if(!base::is.null(save.as)){
        if(save.as == "tiff"){
          grDevices::tiff(base::paste0("maprich-", curma, "mya_", model, ".tiff"), 
               height = 10.5, width = 17, units = "cm", res = 300)
        }
        if(save.as == "jpeg"){
          grDevices::jpeg(base::paste0("maprich-", curma, "mya_", model, ".jpeg"), 
               height = 10.5, width = 17, units = "cm", res = 300)
        }
        if(save.as =="png"){
          grDevices::png(base::paste0("maprich-", curma, "mya_", model, ".png"), 
               height = 10.5, width = 17, units = "cm", res = 300)
        }
      }
        graphics::par(mar = c(1, 2, 2, 4))
        #plot with the default and user defined graphical parameter
        base::do.call(raster::plot, graphparams)
        #add a rectangle as the sea
        graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #add the landmasses to the plot
        raster::plot(shape, col = colland, border = FALSE, add = T, bty = "L")
        #add x-axis and x-axis labels
        graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        #get metadata from the shape file
        
        #add name, model and age at the top rigt of the plot
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = base::paste(curma, " mya", sep = ""), 
                       col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        
        #add the raster without legend
        raster::plot(richraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
        #allow to draw outside the plot
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add raster legend outside the plot
        raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5,
                                      cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                     legend.args = list(text = "richness", line = 1, side = 3, adj = 0.25, cex = 0.6, 
                                        col = col.grid[base::length(col.grid) / 2]))
        raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, 
                                      cex.lab = 0.5, cex.axis = 0.5, col.axis = col.grid[base::length(col.grid) / 2]))
        graphics::par(bty = "o")
        
        
      if(!base::is.null(save.as) && save.as == "pdf"){
        filename <- base::paste0("maprich-", curma, "mya_", model, ".pdf")
        grDevices::pdf(filename, width = 8.385417, height = 4.791667)
        graphics::par(mar = c(1, 2, 2, 4))
        #plot with the default and user defined graphical parameter
        plotmap <- base::do.call(raster::plot, graphparams)
        #add a rectangle as the sea
        plotmap <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #add the landmasses to the plot
        plotmap <- raster::plot(shape, col = colland, border = FALSE, add = T, bty = "L")
        #add x-axis and x-axis labels
        plotmap <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey",
                                  col.axis = "darkgrey", cex.axis = 0.6)
        plotmap <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        plotmap <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        plotmap <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6)
        #get metadata from the shape file
        
        #add name, model and age at the top rigt of the plot
        plotmap <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 1)
        plotmap <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), 
                                  col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        
        #add the raster without legend
        plotmap <- raster::plot(richraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE, bty = "L")
        #allow to draw outside the plot
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add raster legend outside the plot
        plotmap <- raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, 
                                      cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                     legend.args = list(text = "richness", line = 1, side = 3, adj = 0.25, cex = 0.6, 
                                        col = col.grid[base::length(col.grid) / 2]))
        plotmap <- raster::plot(richraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, 
                                      cex.axis = 0.5, col.axis = col.grid[base::length(col.grid) / 2]))
        graphics::par(bty = "o")
        print(plotmap)
      }
      
      if(!base::is.null(save.as)){
        grDevices::dev.off()
      }
      #restore prior margin values
      graphics::par(mar = def.mar)
    }
  
  }
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
#' Generates a diversity data.frame, with the number occurrences of a taxon per locality or a presence/absence data.frame.
#' 
#' @usage spsite(data, unity, res = 1, rank = "genus", pa = FALSE)
#' 
#' @param data data.frame. Fossil occurrences data.
#' @param unity character. unity = "fossilsite" or unity = "cell" defining if the user wants the occurrences per 
#' cell or per fossilsite.
#' @param res numeric. Defining the spatial resolution. By default res = 1. Only used if unity = "cell".
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus"
#' @param pa boolean. Defines if the user wants presence/absence or counted data. By default pa = FALSE.
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
#' df <- formatdata(data)
#' df_auto <- paleocoords(df, time = "automatic")
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
  
  #count how many maps will be created
  #print warning
  
  if(unity == "fossilsite"){
    numdf <- 1
    dflist <- list()
    uage <- base::unique(data$recon_age)
    for(age in 1:base::length(uage)){
      curma <- uage[age]
      
      subdata <- base::subset(data, data$recon_age == curma)
      
      #filter data for the rank
      rankdata <- .rfilter(subdata, rank)
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
        cnames <- colnames(occ)
        occnoloc <- occ[ , 3:base::length(occ)]
        occnoloc[occnoloc > 0] <- 1
        occ <- base::cbind(paleolng = occ$paleolng, paleolat = occ$paleolat, occnoloc)
        colnames(occ) <- cnames
      }
      # occ <- occ[with(occ, order(paleolng, -paleolat)), ]
      dflist[[numdf]] <- occ
      numdf <- numdf + 1
    }
    
    #return the data.frame
    if(base::length(dflist) > 1){
      return(dflist)
    }else{
      return(dflist[[1]])
    }

  }
  if(unity == "cell"){
    dflist <- list()
    numdf <- 1
    uage <- unique(data$recon_age)
    for(age in 1:base::length(uage)){
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
      occ <- base::expand.grid(long, lat)
      base::colnames(occ) <- c("paleolng", "paleolat")
      occ <- occ[with(occ, order(paleolng, -paleolat)), ]
      #fill with default values 0 and add lat, lng and column names
      def.values <- base::matrix(0, nrow = base::nrow(occ), ncol = base::length(urank))
      occ <- base::cbind(occ, def.values)
      base::colnames(occ) <- c("paleolng", "paleolat", urank)
      rankcol <- rank
      #getting the number of occurrences of a taxa for each locality
      latbord <- seq(90, -90, -res)
      for(curocc in 1:base::length(subdata$paleolng)){
        curtaxon <- as.character(subdata[[rankcol]][curocc])
        curlat <- data$paleolat[curocc]
        curlng <- data$paleolng[curocc]
        if(!is.na(curlng) && !is.na(curlat)){
          if(!(curlat %in% latbord)){
            if(curlng == 180){
              if(curlat >= (90 - (res / 2))){
                row <- abs(ceiling((curlat - 90) / res) + 1) + abs(floor((curlng + 180) / res) - 1) * (180 / res)
              }else{
                row <- abs(ceiling((curlat - 90) / res)) + abs(floor((curlng + 180) / res) - 1) * (180 / res)
              }
              
            }else if(curlng == -180){
              if(curlat >= (90 - (res / 2))){
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

      }
      if(pa){
        cnames <- colnames(occ)
        occnoloc <- occ[ , 3:base::length(occ)]
        occnoloc[occnoloc > 0] <- 1
        occ <- base::cbind(paleolng = occ$paleolng, paleolat = occ$paleolat, occnoloc)
        colnames(occ) <- cnames
      }
      dflist[[numdf]] <- (base::as.data.frame(occ))
      numdf <- numdf + 1
      
    }
    
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
#' (taking into account relative abundances of all the fossil records 
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
#' @param rank character. Taxnomic rank. By default rank = "genus".
#' @param res numeric. Defining the spatial resolution. By default res = 1. 
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by getmap.
#' @param fun function or character. To determine what values to assign to cells that are covered by multiple spatial features. 
#' You can use functions such as min, max, or mean, or the character value 'count'. 
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param col.grid character. Defines the color of the raster.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param save.as character. Defines the format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
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
#' df <- formatdata(data)
#' df_auto <- paleocoords(df, time = "automatic")
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
  if(!.checkShape(map)){
    stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
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
  num_recon <- base::length(base::unique(stats::na.omit(data$recon_age)))
  #through revcon_age -> for each one map and the corresponding points#
  #getting the shape file with getmap
  uage <- base::unique(data$recon_age)
  toload <- 0
  for(a in 1:base::length(uage)){
      if(!as.character(uage[a]) %in% mapages){
      toload <- toload + 1
    }
  }
  if(toload > 0){
    print(paste0("You have ", num_recon," reconstruction times (meaning ", num_recon," maps). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minutes for loading."))
  }
  
  
  #creating a raster in size of the shape file
  spatialext <- raster::extent(c(-180, 180, -90, 90))
  ras <- raster::raster(spatialext, res = res)
 
  divlist <- c()
  for(age in 1:base::length(uage)){
    curma <- uage[age]
    
    subdata <- subset(data, data$recon_age == curma)
    
    
    if(unity == "cell"){
      # occ_df_cell <- mapast::spsite(subdata, unity = unity, res = res, rank = rank)
      occ_df_cell <- spsite(subdata, unity = unity, res = res, rank = rank)
      # occ_df_cell <- occ_df_cell[[1]]
      #remove lat and lng from data frame
      drops <- c("paleolat", "paleolng")
      rawocc <- occ_df_cell[ , !(base::names(occ_df_cell) %in% drops)]
      # rawocc <- subset(occ_df_cell, select = -c(paleolng,paleolat))
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
      div.df <- base::data.frame(paleolat = occ_df_cell$paleolat, paleolng = occ_df_cell$paleolng, 
                                 genus = occurrences, recon_age= base::rep(1, base::length(occ_df_cell$paleolng)))
      # base::colnames(div.df)[5] <- "recon_age"
      # div_cell <- mapast::spsite(div.df, unity = "cell", res = res, rank = "genus")
      div_cell <- spsite(div.df, unity = "cell", res = res, rank = "genus")
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
    gridcol <- graphparams$col.grid
    graphparams <- graphparams[- base::which(base::names(graphparams) == "col.grid")]
    #if do.plot is true create a plot
    if(do.plot){
      #save current margin settings and define margin as needed
      def.mar <- graphics::par("mar")

      
      if(!base::is.null(save.as)){
        if(save.as == "tiff"){
          grDevices::tiff(paste0("mapdiv-",curma,"mya_",model,".tiff"), 
               height = 9.5, width = 17.5, units = "cm", res = 300)
        }
        if(save.as == "jpeg"){
          grDevices::jpeg(paste0("mapdiv-",curma,"mya_",model,".jpeg"), 
               height = 9.5, width = 17.5, units = "cm", res = 300)
        }
        if(save.as == "png"){
          grDevices::png(paste0("mapdiv-",curma,"mya_",model,".png"), 
               height = 9.5, width = 17.5, units = "cm", res = 300)
        }
      }
      
      graphics::par(mar = c(1.5, 1.5, 2, 5))
        #plot with parameter list
        base::do.call(raster::plot, graphparams)
        #add a rectangle defining the sea
        graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #add the landmasses
        raster::plot(shape, col = colland, border = FALSE, add = T)
        #add x-axis and x-axis labels
        graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 0.6)
        
        #add name, model, age at the top right of the plot
        graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                       col.axis = "darkgrey", cex.axis = 1)
        graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), 
                       col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the raster without legend
        raster::plot(divraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE)
        #allow to expand the plot
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add legend outside the plot
        # divraster.range <- c(min(na.omit(divraster@data@values)), max(na.omit(divraster@data@values)))
        raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, 
                                      cex = 0.5, cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                    legend.args = list(text = "diversity", line = 1, side = 3, adj = 0.25, 
                                                         cex = 0.6, col = gridcol[base::length(gridcol) / 2]))
        raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, 
                                      cex.lab = 0.5, cex.axis = 0.5, col.axis = gridcol[base::length(gridcol) / 2]))
        graphics::par(bty = "o")

      if(!base::is.null(save.as) && save.as == "pdf"){
        filename <- paste0("mapdiv-",curma,"mya_",model,".pdf")
        grDevices::pdf(filename, width = 8.385417, height = 4.291667, pagecentre = F)
        graphics::par(mar = c(3,0,3,7), xpd = NA)
        
        plotmap <- base::do.call(raster::plot, graphparams)
        #add a rectangle defining the sea
        plotmap <- graphics::rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
                       border = FALSE)
        #add the landmasses
        plotmap <- raster::plot(shape, col = colland, border = FALSE, add = T)
        #add x-axis and x-axis labels
        plotmap <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6)
        plotmap <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6)
        #add y-axis and y-axis labels
        plotmap <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90,-90,4), col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6, las = 1)
        plotmap <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 0.6)
        
        #add name, model, age at the top right of the plot
        plotmap <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", 
                                  col.axis = "darkgrey", cex.axis = 1)
        plotmap <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), 
                                  col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
        #add the raster without legend
        plotmap <- raster::plot(divraster, add = T, axes = F, box = F, col = gridcol, legend = FALSE)
        #allow to expand the plot
        graphics::par(xpd = TRUE)
        graphics::par(bty = "n")
        #add legend outside the plot
        plotmap <- raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, 
                                      cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                     legend.args = list(text = "diversity", line = 1, side = 3, adj = 0.25, cex = 0.6, 
                                        col = gridcol[base::length(gridcol) / 2]))
        plotmap <- raster::plot(divraster, legend.only = TRUE, col = gridcol, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                     axis.args = list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, 
                                      cex.lab = 0.5, cex.axis = 0.5, col.axis = gridcol[base::length(gridcol) / 2]))
        graphics::par(bty = "o")
        print(plotmap)
        
        
      }
        if(!base::is.null(save.as)){
          grDevices::dev.off()
        }
      #restore prior margin settings
      graphics::par(mar = def.mar)
    }
    
    divlist <- c(divlist, divraster)
    
  }
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
#' Calculates the latitudinal diversity of taxa (species, genera, families, orders) and creates a plot of the continental masses with the fossil occurrences and the latitudinal diversity.
#' 
#' @usage latdivgrad (data, method, rank = "genus",
#'                         res = 1, map = NULL, model = "SETON2012",
#'                         colland = "#66666680", colsea = "#00509010", 
#'                         colpoints = "#65432190",
#'                         rich.col = "#654321", pch = 21, 
#'                         do.plot = TRUE, save.as = NULL,...)
#' 
#' @param data data.frame. Fossil occurrence data.
#' @param method character. Defining the method of diversity measure, method = "shannon" or method = "richness".
#' @param rank character. Defining the taxonomic rank of interest. 
#' "species", "genus", "family", "order", "class" or "phylum". By default rank = "genus".
#' @param res numeric. Defining the spatial resolution. By default res = 1. 
#' @param map (list of) SpatialPolygonDataFrames. Containing map(s) which can be created by getmap.
#' @param model character. Defining the model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
#' @param colland character. Defines the color of the land masses. By default colland = "#66666660".
#' @param colsea character. Defines the color of the sea. By default colsea = "#00509010".
#' @param colpoints character. Defines the color of the fossil occurrence-points. By default colpoints = "#65432190". 
#' @param rich.col character. Defines the color of the richness curve. By default rich.col = "#654321".
#' @param pch numeric. Point symbol for plotting the occurences. By default pch = 21.
#' @param do.plot logical. Defines if a plot is created or not. By default do.plot = TRUE. 
#' @param save.as character. Defines the format the plots should be saved. "tiff", "pdf", "jpeg" or "png".
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
#' df <- formatdata(data)
#' df_auto <- paleocoords(df, time = "automatic")
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
#' #save maps as pdf
#' latdivgrad(data = df_auto, method = "richness", rank = "species", res = 1, map = maps,
#'                       model = "SETON2012", save.as = "pdf")
#' latdivgrad(data = df_auto, method = "shannon", rank = "species", res = 1, map = maps,
#'                       model = "SETON2012", save.as = "pdf")
#' 
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
  if(!.checkRange(data)){
    stop("Range of Latitude and/or Longitude is not allowed.")
  }
  #check if the rank is allowed and column is in df
  if(!.checkRank(rank, data)){
    stop(base::paste("Rank: \"", rank, "\" is not a valid rank or a column called \"", rank, "\" is missing in the data.", sep = ""))
  }
  if(!.checkShape(map)){
    stop("Map is/ Maps are not SpatialPolygonsDataFrame.")
  }
  #get ages of maps
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
  num_recon <- base::length(unique(stats::na.omit(data$recon_age)))
  #through revcon_age -> for each one map and the corresponding points#
  #getting the shape file with getmap
  uage <- unique(data$recon_age)
  toload <- 0
  for(a in 1:base::length(uage)){
      if(!as.character(uage[a]) %in% mapages){
      toload <- toload+1
    }
  }
  if(toload > 0){
    print(paste0("You have ", num_recon," reconstruction times (meaning ", num_recon," maps). ",toload, " map(s) need(s) to get loaded. This is going to take about ",toload," minutes for loading."))
  }

  latdivlist <- list()
  numlist <- 1
  for(age in 1:base::length(uage)){
    curma <- uage[age]

    
    subdata <- subset(data, data$recon_age == curma)
    
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
      rankdata <- spsite(subdata, unity = "fossilsite", res = res, rank = rank)
      # rankdata <- rankdata[[1]]
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
    
    def.mar <- graphics::par("mar")
    #if do.plot is true create a plot
    if(do.plot){
      #save current margin settings and define needed margin values 

      # graphics::par(mar = c(1.5,1.5,2,10), xpd = NA)
      
      if(!base::is.null(save.as)){
        if(save.as=="tiff"){
          grDevices::tiff(paste0("latdivgrad-",curma,"mya_",model,".tiff"), 
               height = 8.5, width = 17, units = "cm", res = 300)
        }
        if(save.as=="jpeg"){
          grDevices::jpeg(paste0("latdivgrad-",curma,"mya_",model,".jpeg"), 
               height = 8.5, width = 17, units = "cm", res = 300)
        }
        if(save.as=="png"){
          grDevices::png(paste0("latdivgrad-",curma,"mya_",model,".png"), 
               height = 8.5, width = 17, units = "cm", res = 300)
        }
      }
      
        # def.mar <- graphics::par("mar")
        # graphics::par(mar = c(1.5,1.5,2,10), xpd = NA)
      graphics::par(mar = c(2,2,3,11), xpd = NA)
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
          graphics::axis(side = 3, pos = 90, lwd = 1, xpd = TRUE, at = ax_seq, labels = FALSE , col.ticks = rich.col ,
                         col.axis = rich.col , col = rich.col , cex.axis = 0.6, tck = -0.01)
          graphics::axis(side = 3, pos = 80, lwd = 0, xpd = TRUE, at = ax_seq, labels = ax_lab , col.ticks = rich.col ,
                         col.axis = rich.col , col = rich.col , cex.axis = 0.6, tck = -0.01)
          graphics::axis(side = 3, pos = 90, lwd = 0, xpd = TRUE, at = ax_seq[round(base::length(ax_seq) / 2)], 
                         labels = method, col.ticks = rich.col, col.axis = rich.col, col = rich.col, 
                         cex.axis = 0.8, tck = -0.01, cex.lab = 0.8)  
        }
    
        if(!base::is.null(save.as) && save.as == "pdf"){
          # def.mar <- graphics::par("mar")

          filename <- paste0("latdivgrad-",curma,"mya_",model,".pdf")
          grDevices::pdf(filename, width= 9.385417, height = 4.291667, pagecentre=F)
          graphics::par(mar = c(3,0,3,11), xpd = NA)
          # graphics::par(mar = c(2,0,10,20), xpd = NA)
          #create a plot with the parameter list
          plotmap <- do.call(raster::plot, graphparams)
          #add a rectangle as the sea
          plotmap <- graphics::rect(xleft = -180, xright = 180,
                       ybottom = -90, ytop = 90, col = colsea,
                       border = FALSE)
          #add the landmasses to the plot
          plotmap <- raster::plot(shape, col = colland, border = FALSE, add = T)
          #add the fossil occurrences to the plot
          plotmap <- graphics::points(rankdata$paleolng, rankdata$paleolat, 
                         pch = pch, col = NA, bg = colpoints)
          #add x-axis and x-axis labels
          plotmap <- graphics::axis(side = 1, pos = -84, lwd = 0, xaxp = c(180, -180, 4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
          plotmap <- graphics::axis(side = 1, pos = -89, lwd = 0, at = 0, labels = "Longitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
          #add y-axis and y-axis labels
          plotmap <- graphics::axis(side = 2, pos = -175, lwd = 0, yaxp = c(90, -90, 4), col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6, las = 1)
          plotmap <- graphics::axis(side = 2, pos = -178, lwd = 0, at = 0, labels = "Latitude", col.ticks = "darkgrey",col.axis = "darkgrey", cex.axis = 0.6)
          #add name, model and age at the top right of the plot
          plotmap <- graphics::axis(side = 3, pos = 89, lwd = 0, at = 135 , labels = model, col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 1)
          plotmap <- graphics::axis(side = 3, pos = 81, lwd = 0, at = 135 , labels = paste(curma, " mya", sep = ""), col.ticks = "darkgrey", col.axis = "darkgrey", cex.axis = 0.7)
          if(!is.nan(base::min(yrich))){
            graphics::par(xpd=TRUE)
            #add the richness curve at the right side of the plot
            plotmap <- graphics::polygon (yrich, xrich, col = rich.col, border = F, xpd = T)
            #get the parameters for the richness axis
            ax_seq <- base::seq(base::min(yrich), base::max(yrich), ((base::max(yrich) - base::min(yrich)) / 2))
            ax_lab <- ax_seq - 180
            ax_lab <- base::round(ax_lab / magn, 2)
            #add the richness axes
            plotmap <- graphics::axis(side = 3, pos = 90, lwd = 1, xpd = TRUE, at = ax_seq, labels = FALSE , 
                                      col.ticks = rich.col ,col.axis = rich.col , col = rich.col , 
                                      cex.axis = 0.6, tck = -0.01)
            plotmap <- graphics::axis(side = 3, pos = 80, lwd = 0, xpd = TRUE, at = ax_seq, labels = ax_lab , 
                                      col.ticks = rich.col ,col.axis = rich.col , col = rich.col , cex.axis = 0.6, 
                                      tck = -0.01)
            plotmap <- graphics::axis(side = 3, pos = 90, lwd = 0, xpd = TRUE, at = ax_seq[round(base::length(ax_seq) / 2)], 
                                      labels = method, col.ticks = rich.col, col.axis = rich.col, col = rich.col, 
                                      cex.axis = 0.8, tck = -0.01, cex.lab = 0.8)
          }
          print(plotmap)
        }
        
        if(!base::is.null(save.as)){
          grDevices::dev.off()
        }
        

    }
    #restore the prior margin settings
    graphics::par(mar = def.mar) 
    
    #return latitudinal richness
    latdiv <- latdiv[base::length(latdiv$paleolat):1, ]
    latdivlist[[numlist]] <- latdiv
    numlist <- numlist + 1
    
  }
  
  if(base::length(latdivlist) > 1){
    return(latdivlist)
  }else{
    return(latdivlist[[1]])
  }
  
}





