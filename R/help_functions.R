# private functions
# the following functions are used by our package but users can not change their parameters.

##############################color palette ########################################

## creating a color palette for the raster
## mycols <- colorRampPalette(c("goldenrod1","orangered",
##                              "darkred"))
mycols <- colorRampPalette(c("#F5DEB3",
                             "#A99071",
                             "#654321"))

##"#8c714e"

################.lnglat###################
# .lnglat
# 
# return reconstructed paleocoordinates for given points
# 
# @usage .lnglat(data, uma, model, paleolng, paleolat)
# @param data data.frame. Fossil occurrences data.
# @param uma list of reconstruction ages
# @param model character. The model the map should be created with. "SETON2012" (default), 
#' "MULLER2016", "GOLONKA", "PALEOMAP" or "MATTHEWS2016".
# @param paleolng list for the reconstructed paleolng output of every unique reconstruction age
# @param paleolat list for the reconstructed paleolat output  every unique reconstruction age
# @return multi_return (lists) 
# @keywords internal
# @examples
# \dontrun{
# df <- getdata(base_name = "Canis", interval = "Quaternary")
# .lnglat(df_auto, uma = 2.5, model = "SETON2012", paleotng = c(), paleolat = c())
# }
.lnglat <- function(data, uma, model, paleolng, paleolat) {
  #take all fossils with the current reconstruction age
  part <- base::subset(data, data$recon_age == uma)
  #pts: string for saving the paleolat and paleolng of the fossils
  pts <- ""
  #if there are more  than 200 fossils for the current reconstruction age split the request into several requests
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
        #save lat and lng of fossils in string and create request url for api with current reconstruction age
        pts <- base::substring(pts, 2)
        url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma, "&model=", model, "&return_null_points")
        #read in reconstructed paleocoordinates and save them to paleolat and paleolng
        paleopts <- rjson::fromJSON(file = url)
        for (k in 1:base::length(paleopts$coordinates)){
          #if point can't be reconstructed: save NA
          if(base::is.null(paleopts$coordinates[[k]])){
            paleolng <- c(paleolng, NA)
            paleolat <- c(paleolat, NA)
          }else{
            #otherwise save reconstructed paleolat and paleolng
            paleolng <- c(paleolng, paleopts$coordinates[[k]][1])
            paleolat <- c(paleolat, paleopts$coordinates[[k]][2])
          }
        }
      }else{
        #pass the last points of the subsets for one reconstruction age to the api
        pts <- ""
        part2 <- part[((round - 1) * 200 + 1):base::length(part$recon_age), ]
        for( j in 1:base::length(part2$recon_age)){
          pts <- base::paste0(pts, ",", part2$lng[j], ",", part2$lat[j])
        }
        #remove comma before first point
        pts <- base::substring(pts, 2)
        #create url with points, reconstruction age and model
        url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma, "&model=", model, "&return_null_points")
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
      #increase number of rounds to get next subset of points for current reconstruction age
      round <- round + 1
    }
  }else{
    #if not more than 200 fossils: calculate all paleocoordinates in one step and save them
    for( j in 1:base::length(part$recon_age)){
      pts <- base::paste0(pts, ",", part$lng[j], ",", part$lat[j])
    }
    #remove comma before first point
    pts <- base::substring(pts, 2)
    #create url for api with points, reconstruction time and model
    url <- base::paste0("http://gws.gplates.org/reconstruct/reconstruct_points/?points=", pts, "&time=", uma, "&model=", model, "&return_null_points")
    #read in reconstructed points
    paleopts <- rjson::fromJSON(file = url)
    #save reconstructed plaeocoordinates or save NA if point couldn't be reconstructed
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
  multi_return <- list("paleolng" = paleolng, "paleolat" = paleolat)
  return(multi_return)
}

################plot.graphics###################
# plot.graphics
# 
# return plot or file with plot
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
# @return lists with reconstructed paleocoordinates 
# @keywords internal
# @examples
# \dontrun{
# df <- getdata(base_name = "Canis", interval = "Quaternary")
# .lnglat(df_auto, uma = 2.5, model = "SETON2012", paleotng = c(), paleolat = c())
# }
plot.graphics <- function(method = NULL, curma, model, graphparams, shape,
                          colsea = "#00509010", colland = "#66666680", col.grid = "#654321", colpoints = "#65432190", 
                          magn = NULL, varname, varlegend = NULL, varraster = NULL,
                          curvelegend = FALSE, normallegend = FALSE, spplot = FALSE,
                          yrich = NULL, xrich = NULL, rankdata = NULL,
                          pch = 21, cex = 1, save.as = NULL, ...){
  #save current margin values and define it as needed
  def.mar <- graphics::par("mar")
  #if user wants to save the plots, start graphic device
  if(!base::is.null(save.as)){
    if(save.as == "tiff"){
      grDevices::tiff(base::paste0(varname, curma, "mya_", model, ".tiff"), 
                      height = 10.5, width = 17, units = "cm", res = 300)
    }
    if(save.as == "jpeg"){
      grDevices::jpeg(base::paste0(varname, curma, "mya_", model, ".jpeg"), 
                      height = 10.5, width = 17, units = "cm", res = 300)
    }
    if(save.as =="png"){
      grDevices::png(base::paste0(varname, curma, "mya_", model, ".png"), 
                     height = 10.5, width = 17, units = "cm", res = 300)
    }
  }
  #set margins
  if(curvelegend == TRUE){
    graphics::par(mar = c(2,2,3,11), xpd = NA)
  } else {
    graphics::par(mar = c(1, 2, 2, 4))
  }
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
  #latdivgrad function plot
  if(spplot == TRUE){
    #add the fossil occurrences to the plot
    graphics::points(rankdata$paleolng, rankdata$paleolat, 
                     pch = pch, col = NA, bg = colpoints)
    if(curvelegend == TRUE){
      if(!base::is.nan(base::min(yrich))){
        #add the richness curve at the right side of the plot
        graphics::polygon (yrich, xrich, col = col.grid, border = F, xpd = T)
        #get the parameters for the richness axis
        ax_seq <- base::seq(base::min(yrich), base::max(yrich), ((base::max(yrich) - base::min(yrich)) / 2))
        ax_lab <- ax_seq - 180
        ax_lab <- base::round(ax_lab / magn, 2)
        #add the richness axes
        graphics::axis(side = 3, pos = 90, lwd = 1, xpd = TRUE, at = ax_seq, labels = FALSE , col.ticks = col.grid ,
                       col.axis = col.grid , col = col.grid , cex.axis = 0.6, tck = -0.01)
        graphics::axis(side = 3, pos = 80, lwd = 0, xpd = TRUE, at = ax_seq, labels = ax_lab , col.ticks = col.grid ,
                       col.axis = col.grid , col = col.grid , cex.axis = 0.6, tck = -0.01)
        graphics::axis(side = 3, pos = 90, lwd = 0, xpd = TRUE, at = ax_seq[base::round(base::length(ax_seq) / 2)], 
                       labels = method, col.ticks = col.grid, col.axis = col.grid, col = col.grid, 
                       cex.axis = 0.8, tck = -0.01, cex.lab = 0.8)  
      }
    }
    #all other function plots
  } else {
    #add the raster without legend
    raster::plot(varraster, add = T, axes = F, box = F, col = col.grid, legend = FALSE, bty = "L")
    #allow to draw outside the plot
    graphics::par(xpd = TRUE)
    graphics::par(bty = "n")
    #add raster legend outside the plot
    raster::plot(varraster, legend.only = TRUE, col = col.grid, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                 axis.args = base::list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5,
                                        cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                 legend.args = base::list(text = varlegend, line = 1, side = 3, adj = 0.25, cex = 0.6, 
                                          col = col.grid[base::length(col.grid) / 2]))
    raster::plot(varraster, legend.only = TRUE, col = col.grid, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                 axis.args = base::list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, 
                                        cex.lab = 0.5, cex.axis = 0.5, col.axis = col.grid[base::length(col.grid) / 2]))
    graphics::par(bty = "o")
  }
  #if user wants to save as pdf start pdf device
  if(!base::is.null(save.as) && save.as == "pdf"){
    filename <- base::paste0(varname, curma, "mya_", model, ".pdf")
    #set margins
    if(curvelegend == TRUE){
      grDevices::pdf(filename, width= 9.385417, height = 4.291667, pagecentre=F)
      graphics::par(mar = c(3,0,3,11), xpd = NA)
    } else {
      grDevices::pdf(filename, width = 8.385417, height = 4.791667)
      graphics::par(mar = c(1, 2, 2, 4))
    }
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
    
    if(spplot == TRUE){
      #add the fossil occurrences to the plot
      plotmap <- graphics::points(rankdata$paleolng, rankdata$paleolat, 
                                  pch = pch, col = NA, bg = colpoints)
      if(curvelegend == TRUE){
        if(!base::is.nan(base::min(yrich))){
          graphics::par(xpd=TRUE)
          #add the richness curve at the right side of the plot
          plotmap <- graphics::polygon (yrich, xrich, col = col.grid, border = F, xpd = T)
          #get the parameters for the richness axis
          ax_seq <- base::seq(base::min(yrich), base::max(yrich), ((base::max(yrich) - base::min(yrich)) / 2))
          ax_lab <- ax_seq - 180
          ax_lab <- base::round(ax_lab / magn, 2)
          #add the richness axes
          plotmap <- graphics::axis(side = 3, pos = 90, lwd = 1, xpd = TRUE, at = ax_seq, labels = FALSE , 
                                    col.ticks = col.grid ,col.axis = col.grid , col = col.grid , 
                                    cex.axis = 0.6, tck = -0.01)
          plotmap <- graphics::axis(side = 3, pos = 80, lwd = 0, xpd = TRUE, at = ax_seq, labels = ax_lab , 
                                    col.ticks = col.grid ,col.axis = col.grid , col = col.grid , cex.axis = 0.6, 
                                    tck = -0.01)
          plotmap <- graphics::axis(side = 3, pos = 90, lwd = 0, xpd = TRUE, at = ax_seq[base::round(base::length(ax_seq) / 2)], 
                                    labels = method, col.ticks = col.grid, col.axis = col.grid, col = col.grid, 
                                    cex.axis = 0.8, tck = -0.01, cex.lab = 0.8)
        }
      }
    } else {
      #add the raster without legend
      plotmap <- raster::plot(varraster, add = T, axes = F, box = F, col = col.grid, legend = FALSE, bty = "L")
      #allow to draw outside the plot
      graphics::par(xpd = TRUE)
      graphics::par(bty = "n")
      #add raster legend outside the plot
      plotmap <- raster::plot(varraster, legend.only = TRUE, col = col.grid, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                              axis.args = base::list(tck = -0.2, col = NA, col.ticks = "darkgrey", col.lab = NA, cex = 0.5, 
                                                     cex.lab = 0.5, cex.axis = 0.5, col.axis = NA), 
                              legend.args = base::list(text = varlegend, line = 1, side = 3, adj = 0.25, cex = 0.6, 
                                                       col = col.grid[base::length(col.grid) / 2]))
      plotmap <- raster::plot(varraster, legend.only = TRUE, col = col.grid, smallplot = c(0.92, 0.96, 0.3, 0.7), 
                              axis.args = base::list(line = -0.5, col = NA, col.ticks = NA, col.lab = NA, cex = 0.5, cex.lab = 0.5, 
                                                     cex.axis = 0.5, col.axis = col.grid[base::length(col.grid) / 2]))
      graphics::par(bty = "o")
    }
    base::print(plotmap)
  }
  #if file was created close graphic device
  if(!base::is.null(save.as)){
    grDevices::dev.off()
  }
  #restore prior margin values
  graphics::par(mar = def.mar)
}
