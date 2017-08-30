###########################pm_getmap#############################

#' pm_getmap
#' 
#' generates a shapefile with the paleomap of the chosen 
#' time interval (e.g. "Cretaceous") and a plot
#' 
#' @usage pm_getmap (interval, do.plot, colsea, colland)
#' 
#' @param interval time interval of interest (e.g. "Cretaceous")
#' @param do.plot TRUE/FALSE. TRUE by default.
#' @param colsea to set the color of the ocean in the plot
#' @param colland to set the color of the land masses in the plot
#' @return a shape file and a plot (if do.plot=TRUE)
#' @export 
#' @examples 
#' \dontrun{
#' pm_getmap(interval="Cretaceous") 
#'}

pm_getmap <- function (interval, colsea = "#00509010", 
                       colland = "#66666660", 
                       do.plot = TRUE) {
  ## we might hack this with "with" or "null" for avoiding NOTE on check: 'no visible binding for global variable'
  ## see: http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  
  
  # if user does not set plot=FALSE plot the shape file

  
  if (!requireNamespace("paleogeoDB", quietly = TRUE)) {
    library(devtools)
    install_github("macroecology/paleogeoDB")
  }
  #data(get(interval), package="paleogeoDB")
  
  assign("shape", get(interval))
  
  if (do.plot) {
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    sp::plot(shape, col = "white", border = FALSE, main=interval, xlim=c(-180,180), ylim=c(-90,90)
             , xlab="Longitude", ylab="Latitude"
             , xaxs="i", yaxs="i")
    rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)
    axis(1, xaxp=c(180,-180,4))
    axis(2, yaxp=c(90,-90,4))
    sp::plot(shape, col = colland, border = FALSE, add = TRUE)
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
  # return the shape file
  return(shape)
}



################pm_getdata##############################

#' pm_getdata
#' 
#' uses paleobioDB R package to get data from the Paleobiology Database
#'  
#' 
#' @param interval time interval of interest (e.g. jurassic)
#' @param base_name name of the taxon you want to get data from 
#' (e.g mammalia, canidae, canis) 
#' @param limit numeric. To set the limit of occurrences 
#' to be downloaded from the Paleobiology Database e.g. 500. 
#' There is no limit by default. 
#' @return a data frame with the occurrences and the values needed 
#' for the other functions in paleoMap
#' @export 
#' @examples 
#' \dontrun{
#' pm_getdata(base_name="Canis", interval="Quaternary")
#'}


pm_getdata <- function(interval, base_name, limit="all"){
  pbdb_occurences <- function(){}
  # get data from paleobioDB
  # save data from paleobiodb as data frame
  occ <- data.frame(pbdb_occurrences(base_name=base_name, interval=interval, 
                                     show=c("paleoloc", "phylo"), 
                                     vocab="pbdb", limit=limit))
  #save only the needed parameter
  if (nrow(occ) != 0){
    data <- data.frame(occ$occurrence_no, occ$matched_name, occ$matched_rank,
                       occ$matched_no,
                       occ$early_interval, occ$late_interval,
                       occ$paleolng, occ$paleolat, occ$geoplate,
                       occ$genus, occ$family, occ$order, occ$class, occ$phylum, 
                       occ$genus_no, occ$family_no, occ$order_no,
                       occ$class_no, occ$phylum_no)
    #set correct column names
    colnames(data) <- c("occurrence_no", "matched_name", "matched_rank",
                        "matched_no", "early_interval", "late_interval",
                        "paleolng", "paleolat", "geoplate",
                        "genus", "family", "order", "class", "phylum", 
                        "genus_no","family_no","order_no",
                        "class_no","phylum_no")
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
#' @usage pm_plot (interval, base_name, limit, colsea, colland, 
#' colpoints, cex)
#' 
#' @param interval time interval of interest (e.g. Quaternary)
#' @param data data.frame with the paleogeoreferenced fossil records 
#' (can be obtained using pm_getdata)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colpoints color of the occurrence points
#' @param cex numeric. size of the points. By default cex=1.
#' @return a plot with the configuration of the continents at the selected time interval 
#' and the fossil occurrences
#' @export 
#' @examples 
#' \dontrun{
#' data  <-  pm_getdata (base_name="Mammalia", interval="Cretaceous")
#' pm_plot (interval="Cretaceous", data)
#'}
#'

pm_plot <- function(interval, data,
                    colsea = "#00509010", 
                    colland = "#66666660",
                    colpoints = "#99000020", 
                    cex = 1) {
  
  #getting the shape file for the map and the data for plotting it on the map
  shape <- pm_getmap(interval = interval, do.plot = FALSE)
  #plotting the map and the data
  if (class(data) == "data.frame") {
    #defines size and axes of the plot
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    plot(shape, col = "white", border = FALSE, main=interval, xlim=c(-180,180), ylim=c(-90,90)
             , xlab="Longitude", ylab="Latitude"
             , xaxs="i", yaxs="i")
    rect(xleft = -180, xright = 180, ybottom = -90, 
         ytop = 90, col = colsea, 
         border = FALSE)
    plot(shape, col = colland, border = FALSE, add = TRUE)
    axis(1, xaxp=c(180,-180,4))
    axis(2, yaxp=c(90,-90,4))
    points(data$paleolng, 
           data$paleolat, 
           pch = 16, col = colpoints, 
           cex = cex)
    par(mar=c(5, 4, 4, 2) + 0.1)
  }
}

#####################pm_occraster##############################

#' pm_occraster
#' 
#' creates a raster and a plot of the fossil occurences by taxonomic rank per cell 
#' (a proxy for the sampling effort)
#' 
#' @param shape shapefile from the time interval of interest. 
#' It can be created with pm_getmap
#' @param data a data frame which needs to have a column called paleolat 
#' and a column called paleolng. It can be created with getdata_paleomap
#' @param rank taxonomic rank of interest (e.g. genus, family, etc.). By default rank="species"
#' @param res resolution of the cells in the raster in degrees (by default res=10)
#' @param colsea users can define the color of the ocean 
#' @param colland users can define the color of the land masses
#' @return a raster file and a plot with number of the fossil occurrences in the selected 
#' interval at the selected resolution
#' @export 
#' @examples 
#' \dontrun{
#' shape <- pm_getmap(interval="Quaternary", do.plot = FALSE) 
#' data <- pm_getdata(base_name="Canis", interval="Quaternary")
#' pm_occraster(shape, data)
#'}

pm_occraster <- function(shape, data, 
                         rank = "species", 
                         res = 10,
                         colsea = "#00509010", 
                         colland = "#66666660"){
  
  raster <- NULL
  #filter data for rank
  fdata <- rfilter(data, rank)
  #creating a raster in the size of the shape
  ras <- raster(shape, res = res)
  #raster of the occurences (sampling effort)
  r <- rasterize(fdata[, c("paleolng","paleolat")], ras , fun = "count")
  #plotting the map and the raster on the map
  par(xpd = T, mar = par()$mar + c(0,0,0,7)) #allows to add legend outside plotting window
  plot (shape, col = "white", border = FALSE, main= "occourence raster" , xlim=c(-180,180), ylim=c(-90,90)
        , xlab="Longitude", ylab="Latitude"
        , xaxs="i", yaxs="i")
  rect(xleft = -180, xright = 180, 
       ybottom = -90, ytop = 90, col = colsea, 
       border = FALSE)
  plot (shape, col = colland, border = FALSE, add = TRUE)
  plot(r, col = c(mycols(res * res)), add = TRUE, legend=FALSE)
  #adding axes
  axis(1, xaxp=c(180,-180,4))
  axis(2, yaxp=c(90,-90,4))
  #adding legend
  plot(r, legend.only=TRUE,  col = c(mycols(res * res)), legend.args=list(text="",side=4), add=TRUE)
  #restore default par values
  par(mar=c(5, 4, 4, 2) + 0.1)
  #returning the raster
  return(r)
}

#####################pm_richraster####################

#' pm_richraster
#' 
#' Creates a raster of species richness
#' and makes a plot of the map and raster
#' 
#' @param shape file from the time interval of interest. 
#' Can be created with get_paleomap
#' @param data a data frame which needs to have a column called 
#' paleolat and a column called paleolng, can be created with getdata_paleomap
#' @param res resolution of the raster/ size of the grid cell
#' @param rank gives the rank for the richness raster (e.g. species, genus,...)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Paleocene")
#' data<- pm_getdata(base_name="Testudines", interval="Paleocene")
#' richness<- pm_richraster(shape, data, rank="genus")
#'}
#'

pm_richraster <- function (shape, data, res = 10, rank,
                           colsea = "#00509010", 
                           colland = "#66666660") {
  
  #creating a raster in size of the shape file
  ras <- raster(shape, res = res)
  #getting the raster of the species richness
  r <- rank_filter(ras, data, res = res, rank)
  
  #plotting the map and the raster
  par(xpd = T, mar = par()$mar + c(0,0,0,7)) #allows to add legend outside plotting window
  plot (shape, col = "white", border = FALSE, main= "richness raster" , xlim=c(-180,180), ylim=c(-90,90)
        , xlab="Longitude", ylab="Latitude"
        , xaxs="i", yaxs="i")
  rect(xleft = -180, xright = 180, ybottom = -90, ytop = 90, col = colsea, 
       border = FALSE)
  plot (shape, col=colland, border = FALSE, add = TRUE)
  plot (r, add = TRUE, axes = FALSE, box = FALSE, col=mycols(100), legend=FALSE)
  #adding axes
  axis(1, xaxp=c(180,-180,4))
  axis(2, yaxp=c(90,-90,4))
  #adding legend
  plot(r, legend.only=TRUE,  col = mycols(100), legend.args=list(text="",side=4), add=TRUE)
  #restore default par values
  par(mar=c(5, 4, 4, 2) + 0.1)
  #return the raster
  return(r)
}




########pm_occ###################
#' pm_occ
#' 
#' generates a diversity matrix, with the number occurrences of each species, genus, family or order per locality
#' 
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param rank character: "species", "genus", "family", "order". 
#' By default rank="species"
#' @return data frame with number of species, genera, families or orders per locality
#' @export 
#' @examples 
#' \dontrun{
#' data <- pm_getdata(base_name = "Canis", interval = "Quaternary")
#' result <- pm_occ(data, rank = "species")
#'}


pm_occ <- function(data, rank = "species") {
  #only getting occurences with a known genus
  genus_data <- rfilter(data, rank)
  
  #getting locations
  loc <- data.frame(paleolat = genus_data$paleolat, 
                    paleolng = genus_data$paleolng)
  #getting unique locations
  uloc <- unique(loc)
  
  #getting list of unique taxa
  
  ugenus <- as.vector(unique(genus_data[, "matched_name"]))
  nsites <- uloc
  
  #fill with default values -1
  blank <- matrix(-1, nrow = nrow(nsites), ncol = length(ugenus))
  nsites <- cbind(nsites, blank)
  colnames(nsites) <- c(unlist(names(loc)), ugenus)
  
  #getting the number of occurrences of a genus for each locality
  for (i in 1:nrow(nsites)) {
    #get lat & lng
    lat_i <- nsites[i, "paleolat"]
    lng_i <- nsites[i, "paleolng"]
    for (j in 1:length(ugenus)) {
      #get current genus
      genus_j <- ugenus[j]
      #get all genus at locality
      flat <- subset(genus_data, genus_data$paleolat == lat_i)
      flatlng <- subset(flat, flat$paleolng == lng_i)
      #select only current genus
      fgen <- subset(flatlng, flatlng [,"matched_name"] == genus_j)
      count<- nrow(fgen)
      nsites[i, j + 2] <- count
    }
  }
  return(nsites)
}





########pm_occ_cell###################
#' pm_occ_cell
#' 
#' generates a diversity matrix, with the number occurrences of each species, genus, family or order per cell
#' 
#' @param shape file from the time interval of interest. 
#' Can be created with get_paleomap
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param rank character: "species", "genus", "family", "order". 
#' By default rank="species"
#' @param res numeric. resolution of the cells. By default res=10
#' @return data frame with number of species, genera, families or orders per locality
#' @export 
#' @examples 
#' \dontrun{
#' data <- pm_getdata(base_name = "Canis", interval = "Quaternary")
#' result <- pm_occ_cell(data, rank = "species", res = 10)
#'}



pm_occ_cell <- function(data, rank = "species", res = 10) {
  #only getting occurences with a known genus
  genus_data <-rfilter(data, rank)
  #getting list of unique taxa
  ugenus <- as.vector (unique(genus_data [, "matched_name"]))
  lat <- seq(-90 + (res / 2), 90 -(res / 2), res)
  long <- seq(-180 + (res / 2), 180 -(res / 2), res)
  nsites <- expand.grid (long, lat)
  #fill with default values -1
  blank <- matrix (-1, nrow = nrow (nsites), ncol = length(ugenus))
  nsites <- cbind (nsites, blank)
  colnames(nsites) <- c ("paleolng", "paleolat", ugenus)
  
  #getting the number of occurrences of a genus for each locality
  for (i in 1:nrow(nsites)) {
    #get lat & lng
    lng_i <- nsites[i, "paleolng"]
    lat_i <- nsites[i, "paleolat"]
    for (j in 1:length(ugenus)) {
      #get current genus
      genus_j <- ugenus[j]
      #get all genus at locality
      flat <- subset(genus_data, genus_data$paleolat >= lat_i - (res /2))
      flat <- subset(flat , flat$paleolat < lat_i + (res / 2))
      flatlng <- subset(flat, flat$paleolng >= lng_i - (res / 2))
      flatlng <- subset(flatlng , flatlng$paleolng < lng_i + (res / 2))
      
      #select only current genus
      fgen <- subset(flatlng, flatlng [,"matched_name"] == genus_j)
      count<- nrow (fgen)
      nsites[i, j + 2] <- count
    }
  }
  return(as.data.frame(nsites))
}


#####################pm_divraster_loc####################

#' pm_divraster_loc
#' 
#' calculates the Shannon diversity per unique locality (based on its coordinates),
#' makes a raster file and a plot showing mean, max, min diversity per cell, 
#' or number of unique localities per cell
#' 
#' @usage pm_divraster_loc  (shape, occ_df, res, fun, colsea, colland)
#' 
#' @param shape file from the time interval of interest. 
#' Can be created with get_paleomap
#' @param occ_df a data frame with number of occurrences of a taxa per locality
#' @param res resolution of the raster/ size of the grid cell
#' @param fun values: mean, max, min, "count". functions to be applied when making the raster
#' use mean to get the mean value of diversity in the cell (mean of the different fossil sites), 
#' max to get the maximum diversity, min to get the min value of diversity, 
#' "count" to get the number of fossil sites in per cell
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Quaternary") 
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df <- pm_occ (data, rank="species")
#' pm_divraster_loc (shape, occ_df, fun=mean)
#' pm_divraster_loc (shape, occ_df, fun=max)
#' pm_divraster_loc (shape, occ_df, fun=min)
#' pm_divraster_loc (shape, occ_df, fun="count")
#'}

pm_divraster_loc <- function(shape, occ_df, res=10, fun=mean,
                             colsea="#00509010", colland="#66666680"){
  
  #creating a raster in size of the shape file
  ras <- raster(shape, res=res)
  #getting only species data and no duplictaed in a raster field
  drops <- c("paleolat","paleolng")
  drop_occ <- occ_df[ , !(names(occ_df) %in% drops)]
  cordata1 <- diversity(drop_occ)
  cordata <- data.frame(occ_df$paleolat, 
                        occ_df$paleolng, div= cordata1)
  colnames(cordata) <- c("paleolat", "paleolng", "div")
  
  #getting the raster of the species richness
  r<-rasterize(cordata[,c("paleolng", "paleolat")], ras, 
               field= cordata$div, fun=fun)
  
  #plotting the map and the raster
  par(xpd = T, mar = par()$mar + c(0,0,0,7)) #allows to add legend outside plotting window
  plot (shape, col="white", border=FALSE, main= "Shannon diversity per locality" , xlim=c(-180,180), ylim=c(-90,90)
        , xlab="Longitude", ylab="Latitude"
        , xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
       border=FALSE)
  plot (shape, col=colland, border=FALSE, add=T)
  plot (r, add=T, axes=F, box=F, col=mycols(100), legend=FALSE)
    #adding axes
  axis(1, xaxp=c(180,-180,4))
  axis(2, yaxp=c(90,-90,4))
  #adding legend
  plot(r, legend.only=TRUE,  col = mycols(100), legend.args=list(text="",side=4), add=TRUE)
  #restore default par values
  par(mar=c(5, 4, 4, 2) + 0.1)
  
  #return the raster
  r
}


#####################pm_divraster_cell####################

#' pm_divraster_cell
#' 
#' calculates the Shannon diversity per cell 
#' (taking into account relative abundances of all the fossil records whithin the cell)
#' 
#' @usage pm_divraster_cell  (shape, occ_df, res, colsea, colland)
#' 
#' @param shape file from the time interval of interest. 
#' Can be created with get_paleomap
#' @param occ_df a data frame with number of occurrences of a taxa per locality
#' @param res resolution of the raster/ size of the grid cell
#' @param rank taxonomic rangk of interest. values: "species", "genus", "family", "order". By default rank="species"
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Quaternary") 
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df_cell <- pm_occ_cell (data, rank="species")
#' pm_divraster_cell (shape, occ_df_cell, res=10)
#' ## cells with diversity values = 0 (e.g., 1 species) are discarded.
#'}

pm_divraster_cell <- function(shape, occ_df_cell, res=10, rank="species",
                              colsea="#00509010", colland="#66666680"){
  
  #creating a raster in size of the shape file
  ras <- raster(shape, res=res)
  
  #getting only species data and no duplictaed in a raster field
  drops <- c("paleolat","paleolng")
  drop_occ <- occ_df_cell[ , !(names(occ_df_cell) %in% drops)]
  cordata1 <- diversity(drop_occ)
  cordata <- data.frame(occ_df_cell$paleolat, 
                        occ_df_cell$paleolng, div= cordata1)
  colnames(cordata) <- c("paleolat","paleolng","div")
  
  r<-rasterize(cordata [,c("paleolng","paleolat")], ras, 
               field= cordata$div, fun=max)
  
  #getting the raster of the species richness
  r[r==0]<- NA
  #plotting the map and the raster
  par(xpd = T, mar = par()$mar + c(0,0,0,7)) #allows to add legend outside plotting window
  plot (shape, col="white", border=FALSE, main= "Shannon diversity per cell" , xlim=c(-180,180), ylim=c(-90,90)
        , xlab="Longitude", ylab="Latitude"
        , xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
       border=FALSE)
  plot (shape, col=colland, border=FALSE, add=T)
  plot (r, add=T, axes=F, box=F, col=mycols(100), legend=FALSE)
    #adding axes
  axis(1, xaxp=c(180,-180,4))
  axis(2, yaxp=c(90,-90,4))
  #adding legend
  plot(r, legend.only=TRUE,  col = mycols(100), legend.args=list(text="",side=4), add=TRUE)
  #restore default par values
  par(mar=c(5, 4, 4, 2) + 0.1)
  #return the raster
  r
}



###################################pm_latrich###################
#' pm_latrich
#' 
#' calculates latitudinal diversity of taxa (species, genera, families, orders)
#' 
#' @usage pm_latrich (shape, data, res, rank, 
#' colsea, colland, colpoints, colpointborder, magn)
#' 
#' @param shape a shape file of the corresponding time map
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param rank character, taxonomic rank: "species", "genus", "family", "order". 
#' By default rank="species"
#' @param res resolution in of the segmentation of the latitude. By default res=1.
#' @param colsea defines the color of the sea
#' @param colland defines the color f the landmasses
#' @param colpoints defines the colo of the points for the occurrences
#' @param colpointborder defines color of the border of the occurrence points
#' @param magn numeric. index to magnify the plot of the latitudinal richness 
#' (use when richness values are very low to see the latitudinal pattern more easily)
#' @return data frame with richness of rank and a plot of the continental masses 
#' with the occurrences and the latitudinal richness
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Quaternary") 
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' pm_latrich (shape, data, rank="species", res=10)
#'}


pm_latrich <- function(shape, data, res=10, 
                       rank="species",
                       colsea="#00509010", colland="#66666680", 
                       colpoints="#FFC12530",
                       colpointborder="black", magn=10){
  data2 <-rfilter(data, rank)
  #setting min and max value for lat
  lr <- data.frame(lat_min= seq(-90,90-res,res), 
                   lat_max=seq(-90+res, 90, res))
  #creating empty richness data frame
  richn <- NULL
  #going through lats
  for(lat in seq(-90,90-res,res)){
    sub1 <- subset(data2, data2$paleolat>=lat)
    sub2 <- subset(sub1, sub1$paleolat<(lat+res))
    sub3 <- unique(sub2 [,3])
    #count and save the number of different genus at each latitude
    richn <- c(richn, length(sub3))
  }
  #combine min,max lat and richness
  lr <- cbind(lr, richn)
  centros<- (seq(-90,90-res,res)+(seq(-90,90-res,res) + res))/2
  rich<- 185 + (lr$richn*magn)
  yy<- c(185, rich, 185)
  xx<- c(-90, centros, 90)
  
  par(xpd = T, mar = par()$mar + c(0,0,0,7))
  plot (shape, col="white", border = FALSE, main= "latitudinal richness" , xlim=c(-180,180), ylim=c(-90,90)
        , xlab="Longitude", ylab="Latitude"
        , xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, 
       ybottom=-90, ytop=90, col=colsea, 
       border=FALSE)
  plot (shape, col=colland, border=FALSE, add=T)
  points (data2$paleolng, data2$paleolat, 
          pch=21, col=colpointborder, bg=colpoints)
  axis(1, xaxp=c(180,-180,4))
  axis(2, yaxp=c(90,-90,4))
  polygon (yy, xx, col="goldenrod1", border=F)
  par(mar=c(5, 4, 4, 2) + 0.1)
  #return latitudinal richness
  return (lr)
}


#' pm_latdiv
#' 
#' calculates the Shannon diversity along the latitudinal gradient based on 
#' the individual values of diverstiy the fossil localities of those latitudes.
#' The function returns the mean or max values of diversity of the sampled localities
#' along the latitudinal gradient.
#' 
#' @usage pm_latdiv (occ_df, shape, fun, colsea, solland, colpoints, colpointborder)
#' 
#' @param occ_df a data frame with abundance of taxa per locality (see example)
#' @param shape a shape file of the corresponding time map
#' @param res. numeric. spatial resolucion of the latitudinal bins. res=10 by default.
#' @param fun values: mean, max. functions to be applied to get one single value of diversity,
#' use mean to get the mean value of diversity 
#' (mean of the different fossil sites at the same latitudinal bin), 
#' max to get the maximum observed diversity, min to get the min observed diversity. 
#' @param colsea defines the color of the sea
#' @param colland defines the color f the landmasses
#' @param colpoints defines the colo of the points for the occurrences
#' @param colpointborder defines color of the border of the occurrence points
#' @return data frame with shannon diversity,
#' a plot of the corresponding time map with the occurrences and their
#' latitudinal diversity
#' 
#' @export 
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df <- pm_occ (data)
#' pm_latdiv (occ_df, shape, fun=mean)
#' pm_latdiv (occ_df, shape, fun=max)
#'}

pm_latdiv <- function(occ_df, shape, res=10, 
                      fun= max,
                      colsea="#00509010", colland="#66666680", 
                      colpoints="#FFC12530",
                      colpointborder="black"){
  
  
  #calculate the shannon diversity
  drops <- c("paleolat","paleolng")
  drop_occ <- occ_df[ , !(names(occ_df) %in% drops)]
  H_data <- diversity(drop_occ)
  
  #get the localities
  locs <- cbind(occ_df[,"paleolat"],occ_df[,"paleolng"], H_data)
  colnames(locs) <- c("paleolat", "paleolng", "div")
  cornum <- NULL
  for(lat in seq(-90,80,res)){
    slocs <- subset(locs, locs[,"paleolat"]>=lat)
    slocs <- subset(slocs, slocs[,"paleolat"]<lat+res)
    if (nrow (slocs) == 0) {
      cornum <- c(cornum, 0)  
    } else {
      cornum <- c(cornum, fun (slocs[,"div"])) 
    }
  }
  
  latmin <- seq(-90,80,res)
  latmax <- seq(-80,90,res)
  #create data frame withh latmin latmax and corrected richness
  lr <- data.frame(latmin, latmax, cornum)
  colnames(lr) <- c("maxlat", "minlat", "div")
  
  centros<- (seq(-90,90-res,res)+(seq(-90,90-res,res) + res))/2
  rich<- 185 + (lr$div*20)
  yy<- c(185, rich, 185)
  xx<- c(-90, centros, 90)
  
  par(xpd = T, mar = par()$mar + c(0,0,0,7))
  plot (shape, col="white", border = FALSE, main= "latitudinal diversity" , xlim=c(-180,180), ylim=c(-90,90)
        , xlab="Longitude", ylab="Latitude"
        , xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, 
       ybottom=-90, ytop=90, col=colsea, 
       border=FALSE)
  plot (shape, col=colland, border=FALSE, 
        add=T)
  points (occ_df [, "paleolng"], occ_df [, "paleolat"], 
          pch=21, col=colpointborder, 
          bg=colpoints)
  axis(1, xaxp=c(180,-180,4))
  axis(2, yaxp=c(90,-90,4))
  polygon (yy, xx, col="goldenrod1", 
           border=F)
  par(mar=c(5, 4, 4, 2) + 0.1)
  #return latitudinal richness
  return (lr)
}
