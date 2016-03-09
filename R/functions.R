###########################pm_getmap#############################

#' pm_getmap
#' 
#' generates a shapefile with the map of the choosen time interval (e.g. "Triassic")
#' 
#' @usage pm_getmap (interval, do.plot, colsea, colland)
#' 
#' @param interval time interval of interest (e.g. "Triassic")
#' @param do.plot TRUE/FALSE. TRUE by default.
#' @param colsea to set the color of the ocean in the plot
#' @param colland to set the color of the land masses in the plot
#' @return a shape file for the choosen time interval and a plot (if do.plot=TRUE)
#' @export 
#' @examples 
#' \dontrun{
#' pm_getmap(interval="Quaternary") 
#'}

pm_getmap <- function (interval, colsea="#00509010", 
                       colland="#66666660", 
                       do.plot=TRUE){
  ## we might hack this with "with" or "null" for avoiding NOTE on check: 'no visible binding for global variable'
  ## see: http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  
load (paste ("Data/", interval, ".rda", sep=""), envir=environment())
  # if user does not set plot=FALSE plot the shape file
  # get the shape file with help function getShape to open lazyload data
  shape <- getshape(interval)
  
  if(do.plot== TRUE){
    par (mar=c(0,0,0,0))
    plot (shape, col="white", border=FALSE)
    rect(xleft=-180, xright=180, ybottom=-90, 
         ytop=90, col=sea, 
         border=FALSE)
    plot (shape, col=colland, border=FALSE, add=T)
  }
  # return the shape file
  shape
}



################pm_getdata##############################

#' pm_getdata
#' 
#' uses paleobioDB R package to get data from the Paleobiology Database
#'  
#' @usage pm_getdata (interval, base_name, limit)
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
#' pm_getdata (base_name="Canis", interval="Quaternary")
#'}


pm_getdata <- function(interval, base_name, limit="all"){
  pbdb_occurences <- function(){}
  # get data from paleobioDB
  # save data from paleobiodb as data frame
  occ <- data.frame(pbdb_occurrences (base_name=base_name, interval=interval, 
                                      show=c("paleoloc", "phylo"), 
                                      vocab="pbdb", limit=limit))
 #save only the needed parameter
  if (nrow (occ)!= 0){
    data <- data.frame(occ$matched_name, occ$matched_rank,
                       occ$matched_no,
                       occ$early_interval, occ$late_interval,
                       occ$paleolng, occ$paleolat, occ$geoplate,
                       occ$genus, occ$family, occ$order, occ$class, occ$phylum, 
                       occ$genus_no, occ$family_no, occ$order_no,
                                      occ$class_no, occ$phylum_no)
    #set correct column names
    colnames(data) <- c("matched_name", "matched_rank", "matched_no",
                        "early_interval", "late_interval",
                        "paleolng", "paleolat", "geoplate",
                        "genus", "family", "order", "class", "phylum", 
                        "genus_no","family_no","order_no",
                        "class_no","phylum_no")
    #return data frame
    return (data) 
  #catching error when there are no occurences for the request
  }else{
    print ("There is no data that matches your query on the paleobioDB. 
           Check if the spelling, temporal intervals, etc. are correct")
    return (0)
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
#' @param base_name taxa to be downloaded for the paleobioDB (e.g Canis, Canidae, Carnivora, etc.) 
#' @param limit enables the user to set the maximum number of 
#' records downloaded from the paleobioDB (e.g. 1000)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colpoints color of the occurrence points
#' @param cex numeric. size of the points. By default cex=3.
#' @return a plot with the configuration of the continents at the selected time interval 
#' and the fossil occurrences
#' @export 
#' @examples 
#' \dontrun{
#' pm_plot (interval="Quaternary", base_name= "Canis")
#'}
#'

pm_plot <- function(interval, base_name,
                    limit="all",
                    colsea="#00509010", 
                    colland="#66666660",
                    colpoints="#99000020", 
                    cex=3){
  #create variables for labeling the plot
  title <- paste (interval, base_name, sep="  -  ")
  #getting the shape file for the map and the data for plotting it on the map
  shape <- pm_getmap(interval=interval, do.plot=FALSE)
  data <- pm_getdata(interval=interval, 
                     base_name=base_name, limit=limit)
  #plotting the map and the data
  if (class (data) == "data.frame"){
    #defines size and axes of the plot
    par (mar=c(0,0,0,0))
    plot (shape, col="white", border=FALSE)
    rect(xleft=-180, xright=180, ybottom=-90, 
         ytop=90, col=sea, 
         border=FALSE)
    plot (shape, col=colland, border=FALSE, add=T)
    points(data$paleolng, 
           data$paleolat, 
           pch=16, col= colpoints, 
           cex=cex)
    text (x=0, y=100, title)
  }
}

#####################pm_occraster##############################

#' pm_occraster
#' 
#' creates a raster and a plot of the fossil occurences by taxonomic rank per cell 
#' (a proxy for the sampling effort)
#' 
#' @usage pm_occraster (shape, data, rank, res, colsea, colland)
#' @param shape shapefile from the time interval of interest. 
#' It can be created with pm_getmap
#' @param data a data frame which needs to have a column called paleolat 
#' and a column called paleolng. It can be created with getdata_paleomap
#' @param rank taxonomic rank of interest (e.g. genus, family, etc.). By default rank="genus"
#' @param res resolution of the cells in the raster in degrees (by default res=10)
#' @param colsea users can define the color of the ocean 
#' @param colland users can define the color of the land masses
#' @return a raster file and a plot with number of the fossil occurrences in the selected 
#' interval at the selected resolution
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getmap(interval="Quaternary") 
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' pm_occraster (shape, data)
#'}

pm_occraster <- function(shape, data, 
                         rank= "genus", 
                         res=10,
                         colsea="#00509010", 
                         colland="#66666660"){
  
  raster <- NULL
  #filter data for rank
  fdata <- rfilter(data, rank)
  #creating a raster in the size of the shape
  ras <- raster(shape, res=res)
  #raster of the occurences (sampling effort)
  r<-rasterize(fdata[,1:2],ras
               ,fun="count")
  #plotting the map and the raster on the map
  par (mar=c(0,0,0,8))
  plot (shape, col="white", border=FALSE)
  rect(xleft=-180, xright=180, 
       ybottom=-90, ytop=90, col=colsea, 
       border=FALSE)
  plot (shape, col=colland, border=FALSE, add=T)
  plot(r,col=c(mycols(res*res)), add=T)
  #returning the raster
  r
}

#####################pm_richraster####################

#' pm_richraster
#' 
#' creates a raster of species richness
#' and makes a plot of the map and raster
#' 
#' @usage pm_richraster (shape, data, res, rank, colsea, colland)
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
#' shape<- pm_getmap(interval="Quaternary") 
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' richness<- pm_richraster (shape, data, rank="genus")
#'}
#'

pm_richraster <- function(shape, data, res=10, rank,
                          colsea="#00509010", 
                          colland="#66666660"){
  
  #creating a raster in size of the shape file
  ras <- raster(shape, res=res)
  #getting the raster of the species richness
  r <- rank_filter(ras, data, res=res, rank)
  
  #plotting the map and the raster
  par (mar=c(0,0,0,5))
  plot (shape, col="white", border=FALSE)
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=sea, 
       border=FALSE)
  plot (shape, col=land, border=FALSE, add=T)
  plot (r, add=T, axes=F, box=F, col=mycols(100))
  #return the raster
  r
}




########pm_occ###################
#' pm_occ
#' 
#' generates a diversity matrix, with the number occurrences of each species, genus, family or order per locality
#' 
#' @usage pm_occ (data, rank)
#' 
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param rank character: "species", "genus", "family", "order". 
#' By default rank="genus"
#' @return data frame with number of species, genera, families or orders per locality
#' @export 
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' pm_occ (data, rank=species)
#'}


pm_occ <- function(data, rank="genus") {
  #only getting occurences with a known genus
  genus_data <-rfilter(data, rank)
  
  #getting locations
  loc <-data.frame(paleolat= genus_data$paleolat, 
                   paleolng= genus_data$paleolng)
  #getting unique locations
  uloc <- unique(loc)
  
  #getting list of unique taxa
  
  ugenus <- as.vector (unique(genus_data [,3]))
  nsites<- uloc
  
  #fill with default values -1
  blank<- matrix (-1, nrow=nrow (nsites), ncol=length (ugenus))
  nsites<- cbind (nsites, blank)
  colnames(nsites) <- c (unlist (names (loc)), ugenus)
  
  #getting the number of occurrences of a genus for each locality
  for (i in 1:nrow(nsites)) {
    #get lat & lng
    lat_i <- nsites[i,1]
    lng_i <- nsites[i,2]
    for (j in 1:length(ugenus)) {
      #get current genus
      genus_j <- ugenus[j]
      #get all genus at locality
      flat <- subset(genus_data, genus_data$paleolat == lat_i)
      flatlng <- subset(flat, flat$paleolng == lng_i)
      #select only current genus
      fgen <- subset(flatlng, flatlng [,3] == genus_j)
      count<- nrow (fgen)
      nsites[i, j + 2] <- count
    }
  }
  
  nsites
}





########pm_occ###################
#' pm_occ_cell
#' 
#' generates a diversity matrix, with the number occurrences of each species, genus, family or order per cell
#' 
#' @usage pm_occ (data, rank, res)
#' 
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param rank character: "species", "genus", "family", "order". 
#' By default rank="genus"
#' @param res numeric. resolution of the cells. By default res=10
#' @return data frame with number of species, genera, families or orders per locality
#' @export 
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' pm_occ_cell (data, rank="species", res=10)
#'}


pm_occ_cell <- function(data, rank="genus", res=10) {
  #only getting occurences with a known genus
  genus_data <-rfilter(data, rank)
  #getting list of unique taxa
  ugenus <- as.vector (unique(genus_data [,3]))
 
  
  lat<- seq(-90 + (res/2), 90 -(res/2), res)
  long<- seq(-180 + (res/2), 180 -(res/2), res)
  
  nsites<- expand.grid (long, lat)
  
  #fill with default values -1
  blank<- matrix (-1, nrow= nrow (nsites), ncol=length (ugenus))
  nsites<- cbind (nsites, blank)
  colnames(nsites) <- c ("long", "lat", ugenus)
  
  #getting the number of occurrences of a genus for each locality
  for (i in 1:nrow(nsites)) {
    #get lat & lng
   
    lng_i <- nsites[i,1]
    lat_i <- nsites[i,2]
    for (j in 1:length(ugenus)) {
      #get current genus
      genus_j <- ugenus[j]
      #get all genus at locality
      flat <- subset(genus_data, genus_data$paleolat >= lat_i - (res/2))
      flat <- subset(flat , flat$paleolat < lat_i + (res/2))
      flatlng <- subset(flat, flat$paleolng >= lng_i - (res/2))
      flatlng <- subset(flatlng , flatlng$paleolng < lng_i + (res/2))
      
      #select only current genus
      fgen <- subset(flatlng, flatlng [,3] == genus_j)
      count<- nrow (fgen)
      nsites[i, j + 2] <- count
    }
  }
  
  as.data.frame (nsites)
}



#####################pm_divraster####################

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
  cordata1 <- diversity(occ_df[,3:ncol(occ_df)])
  cordata <- data.frame(occ_df$paleolng, 
                        occ_df$paleolat, div= cordata1)
  
  #getting the raster of the species richness
  r<-rasterize(cordata [,1:2], ras, 
               field= cordata$div, fun=fun)
  
  #plotting the map and the raster
  par (mar=c(0,0,0,5))
  plot (shape, col="white", border=FALSE)
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=sea, 
       border=FALSE)
  plot (shape, col=land, border=FALSE, add=T)
  plot (r, add=T, axes=F, box=F, col=mycols(100))
  
  #return the raster
  r
}


#####################pm_divraster####################

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
  cordata1 <- diversity (occ_df_cell [,3:ncol(occ_df_cell)])
  cordata <- data.frame(occ_df_cell$long, 
                        occ_df_cell$lat, div= cordata1)
  
  r<-rasterize(cordata [,1:2], ras, 
               field= cordata$div, fun=max)
  
  #getting the raster of the species richness
  r[r==0]<- NA
  #plotting the map and the raster
  par (mar=c(0,0,0,5))
  plot (shape, col="white", border=FALSE)
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=sea, 
       border=FALSE)
  plot (shape, col=land, border=FALSE, add=T)
  plot (r, add=T, axes=F, box=F, col=mycols(100))
  
  #return the raster
  r
}



###################################pm_latrich###################
#' pm_latrich
#' 
#' calculates latitudinal diversity of taxa (species, genera, families, orders)
#' 
#' @usage pm_latrich (shape, data, res, rank, do.plot, 
#' colsea, colland, colpoints, colpointborder, magn)
#' 
#' @param shape a shape file of the corresponding time map
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param rank character, taxonomic rank: "species", "genus", "family", "order". 
#' By default rank="species"
#' @param res resolution in of the segmentation of the latitude. By default res=1.
#' @param do.plot TRUE if plot is wanted, false otherwise
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
                       do.plot=TRUE, rank="species",
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
  
  par (xpd = NA, mar=c(0, 0, 0, 8))
  plot (shape, col="white", border=FALSE)
  rect(xleft=-180, xright=180, 
        ybottom=-90, ytop=90, col=colsea, 
        border=FALSE)
   plot (shape, col=colland, border=FALSE, add=T)
   points (data2$paleolng, data2$paleolat, 
           pch=21, col=colpointborder, bg=colpoints)
   polygon (yy, xx, col="goldenrod1", border=F)
    
  #return latitudinal richness
  return (lr)
}


#' pm_latdiv
#' 
#' calculates the Shannon diversity in the latitudinal gradient
#' 
#' @usage pm_latdiv (occ_df)
#' 
#' @param occ_df a data frame with abundance of taxa per locality (see example)
#' @param shape a shape file of the corresponding time map
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param do.plot TRUE if plot is wanted, false otherwise
#' @param bar.side defines wether the barplot of the latitudinal richness 
#' is on the right or on the left side of the map
#' @param colsea defines the color of the sea
#' @param colland defines the color f the landmasses
#' @param colborder defines the color of the borders of the land masses
#' @param colpoints defines the colo of the points for the occurrences
#' @param colpointborder defines color of the border of the occurrence points
#' @return data frame with shannon corrected richness of rank and a plot of the corresponding time map with the occurrences and a barplot of the corrected latitudinal richness next to the map
#' @export 
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' occ_df <- pm_occ (data)
#' pm_latdiv (occ_df)
#'}

pm_latdiv <- function(occ_df, shape, res=10, 
                       do.plot=TRUE, rank="species",
                       colsea="#00509010", colland="#66666680", 
                       colpoints="#FFC12530",
                       colpointborder="black")
  
  
  diversity <- NULL
  #calculate the shannon diversity
  H_data <- diversity(occ_df[,3:ncol(occ_df)])
  
  #get the localities
  locs <- cbind(occ_df[,1],occ_df[,2], as.vector(H_data))
  cornum <- NULL

  for(lat in seq(-90,80,res)){
    slocs <- subset(locs, locs[,1]>=lat)
    slocs <- subset(slocs, slocs[,1]<lat+res)
    cornum <- c(cornum, apply (slocs[,3]), )
  }
  
  latmin <- seq(-90,80,res)
  latmax <- seq(-80,90,res)
  #create data frame withh latmin latmax and corrected richness
  cordf <- data.frame(latmin, latmax, cornum)
  colnames(cordf) <- c("maxlat", "minlat", "richness")
  
  #create plot
  if(do.plot==TRUE){
    #create plot & barplot
    if(bar.side=="left"){
      barplot(cordf$richness, horiz=TRUE, xlim=c(max(cordf$richness),0))
    }
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="Longitude", ylab="Latitude"
         , main="Raster - richness of genus", xaxs="i", yaxs="i")
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea)
    plot(shape, col=colland, border=colborder, add=TRUE)
    points (data$paleolng, data$paleolat, pch=21, col=colpointborder, bg=colpoints)
    if(bar.side=="right"){
      barplot(cordf$richness, horiz=TRUE, xlim=c(0,max(cordf$richness)))
    }
  }
  
  #return corrected richness
  cordf
}
