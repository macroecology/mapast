###########################pm_getmap#############################

#' pm_getmap
#' 
#' gets the shapefile for the map of a choosen time interval (e.g. "triassic")
#' 
#' @usage pm_getmap (interval, plot, colsea, colland, colborder)
#' 
#' @param interval time interval of interest
#' @param colsea to set the color of the ocean in the plot
#' @param colland to set the color of the land masses
#' @param colborder to set the color of the borders of the land masses
#' @param do.plot TRUE/FALSE, if TRUE the output includes a plot
#' @return a shape file for the choosen time interval and a plot (if plot=TRUE)
#' @export 
#' @examples /dontrun{
#' pm_getmap(interval="Quaternary") 
#'}
#'

pm_getmap <- function (interval, colsea="#E5E5E520", 
                       colland="#66666680", colborder="#2B2B2B30", 
                       do.plot=TRUE){
  ## we might hack this with "with" or "null" for avoiding NOTE on check: 'no visible binding for global variable'
  ## see: http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  
  load (paste (interval, ".rda", sep=""), 
        envir=environment())
  # if user does not set plot=FALSE plot the shape file
  if(do.plot== TRUE){
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="Longitude", ylab="Latitude"
         , main=interval, xaxs="i", yaxs="i")
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, 
         border=FALSE)
    plot(map, col=colland, border=colborder, add=TRUE)
    box(which="plot")
  }
  # return the shape file
  map
}

################pm_getdata##############################

#' pm_getdata
#' 
#' uses paleobioDB R package to get data from paleobiology Database
#'  
#' @usage pm_getdata (interval, base_name, limit)
#' 
#' @param interval time interval of interest (e.g. jurassic)
#' @param base_name name of the base (e.g reptiles) you want to get data from
#' @param limit how many entrances from pbdb you want to have, e.g. 500. 
#' There is no limit by default 
#' @return a shape file for the choosen time interval
#' @export 
#' @examples /dontrun{
#' pm_getdata (base_name="Canis", interval="Quaternary")
#'}
#'

pm_getdata <- function(interval, base_name, limit="all"){
    # create an empty data variable for storing occurences
    data <- c()
    # get data from paleobioDB
    occ <- data.frame(pbdb_occurrences (base_name=base_name, interval=interval, 
                      show=c("paleoloc", "phylo"), 
                      vocab="pbdb", limit=limit))
    # save data from paleobiodb as data frame
    # data <- data.frame(occ)
    #return data frame
    if (nrow (occ)!= 0){
      data <- data.frame(occ$matched_name, occ$matched_rank,
                         occ$early_interval, occ$late_interval,
                         occ$paleolng, occ$paleolat, occ$geoplate,
                         occ$genus, occ$family, occ$order, occ$class, occ$phylum)
      colnames(data) <- c("matched_name", "matched_rank",
                          "early_interval", "late_interval",
                          "paleolng", "paleolat", "geoplate",
                          "genus", "family", "order", "class", "phylum")
      return (data) 
    }else{
      print ("There is no data that matches your query on the paleobioDB. Check if the spelling, temporal intervals, etc. are correct")
      return (0)
    }
}



####################pm_plot#################################

#' pm_plot
#' 
#' plots your query from paleobioDB directly onto the map of the selected time interval
#' 
#' 
#' @usage pm_plot (interval, base_name, limit, colsea, colland, 
#' colborder, colpoints, colpointborder)
#' 
#' @param interval time interval of interest (e.g. jurassic)
#' @param base_name larger taxonomic rank for the query to the paleobioDB (e.g reptiles) 
#' @param limit enables the user to set the maximum number of 
#' records downloaded from the paleobioDB (e.g. 1000)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colborder color of the landmass borders
#' @param colpoints color of the points of the occurences
#' @param colpointborder color of the border of the points
#' @return a plot with the configuration of the continents at the selected time interval 
#' and the fossil occurrences
#' @export 
#' @examples /dontrun{
#' pm_plot (interval="Quaternary", base_name= "Canis")
#'}
#'

pm_plot <- function(interval, base_name,
                    limit="all",
                    colsea="#E5E5E520", colland="#66666680", 
                    colborder="#2B2B2B30", colpoints="#9ACD3250",
                    colpointborder="black"){
  #create variables for labeling the plot
  title <-paste("Time interval: ", interval, sep="")
  subtitle <- paste("Base name: ", base_name, sep="")
  #getting the shape file for the map and the data for plotting it on the map
  shape <- pm_getmap(interval=interval, do.plot=FALSE)
  data <- pm_getdata(interval=interval, base_name=base_name, limit=limit)
  #plotting the map and the data
  if (class (data) == "data.frame"){
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="Longitude", ylab="Latitude"
       , main=interval, xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea, border=FALSE)
  mtext(subtitle)
  plot(shape, col=colland, border=colborder, add=TRUE)
  points (data$paleolng, data$paleolat, pch=21, col=colpointborder, bg=colpoints)
  box(which="plot")
  }
}

#####################pm_occraster##############################

#' pm_occraster
#' 
#' creates a raster and a plot of the fossil occurences by taxonomic rank per cell 
#' (a proxy for the sampling effort)
#' 
#' @usage pm_occraster (shape, data, rank, colsea, colland, colborder)
#' @param shape shapefile from the time interval of interest. 
#' Can be created with pm_getmap
#' @param data a data frame which needs to have a column called paleolat 
#' and a column called paleolng, can be created with getdata_paleomap
#' @param rank text. Taxonomic rank of interest (e.g. genus, family, etc.)
#' @param res resolution of the cells in the raster (10 degrees by default)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colborder color of the landmass borders
#' @return a raster file and a plot of the time intervall, the fossil occurences and the raster file.
#' @export 
#' @examples /dontrun{
#' shape<- pm_getmap(interval="Quaternary") 
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' pm_occraster (shape, data)
#'}

pm_occraster <- function(shape, data, rank= "genus", res=10,
                         colsea="#E5E5E520", colland="#66666680", 
                         colborder="#2B2B2B30"){
    
  #filter data for rank
  fdata <- rfilter(data, rank)
  #creating a raster in the size of the shape
    ras <- raster(shape, res=res)
    #raster of the occurences (sampling effort)
    r<-rasterize(fdata[,5:6],ras
                 ,fun=sum)
    #plotting the map and the raster on the map
    par (mar=c(5,5,5,5))
    plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
         , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
         , xlab="Longitude", ylab="Latitude"
         , main=paste("Raster - sampling effort of ", rank), xaxs="i", yaxs="i")
    rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea)
    plot(shape, col=colland, border=colborder, add=TRUE)
    plot(r,col=c(mycols(res*res)), add=T)
    box(which="plot")
    #returning the raster
    r
}

#####################pm_richraster####################

#' pm_richraster
#' 
#' creates a raster of species richness
#' and makes a plot of the map and raster
#' 
#' @usage pm_richraster (shape, data, res, rank, colsea, colland, colborder)
#' 
#' @param shape file from the time interval of interest. 
#' Can be created with get_paleomap
#' @param data a data frame which needs to have a column called 
#' paleolat and a column called paleolng, can be created with getdata_paleomap
#' @param res resolution of the raster/ size of the grid cell
#' @param rank gives the rank for the richness raster (e.g. species, genus,...)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colborder color of the landmass borders
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples /dontrun{
#' shape<- pm_getdata (base_name="Canis", interval="Quaternary")
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' myraster <- pm_richraster (shape, data)
#' plot(myraster)
#'}
#'


pm_richraster <- function(shape, data, res=10, rank,
                          colsea="#E5E5E520", colland="#66666680", 
                          colborder="#2B2B2B30"){
  #creating a raster in size of the shape file
  ras <- raster(shape, res=res)
  #getting only species data and no duplictaed in a raster field
  fdata <- rank_filter(data, res=res, rank)
  #getting the raster of the species richness
  r<-rasterize(fdata[,5:6],ras,fun=sum)
  #plotting the map and the raster
   plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="longitude", ylab="latitude"
       , main=paste("Raster - richness of ", rank), xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea)
  plot(shape, col=colland, border=colborder, add=TRUE)
  plot(r, col= mycols(res*res), add=T)
  box(which="plot")
  #return the raster
  r
}



########pm_ngl###################
#number of genus per locality
pm_ngl <- function(data) {
  genus_data <-rfilter(data, "genus")
  loc <-data.frame(genus_data$paleolat, genus_data$paleolng)
  names(loc) <- c("paleolat", "paleolng")
  uloc <- unique(loc)
  genus <- data.frame(genus_data$genus)
  ugenus <- unique(genus)
  
  
  helpnames <- data.frame(c("paleolat", "paleolng"))
  colnames(helpnames) <- "genus_data.genus"
  dfnames <- rbind(helpnames, ugenus)
  n <- c()
  for (i in 1:length(dfnames$genus_data.genus)) {
    n <- c(n, as.vector(dfnames[i,1]))
  }
  
  nsites <- NULL
  nsites <- cbind(nsites, uloc$paleolat)
  nsites <- cbind(nsites, uloc$paleolng)
  for (i in 1:length(ugenus[,1])) {
    nsites <- cbind(nsites, rep(-1, length(uloc$paleolat)))
  }
  
  colnames(nsites) <- as.vector(dfnames[,1])
  
  #verbessere funktion
  # -filter gro?en df nach lat_i & lng_i vorm suchen
  
  
  for (i in 1:length(nsites[,1])) {
    lat_i <- as.numeric(as.character(nsites[i,1]))
    lng_i <- as.numeric(as.character(nsites[i,2]))
    for (j in 1:length(ugenus[,1])) {
      count <- 0
      genus_j <- as.character(ugenus[j,1])
      flat <-
        subset(genus_data, genus_data$paleolat == lat_i)
      flatlng <- subset(flat, flat$paleolng == lng_i)
      fgen <- subset(flatlng, flatlng$genus == genus_j)
      count <- length(fgen$genus)
      nsites[i, j + 2] <- count
    }
  }
  nsites
}

##############################color palette ########################################

#creating a color palette for the raster
mycols <- colorRampPalette(colors=c(rgb(0.255*3,0.255*3,0,0.5), rgb(0.144*3,0.238*3,0.144,0.5), rgb(0,1,0,0.5)))


#################filter#######################
#filters all occurences which are not species and duplicates in raster cells


#' rank_filter
#' 
#' filters the data frame so tehre are only species left 
#' and for each raster every species only once
#' 
#' @usage rank_filer (data, res, rank)
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng,
#'  can be created with getdata_paleomap
#' @param res resolution of the raster file
#' @param rank rank of interest
#' @return filtered data frame with only species
#' @examples /dontrun{
#' filtered_data <- rank_filter (data, res, rank)
#' show(data)
#'}
#'

rank_filter <- function(data, res, rank){
  #gets colnames for new data frame
  filter <- data[0,]
  #filters only species
  if(rank=="species"){
    data <- subset(data, matched_rank=="species")
  }
  if(rank=="genus"){
    data <- subset(data, genus!="NA")
  }
  if(rank=="family"){
    data <- subset(data, family!="NA")
  }
  if(rank=="order"){
    data <- subset(data, order!="NA")
  }
  #getting each species only once in 10*10 raster cell
  for(i in seq(-180,180,res)){
    frame <- data[0,]
    for (j in seq(-90,90,res)){
      for (k in 1:length(data$paleolng)){
        if(data$paleolng>=i && data$paleolng <=(i+res) && data$paleolat>=j &&
             data$paleolat <=(j+res)){
          frame <- rbind(frame,data[k,])
        }
      }
      
    }
    #add for each raster the filtered data
    if(rank=="species"){ 
      filter <- rbind(filter, subset(frame, !duplicated(frame$matched_name)))
    }
    if(rank=="genus"){
      filter <- rbind(filter, subset(frame, !duplicated(frame$genus)))
    }
    if(rank=="family"){
      filter <- rbind(filter, subset(frame, !duplicated(frame$family)))
    }
    if(rank=="order"){
      filter <- rbind(filter, subset(frame, !duplicated(frame$order)))
    }
  }
  #return filtered data frame
  filter
}

rfilter <- function(data, rank){
  if(rank=="species"){
    data <- subset(data, matched_rank=="species")
  }
  if(rank=="genus"){
    data <- subset(data, genus!="NA")
  }
  if(rank=="family"){
    data <- subset(data, family!="NA")
  }
  if(rank=="order"){
    data <- subset(data, order!="NA")
  }
  data
}
