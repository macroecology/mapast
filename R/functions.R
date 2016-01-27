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
#' @param limit how many entrances from pbdb you want to have, e.g. 500. 
#' There is no limit by default 
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
#' plots your query from paleobioDB directly onto the map of the selected time interval
#' 
#' 
#' @usage pm_plot (interval, base_name, limit, colsea, colland, 
#' colpoints)
#' 
#' @param interval time interval of interest (e.g. jurassic)
#' @param base_name larger taxonomic rank for the query to the paleobioDB (e.g reptiles) 
#' @param limit enables the user to set the maximum number of 
#' records downloaded from the paleobioDB (e.g. 1000)
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colpoints color of the points of the occurences
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
  r<-rasterize(fdata[,5:6],ras
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
#' shape<- pm_getdata (base_name="Canis", interval="Quaternary")
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

#####################pm_divraster####################

#' pm_divraster
#' 
#' creates a raster of the Shannon diversity per cell
#' and makes a plot of the map
#' 
#' @usage pm_divraster  (shape, ngl_data, res, colsea, colland, colborder)
#' 
#' @param shape file from the time interval of interest. 
#' Can be created with get_paleomap
#' @param ngl_data a data frame with number of genus per locality
#' @param res resolution of the raster/ size of the grid cell
#' @param colsea color of the ocean
#' @param colland color of the land masses
#' @param colborder color of the landmass borders
#' @return plot with map of the time intervall, the fossil occurences and the 
#' raster file. And the raster file itself
#' @export 
#' @examples 
#' \dontrun{
#' shape<- pm_getdata (base_name="Canis", interval="Quaternary")
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' ngl_data <- pm_ngl(data)
#' myraster <- pm_divraster (shape, ngl_data)
#' plot(myraster)
#'}

pm_divraster <- function(shape, ngl_data, res=10,
                             colsea="#E5E5E520", colland="#66666680", 
                             colborder="#2B2B2B30"){
  raster <- diversity <- rasterize <- NULL
  #creating a raster in size of the shape file
  ras <- raster(shape, res=res)
  #getting only species data and no duplictaed in a raster field
  cordata1 <- diversity(ngl_data[,3:ncol(ngl_data)])
  cordata <- data.frame(cbind(cbind(data.frame(ngl_data)$paleolng, data.frame(ngl_data)$paleolat), cordata1))
  # colnames(cordiv) <- c("lat","lng","richness")
  cordata[,3] <- cordata[,3]*100
  newdif <- data.frame()
  for(i in 1:length(cordata[,3])){
    if(cordata[i,3]==0){
      newdif <- rbind(newdif, cordata[i,])
    }
    else{
      for(j in 1:cordata[i,3]){
        newdif <- rbind(newdif, cordata[i,])
      }
      
    }
  }
  #getting the raster of the species richness
  r<-rasterize(newdif[,1:2],ras, fun=sum)
  #plotting the map and the raster
  par (mar=c(5,5,5,5))
  plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
       , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
       , xlab="Longitude", ylab="Latitude"
       , main="Raster - richness of genus", xaxs="i", yaxs="i")
  rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea)
  plot(shape, col=colland, border=colborder, add=TRUE)
  plot(r, col= mycols(res*res), add=T)
  box(which="plot")
  #return the raster
  r
}

########pm_ngl###################
#' pm_ngl
#' 
#' calculates the number of genus per locality
#' 
#' @usage pm_ngl (data)
#' 
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @return data frame with number of genus per locality
#' @export 
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' my_ngl <- pm_ngl (data)
#' show(my_ngl)
#'}

pm_ngl <- function(data) {
  #only getting occurences with a known genus
  genus_data <-rfilter(data, "genus")
  #getting locations
  loc <-data.frame(genus_data$paleolat, genus_data$paleolng)
  names(loc) <- c("paleolat", "paleolng")
  #getting unique locations
  uloc <- unique(loc)
  #getting list of unique genus
  genus <- NULL
  genus <- data.frame(genus_data$genus)
  ugenus <- unique(genus)
  
  #creating data frame with number of different genus per unique locality
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
  #fill with default values -1
  for (i in 1:length(ugenus[,1])) {
    nsites <- cbind(nsites, rep(-1, length(uloc$paleolat)))
  }
  
  colnames(nsites) <- as.vector(dfnames[,1])
  
  #getting the number of occurrences of a genus for each locality
  for (i in 1:length(nsites[,1])) {
    #get current lat & lng
    lat_i <- as.numeric(as.character(nsites[i,1]))
    lng_i <- as.numeric(as.character(nsites[i,2]))
    for (j in 1:length(ugenus[,1])) {
      count <- 0
      #get current genus
      genus_j <- as.character(ugenus[j,1])
      #get all genus at locality
      flat <-
        subset(genus_data, genus_data$paleolat == lat_i)
      flatlng <- subset(flat, flat$paleolng == lng_i)
      #select only current genus
      fgen <- subset(flatlng, flatlng$genus == genus_j)
      count <- length(fgen$genus)
      nsites[i, j + 2] <- count
    }
  }
  #return the number of genus per locality data frame
  nsites
}

###################################pm_latrich###################
#' pm_latrich
#' 
#' calculates the latitudinal richness of the genus
#' 
#' @usage pm_latrich (data, res)
#' 
#' @param shape a shape file of the corresponding time map
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param res resolution in of the segmentation of the latitude
#' @param do.plot TRUE if plot is wanted, false otherwise
#' @param bar.side defines wether the barplot of the latitudinal richness is on the right or on the left side of the map
#' @param colsea defines the color of the sea
#' @param colland defines the color f the landmasses
#' @param colborder defines the color of the borders of the land masses
#' @param colpoints defines the colo of the points for the occurrences
#' @param colpointborder defines color of the border of the occurrence points
#' @return data frame with richness of rank and a plot of the continental masses with the occurrences and a barplot of the latitudinal richness
#' @export 
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' latrich <- pm_latrich (data, res=10)
#' show(latrich)
#'}

pm_latrich <- function(shape, data, res, do.plot=TRUE, bar.side="right", 
                       colsea="#E5E5E520", colland="#66666680", 
                       colborder="#2B2B2B30", colpoints="#9ACD3250",
                       colpointborder="black"){
  #setting min and max value for lat
  lr <- data.frame(seq(-90,90-res,res), seq(-90+res, 90, res))
  #creating empty richness data frame
  richn <- c()
  #going through lats
  for(lat in seq(-90,90-res,res)){
    sub1 <- subset(data, data$paleolat>=lat)
    sub2 <- subset(sub1, sub1$paleolat<(lat+res))
    sub2 <- na.omit(sub2$genus)
    sub3 <- unique(sub2)
    #count and save the number of different genus at each latitude
    richn <- c(richn, length(sub3))
  }
  #combine min,max lat and richness
  lr <- cbind(lr,richn)
  colnames(lr) <- c("min paleolat", "max paleolat", "richness")
  
  if(do.plot==TRUE){
   #create plot & barplot
    if(bar.side=="left"){
      barplot(lr$richness, horiz=TRUE, xlim=c(max(lr$richness),0))
    }
   plot(1, type="n", xlim=c(-180,180), ylim=c(-90,90)
        , xaxp=c(180,-180,4), yaxp=c(90,-90,4)
        , xlab="Longitude", ylab="Latitude"
        , main="Raster - richness of genus", xaxs="i", yaxs="i")
   rect(xleft=-180, xright=180, ybottom=-90, ytop=90, col=colsea)
   plot(shape, col=colland, border=colborder, add=TRUE)
   points (data$paleolng, data$paleolat, pch=21, col=colpointborder, bg=colpoints)
   if(bar.side=="right"){
     barplot(lr$richness, horiz=TRUE, xlim=c(0,max(lr$richness)))
   }
  }
  
  #return latitudinal richness
  lr
  
}


#########################pm_nloc#############################
#' pm_nloc
#' 
#' calculates the number of localities per grid cell for genus
#' 
#' @usage pm_nloc (data, res)
#' 
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param res resolution of the grid cells
#' @return data frame with number of localities
#' @export 
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' numloc <- pm_nloc (data, res=10)
#' show(numloc)
#'}

pm_nloc <- function(data, res){
  #get all genus
  fdata <- subset(data, data$genus!="NA")
  #get all lat and lng for localities
  loc <- data.frame(fdata$paleolat, fdata$paleolng)
  colnames(loc)<-c("paleolat", "paleolng")
  
  #create number of localities data frame
  nloc <- data.frame()
  #save -1 as default values
  for(i in 1:((180/res))){
    nloc <- rbind(nloc, rep(-1,((360/res))))
  }
  #create colnames and rownames
  coln <-c()
  rown<-c()
  for(lng in seq(-180,180-res,res)){
    
    coln <- c(coln, paste(as.character(lng), as.character(lng+res), sep=";"))
  }
  for(lat in seq(-90,90-res,res)){
    rown <- c(rown, paste(as.character(lat), as.character(lat+res), sep=";"))
  }
  colnames(nloc) <- coln
  rownames(nloc) <- rown
  #set i=1 as starting value for the row index
  i <- 1
  #go through lng
  for(lng in seq(-180,180-res,res)){
    #set j=1 for the col index
    j<-1
    #go through lat
    for(lat in seq(-90,90-res,res)){
      #lloc default value -1
      lloc <--1
      #get all the localities in the current grid cell
      tloc <- subset(loc, loc$paleolat>=lat)
      tloc <- subset(tloc, tloc$paleolat <lat+res)
      tloc <- subset(tloc, tloc$paleolng>=lng)
      tloc <- subset(tloc, tloc$paleolng <lng+res)
      #get unique loc
      utloc <- unique(tloc)
      #save number of unique localities
      lloc <- length(utloc$paleolat)
      
      nloc[j, i] <- lloc
      #go to the next cell
      j <- j+1
    }
    #go to the next row
    i <- i+1
  }
  #return the number of localities per cell
  nloc
}

################pm_corlatrich###################
#' pm_latdiv
#' 
#' calculates the Shannon diversity (at a genus level) in the latitudinal gradient
#' 
#' @usage pm_corlatrich (ngl_data)
#' 
#' @param ngl_data a data frame with number of genus per locality 
#' Can be created with pm_getdata(interval, base_name)
#' @param shape a shape file of the corresponding time map
#' @param data a data frame with fossil occurrences 
#' Can be created with pm_getdata(interval, base_name)
#' @param do.plot TRUE if plot is wanted, false otherwise
#' @param bar.side defines wether the barplot of the latitudinal richness is on the right or on the left side of the map
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
#' ngl_data <- pm_ngl(data)
#' latdiv <- pm_latdiv (ngl_data)
#' show(latdiv)
#'}

pm_latdiv <- function(ngl_data, shape, data, do.plot=TRUE, bar.side="right", colsea="#E5E5E520", colland="#66666680", 
                          colborder="#2B2B2B30", colpoints="#9ACD3250",
                          colpointborder="black"){
  diversity <- NULL
  #calculate the shannon diversity
  H_data <- diversity(ngl_data[,3:ncol(ngl_data)])
  
  #get the localities
  locs <- cbind(ngl_data[,1],ngl_data[,2], as.vector(H_data))
  cornum <- c()
  #sum the corrected values up if they are in the same latmx,latmin interval
  for(lat in seq(-90,80,10)){
    slocs <- subset(locs, locs[,1]>=lat)
    slocs <- subset(slocs, slocs[,1]<lat+10)
    cornum <- c(cornum ,sum(slocs[,3]))
  }
  latmin <- seq(-90,80,10)
  latmax <- seq(-80,90,10)
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
