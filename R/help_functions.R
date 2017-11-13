# private functions
# the following functions are used by our package but users can not change their parameters.

##############################color palette ########################################

#creating a color palette for the raster
# mycols <- colorRampPalette(c("goldenrod1","orangered", 
#                              "darkred"))
mycols <- colorRampPalette(c("#F5DEB3",
                             "#D2B48C",
                             "#654321"))


#################.rank_filter#######################
#.rank_filter
# 
#creates the raster files with the number of unique taxa by pixel
#
#@usage .rank_filter(r, data, res, rank)
#@param r blank raster
#@param data a data frame which needs to have a column called paleolat and 
#a column called paleolng, can be created with getdata_paleomap
#@param res resolution of the raster file
#@param rank rank of interest
#@return a raster with the taxa richness
#@examples 
#\dontrun{
#data<- pm_getdata(base_name = "Canis", interval = "Quaternary")
#.rank_filter(r, data, res = 10, rank = "genus")
#}

.rank_filter <- function(r, data, res, rank) {
  #gets colnames for new data frame
  
  if (rank == "species") {
    if (length(data$matched_rank) != 0) {
      identified <- data[!is.na(data$matched_rank), ]
      species <- identified[identified$matched_rank == rank, ]
      S <- split(species, species$matched_no)
    }
    
    R <- lapply(S, function(y) {
      s <- split(y, paste(y$paleolng, y$paleolat))
      X <- as.matrix(do.call(rbind,lapply(s,function(x)c(x$paleolng[1],
                                                       x$paleolat[1],1))))
      X <- rbind(X[1,], X)
      r2 <- rasterize(X[,1:2], r, X[,3])
    })
    
    all<-calc(stack(R), function(x) sum(x,na.rm=TRUE))
    values(all)[values(all)==0]<-NA
    all
  }
  
  if (rank != "species") {
    ranks<-data.frame(rank=c("genus","family","order","class","phylum"),
                      matched_rank=c("genus_no","family_no","order_no",
                                     "class_no","phylum_no"))
    
    if (length(data$matched_rank) != 0) {
      identified <- data[!is.na(data$matched_rank), ]
      col <- paste(ranks$matched_rank[ranks$rank==rank])
      ident <- identified[!is.na(identified[,col]),]
      f <- paste(ident[, col])
      S <- split(ident, f)
    }
    
    R<-lapply(S,function(y){
      s<-split(y,paste(y$paleolng,y$paleolat))
      X<-as.matrix(do.call(rbind,lapply(s,function(x)c(x$paleolng[1],
                                                       x$paleolat[1],1))))
      X<-rbind(X[1,],X)
      r2<-rasterize(X[,1:2],r,X[,3])
      }
    )
    names(R)=NULL
    all<-calc(stack(R), function(x) sum(x,na.rm=TRUE))
    values(all)[values(all)==0]<-NA
    all
  }
  all 
}

################.rfilter###################
#.rfilter
#
#filters the data frame so there are only species left 
#and for each raster every species only once
#@usage .rfilter (data, rank)
#@param data a data frame which needs to have a column called paleolat and 
#a column called paleolng, can be created with getdata_paleomap
#@param rank rank of interest
#@return filtered data frame with only species
#examples 
#\dontrun{
#data<- pm_getdata (base_name="Canis", interval="Quaternary")
#filtered_data <- .rfilter (data, rank="genus")
#show(filtered_data)
#}

.rfilter <- function(data, rank) {
  if (rank=="species") {
    matched_rank <- NULL
    genus <- NULL
    data <- subset(data, data$matched_rank=="species")
    data<- data[, c("paleolat", "paleolng", "matched_name")]
  }
  if (rank=="genus") {
    data <- subset(data, data$genus!="NA")
    data<- data[, c("paleolat", "paleolng", "genus")]
  }
  if (rank=="family") {
    data <- subset(data, data$family!="NA")
    data<- data[, c("paleolat", "paleolng", "family")]
  }
  if (rank=="order") {
    data <- subset(data, data$order!="NA")
    data<- data[, c("paleolat", "paleolng", "order")]
  }
  if (rank=="class") {
    data <- subset(data, data$order!="NA")
    data<- data[, c("paleolat", "paleolng", "class")]
  }
  if (rank=="phylum") {
    data <- subset(data, data$order!="NA")
    data<- data[, c("paleolat", "paleolng", "phylum")]
  }
  
  return(data)
}


################.checkPbdb###################
#.checkPbdb
#
#checks if all columns that we need are in the returned df from the paleobioDB
#@usage .checkPbdb (occ)
#@param occ data frame from pbdb_occurence request
#@return data.frame with needed columns filled with NA if not in original dataframe
#examples 
#\dontrun{
#occ <- base::data.frame(paleobioDB::pbdb_occurrences(base_name=base_name, interval=interval, 
#             show=c("paleoloc", "phylo"), 
#             vocab="pbdb", limit=limit))
#occ <- .checkPbdb(occ)
#}

.checkPbdb <- function(occ){
  cols <- c("occurrence_no", "matched_name", "matched_rank",
            "matched_no", "early_interval", "late_interval",
            "paleolng", "paleolat", "geoplate",
            "genus", "family", "order", "class", "phylum", 
            "genus_no","family_no","order_no",
            "class_no","phylum_no", "early_age", "late_age")
  new <- data.frame(matrix(0, ncol=length(cols), nrow=nrow(occ)))
  colnames(new) <- cols
  for( i in cols){
    if(i %in% names(occ)){
      new[[i]] <- occ[[i]]
    }else{
      v <- rep(NA, nrow(occ))
      new[[i]] <- v
    }
  }
  return(new)
}


################.getShapeInfo#################
#.getShapeInfo
#
#extracts Name, fromage, toage and model info from shape files
#@usage .getShapeInfo(shape)
#@param shape a SpatialPolygonsDataFrame containing a map
#@return vector
#examples 
#\dontrun{
#shape.info <- getShapeInfo(shape)
#}
.getShapeInfo <- function(shape){
  name <- ""
  model <- ""
  fromage <- shape$FROMAGE[1]
  toage <- shape$TOAGE[1]
  shape.name <- shape$NAME[1]
  if(length(grep("Smith", shape.name))!=0){
    model <- "Smith"
    name <- paste0("fromage ", shape$FROMAGE, "mya")
  }else if(length(grep("Golonka", shape.name))!=0){
    model <- "Golonka"
    name <- paste0("fromage ", shape$FROMAGE, "mya")
  }else{
    model <- "GPlates"
    name <- shape.name
  }
  info <- c(name, model, fromage, toage)
  
  info
}
