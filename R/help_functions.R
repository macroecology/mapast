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
#################.rank_filter#######################
# .rank_filter
# 
# creates the raster files with the number of unique taxa by pixel
# 
# @usage .rank_filter(r, data, res, rank)
# @param r blank raster
# @param data a data frame which needs to have a column called paleolat and
# a column called paleolng, can be created with getdata
# @param res resolution of the raster file
# @param rank rank of interest
# @return a raster with the taxa richness
# @keywords internal
# @examples
# \dontrun{
# data<- getdata(base_name = "Canis", interval = "Quaternary")
# .rank_filter(r, data, res = 10, rank = "genus")
# }

.rank_filter <- function(r, data, res, rank) {
  #get species data
  if (rank == "species") {
    if (base::length(data$matched_rank) != 0) {
      #only extract data where a matched rank exists
      rank.df <- data[!base::is.na(data$matched_rank), ]
      #extract only species data
      rank.df <- rank.df[rank.df$matched_rank == rank, ]
      #split of the matched number
      rank.df <- base::split(rank.df, rank.df$matched_no)
    }
  }else{
    ranks <- base::data.frame(rank=c("genus","family","order","class","phylum"),
                      matched_rank=c("genus_no","family_no","order_no",
                                     "class_no","phylum_no"))
    
    if (base::length(data$matched_rank) != 0) {
      rank.df <- data[!base::is.na(data$matched_rank), ]
      rankcolumn <- base::paste(ranks$matched_rank[ranks$rank==rank])
      rank.df <- rank.df[!base::is.na(rank.df[,rankcolumn]),]
      split_no <- base::paste(rank.df[, rankcolumn])
      rank.df <- base::split(rank.df, split_no)
    }
  }
  rankraster <- base::lapply(rank.df, function(y) {
    #split off paleolat and paleolng
    latlng <- base::split(y, base::paste(y$paleolng, y$paleolat))
    latlngmatrix <- base::as.matrix(base::do.call(base::rbind,base::lapply(latlng,function(x)c(x$paleolng[1],
                                                     x$paleolat[1],1))))
    latlngmatrix <- base::rbind(latlngmatrix[1,], latlngmatrix)
    latlngras <- raster::rasterize(latlngmatrix[,1:2], r, latlngmatrix[,3])
  })
  rankraster <- raster::calc(raster::stack(rankraster), function(x) base::sum(x,na.rm=TRUE))
  raster::values(rankraster)[raster::values(rankraster)==0]<-NA
  return(rankraster)
}

################.rfilter###################
##.rfilter
##
##filters the data frame so there are only species left 
##and for each raster every species only once
##@usage .rfilter (data, rank)
##@param data a data frame which needs to have a column called paleolat and 
##a column called paleolng, can be created with getdata_paleomap
##@param rank rank of interest
##@return filtered data frame with only species
##examples 
##\dontrun{
##data<- getdata (base_name="Canis", interval="Quaternary")
##filtered_data <- .rfilter (data, rank="genus")
##show(filtered_data)
##}

.rfilter <- function(data, rank) {
  if (rank=="species") {
    matched_rank <- NULL
    genus <- NULL
    #save all fossil occurrences where the rank is species
    rankdata <- base::subset(data, data$matched_rank=="species")
    rankdata<- rankdata[, c("paleolat", "paleolng", "matched_name")]
  }
  if (rank=="genus") {
    #save all fossil occurrences where there is a known genus
    rankdata <- base::subset(data, data$genus!="NA")
    rankdata<- rankdata[, c("paleolat", "paleolng", "genus")]
  }
  if (rank=="family") {
    #save all fossil occurrences where there is a known family
    rankdata <- base::subset(data, data$family!="NA")
    rankdata<- rankdata[, c("paleolat", "paleolng", "family")]
  }
  if (rank=="order") {
    #save all fossil occurrences where there is a known order
    rankdata <- base::subset(data, data$order!="NA")
    rankdata<- rankdata[, c("paleolat", "paleolng", "order")]
  }
  if (rank=="class") {
    #save all fossil occurrences where there is a known class
    rankdata <- base::subset(data, data$order!="NA")
    rankdata<- rankdata[, c("paleolat", "paleolng", "class")]
  }
  if (rank=="phylum") {
    #save all fossil occurrences where there is a known phylum
    rankdata <- base::subset(data, data$order!="NA")
    rankdata<- rankdata[, c("paleolat", "paleolng", "phylum")]
  }
  #return the rankdata filtered for the taxonomic rank
  return(rankdata)
}


################.checkPbdb###################
##.checkPbdb
##
##checks if all columns that we need are in the returned df from the paleobioDB
##@usage .checkPbdb (occ)
##@param occ data frame from pbdb_occurence request
##@return data.frame with needed columns filled with NA if not in original dataframe
##examples 
##\dontrun{
##occ <- base::data.frame(paleobioDB::pbdb_occurrences(base_name=base_name, interval=interval, 
##             show=c("paleoloc", "phylo"), 
##             vocab="pbdb", limit=limit))
##occ <- .checkPbdb(occ)
##}

.checkPbdb <- function(occ){
  #list of wanted columns
  cols <- c("occurrence_no", "matched_name", "matched_rank",
            "matched_no", "early_interval", "late_interval",
            "paleolng", "paleolat", "geoplate",
            "genus", "family", "order", "class", "phylum", 
            "genus_no","family_no","order_no",
            "class_no","phylum_no", "early_age", "late_age")
  # create a data frame with all wanted columns
  complete.df <- base::data.frame(base::matrix(0, ncol=length(cols), nrow=nrow(occ)))
  base::colnames(complete.df) <- cols
  #go hrough columns
  for( i in cols){
    #if column in original df exists, save in new df
    if(i %in% names(occ)){
      complete.df[[i]] <- occ[[i]]
    }else{
      #if it is not existing fill the new df with NA's
      missingcol <- base::rep(NA, base::nrow(occ))
      complete.df[[i]] <- missingcol
    }
  }
  return(complete.df)
}


################.getShapeInfo#################
##.getShapeInfo
##
##extracts Name, fromage, toage and model info from shape files
##@usage .getShapeInfo(shape)
##@param shape a SpatialPolygonsDataFrame containing a map
##@return vector
##examples 
##\dontrun{
##shape.info <- getShapeInfo(shape)
##}
.getShapeInfo <- function(shape){
  name <- ""
  model <- ""
  fromage <- shape$FROMAGE[1]
  toage <- shape$TOAGE[1]
  shape.name <- shape$NAME[1]
  if(base::length(base::grep("Smith", shape.name))!=0){
    model <- "Smith"
    name <- base::paste0("fromage ", shape$FROMAGE, " mya")
  }else if(base::length(base::grep("Golonka", shape.name))!=0){
    model <- "Golonka"
    name <- base::paste0("fromage ", shape$FROMAGE, " mya")
  }else{
    model <- "GPlates"
    name <- shape.name
  }
  info <- c(name, model, fromage, toage)
  
  return(info)
}


################.mapAvailable#################
##.mapAvailable
##
##looks up if map is available
##@usage .mapAvailable(interval, model)
##@param interval time interval of interest
##@param model model of interest
##@return boolean
.mapAvailable <- function(interval, model){
  df_maps <- NULL
  utils::data(df_maps,envir = base::environment())
  
  maps <- df_maps[df_maps$interval==interval,]
  maps <- maps[maps$model==model,]
  
  available <- base::length(maps$model)>=1
  
  return(available)
  
}