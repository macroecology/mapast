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
  if (rank=="species") {
    #save all fossil occurrences where the rank is species
    rank.df <- base::subset(data, !is.na(data$species))
  }
  if (rank=="genus") {
    #save all fossil occurrences where there is a known genus
    rank.df <- base::subset(data, !is.na(data$genus))
  }
  if (rank=="family") {
    #save all fossil occurrences where there is a known family
    rank.df <- base::subset(data, !is.na(data$family))
  }
  if (rank=="order") {
    #save all fossil occurrences where there is a known order
    rank.df <- base::subset(data, data$order!="NA")
  }
  if (rank=="class") {
    #save all fossil occurrences where there is a known class
    rank.df <- base::subset(data, data$class!="NA")
  }
  if (rank=="phylum") {
    #save all fossil occurrences where there is a known phylum
    rank.df <- base::subset(data, data$order!="NA")
  }
  rank.df <- stats::setNames(split(rank.df, seq(nrow(rank.df))), rownames(rank.df))
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
##a column called paleolng, can be created with getdata
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
    #save all fossil occurrences where the rank is species
    rankdata <- base::subset(data, !is.na(data$species))
    rankdata<- rankdata[, c("paleolat", "paleolng", "species")]
  }
  if (rank=="genus") {
    #save all fossil occurrences where there is a known genus
    rankdata <- base::subset(data, !is.na(data$genus))
    rankdata<- rankdata[, c("paleolat", "paleolng", "genus")]
  }
  if (rank=="family") {
    #save all fossil occurrences where there is a known family
    rankdata <- base::subset(data, !is.na(data$family))
    rankdata<- rankdata[, c("paleolat", "paleolng", "family")]
  }
  if (rank=="order") {
    #save all fossil occurrences where there is a known order
    rankdata <- base::subset(data, data$order!="NA")
    rankdata<- rankdata[, c("paleolat", "paleolng", "order")]
  }
  if (rank=="class") {
    #save all fossil occurrences where there is a known class
    rankdata <- base::subset(data, data$class!="NA")
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
