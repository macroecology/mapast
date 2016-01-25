# private functions
# the following functions are used by our package but users can not change their parameters.

##############################color palette ########################################

#creating a color palette for the raster
mycols <- colorRampPalette(colors=c(rgb(0.255*3,0.255*3,0,0.5), 
                                    rgb(0.144*3,0.238*3,0.144,0.5), 
                                    rgb(0,1,0,0.5)))


#################filter#######################
#' rank_filter
#' 
#' filters the data frame so tehre are only species left 
#' and for each raster every species only once
#' 
#' @usage rank_filter (data, res, rank)
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng,
#'  can be created with getdata_paleomap
#' @param res resolution of the raster file
#' @param rank rank of interest
#' @return filtered data frame with only species
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' filtered_data <- rank_filter (data, res=10, rank="genus")
#' show(filtered_data)
#'}

rank_filter <- function(data, res, rank){
  #gets colnames for new data frame
  filter <- data[0,]
  data <- rfilter(data, rank)
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

#' rfilter
#' 
#' filters the data frame so tehre are only species left 
#' and for each raster every species only once
#' 
#' @usage rfilter (data, rank)
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng,
#'  can be created with getdata_paleomap
#' @param rank rank of interest
#' @return filtered data frame with only species
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' filtered_data <- rfilter (data, rank="genus")
#' show(filtered_data)
#'}

rfilter <- function(data, rank){
  if(rank=="species"){
    matched_rank <- NULL
    genus <- NULL
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


#########help for shape files##############
#we need this function because we cannot open lazyload data with an input parameter because it is no value.

## we might hack this with "with" or "null" for avoiding NOTE on check: 'no visible binding for global variable'
## see: http://stackoverflw.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
Aalenian <- Aeronian <- Albian <- Anisian <- Aptian <- Aquitanian <- Artinskian <- Asselian <- Bajocian <- Barremian <- Bartonian <- Bashkirian <- Bathonian <- Berriasian <- Burdigalian <- Calabrian <- Callovian <- Cambrian <- Campanian <- Capitanian <- Carboniferous <- Carnian <- Cenomanian <- Cenozoic <- Changhsingian <- Chattian <- Cisuralian <- Coniacian <- Cretaceous <- Danian <- Dapingian <- Darriwilian <- Devonian <- Drumian <- Early_Cretaceous <- Early_Devonian <- Early_Jurassic <- Early_Ordovician<- Early_Triassic <- Eifelian <- Emsian <- Eocene <- Famennian <- Floian <- Fortunian <- Frasnian <- Furongian <- Gelasian <- Givetian <- Gorstian <- Guadalupian <- Guzhangian <- Gzhelian <- Hauterivian <- Hettangian <- Hirnantian <- Holocene <- Homerian <- Induan <- Jiangshanian <- Jurassic <- Kasimovian <- Katian <- Kimmeridgian <- Kungurian <- Ladinian <- Langhian <- Late_Cretaceous <- Late_Devonian <- Late_Jurassic <- Late_Pleistocene <- Late_Ordovician <- Late_Triassic <- Llandovery <- Lochkovian <- Lopingian <- Ludfordian <- Ludlow <- Lutetian <- Maastrichtian <- Mesozoic <- Messinian <- Middle_Devonian <- Middle_Jurassic <- Middle_Ordovician <- Middle_Pleistocene <- Middle_Triassic <- Miocene <- Mississippian <- Moscovian <- Neogene <- Norian <- Olenekian <- Oligocene <- Ordovician <- Oxfordian <- Paibian <- Paleocene <- Paleogene <- Paleozoic <- Pennsylvanian <- Permian <- Phanerozoic <- Piacenzian <- Pleistocene <- Pliensbachian <- Pliocene <- Pragian <- Priabonian <- Pridoli <- Quaternary <- Rhaetian <- Rhuddanian <- Roadian <- Rupelian <- Sakmarian <- Sandbian <- Santonian <- Selandian <- Series_2 <- Series_3 <- Serpukhovian <- Serravallian <- Sheinwoodian <- Silurian <- Sinemurian <- Stage_2 <- Stage_3 <- Stage_4 <- Stage_5 <- Stage_10 <- Telychian <- Terreneuvian <- Thanetian <- Tithonian <- Toarcian <- Tortonian <- Tournaisian <- Tremadocian <- Triassic <- Turonian <- Valanginian <- Visean <- Wenlock <- Wordian <- Wuchiapingian <- Ypresian <- Zanclean <- NULL 
getshape <- function(interval, colland, colborder, do.plot){
  if(interval=="Paleozoic" || interval=="paleozoic")
    if(do.plot== TRUE){{
      with(data, plot(Paleozoic, col=colland, border=colborder, add=TRUE))
    }
      return(Paleozoic)
    }
  if(interval=="Cambrian" || interval=="cambrian"){
    if(do.plot== TRUE){
      plot(Cambrian, col=colland, border=colborder, add=TRUE)
    }
    return(Cambrian)
  }
  if(interval=="Terreneuvian" || interval=="terreneuvian"){
    if(do.plot== TRUE){
      plot(Terreneuvian, col=colland, border=colborder, add=TRUE)
    }
    return(Terreneuvian)
  }
  if(interval=="Fortunian" || interval=="fortunian"){
    if(do.plot== TRUE){
      plot(Fortunian, col=colland, border=colborder, add=TRUE)
    }
    return(Fortunian)
  }
  if(interval=="Stage_2" || interval=="stage_2"){
    if(do.plot== TRUE){
      plot(Stage_2, col=colland, border=colborder, add=TRUE)
    }
    return(Stage_2)
  }
  if(interval=="Series_2" || interval=="series_2"){
    if(do.plot== TRUE){
      plot(Series_2, col=colland, border=colborder, add=TRUE)
    }
    return(Series_2)
  }
  if(interval=="Stage_3" || interval=="stage_3"){
    if(do.plot== TRUE){
      plot(Stage_3, col=colland, border=colborder, add=TRUE)
    }
    return(Stage_3)
  }
  if(interval=="Stage_4" || interval=="stage_4"){
    if(do.plot== TRUE){
      plot(Stage_4, col=colland, border=colborder, add=TRUE)
    }
    return(Stage_4)
  }
  if(interval=="Series_3" || interval=="series_3"){
    if(do.plot== TRUE){
      plot(Series_3, col=colland, border=colborder, add=TRUE)
    }
    return(Series_3)
  }
  if(interval=="Stage_5" || interval=="stage_5"){
    if(do.plot== TRUE){
      plot(Stage_5, col=colland, border=colborder, add=TRUE)
    }
    return(Stage_5)
  }
  if(interval=="Drumian" || interval=="drumian"){
    if(do.plot== TRUE){
      plot(Drumian, col=colland, border=colborder, add=TRUE)
    }
    return(Drumian)
  }
  if(interval=="Guzhangian" || interval=="guzhangian"){
    if(do.plot== TRUE){
      plot(Guzhangian, col=colland, border=colborder, add=TRUE)
    }
    return(Guzhangian)
  }
  if(interval=="Furongian" || interval=="furongian"){
    if(do.plot== TRUE){
      plot(Furongian, col=colland, border=colborder, add=TRUE)
    }
    return(Furongian)
  }
  if(interval=="Paibian" || interval=="paibian"){
    if(do.plot== TRUE){
      plot(Paibian, col=colland, border=colborder, add=TRUE)
    }
    return(Paibian)
  }
  if(interval=="Jiangshanian" || interval=="jiangshanian"){
    if(do.plot== TRUE){
      plot(Jiangshanian, col=colland, border=colborder, add=TRUE)
    }
    return(Jiangshanian)
  }
  if(interval=="Stage_10" || interval=="stage_10"){
    if(do.plot== TRUE){
      plot(Stage_10, col=colland, border=colborder, add=TRUE)
    }
    return(Stage_10)
  }
  if(interval=="Ordovician" || interval=="ordovician"){
    if(do.plot== TRUE){
      plot(Ordovician, col=colland, border=colborder, add=TRUE)
    }
    return(Ordovician)
  }
  if(interval=="Early_Ordovician" || interval=="early_ordovician"){
    if(do.plot== TRUE){
      plot(Early_Ordovician, col=colland, border=colborder, add=TRUE)
    }
    return(Early_Ordovician)
  }
  if(interval=="Tremadocian" || interval=="tremadocian"){
    if(do.plot== TRUE){ 
      plot(Tremadocian, col=colland, border=colborder, add=TRUE)
    }
    return(Tremadocian)
  }
  if(interval=="Floian" || interval=="floian"){
    if(do.plot== TRUE){
      plot(Floian, col=colland, border=colborder, add=TRUE)
    }
    return(Floian)
  }
  if(interval=="Middle_Ordovician" || interval=="middle_ordovician"){
    if(do.plot== TRUE){
      plot(Middle_Ordovician, col=colland, border=colborder, add=TRUE)
    }
    return(Middle_Ordovician)
  }
  if(interval=="Middle_Pleistocene" || interval=="middle_pleistocene"){
    if(do.plot== TRUE){
      plot(Middle_Pleistocene, col=colland, border=colborder, add=TRUE)
    }
    return(Middle_Pleistocene)
  }
  if(interval=="Dapingian" || interval=="dapingian"){
    if(do.plot== TRUE){
      plot(Dapingian, col=colland, border=colborder, add=TRUE)
    }
    return(Dapingian)
  }
  if(interval=="Darriwilian" || interval=="darriwilian"){
    if(do.plot== TRUE){
      plot(Darriwilian, col=colland, border=colborder, add=TRUE)
    }
    return(Darriwilian)
  }
  if(interval=="Late_Ordovician" || interval=="late_ordovician"){
    if(do.plot== TRUE){
      plot(Late_Ordovician, col=colland, border=colborder, add=TRUE)
    }
    return(Late_Ordovician)
  }
  if(interval=="Late_Pleistocene" || interval=="late_pleistocene"){
    if(do.plot== TRUE){
      plot(Late_Pleistocene, col=colland, border=colborder, add=TRUE)
    }
    return(Late_Pleistocene)
  }
  if(interval=="Sandbian" || interval=="sandbian"){
    if(do.plot== TRUE){
      plot(Sandbian, col=colland, border=colborder, add=TRUE)
    }
    return(Sandbian)
  }
  if(interval=="Katian" || interval=="katian"){
    if(do.plot== TRUE){
      plot(Katian, col=colland, border=colborder, add=TRUE)
    }
    return(Katian)
  }
  if(interval=="Holocene" || interval=="holocene"){
    if(do.plot== TRUE){
      plot(Holocene, col=colland, border=colborder, add=TRUE)
    }
    return(Holocene)
  }
  if(interval=="Hirnantian" || interval=="hirnantian"){
    if(do.plot== TRUE){
      plot(Hirnantian, col=colland, border=colborder, add=TRUE)
    }
    return(Hirnantian)
  }
  if(interval=="Llandovery" || interval=="llandovery"){
    if(do.plot== TRUE){
      plot(Llandovery, col=colland, border=colborder, add=TRUE)
    }
    return(Llandovery)
  }
  if(interval=="Rhuddanian" || interval=="rhuddanian"){
    if(do.plot== TRUE){
      plot(Rhuddanian, col=colland, border=colborder, add=TRUE)
    }
    return(Rhuddanian)
  }
  if(interval=="Aeronian" || interval=="aeronian"){
    if(do.plot== TRUE){
      plot(Aeronian, col=colland, border=colborder, add=TRUE)
    }
    return(Aeronian)
  }
  if(interval=="Telychian" || interval=="telychian"){
    if(do.plot== TRUE){
      plot(Telychian, col=colland, border=colborder, add=TRUE)
    }
    return(Telychian)
  }
  if(interval=="Wenlock" || interval=="wenlock"){
    if(do.plot== TRUE){
      plot(Wenlock, col=colland, border=colborder, add=TRUE)
    }
    return(Wenlock)
  }
  if(interval=="Sheinwoodian" || interval=="sheinwoodian"){
    if(do.plot== TRUE){
      plot(Sheinwoodian, col=colland, border=colborder, add=TRUE)
    }
    return(Sheinwoodian)
  }
  if(interval=="Homerian" || interval=="homerian"){
    if(do.plot== TRUE){
      plot(Homerian, col=colland, border=colborder, add=TRUE)
    }
    return(Homerian)
  }
  if(interval=="Silurian" || interval=="silurian"){
    if(do.plot== TRUE){
      plot(Silurian, col=colland, border=colborder, add=TRUE)
    }
    return(Silurian)
  }
  if(interval=="Ludlow" || interval=="ludlow"){
    if(do.plot== TRUE){
      plot(Ludlow, col=colland, border=colborder, add=TRUE)
    }
    return(Ludlow)
  }
  if(interval=="Gorstian" || interval=="gorstian"){
    if(do.plot== TRUE){
      plot(Gorstian, col=colland, border=colborder, add=TRUE)
    }
    return(Gorstian)
  }
  if(interval=="Ludfordian" || interval=="ludfordian"){
    if(do.plot== TRUE){
      plot(Ludfordian, col=colland, border=colborder, add=TRUE)
    }
    return(Ludfordian)
  }
  if(interval=="Pridoli" || interval=="pridoli"){
    if(do.plot== TRUE){
      plot(Pridoli, col=colland, border=colborder, add=TRUE)
    }
    return(Pridoli)
  }
  if(interval=="Devonian" || interval=="devonian"){
    if(do.plot== TRUE){
      plot(Devonian, col=colland, border=colborder, add=TRUE)
    }
    return(Devonian)
  }
  if(interval=="Early_Devonian" || interval=="early_devonian"){
    if(do.plot== TRUE){
      plot(Early_Devonian, col=colland, border=colborder, add=TRUE)
    }
    return(Early_Devonian)
  }
  if(interval=="Lochkovian" || interval=="lochkovian"){
    if(do.plot== TRUE){
      plot(Lochkovian, col=colland, border=colborder, add=TRUE)
    }
    return(Lochkovian)
  }
  if(interval=="Pragian" || interval=="pragian"){
    if(do.plot== TRUE){
      plot(Pragian, col=colland, border=colborder, add=TRUE)
    }
    return(Pragian)
  }
  if(interval=="Emsian" || interval=="emsian"){
    if(do.plot== TRUE){
      plot(Emsian, col=colland, border=colborder, add=TRUE)
    }
    return(Emsian)
  }
  if(interval=="Middle_Devonian" || interval=="middle_devonian"){
    if(do.plot== TRUE){
      plot(Middle_Devonian, col=colland, border=colborder, add=TRUE)
    }
    return(Middle_Devonian)
  }
  if(interval=="Middle_Jurassic" || interval=="middle_jurassic"){
    if(do.plot== TRUE){
      plot(Middle_Jurassic, col=colland, border=colborder, add=TRUE)
    }
    return(Middle_Jurassic)
  }
  if(interval=="Eifelian" || interval=="eifelian"){
    if(do.plot== TRUE){
      plot(Eifelian, col=colland, border=colborder, add=TRUE)
    }
    return(Eifelian)
  }
  if(interval=="Givetian" || interval=="givetian"){
    if(do.plot== TRUE){
      plot(Givetian, col=colland, border=colborder, add=TRUE)
    }
    return(Givetian)
  }
  if(interval=="Late_Devonian" || interval=="late_devonian"){
    if(do.plot== TRUE){
      plot(Late_Devonian, col=colland, border=colborder, add=TRUE)
    }
    return(Late_Devonian)
  }
  if(interval=="Frasnian" || interval=="frasnian"){
    if(do.plot== TRUE){
      plot(Frasnian, col=colland, border=colborder, add=TRUE)
    }
    return(Frasnian)
  }
  if(interval=="Famennian" || interval=="famennian"){
    if(do.plot== TRUE){
      plot(Famennian, col=colland, border=colborder, add=TRUE)
    }
    return(Famennian)
  }
  if(interval=="Carboniferous" || interval=="carboniferous"){
    if(do.plot== TRUE){
      plot(Carboniferous, col=colland, border=colborder, add=TRUE)
    }
    return(Carboniferous)
  }
  if(interval=="Mississippian" || interval=="mississippian"){
    if(do.plot== TRUE){
      plot(Mississippian, col=colland, border=colborder, add=TRUE)
    }
    return(Mississippian)
  }
  if(interval=="Visean" || interval=="visean"){
    if(do.plot== TRUE){
      plot(Visean, col=colland, border=colborder, add=TRUE)
    }
    return(Visean)
  }
  if(interval=="Serpukhovian" || interval=="serpukhovian"){
    if(do.plot== TRUE){
      plot(Serpukhovian, col=colland, border=colborder, add=TRUE)
    }
    return(Serpukhovian)
  }
  if(interval=="Pennsylvanian" || interval=="pennsylvanian"){
    if(do.plot== TRUE){
      plot(Pennsylvanian, col=colland, border=colborder, add=TRUE)
    }
    return(Pennsylvanian)
  }
  if(interval=="Bashkirian" || interval=="bashkirian"){
    if(do.plot== TRUE){
      plot(Bashkirian, col=colland, border=colborder, add=TRUE)
    }
    return(Bashkirian)
  }
  if(interval=="Moscovian" || interval=="moscovian"){
    if(do.plot== TRUE){
      plot(Moscovian, col=colland, border=colborder, add=TRUE)
    }
    return(Moscovian)
  }
  if(interval=="Kasimovian" || interval=="kasimovian"){
    if(do.plot== TRUE){
      plot(Kasimovian, col=colland, border=colborder, add=TRUE)
    }
    return(Kasimovian)
  }
  if(interval=="Gzhelian" || interval=="gzhelian"){
    if(do.plot== TRUE){
      plot(Gzhelian, col=colland, border=colborder, add=TRUE)
    }
    return(Gzhelian)
  }
  if(interval=="Phanerozoic" || interval=="phanerozoic"){
    if(do.plot== TRUE){
      plot(Phanerozoic, col=colland, border=colborder, add=TRUE)
    }
    return(Phanerozoic)
  }
  if(interval=="Permian" || interval=="permian"){
    if(do.plot== TRUE){
      plot(Permian, col=colland, border=colborder, add=TRUE)
    }
    return(Permian)
  }
  if(interval=="Piacenzian" || interval=="piacenzian"){
    if(do.plot== TRUE){
      plot(Piacenzian, col=colland, border=colborder, add=TRUE)
    }
    return(Piacenzian)
  }
  if(interval=="Cisuralian" || interval=="cisuralian"){
    if(do.plot== TRUE){
      plot(Cisuralian, col=colland, border=colborder, add=TRUE)
    }
    return(Cisuralian)
  }
  if(interval=="Asselian" || interval=="asselian"){
    if(do.plot== TRUE){
      plot(Asselian, col=colland, border=colborder, add=TRUE)
    }
    return(Asselian)
  }
  if(interval=="Sakmarian" || interval=="sakmarian"){
    if(do.plot== TRUE){
      plot(Sakmarian, col=colland, border=colborder, add=TRUE)
    }
    return(Sakmarian)
  }
  if(interval=="Artinskian" || interval=="artinskian"){
    if(do.plot== TRUE){
      plot(Artinskian, col=colland, border=colborder, add=TRUE)
    }
    return(Artinskian)
  }
  if(interval=="Kungurian" || interval=="kungurian"){
    if(do.plot== TRUE){
      plot(Kungurian, col=colland, border=colborder, add=TRUE)
    }
    return(Kungurian)
  }
  if(interval=="Guadalupian" || interval=="guadalupian"){
    if(do.plot== TRUE){
      plot(Guadalupian, col=colland, border=colborder, add=TRUE)
    }
    return(Guadalupian)
  }
  if(interval=="Roadian" || interval=="roadian"){
    if(do.plot== TRUE){
      plot(Roadian, col=colland, border=colborder, add=TRUE)
    }
    return(Roadian)
  }
  if(interval=="Wordian" || interval=="wordian"){
    if(do.plot== TRUE){
      plot(Wordian, col=colland, border=colborder, add=TRUE)
    }
    return(Wordian)
  }
  if(interval=="Capitanian" || interval=="capitanian"){
    if(do.plot== TRUE){
      plot(Capitanian, col=colland, border=colborder, add=TRUE)
    }
    return(Capitanian)
  }
  if(interval=="Lopingian" || interval=="lopingian"){
    if(do.plot== TRUE){
      plot(Lopingian, col=colland, border=colborder, add=TRUE)
    }
    return(Lopingian)
  }
  if(interval=="Wuchiapingian" || interval=="wuchiapingian"){
    if(do.plot== TRUE){
      plot(Wuchiapingian, col=colland, border=colborder, add=TRUE)
    }
    return(Wuchiapingian)
  }
  if(interval=="Changhsingian" || interval=="changhsingian"){
    if(do.plot== TRUE){
      plot(Changhsingian, col=colland, border=colborder, add=TRUE)
    }
    return(Changhsingian)
  }
  if(interval=="Triassic" || interval=="triassic"){
    if(do.plot== TRUE){
      plot(Triassic, col=colland, border=colborder, add=TRUE)
    }
    return(Triassic)
  }
  if(interval=="Early_Triassic" || interval=="early_triassic"){
    if(do.plot== TRUE){
      plot(Early_Triassic, col=colland, border=colborder, add=TRUE)
    }
    return(Early_Triassic)
  }
  if(interval=="Induan" || interval=="induan"){
    if(do.plot== TRUE){
      plot(Induan, col=colland, border=colborder, add=TRUE)
    }
    return(Induan)
  }
  if(interval=="Olenekian" || interval=="olenekian"){
    if(do.plot== TRUE){
      plot(Olenekian, col=colland, border=colborder, add=TRUE)
    }
    return(Olenekian)
  }
  if(interval=="Middle_Triassic" || interval=="middle_triassic"){
    if(do.plot== TRUE){
      plot(Middle_Triassic, col=colland, border=colborder, add=TRUE)
    }
    return(Middle_Triassic)
  }
  if(interval=="Anisian" || interval=="anisian"){
    if(do.plot== TRUE){
      plot(Anisian, col=colland, border=colborder, add=TRUE)
    }
    return(Anisian)
  }
  if(interval=="Ladinian" || interval=="ladinian"){
    if(do.plot== TRUE){
      plot(Ladinian, col=colland, border=colborder, add=TRUE)
    }
    return(Ladinian)
  }
  if(interval=="Late_Triassic" || interval=="late_triassic"){
    if(do.plot== TRUE){
      plot(Late_Triassic, col=colland, border=colborder, add=TRUE)
    }
    return(Late_Triassic)
  }
  if(interval=="Carnian" || interval=="carnian"){
    if(do.plot== TRUE){
      plot(Carnian, col=colland, border=colborder, add=TRUE)
    }
    return(Carnian)
  }
  if(interval=="Norian" || interval=="norian"){
    if(do.plot== TRUE){
      plot(Norian, col=colland, border=colborder, add=TRUE)
    }
    return(Norian)
  }
  if(interval=="Rhaetian" || interval=="rhaetian"){
    if(do.plot== TRUE){
      plot(Rhaetian, col=colland, border=colborder, add=TRUE)
    }
    return(Rhaetian)
  }
  if(interval=="Jurassic" || interval=="jurassic"){
    if(do.plot== TRUE){
      plot(Jurassic, col=colland, border=colborder, add=TRUE)
    }
    return(Jurassic)
  }
  if(interval=="Early_Jurassic" || interval=="early_jurassic"){
    if(do.plot== TRUE){
      plot(Early_Jurassic, col=colland, border=colborder, add=TRUE)
    }
    return(Early_Jurassic)
  }
  if(interval=="Hettangian" || interval=="hettangian"){
    if(do.plot== TRUE){
      plot(Hettangian, col=colland, border=colborder, add=TRUE)
    }
    return(Hettangian)
  }
  if(interval=="Sinemurian" || interval=="sinemurian"){
    if(do.plot== TRUE){
      plot(Sinemurian, col=colland, border=colborder, add=TRUE)
    }
    return(Sinemurian)
  }
  if(interval=="Pliensbachian" || interval=="pliensbachian"){
    if(do.plot== TRUE){
      plot(Pliensbachian, col=colland, border=colborder, add=TRUE)
    }
    return(Pliensbachian)
  }
  if(interval=="Toarcian" || interval=="toarcian"){
    if(do.plot== TRUE){
      plot(Toarcian, col=colland, border=colborder, add=TRUE)
    }
    return(Toarcian)
  }
  if(interval=="Aalenian" || interval=="aalenian"){
    if(do.plot== TRUE){
      plot(Aalenian, col=colland, border=colborder, add=TRUE)
    }
    return(Aalenian)
  }
  if(interval=="Bajocian" || interval=="bajocian"){
    if(do.plot== TRUE){
      plot(Bajocian, col=colland, border=colborder, add=TRUE)
    }
    return(Bajocian)
  }
  if(interval=="Bathonian" || interval=="bathonian"){
    if(do.plot== TRUE){
      plot(Bathonian, col=colland, border=colborder, add=TRUE)
    }
    return(Bathonian)
  }
  if(interval=="Callovian" || interval=="callovian"){
    if(do.plot== TRUE){
      plot(Callovian, col=colland, border=colborder, add=TRUE)
    }
    return(Callovian)
  }
  if(interval=="Late_Jurassic" || interval=="late_jurassic"){
    if(do.plot== TRUE){
      plot(Late_Jurassic, col=colland, border=colborder, add=TRUE)
    }
    return(Late_Jurassic)
  }
  if(interval=="Oxfordian" || interval=="oxfordian"){
    if(do.plot== TRUE){
      plot(Oxfordian, col=colland, border=colborder, add=TRUE)
    }
    return(Oxfordian)
  }
  if(interval=="Kimmeridgian" || interval=="kimmeridgian"){
    if(do.plot== TRUE){
      plot(Kimmeridgian, col=colland, border=colborder, add=TRUE)
    }
    return(Kimmeridgian)
  }
  if(interval=="Tithonian" || interval=="tithonian"){
    if(do.plot== TRUE){
      plot(Tithonian, col=colland, border=colborder, add=TRUE)
    }
    return(Tithonian)
  }
  if(interval=="Cretaceous" || interval=="cretaceous"){
    if(do.plot== TRUE){
      plot(Cretaceous, col=colland, border=colborder, add=TRUE)
    }
    return(Cretaceous)
  }
  if(interval=="Early_Cretaceous" || interval=="early_cretaceous"){
    if(do.plot== TRUE){
      plot(Early_Cretaceous, col=colland, border=colborder, add=TRUE)
    }
    return(Early_Cretaceous)
  }
  if(interval=="Berriasian" || interval=="Berriasian"){
    if(do.plot== TRUE){
      plot(Berriasian, col=colland, border=colborder, add=TRUE)
    }
    return(Berriasian)
  }
  if(interval=="Valanginian" || interval=="valanginian"){
    if(do.plot== TRUE){
      plot(Valanginian, col=colland, border=colborder, add=TRUE)
    }
    return(Valanginian)
  }
  if(interval=="Hauterivian" || interval=="hauterivian"){
    if(do.plot== TRUE){
      plot(Hauterivian, col=colland, border=colborder, add=TRUE)
    }
    return(Hauterivian)
  }
  if(interval=="Barremian" || interval=="barremian"){
    if(do.plot== TRUE){
      plot(Barremian, col=colland, border=colborder, add=TRUE)
    }
    return(Barremian)
  }
  if(interval=="Aptian" || interval=="aptian"){
    if(do.plot== TRUE){
      plot(Aptian, col=colland, border=colborder, add=TRUE)
    }
    return(Aptian)
  }
  if(interval=="Albian" || interval=="albian"){
    if(do.plot== TRUE){
      plot(Albian, col=colland, border=colborder, add=TRUE)
    }
    return(Albian)
  }
  if(interval=="Late_Cretaceous" || interval=="late_cretaceous"){
    if(do.plot== TRUE){
      plot(Late_Cretaceous, col=colland, border=colborder, add=TRUE)
    }
    return(Late_Cretaceous)
  }
  if(interval=="Cenomanian" || interval=="cenomanian"){
    if(do.plot== TRUE){
      plot(Cenomanian, col=colland, border=colborder, add=TRUE)
    }
    return(Cenomanian)
  }
  if(interval=="Turonian" || interval=="turonian"){
    if(do.plot== TRUE){
      plot(Turonian, col=colland, border=colborder, add=TRUE)
    }
    return(Turonian)
  }
  if(interval=="Coniacian" || interval=="coniacian"){
    if(do.plot== TRUE){
      plot(Coniacian, col=colland, border=colborder, add=TRUE)
    }
    return(Coniacian)
  }
  if(interval=="Santonian" || interval=="santonian"){
    if(do.plot== TRUE){
      plot(Santonian, col=colland, border=colborder, add=TRUE)
    }
    return(Santonian)
  }
  if(interval=="Campanian" || interval=="campanian"){
    if(do.plot== TRUE){
      plot(Campanian, col=colland, border=colborder, add=TRUE)
    }
    return(Campanian)
  }
  if(interval=="Mesozoic" || interval=="mesozoic"){
    if(do.plot== TRUE){
      plot(Mesozoic, col=colland, border=colborder, add=TRUE)
    }
    return(Mesozoic)
  }
  if(interval=="Maastrichtian" || interval=="maastrichtian"){
    if(do.plot== TRUE){
      plot(Maastrichtian, col=colland, border=colborder, add=TRUE)
    }
    return(Maastrichtian)
  }
  if(interval=="Cenozoic" || interval=="cenozoic"){
    if(do.plot== TRUE){
      plot(Cenozoic, col=colland, border=colborder, add=TRUE)
    }
    return(Cenozoic)
  }
  if(interval=="Paleogene" || interval=="paleogene"){
    if(do.plot== TRUE){
      plot(Paleogene, col=colland, border=colborder, add=TRUE)
    }
    return(Paleogene)
  }
  if(interval=="Paleocene" || interval=="paleocene"){
    if(do.plot== TRUE){
      plot(Paleocene, col=colland, border=colborder, add=TRUE)
    }
    return(Paleocene)
  }
  if(interval=="Danian" || interval=="danian"){
    if(do.plot== TRUE){
      plot(Danian, col=colland, border=colborder, add=TRUE)
    }
    return(Danian)
  }
  if(interval=="Selandian" || interval=="selandian"){
    if(do.plot== TRUE){
      plot(Selandian, col=colland, border=colborder, add=TRUE)
    }
    return(Selandian)
  }
  if(interval=="Thanetian" || interval=="thanetian"){
    if(do.plot== TRUE){
      plot(Thanetian, col=colland, border=colborder, add=TRUE)
    }
    return(Thanetian)
  }
  if(interval=="Eocene" || interval=="eocene"){
    if(do.plot== TRUE){
      plot(Eocene, col=colland, border=colborder, add=TRUE)
    }
    return(Eocene)
  }
  if(interval=="Ypresian" || interval=="ypresian"){
    if(do.plot== TRUE){
      plot(Ypresian, col=colland, border=colborder, add=TRUE)
    }
    return(Ypresian)
  }
  if(interval=="Lutetian" || interval=="lutetian"){
    if(do.plot== TRUE){
      plot(Lutetian, col=colland, border=colborder, add=TRUE)
    }
    return(Lutetian)
  }
  if(interval=="Bartonian" || interval=="bartonian"){
    if(do.plot== TRUE){
      plot(Bartonian, col=colland, border=colborder, add=TRUE)
    }
    return(Bartonian)
  }
  if(interval=="Priabonian" || interval=="priabonian"){
    if(do.plot== TRUE){
      plot(Priabonian, col=colland, border=colborder, add=TRUE)
    }
    return(Priabonian)
  }
  if(interval=="Oligocene" || interval=="oligocene"){
    if(do.plot== TRUE){
      plot(Oligocene, col=colland, border=colborder, add=TRUE)
    }
    return(Oligocene)
  }
  if(interval=="Rupelian" || interval=="rupelian"){
    if(do.plot== TRUE){
      plot(Rupelian, col=colland, border=colborder, add=TRUE)
    }
    return(Rupelian)
  }
  if(interval=="Chattian" || interval=="chattian"){
    if(do.plot== TRUE){
      plot(Chattian, col=colland, border=colborder, add=TRUE)
    }
    return(Chattian)
  }
  if(interval=="Neogene" || interval=="neogene"){
    if(do.plot== TRUE){
      plot(Neogene, col=colland, border=colborder, add=TRUE)
    }
    return(Neogene)
  }
  if(interval=="Miocene" || interval=="miocene"){
    if(do.plot== TRUE){
      plot(Miocene, col=colland, border=colborder, add=TRUE)
    }
    return(Miocene)
  }
  if(interval=="Aquitanian" || interval=="aquitanian"){
    if(do.plot== TRUE){
      plot(Aquitanian, col=colland, border=colborder, add=TRUE)
    }
    return(Aquitanian)
  }
  if(interval=="Burdigalian" || interval=="burdigalian"){
    if(do.plot== TRUE){
      plot(Burdigalian, col=colland, border=colborder, add=TRUE)
    }
    return(Burdigalian)
  }
  if(interval=="Calabrian" || interval=="calabrian"){
    if(do.plot== TRUE){
      plot(Calabrian, col=colland, border=colborder, add=TRUE)
    }
    return(Calabrian)
  }
  if(interval=="Langhian" || interval=="langhian"){
    if(do.plot== TRUE){
      plot(Langhian, col=colland, border=colborder, add=TRUE)
    }
    return(Langhian)
  }
  if(interval=="Serravallian" || interval=="serravallian"){
    if(do.plot== TRUE){
      plot(Serravallian, col=colland, border=colborder, add=TRUE)
    }
    return(Serravallian)
  }
  if(interval=="Tortonian" || interval=="tortonian"){
    if(do.plot== TRUE){
      plot(Tortonian, col=colland, border=colborder, add=TRUE)
    }
    return(Tortonian)
  }
  if(interval=="Tournaisian" || interval=="tournaisian"){
    if(do.plot== TRUE){
      plot(Tournaisian, col=colland, border=colborder, add=TRUE)
    }
    return(Tournaisian)
  }
  if(interval=="Messinian" || interval=="messinian"){
    if(do.plot== TRUE){
      plot(Messinian, col=colland, border=colborder, add=TRUE)
    }
    return(Messinian)
  }
  if(interval=="Pliocene" || interval=="pliocene"){
    if(do.plot== TRUE){
      plot(Pliocene, col=colland, border=colborder, add=TRUE)
    }
    return(Pliocene)
  }
  if(interval=="Zanclean" || interval=="zanclean"){
    if(do.plot== TRUE){
      plot(Zanclean, col=colland, border=colborder, add=TRUE)
    }
    return(Zanclean)
  }
  if(interval=="Quaternary" || interval=="quaternary"){
    if(do.plot== TRUE){
      plot(Quaternary, col=colland, border=colborder, add=TRUE)
    }
    return(Quaternary)
  }
  if(interval=="Pleistocene" || interval=="pleistocene"){
    if(do.plot== TRUE){
      plot(Pleistocene, col=colland, border=colborder, add=TRUE)
    }
    return(Pleistocene)
  }
  if(interval=="Gelasian" || interval=="gelasian"){
    if(do.plot== TRUE){
      plot(Gelasian, col=colland, border=colborder, add=TRUE)
    }
    return(Gelasian)
  }
}