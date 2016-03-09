# private functions
# the following functions are used by our package but users can not change their parameters.

##############################color palette ########################################

#creating a color palette for the raster
mycols <- colorRampPalette(c("goldenrod1","orangered", 
                                          "darkred"))


#################filter#######################
#' rank_filter
#' 
#' creates the raster files with the number of unique taxa by pixel
#' 
#' @usage rank_filter (data, res, rank)
#' @param data a data frame which needs to have a column called paleolat and a column called paleolng,
#'  can be created with getdata_paleomap
#' @param ras blank raster
#' @param res resolution of the raster file
#' @param rank rank of interest
#' @return a raster with the taxa richness
#' @examples 
#' \dontrun{
#' data<- pm_getdata (base_name="Canis", interval="Quaternary")
#' rank_filter (data, res=10, rank="genus")
#'}

rank_filter <- function(r=ras, data, res, rank){
  #gets colnames for new data frame
 
  if (rank=="species"){
  if (length (data$matched_rank)!=0){
    identified<-data [!is.na(data$matched_rank), ]
    species<- identified [identified$matched_rank==rank, ]
    S<- split(species, species$matched_no)
  }
  
  R<-lapply(S,function(y){
    s<-split(y,paste(y$paleolng,y$paleolat))
    X<-as.matrix(do.call(rbind,lapply(s,function(x)c(x$paleolng[1],
                                                     x$paleolat[1],1))))
    X<-rbind(X[1,],X)
    r2<-rasterize(X[,1:2],r,X[,3])
  })
  names(R)==NULL
  all<-calc(stack(R), function(x) sum(x,na.rm=T))
  values(all)[values(all)==0]<-NA
  all
  }
  
  if (rank!="species"){
  ranks<-data.frame(rank=c("genus","family","order","class","phylum"),
                      matched_rank=c("genus_no","family_no","order_no",
                                     "class_no","phylum_no"))
                      
    if (length (data$matched_rank)!=0){
      identified<-data [!is.na(data$matched_rank), ]
      col<-paste(ranks$matched_rank[ranks$rank==rank])
      ident<-identified[!is.na(identified[,col]),]
      f<-paste(ident[,col])
      S<-split(ident,f)
    }
    
    R<-lapply(S,function(y){
      s<-split(y,paste(y$paleolng,y$paleolat))
      X<-as.matrix(do.call(rbind,lapply(s,function(x)c(x$paleolng[1],
                                                       x$paleolat[1],1))))
      X<-rbind(X[1,],X)
      r2<-rasterize(X[,1:2],r,X[,3])
    })
    names(R)=NULL
    all<-calc(stack(R), function(x) sum(x,na.rm=T))
    values(all)[values(all)==0]<-NA
    all
  }
all 
}

#' rfilter
#' 
#' filters the data frame so there are only species left 
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
    data<- data [, c(5:6, 1)]
  }
  if(rank=="genus"){
    data <- subset(data, genus!="NA")
    data<- data [, c(5:6, 8)]
  }
  if(rank=="family"){
    data <- subset(data, family!="NA")
    data<- data [, c(5:6, 9)]
  }
  if(rank=="order"){
    data <- subset(data, order!="NA")
    data<- data [, c(5:6, 10)]
  }
  data
}


#########help for shape files##############
#we need this function because we cannot open lazyload data with an input parameter because it is no value.

## we might hack this with "with" or "null" for avoiding NOTE on check: 'no visible binding for global variable'
## see: http://stackoverflw.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when


getshape <- function (interval)
{
  if(interval=="Paleozoic" || interval=="paleozoic"){
    return(Paleozoic)
  }
  
  if(interval=="Cambrian" || interval=="cambrian"){
    
    return(Cambrian)
  }
  if(interval=="Terreneuvian" || interval=="terreneuvian"){
    
    return(Terreneuvian)
  }
  if(interval=="Fortunian" || interval=="fortunian"){
    
    return(Fortunian)
  }
  if(interval=="Stage_2" || interval=="stage_2"){
    return(Stage_2)
  }
  if(interval=="Series_2" || interval=="series_2"){
    
    return(Series_2)
  }
  if(interval=="Stage_3" || interval=="stage_3"){
    
    return(Stage_3)
  }
  if(interval=="Stage_4" || interval=="stage_4"){
    
    return(Stage_4)
  }
  if(interval=="Series_3" || interval=="series_3"){
    
    return(Series_3)
  }
  if(interval=="Stage_5" || interval=="stage_5"){
    
    return(Stage_5)
  }
  if(interval=="Drumian" || interval=="drumian"){
    
    return(Drumian)
  }
  if(interval=="Guzhangian" || interval=="guzhangian"){
    
    return(Guzhangian)
  }
  if(interval=="Furongian" || interval=="furongian"){
    
    return(Furongian)
  }
  if(interval=="Paibian" || interval=="paibian"){
    
    return(Paibian)
  }
  if(interval=="Jiangshanian" || interval=="jiangshanian"){
    
    return(Jiangshanian)
  }
  if(interval=="Stage_10" || interval=="stage_10"){
    
    return(Stage_10)
  }
  if(interval=="Ordovician" || interval=="ordovician"){
    
    return(Ordovician)
  }
  if(interval=="Early_Ordovician" || interval=="early_ordovician"){
    
    return(Early_Ordovician)
  }
  if(interval=="Tremadocian" || interval=="tremadocian"){
   
    return(Tremadocian)
  }
  if(interval=="Floian" || interval=="floian"){
    
    return(Floian)
  }
  if(interval=="Middle_Ordovician" || interval=="middle_ordovician"){
    
    return(Middle_Ordovician)
  }
  if(interval=="Middle_Pleistocene" || interval=="middle_pleistocene"){
    
    return(Middle_Pleistocene)
  }
  if(interval=="Dapingian" || interval=="dapingian"){
    
    return(Dapingian)
  }
  if(interval=="Darriwilian" || interval=="darriwilian"){
    
    return(Darriwilian)
  }
  if(interval=="Late_Ordovician" || interval=="late_ordovician"){
    
    return(Late_Ordovician)
  }
  if(interval=="Late_Pleistocene" || interval=="late_pleistocene"){
    
    return(Late_Pleistocene)
  }
  if(interval=="Sandbian" || interval=="sandbian"){
    
    return(Sandbian)
  }
  if(interval=="Katian" || interval=="katian"){
    
    return(Katian)
  }
  if(interval=="Holocene" || interval=="holocene"){
    
    return(Holocene)
  }
  if(interval=="Hirnantian" || interval=="hirnantian"){
    
    return(Hirnantian)
  }
  if(interval=="Llandovery" || interval=="llandovery"){
    
    return(Llandovery)
  }
  if(interval=="Rhuddanian" || interval=="rhuddanian"){
    
    return(Rhuddanian)
  }
  if(interval=="Aeronian" || interval=="aeronian"){
    
    return(Aeronian)
  }
  if(interval=="Telychian" || interval=="telychian"){
    
    return(Telychian)
  }
  if(interval=="Wenlock" || interval=="wenlock"){
   
    return(Wenlock)
  }
  if(interval=="Sheinwoodian" || interval=="sheinwoodian"){
    
    return(Sheinwoodian)
  }
  if(interval=="Homerian" || interval=="homerian"){
    
    return(Homerian)
  }
  if(interval=="Silurian" || interval=="silurian"){
    
    return(Silurian)
  }
  if(interval=="Ludlow" || interval=="ludlow"){
    
    return(Ludlow)
  }
  if(interval=="Gorstian" || interval=="gorstian"){
    
    return(Gorstian)
  }
  if(interval=="Ludfordian" || interval=="ludfordian"){
    
    return(Ludfordian)
  }
  if(interval=="Pridoli" || interval=="pridoli"){
   
    return(Pridoli)
  }
  if(interval=="Devonian" || interval=="devonian"){
    
    return(Devonian)
  }
  if(interval=="Early_Devonian" || interval=="early_devonian"){
    
    return(Early_Devonian)
  }
  if(interval=="Lochkovian" || interval=="lochkovian"){
    
    return(Lochkovian)
  }
  if(interval=="Pragian" || interval=="pragian"){
    
    return(Pragian)
  }
  if(interval=="Emsian" || interval=="emsian"){
    
    return(Emsian)
  }
  if(interval=="Middle_Devonian" || interval=="middle_devonian"){
    
    return(Middle_Devonian)
  }
  if(interval=="Middle_Jurassic" || interval=="middle_jurassic"){
    
    return(Middle_Jurassic)
  }
  if(interval=="Eifelian" || interval=="eifelian"){
    
    return(Eifelian)
  }
  if(interval=="Givetian" || interval=="givetian"){
    
    return(Givetian)
  }
  if(interval=="Late_Devonian" || interval=="late_devonian"){
    
    return(Late_Devonian)
  }
  if(interval=="Frasnian" || interval=="frasnian"){
    
    return(Frasnian)
  }
  if(interval=="Famennian" || interval=="famennian"){
   
    return(Famennian)
  }
  if(interval=="Carboniferous" || interval=="carboniferous"){
   
    return(Carboniferous)
  }
  if(interval=="Mississippian" || interval=="mississippian"){
    
    return(Mississippian)
  }
  if(interval=="Visean" || interval=="visean"){
    
    return(Visean)
  }
  if(interval=="Serpukhovian" || interval=="serpukhovian"){
    
    return(Serpukhovian)
  }
  if(interval=="Pennsylvanian" || interval=="pennsylvanian"){
    
    return(Pennsylvanian)
  }
  if(interval=="Bashkirian" || interval=="bashkirian"){
    
    return(Bashkirian)
  }
  if(interval=="Moscovian" || interval=="moscovian"){
    
    return(Moscovian)
  }
  if(interval=="Kasimovian" || interval=="kasimovian"){
    
    return(Kasimovian)
  }
  if(interval=="Gzhelian" || interval=="gzhelian"){
    
    return(Gzhelian)
  }
  if(interval=="Phanerozoic" || interval=="phanerozoic"){
    
    return(Phanerozoic)
  }
  if(interval=="Permian" || interval=="permian"){
    
    return(Permian)
  }
  if(interval=="Piacenzian" || interval=="piacenzian"){
    
    return(Piacenzian)
  }
  if(interval=="Cisuralian" || interval=="cisuralian"){
    
    return(Cisuralian)
  }
  if(interval=="Asselian" || interval=="asselian"){
    
    return(Asselian)
  }
  if(interval=="Sakmarian" || interval=="sakmarian"){
    
    return(Sakmarian)
  }
  if(interval=="Artinskian" || interval=="artinskian"){
    
    return(Artinskian)
  }
  if(interval=="Kungurian" || interval=="kungurian"){
    
    return(Kungurian)
  }
  if(interval=="Guadalupian" || interval=="guadalupian"){
    
    return(Guadalupian)
  }
  if(interval=="Roadian" || interval=="roadian"){
    
    return(Roadian)
  }
  if(interval=="Wordian" || interval=="wordian"){
    
    return(Wordian)
  }
  if(interval=="Capitanian" || interval=="capitanian"){
    
    return(Capitanian)
  }
  if(interval=="Lopingian" || interval=="lopingian"){
    
    return(Lopingian)
  }
  if(interval=="Wuchiapingian" || interval=="wuchiapingian"){
    
    return(Wuchiapingian)
  }
  if(interval=="Changhsingian" || interval=="changhsingian"){
    
    return(Changhsingian)
  }
  if(interval=="Triassic" || interval=="triassic"){
    
    return(Triassic)
  }
  if(interval=="Early_Triassic" || interval=="early_triassic"){
    
    return(Early_Triassic)
  }
  if(interval=="Induan" || interval=="induan"){
    
    return(Induan)
  }
  if(interval=="Olenekian" || interval=="olenekian"){
    
    return(Olenekian)
  }
  if(interval=="Middle_Triassic" || interval=="middle_triassic"){
    
    return(Middle_Triassic)
  }
  if(interval=="Anisian" || interval=="anisian"){
   
    return(Anisian)
  }
  if(interval=="Ladinian" || interval=="ladinian"){
    
    return(Ladinian)
  }
  if(interval=="Late_Triassic" || interval=="late_triassic"){
    
    return(Late_Triassic)
  }
  if(interval=="Carnian" || interval=="carnian"){
    
    return(Carnian)
  }
  if(interval=="Norian" || interval=="norian"){
    
    return(Norian)
  }
  if(interval=="Rhaetian" || interval=="rhaetian"){
    
    return(Rhaetian)
  }
  if(interval=="Jurassic" || interval=="jurassic"){
    
    return(Jurassic)
  }
  if(interval=="Early_Jurassic" || interval=="early_jurassic"){
    
    return(Early_Jurassic)
  }
  if(interval=="Hettangian" || interval=="hettangian"){
    
    return(Hettangian)
  }
  if(interval=="Sinemurian" || interval=="sinemurian"){
    
    return(Sinemurian)
  }
  if(interval=="Pliensbachian" || interval=="pliensbachian"){
    
    return(Pliensbachian)
  }
  if(interval=="Toarcian" || interval=="toarcian"){
    
    return(Toarcian)
  }
  if(interval=="Aalenian" || interval=="aalenian"){
    
    return(Aalenian)
  }
  if(interval=="Bajocian" || interval=="bajocian"){
    
    return(Bajocian)
  }
  if(interval=="Bathonian" || interval=="bathonian"){
    
    return(Bathonian)
  }
  if(interval=="Callovian" || interval=="callovian"){
    
    return(Callovian)
  }
  if(interval=="Late_Jurassic" || interval=="late_jurassic"){
    
    return(Late_Jurassic)
  }
  if(interval=="Oxfordian" || interval=="oxfordian"){
    
    return(Oxfordian)
  }
  if(interval=="Kimmeridgian" || interval=="kimmeridgian"){
    
    return(Kimmeridgian)
  }
  if(interval=="Tithonian" || interval=="tithonian"){
    
    return(Tithonian)
  }
  if(interval=="Cretaceous" || interval=="cretaceous"){
    
    return(Cretaceous)
  }
  if(interval=="Early_Cretaceous" || interval=="early_cretaceous"){
    
    return(Early_Cretaceous)
  }
  if(interval=="Berriasian" || interval=="Berriasian"){
    
    return(Berriasian)
  }
  if(interval=="Valanginian" || interval=="valanginian"){
    
    return(Valanginian)
  }
  if(interval=="Hauterivian" || interval=="hauterivian"){
    
    return(Hauterivian)
  }
  if(interval=="Barremian" || interval=="barremian"){
    
    return(Barremian)
  }
  if(interval=="Aptian" || interval=="aptian"){
    
    return(Aptian)
  }
  if(interval=="Albian" || interval=="albian"){
    
    return(Albian)
  }
  if(interval=="Late_Cretaceous" || interval=="late_cretaceous"){
    
    return(Late_Cretaceous)
  }
  if(interval=="Cenomanian" || interval=="cenomanian"){
    
    return(Cenomanian)
  }
  if(interval=="Turonian" || interval=="turonian"){
    
    return(Turonian)
  }
  if(interval=="Coniacian" || interval=="coniacian"){
    
    return(Coniacian)
  }
  if(interval=="Santonian" || interval=="santonian"){
    
    return(Santonian)
  }
  if(interval=="Campanian" || interval=="campanian"){
    
    return(Campanian)
  }
  if(interval=="Mesozoic" || interval=="mesozoic"){
    
    return(Mesozoic)
  }
  if(interval=="Maastrichtian" || interval=="maastrichtian"){
    
    return(Maastrichtian)
  }
  if(interval=="Cenozoic" || interval=="cenozoic"){
    
    return(Cenozoic)
  }
  if(interval=="Paleogene" || interval=="paleogene"){
    
    return(Paleogene)
  }
  if(interval=="Paleocene" || interval=="paleocene"){
    
    return(Paleocene)
  }
  if(interval=="Danian" || interval=="danian"){
    
    return(Danian)
  }
  if(interval=="Selandian" || interval=="selandian"){
    
    return(Selandian)
  }
  if(interval=="Thanetian" || interval=="thanetian"){
    
    return(Thanetian)
  }
  if(interval=="Eocene" || interval=="eocene"){
    
    return(Eocene)
  }
  if(interval=="Ypresian" || interval=="ypresian"){
    
    return(Ypresian)
  }
  if(interval=="Lutetian" || interval=="lutetian"){
    
    return(Lutetian)
  }
  if(interval=="Bartonian" || interval=="bartonian"){
    
    return(Bartonian)
  }
  if(interval=="Priabonian" || interval=="priabonian"){
    
    return(Priabonian)
  }
  if(interval=="Oligocene" || interval=="oligocene"){
    
    return(Oligocene)
  }
  if(interval=="Rupelian" || interval=="rupelian"){
    
    return(Rupelian)
  }
  if(interval=="Chattian" || interval=="chattian"){
    
    return(Chattian)
  }
  if(interval=="Neogene" || interval=="neogene"){
    
    return(Neogene)
  }
  if(interval=="Miocene" || interval=="miocene"){
    
    return(Miocene)
  }
  if(interval=="Aquitanian" || interval=="aquitanian"){
 
    return(Aquitanian)
  }
  if(interval=="Burdigalian" || interval=="burdigalian"){
    
    return(Burdigalian)
  }
  if(interval=="Calabrian" || interval=="calabrian"){
    
    return(Calabrian)
  }
  if(interval=="Langhian" || interval=="langhian"){
   
    return(Langhian)
  }
  if(interval=="Serravallian" || interval=="serravallian"){
    
    return(Serravallian)
  }
  if(interval=="Tortonian" || interval=="tortonian"){
    
    return(Tortonian)
  }
  if(interval=="Tournaisian" || interval=="tournaisian"){
    
    return(Tournaisian)
  }
  if(interval=="Messinian" || interval=="messinian"){
    
    return(Messinian)
  }
  if(interval=="Pliocene" || interval=="pliocene"){
   
    return(Pliocene)
  }
  if(interval=="Zanclean" || interval=="zanclean"){
   
    return(Zanclean)
  }
  if(interval=="Quaternary" || interval=="quaternary"){
  
    return(Quaternary)
  }
  if(interval=="Pleistocene" || interval=="pleistocene"){
   
    return(Pleistocene)
  }
  if(interval=="Gelasian" || interval=="gelasian"){
    
    return(Gelasian)
  }
}

