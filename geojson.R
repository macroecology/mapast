install.packages (c("rjson", "RCurl"))
library (rjson)
library (RCurl)
library (rgdal)

"GeoJSON" %in% ogrDrivers()$name

ogrInfo(getURL("https://raw.githubusercontent.com/paleobiodb/navigator/master/build/js/plates/Aalenian.json", 
               ssl.verifypeer = FALSE), layer="OGRGeoJSON") 

map<- readOGR(getURL("https://raw.githubusercontent.com/paleobiodb/navigator/master/build/js/plates/Aalenian.json", 
                     ssl.verifypeer = FALSE), layer="Aalenian")


class (map)
names (map)
map$id
str (map)

library (paleobioDB)
test<- pbdb_occurrences (base_name="Amphibia", interval="Aalenian", 
                         show=c("coords", "phylo", "ident", "paleoloc"), 
                         vocab="pbdb")
points (test$paleolng, test$paleo$lat, pch=19, col="red")

X11()
plot(map, col="black")
points (test$paleolng, test$paleo$lat, pch=19, col="red")
