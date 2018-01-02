[![Build Status](https://travis-ci.org/macroecology/pasta.svg)](https://travis-ci.org/macroecology/pasta)

pasta
=======

### About

`pasta` is a package that combines paleomaps from [GPlates](http://www.gplates.org/), Smith and Golonka with fossil records from [Paleobiology Database](http://paleobiodb.org/). It can be used to download shapefiles with reconstructions of the past configuration of the continents, and to generate paleodiversity maps.

### Quick start

**Install**

Install pasta from CRAN

```coffee
install.packages("pasta")
library(pasta)
```

Install pasta developing version from github

```coffee
install.packages("devtools")
library(devtools)
install_github("macroecology/pasta")
library(pasta)
```

**General overview**

`pasta` version 0.1 has 3 functions for getting and visualising paleogeographical maps and fossil data, 4 functions for constructing diversity rasters (paleorichness and Shannon paleodiversity) and 2 functions for creating diversity matrix (based on localities or cells) and 2 functions for getting latitudinal paleodiversity gradients.

## Get and visualise paleogeograhical maps and fossil data

**getmap** 
returns the shapefile of a choosen paleogeographical time interval and a plot

```coffee
> shape  <-  getmap(interval="Cretaceous", model="GPlates")
> shape
```

```coffee
class       : SpatialPolygonsDataFrame 
features    : 86 
extent      : -180, 180, -88.8737, 83.6951  (xmin, xmax, ymin, ymax)
coord. ref. : NA 
variables   : 3
names       : FROMAGE, TOAGE,       NAME 
min values  :     145,    66, Cretaceous 
max values  :     145,    66, Cretaceous 
```
![plot of chunk map](figure/cretaceous.png)


**getdata**
returns a dataframe with the selected fossil occurrences, downloaded from the [Paleobiology Database](http://paleobiodb.org/)

```coffee

> data  <-  getdata (base_name="Testudines", interval="Paleocene")
> head(data)

```

```coffee
  occurrence_no         matched_name   matched_rank matched_no     early_interval   late_interval
1         40165  Peritresius ornatus        species     173397          Thanetian            <NA>
2         40166 Rhetechelys platyops        species     128351          Thanetian            <NA>
3        149344   Trionyx virginiana        species     231917          Thanetian            <NA>
4        205060  Judithemys backmani        species     253836          Paleocene            <NA>
5        205061         Trionychinae unranked clade     276091          Paleocene            <NA>
6        281674 Taphrosphys sulcatus        species     131500 Late Maastrichtian Early Paleocene
  paleolng paleolat geoplate       genus          family      order    class   phylum genus_no
1   -44.51    40.13      109 Peritresius     Chelydridae Testudines Reptilia Chordata    36360
2   -44.51    40.13      109 Rhetechelys  Pancheloniidae Testudines Reptilia Chordata   128349
3   -47.60    38.76      109        <NA> Pantrionychidae Testudines Reptilia Chordata       NA
4   -67.18    57.12      101  Judithemys   Macrobaenidae Testudines Reptilia Chordata    56471
5   -67.18    57.12      101        <NA>    Trionychidae Testudines Reptilia Chordata       NA
6   -39.82    40.92      109 Taphrosphys   Bothremydidae Testudines Reptilia Chordata    37608
  family_no order_no class_no phylum_no early_age late_age
1     37704    56475    36322     33815      59.2     56.0
2    165650    56475    36322     33815      59.2     56.0
3    361334    56475    36322     33815      59.2     56.0
4     56472    56475    36322     33815      66.0     56.0
5     37674    56475    36322     33815      66.0     56.0
6     67318    56475    36322     33815      70.6     61.7

```

**pastplot**

Returns a plot with the paleomap and the fossil occurrences.

```coffee

data  <-  getdata (base_name="Testudines", 
                      interval="Paleocene")
pastplot (interval="Paleocene", model="GPlates", data)

```

![plot of chunk map](figure/occ_test.png)

## Functions for paleogeographical analyses

**mapocc**
Returns a RasterLayer of the sampling effort and a map with the raster on it.

```coffee
> shape <- getmap(interval="Paleocene", model="GPlates") 
> data <- getdata (base_name="Testudines", 
                    interval="Paleocene")
> mapocc (shape, data, rank="species")

``` 

```coffee
class       : RasterLayer 
dimensions  : 18, 36, 648  (nrow, ncol, ncell)
resolution  : 10, 10  (x, y)
extent      : -180, 180, -92.0605, 87.9395  (xmin, xmax, ymin, ymax)
coord. ref. : NA 
data source : in memory
names       : layer 
values      : 1, 58  (min, max)

```

![plot of chunk map](figure/raster_test.png) 

**maprich**
Returns a RasterLayer of richness and a map with the raster on it.

```coffee

> maprich (shape, data, rank="species")

``` 

```coffee
class       : RasterLayer 
dimensions  : 18, 36, 648  (nrow, ncol, ncell)
resolution  : 10, 10  (x, y)
extent      : -180, 180, -92.0605, 87.9395  (xmin, xmax, ymin, ymax)
coord. ref. : NA 
data source : in memory
names       : layer 
values      : 1, 14  (min, max)

```

![plot of chunk map](figure/raster_rich_test.png) 

## Paleodiversity

**spsite**
Returns a dataframe of taxa occurrences vs. localities (unity="fossilsite) or cells(unity="cell") (~ sites/cells per taxa matrix)

```coffee
> data <- getdata(base_name = "Canis", interval = "Quaternary")
> result <- spsite(data, unity="fossilsite", rank = "genus")
> head(result)
``` 

```coffee
    paleolng paleolat Canis
612  -170.38    63.70     1
611  -170.05    66.03     1
610  -169.90    66.05     1
69   -161.94    66.88     1
83   -147.67    65.10     2
160  -140.00    68.20     1
``` 

```coffee
> data <- getdata(base_name = "Canis", interval = "Quaternary")
> result_cell <- spsite(data, unity="cell", rank = "genus")
> head(result)
``` 

```coffee
      paleolng paleolat Canis
64441   -179.5     89.5     0
64081   -179.5     88.5     0
63721   -179.5     87.5     0
63361   -179.5     86.5     0
63001   -179.5     85.5     0
62641   -179.5     84.5     0
```


**mapdiv**
1- calculates the Shannon diversity per unique locality (based on its coordinates),
2- makes a raster file and a plot showing mean, max, min diversity per cell, 
or number of unique localities per cell

```coffee
> data<- getdata (base_name="Canis", interval="Quaternary")
> div_site <- mapdiv (shape, data, unity="fossilsite", rank="genus", res=1)
``` 

```coffee
class       : RasterLayer 
dimensions  : 180, 360, 64800  (nrow, ncol, ncell)
resolution  : 1, 1  (x, y)
extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
coord. ref. : NA 
data source : in memory
names       : layer 
values      : NA, NA  (min, max)
``` 
![plot of chunk map](figure/diversity.png) 

```coffee
> data<- getdata (base_name="Canis", interval="Quaternary")
> div_cell <- mapdiv (shape, data, unity="cell", rank="genus", res=1)
``` 

```coffee
class       : RasterLayer 
dimensions  : 180, 360, 64800  (nrow, ncol, ncell)
resolution  : 1, 1  (x, y)
extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
coord. ref. : NA 
data source : in memory
names       : layer 
values      : NA, NA  (min, max)

``` 

![plot of chunk map](figure/diversity_cell.png) 


**latdivgrad**
 calculates latitudinal diversity (either shannon or richness) of taxa (species, genera, families, orders)

```coffee
> shape<- getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
> data<- getdata (base_name="Canis", interval="Quaternary")
> rich <- latdivgrad (shape, data, method="richness", rank = "genus", res=1)
> head(rich)
```
```coffee
    paleolat div
180     89.5   0
179     88.5   0
178     87.5   0
177     86.5   0
176     85.5   0
175     84.5   0
```

![plot of chunk map](figure/lat_rich.png) 

```coffee
> shape<- getmap(interval="Quaternary", model="GPlates", do.plot=FALSE)
> data<- getdata (base_name="Canis", interval="Quaternary")
> shannon <- latdivgrad (shape, data, method="shannon", rank = "species", res=1)
> head(shannon)
```

```coffee
    paleolat div
180     89.5   0
179     88.5   0
178     87.5   0
177     86.5   0
176     85.5   0
175     84.5   0
```

![plot of chunk map](figure/lat_div.png) 


## Meta

Please report any [issues or  bugs](https://github.com/macroecology/pasta/issues).

License: GPL-2

To cite package `pasta` in publications use:

```coffee
To cite package `pasta` in publications use:

Sara Varela, K. Sonja Rothkugel (2017). pasta:  combine paleogeography and paleobiodiversity. R package version 0.1. https://github.com/macroecology/pasta

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {pasta:  combine paleogeography and paleobiodiversity},
    author = {Sara Varela} and {Sonja Rothkugel},
    year = {2016},
    note = {R package version 0.1},
    base = {https://github.com/macroecology/pasta},
  }
```

---
