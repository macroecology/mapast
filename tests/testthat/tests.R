context("pm_getdata")
test_that("tests on getdata_paleomap which should return a data.frame with
          at least paleolat, paleolng, taxon_name and taxon_rank")
  #get the data
  data<-  pm_getdata(interval="Quaternary", base_name="reptilia", limit=1000)
  #test if data is data frame
  expect_true(is.data.frame(data))
  #test if important colnames are there
  expect_true(grep("paleolat", names(data))!=0)
  expect_true(grep("paleolng", names(data))!=0)
  expect_true(grep("taxon_name", names(data))!=0)
  expect_true(grep("taxon_rank", names(data))!=0)
  #test if paleolat and long ranges are correct
  minlat <- min(data$paleolat)
  expect_true(minlat>=-90)
  maxlat <- max(data$paleolat)
  expect_true(maxlat<=90)
  minlng <- min(data$paleolng)
  expect_true(minlng>=-180)
  maxlng <- max(data$pleolng)
  expect_true(maxlng<=180)
  

context("pm_getmap")
test_that("tests on get_paleomap, if output is a shapefile") 
  #get shapefile
  shape <- pm_getmap(interval="Jurassic", plot=TRUE, colsea="#00005020", colland="#2B2B2B80", colborder="#2B2B2B30")
  #test if it is a shape file
  expect_true(shape@class[1], equals("SpatialPolygonsDataFrame"))

context("pm_occraster")
test_that("test on raster_paleomap, if output is raster")
  #get data and shapefile
  shape <- pm_getmap(interval="Jurassic",plot=TRUE, colsea="#00005020", colland="#2B2B2B80", colborder="#2B2B2B30")
  data <- pm_getdata(interval="Jurassic", base_name="reptilia", limit=1000)
  #create raster
  ras <- pm_occraster(shape, data)
  #test if raster is a RasterLayer
  expect_that(ras@class[1], equals("RasterLayer"))

context("pm_richraster")
test_that("test on spraster_paleomap, if output is raster")
  #get data and shape file
  shape <- pm_getmap(interval="Jurassic", plot=TRUE, colsea="#00005020", colland="#2B2B2B80", colborder="#2B2B2B30")
  data <- pm_getdata(interval="Jurassic", base_name="reptilia", limit=1000)
  #create species richness raster
  spras <- pm_richraster(shape, data)
  #test if class of raster is RasterLayer
  expect_that(spras@class[1], equals("RasterLayer"))

context("rank_filter")
test_that("test if output is a dataframe and if there are only species in the data frame")
  #get data
  data <- pm_getdata(interval="Jurassic", base_name="reptilia", limit=1000)
  #filter data
  filter_data <- rank_filter(data, res=10, rank="species")
  #test if it is a data frame
  expect_true(is.data.frame(filter_data))
  #test if most important columns with the correct names are in the data frame
  expect_true(grep("paleolat", names(filter_data))!=0)
  expect_true(grep("paleolng", names(filter_data))!=0)
  expect_true(grep("taxon_name", names(filter_data))!=0)
  expect_true(grep("taxon_rank", names(filter_data))!=0)
  #test if the paleolat and long ranges are correct
  minlat <- min(filter_data$paleolat)
  expect_true(minlat>=-90)
  maxlat <- max(filter_data$paleolat)
  expect_true(maxlat<=90)
  minlng <- min(filter_data$paleolng)
  expect_true(minlng>=-180)
  maxlng <- max(filter_data$pleolng)
  expect_true(maxlng<=180)
  #check if there are really only species in the list
  taxon_rank <- as.vector(filter_data$taxon_rank)
  bool <- TRUE
  for (i in 1: length(taxon_rank)){
    bool <- (bool && (taxon_rank[i]=="species"))
  }
  expect_true(bool)
  