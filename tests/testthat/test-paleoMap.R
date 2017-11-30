#get the data
base::load(base::system.file("testdata", "testdata.rda", package = "paleoMap"))


data <- data[1:100,]
# data <- paleoMap::pm_getdata("Oligocene", "mammalia", limit=100)
# shape <- paleoMap::pm_getmap("Oligocene", "GPlates")
occ_species <- paleoMap::pm_occ(data, rank="species") 
occ_cell_species <- paleoMap::pm_occ_cell(data, rank="species") 

#####pm_occraster#####
testthat::context("pm_occraster")
testthat::test_that("test on pm_occraster, if output is raster", {
  #create raster
  ras <- paleoMap::pm_occraster(shape, data, rank="genus")
  #test if raster is a RasterLayer
  testthat::expect_that(ras@class[1], equals("RasterLayer"))
  rm(ras)
})
 
#####pm_richraster#####
testthat::context("pm_richraster")
testthat::test_that("test on pm_richraster, if output is raster",{
  #create species richness raster
  spras <- paleoMap::pm_richraster(shape, data, rank="species")
  #test if class of raster is RasterLayer
  testthat::expect_that(spras@class[1], equals("RasterLayer"))
  rm(spras)
})
  
######pm_occ#####
testthat::context("pm_occ")
  #create ocurrence matrices
  occ_species <- paleoMap::pm_occ(data, rank="species")
  occ_genus <- paleoMap::pm_occ(data, rank="genus")
  occ_family <- paleoMap::pm_occ(data, rank="family")
  occ_order <- paleoMap::pm_occ(data, rank="order")
  occ_class <- paleoMap::pm_occ(data, rank="class")
  occ_phylum <-paleoMap::pm_occ(data, rank="phylum")
  #test if paleolat and paleolng are inside and correct
testthat::test_that("test that pm_occ gives correct output for species", {
  coln_species <- base::colnames(occ_species)
  nums_species <- base::as.vector(t(occ_species[3:base::length(occ_species)]))
  testthat::expect_true(base::is.numeric(nums_species))
  testthat::expect_true("paleolat" %in% coln_species && "paleolng" %in% coln_species)
  testthat::expect_true(base::min(occ_species$paleolat)>=-90 && base::max(occ_species$paleolat)<=90 
                        && base::min(occ_species$paleolng)>=-180 && base::max(occ_species$paleolng<=180))
  })

testthat::test_that("test that pm_occ gives correct output for genus", {
    coln_genus <- base::colnames(occ_genus)
    nums_genus <- base::as.vector(t(occ_genus[3:base::length(occ_genus)]))
    testthat::expect_true( base::is.numeric(nums_genus))
    testthat::expect_true("paleolat" %in% coln_genus && "paleolng" %in% coln_genus)
    testthat::expect_true(base::min(occ_genus$paleolat)>=-90 && base::max(occ_genus$paleolat)<=90 
                          && base::min(occ_genus$paleolng)>=-180 && base::max(occ_genus$paleolng<=180))
  })

testthat::test_that("test that pm_occ gives correct output for family", {
  coln_family <- base::colnames(occ_family)
  nums_family <- base::as.vector(t(occ_family[3:base::length(occ_family)]))
  testthat::expect_true(base::is.numeric(nums_family))
  testthat::expect_true("paleolat" %in% coln_family && "paleolng" %in% coln_family)
  testthat::expect_true(base::min(occ_family$paleolat)>=-90 && base::max(occ_family$paleolat)<=90 
              && base::min(occ_family$paleolng)>=-180 && base::max(occ_family$paleolng<=180))
})

testthat::test_that("test that pm_occ gives correct output for order", {
  coln_order <- base::colnames(occ_order)
  testthat::expect_true("paleolat" %in% coln_order && "paleolng" %in% coln_order)
  testthat::expect_true(base::min(occ_order$paleolat)>=-90 && base::max(occ_order$paleolat)<=90 
              && base::min(occ_order$paleolng)>=-180 && base::max(occ_order$paleolng<=180))
})

testthat::test_that("test that pm_occ gives correct output for class", {
  coln_class <- base::colnames(occ_class)
  nums_class <- base::as.vector(t(occ_class[3:base::length(occ_class)]))
  testthat::expect_true(base::is.numeric(nums_class))
  testthat::expect_true("paleolat" %in% coln_class && "paleolng" %in% coln_class)
  testthat::expect_true(base::min(occ_class$paleolat)>=-90 && base::max(occ_class$paleolat)<=90 
              && base::min(occ_class$paleolng)>=-180 && base::max(occ_class$paleolng<=180))
})

testthat::test_that("test that pm_occ gives correct output for phylum", {
  coln_phylum <- base::colnames(occ_phylum)
  nums_phylum <- base::as.vector(t(occ_phylum[3:base::length(occ_phylum)]))
  testthat::expect_true(base::is.numeric(nums_phylum))
  testthat::expect_true("paleolat" %in% coln_phylum && "paleolng" %in% coln_phylum)
  testthat::expect_true(base::min(occ_phylum$paleolat)>=-90 && base::max(occ_phylum$paleolat)<=90 
              && base::min(occ_phylum$paleolng)>=-180 && base::max(occ_phylum$paleolng<=180))
})
rm(occ_genus, occ_family, occ_order, occ_class, occ_phylum)
rm(coln_species, coln_genus, coln_family, coln_order, coln_class, coln_phylum)
rm(nums_species, nums_genus, nums_family, nums_order, nums_class, nums_phylum)
 
######pm_occ_cell######   
testthat::context("pm_occ_cell")
#get output of function
occ_cell_species <- paleoMap::pm_occ_cell(data, rank="species")
occ_cell_genus <- paleoMap::pm_occ_cell(data, rank="genus")
occ_cell_family <- paleoMap::pm_occ_cell(data, rank="family")
occ_cell_order <- paleoMap::pm_occ_cell(data, rank="order")
occ_cell_class <- paleoMap::pm_occ_cell(data, rank="class")
occ_cell_phylum <-paleoMap::pm_occ_cell(data, rank="phylum")
#test if paleolat and paleolng are inside and correct
testthat::test_that("test that pm_occ_cell gives correct output for species", {
  coln_cell_species <- base::colnames(occ_cell_species)
  nums_cell_species <- base::as.vector(t(occ_cell_species[3:base::length(occ_cell_species)]))
  testthat::expect_true(base::is.numeric(nums_cell_species))
  testthat::expect_true("paleolat" %in% coln_cell_species && "paleolng" %in% coln_cell_species)
  testthat::expect_true(base::min(occ_cell_species$paleolat)>=-90 && base::max(occ_cell_species$paleolat)<=90 
            && base::min(occ_cell_species$paleolng)>=-180 && base::max(occ_cell_species$paleolng<=180))

})

testthat::test_that("test that pm_occ_cell gives correct output for genus", {
  coln_cell_genus <- base::colnames(occ_cell_genus)
  nums_cell_genus <- base::as.vector(t(occ_cell_genus[3:base::length(occ_cell_genus)]))
  testthat::expect_true(base::is.numeric(nums_cell_genus))
  testthat::expect_true("paleolat" %in% coln_cell_genus && "paleolng" %in% coln_cell_genus)
  testthat::expect_true(base::min(occ_cell_genus$paleolat)>=-90 && base::max(occ_cell_genus$paleolat)<=90 
              && base::min(occ_cell_genus$paleolng)>=-180 && base::max(occ_cell_genus$paleolng<=180))
})
  
testthat::test_that("test that pm_occ_cell gives correct output for family", {
  coln_cell_family <- base::colnames(occ_cell_family)
  nums_cell_family <- base::as.vector(t(occ_cell_family[3:base::length(occ_cell_family)]))
  testthat::expect_true(base::is.numeric(nums_cell_family))
  testthat::expect_true("paleolat" %in% coln_cell_family && "paleolng" %in% coln_cell_family)
  testthat::expect_true(base::min(occ_cell_family$paleolat)>=-90 && base::max(occ_cell_family$paleolat)<=90 
              && base::min(occ_cell_family$paleolng)>=-180 && base::max(occ_cell_family$paleolng<=180))
})

testthat::test_that("test that pm_occ_cell gives correct output for order", {
  coln_cell_order <- base::colnames(occ_cell_order)
  nums_cell_order <- base::as.vector(t(occ_cell_order[3:base::length(occ_cell_order)]))
  testthat::expect_true(base::is.numeric(nums_cell_order))
  testthat::expect_true("paleolat" %in% coln_cell_order && "paleolng" %in% coln_cell_order)
  testthat::expect_true(base::min(occ_cell_order$paleolat)>=-90 && base::max(occ_cell_order$paleolat)<=90 
              && base::min(occ_cell_order$paleolng)>=-180 && base::max(occ_cell_order$paleolng<=180))
})

testthat::test_that("test that pm_occ_cell gives correct output for class", {
  coln_cell_class <- base::colnames(occ_cell_class)
  nums_cell_class <- base::as.vector(t(occ_cell_class[3:base::length(occ_cell_class)]))
  testthat::expect_true(base::is.numeric(nums_cell_class))
  testthat::expect_true("paleolat" %in% coln_cell_class && "paleolng" %in% coln_cell_class)
  testthat::expect_true(base::min(occ_cell_class$paleolat)>=-90 && base::max(occ_cell_class$paleolat)<=90 
              && base::min(occ_cell_class$paleolng)>=-180 && base::max(occ_cell_class$paleolng<=180))
})
  
testthat::test_that("test that pm_occ_cell gives correct output for phylum", {
  coln_cell_phylum <- base::colnames(occ_cell_phylum)
  nums_cell_phylum <- base::as.vector(t(occ_cell_phylum[3:base::length(occ_cell_phylum)]))
  testthat::expect_true(base::is.numeric(nums_cell_phylum))
  testthat::expect_true("paleolat" %in% coln_cell_phylum && "paleolng" %in% coln_cell_phylum)
  testthat::expect_true(base::min(occ_cell_phylum$paleolat)>=-90 && base::max(occ_cell_phylum$paleolat)<=90 
              && base::min(occ_cell_phylum$paleolng)>=-180 && base::max(occ_cell_phylum$paleolng<=180))
})
  
rm(occ_cell_genus, occ_cell_family, occ_cell_order, occ_cell_class, occ_cell_phylum)
rm(coln_cell_species, coln_cell_genus, coln_cell_family, coln_cell_order, coln_cell_class, coln_cell_phylum)
rm(nums_cell_species, nums_cell_genus, nums_cell_family, nums_cell_order, nums_cell_class, nums_cell_phylum)

#####pm_divraster_loc######
testthat::context("pm_divraster_loc")
#get diversity raster
div_mean <- paleoMap::pm_divraster_loc (shape, occ_species, fun=mean)
div_max <- paleoMap::pm_divraster_loc (shape, occ_species, fun=max)
div_min <- paleoMap::pm_divraster_loc (shape, occ_species, fun=min)
div_count <- paleoMap::pm_divraster_loc (shape, occ_species, fun="count")
testthat::test_that("test that output is a RasterLayer", {
  testthat::expect_that(div_mean@class[1], equals("RasterLayer"))
  testthat::expect_that(div_max@class[1], equals("RasterLayer"))
  testthat::expect_that(div_min@class[1], equals("RasterLayer"))
  testthat::expect_that(div_count@class[1], equals("RasterLayer"))
})

rm(div_mean, div_max, div_min, div_count)
  
#####pm_divraster_cell######
testthat::context("pm_divraster_cell")
#get divraster cell
div_cell <- paleoMap::pm_divraster_cell(shape, occ_cell_species)
testthat::test_that("test that output is a RasterLayer", {
  testthat::expect_that(div_cell@class[1], equals("RasterLayer"))
})

rm(div_cell)

#####pm_latrich#####
testthat::context("pm_latrich")
latrich_species <- paleoMap::pm_latrich(shape, data, rank="species")
names_species <- base::names(latrich_species)
testthat::test_that("test that column names are correct", {
  testthat::expect_true("paleolat" %in% names_species)
  testthat::expect_true("richness" %in% names_species)
})

testthat::test_that("test that first two rows habe only data from ", {
  testthat::expect_true(base::min(latrich_species$lat_min)>=-90 && base::max(latrich_species$lat_min)<=90 
              && base::min(latrich_species$lat_max)>=-90 && base::max(latrich_species$lat_max)<=90)
})

testthat::test_that("test that richness has only numeric values", {
  nums_latrich <- base::as.vector(t(latrich_species$richn))
  testthat::expect_true(base::is.numeric(nums_latrich))
})

rm(latrich_species, nums_latrich, names_species)

#####pm_latdiv#####
testthat::context("pm_latdiv")
#get max and mean latitudinal diversity
latdiv_max <- pm_latdiv(shape, occ_species, fun=max)
latdiv_mean <- pm_latdiv(shape, occ_species, fun=mean)
names_max <- base::names(latdiv_max)
names_mean <- base::names(latdiv_mean)
testthat::test_that("test that output has columns minlat, maxlat, div", {
  testthat::expect_true("paleolat" %in% names_max && "paleolat" %in% names_mean)
  testthat::expect_true("div" %in% names_max && "div" %in% names_mean)
})

  
testthat::test_that("test that latitude values are correct", {
  testthat::expect_true(base::min(latdiv_max$maxlat)>=-90 && base::max(latdiv_max$maxlat)<=90 
              && base::min(latdiv_max$minlat)>=-90 && base::max(latdiv_max$minlat)<=90)
  testthat::expect_true(base::min(latdiv_mean$maxlat)>=-90 && base::max(latdiv_mean$maxlat)<=90 
              && base::min(latdiv_mean$minlat)>=-90 && base::max(latdiv_mean$minlat)<=90)
})

testthat::test_that("test that diversity column has only numeric values", {
  nums_latdiv_max <- base::as.vector(t(latdiv_max$div))
  testthat::expect_true(base::is.numeric(nums_latdiv_max))
  nums_latdiv_mean <- base::as.vector(t(latdiv_mean$div))
  testthat::expect_true(base::is.numeric(nums_latdiv_mean))
})
rm(latdiv_max, latdiv_mean, nums_latdiv_max, nums_latdiv_mean, names_max, names_mean)
rm(occ_species, occ_cell_species)
  
  
rm(shape, data)