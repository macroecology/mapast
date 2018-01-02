#get the data
base::load(base::system.file("testdata", "testdata.rda", package = "pasta"))


data <- data[1:100,]
data <- data[which(!data$matched_rank=="unranked clade"),]


#####mapocc#####
testthat::context("mapocc")
testthat::test_that("test on mapocc, if output is raster", {
  #create raster
  ras <- pasta::mapocc(shape, data, rank="genus")
  #test if raster is a RasterLayer
  testthat::expect_that(ras@class[1], equals("RasterLayer"))
  rm(ras)
})
 
#####maprich#####
testthat::context("maprich")
testthat::test_that("test on maprich, if output is raster",{
  #create species richness raster
  spras <- pasta::maprich(shape, data)
  #test if class of raster is RasterLayer
  testthat::expect_that(spras@class[1], equals("RasterLayer"))
  rm(spras)
})
  
######spsite#####
testthat::context("spsite")
  #create ocurrence matrices
  occ_species <- pasta::spsite(data, unity="fossilsite", rank="species")
  occ_genus <- pasta::spsite(data, unity="fossilsite", rank="genus")
  occ_family <- pasta::spsite(data, unity="fossilsite", rank="family")
  occ_order <- pasta::spsite(data, unity="fossilsite", rank="order")
  occ_class <- pasta::spsite(data, unity="fossilsite", rank="class")
  occ_phylum <-pasta::spsite(data, unity="fossilsite", rank="phylum")
  #test if paleolat and paleolng are inside and correct
testthat::test_that("test that spsite gives correct output for species", {
  coln_species <- base::colnames(occ_species)
  nums_species <- base::as.vector(t(occ_species[3:base::length(occ_species)]))
  testthat::expect_true(base::is.numeric(nums_species))
  testthat::expect_true("paleolat" %in% coln_species && "paleolng" %in% coln_species)
  testthat::expect_true(base::min(occ_species$paleolat)>=-90 && base::max(occ_species$paleolat)<=90 
                        && base::min(occ_species$paleolng)>=-180 && base::max(occ_species$paleolng<=180))
  })

testthat::test_that("test that spsite gives correct output for genus", {
    coln_genus <- base::colnames(occ_genus)
    nums_genus <- base::as.vector(t(occ_genus[3:base::length(occ_genus)]))
    testthat::expect_true( base::is.numeric(nums_genus))
    testthat::expect_true("paleolat" %in% coln_genus && "paleolng" %in% coln_genus)
    testthat::expect_true(base::min(occ_genus$paleolat)>=-90 && base::max(occ_genus$paleolat)<=90 
                          && base::min(occ_genus$paleolng)>=-180 && base::max(occ_genus$paleolng<=180))
  })

testthat::test_that("test that spsite gives correct output for family", {
  coln_family <- base::colnames(occ_family)
  nums_family <- base::as.vector(t(occ_family[3:base::length(occ_family)]))
  testthat::expect_true(base::is.numeric(nums_family))
  testthat::expect_true("paleolat" %in% coln_family && "paleolng" %in% coln_family)
  testthat::expect_true(base::min(occ_family$paleolat)>=-90 && base::max(occ_family$paleolat)<=90 
              && base::min(occ_family$paleolng)>=-180 && base::max(occ_family$paleolng<=180))
})

testthat::test_that("test that spsite gives correct output for order", {
  coln_order <- base::colnames(occ_order)
  testthat::expect_true("paleolat" %in% coln_order && "paleolng" %in% coln_order)
  testthat::expect_true(base::min(occ_order$paleolat)>=-90 && base::max(occ_order$paleolat)<=90 
              && base::min(occ_order$paleolng)>=-180 && base::max(occ_order$paleolng<=180))
})
C
testthat::test_that("test that spsite gives correct output for phylum", {
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
 
######spsite######   
testthat::context("spsite")
# #get output of function
occ_cell_species <- pasta::spsite(data, unity="cell", rank="species")
occ_cell_genus <- pasta::spsite(data, unity="cell", rank="genus")
occ_cell_family <- pasta::spsite(data, unity="cell", rank="family")
occ_cell_order <- pasta::spsite(data, unity="cell", rank="order")
occ_cell_class <- pasta::spsite(data, unity="cell", rank="class")
occ_cell_phylum <-pasta::spsite(data, unity="cell", rank="phylum")
#test if paleolat and paleolng are inside and correct
testthat::test_that("test that spsite gives correct output for species", {
  coln_cell_species <- base::colnames(occ_cell_species)
  nums_cell_species <- base::as.vector(t(occ_cell_species[3:base::length(occ_cell_species)]))
  testthat::expect_true(base::is.numeric(nums_cell_species))
  testthat::expect_true("paleolat" %in% coln_cell_species && "paleolng" %in% coln_cell_species)
  testthat::expect_true(base::min(occ_cell_species$paleolat)>=-90 && base::max(occ_cell_species$paleolat)<=90
            && base::min(occ_cell_species$paleolng)>=-180 && base::max(occ_cell_species$paleolng<=180))

})

testthat::test_that("test that spsite gives correct output for genus", {
  coln_cell_genus <- base::colnames(occ_cell_genus)
  nums_cell_genus <- base::as.vector(t(occ_cell_genus[3:base::length(occ_cell_genus)]))
  testthat::expect_true(base::is.numeric(nums_cell_genus))
  testthat::expect_true("paleolat" %in% coln_cell_genus && "paleolng" %in% coln_cell_genus)
  testthat::expect_true(base::min(occ_cell_genus$paleolat)>=-90 && base::max(occ_cell_genus$paleolat)<=90
              && base::min(occ_cell_genus$paleolng)>=-180 && base::max(occ_cell_genus$paleolng<=180))
})

testthat::test_that("test that spsite gives correct output for family", {
  coln_cell_family <- base::colnames(occ_cell_family)
  nums_cell_family <- base::as.vector(t(occ_cell_family[3:base::length(occ_cell_family)]))
  testthat::expect_true(base::is.numeric(nums_cell_family))
  testthat::expect_true("paleolat" %in% coln_cell_family && "paleolng" %in% coln_cell_family)
  testthat::expect_true(base::min(occ_cell_family$paleolat)>=-90 && base::max(occ_cell_family$paleolat)<=90
              && base::min(occ_cell_family$paleolng)>=-180 && base::max(occ_cell_family$paleolng<=180))
})

testthat::test_that("test that spsite gives correct output for order", {
  coln_cell_order <- base::colnames(occ_cell_order)
  nums_cell_order <- base::as.vector(t(occ_cell_order[3:base::length(occ_cell_order)]))
  testthat::expect_true(base::is.numeric(nums_cell_order))
  testthat::expect_true("paleolat" %in% coln_cell_order && "paleolng" %in% coln_cell_order)
  testthat::expect_true(base::min(occ_cell_order$paleolat)>=-90 && base::max(occ_cell_order$paleolat)<=90
              && base::min(occ_cell_order$paleolng)>=-180 && base::max(occ_cell_order$paleolng<=180))
})

testthat::test_that("test that spsite gives correct output for class", {
  coln_cell_class <- base::colnames(occ_cell_class)
  nums_cell_class <- base::as.vector(t(occ_cell_class[3:base::length(occ_cell_class)]))
  testthat::expect_true(base::is.numeric(nums_cell_class))
  testthat::expect_true("paleolat" %in% coln_cell_class && "paleolng" %in% coln_cell_class)
  testthat::expect_true(base::min(occ_cell_class$paleolat)>=-90 && base::max(occ_cell_class$paleolat)<=90
              && base::min(occ_cell_class$paleolng)>=-180 && base::max(occ_cell_class$paleolng<=180))
})

testthat::test_that("test that spsite gives correct output for phylum", {
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


#####mapdiv#####
testthat::context("mapdiv")
div_mean <- pasta::mapdiv(shape, data, unity="fossilsite", fun=mean)
div_max <- pasta::mapdiv(shape, data, unity="fossilsite", fun=max)
div_min <- pasta::mapdiv(shape, data, unity="fossilsite", fun=min)
div_count <- pasta::mapdiv(shape, data, unity="fossilsite", fun="count")
testthat::test_that("test that mapdiv with fossilsite output is a RasterLayer", {
  testthat::expect_that(div_mean@class[1], equals("RasterLayer"))
  testthat::expect_that(div_max@class[1], equals("RasterLayer"))
  testthat::expect_that(div_min@class[1], equals("RasterLayer"))
  testthat::expect_that(div_count@class[1], equals("RasterLayer"))
})
rm(div_mean, div_max, div_min, div_count)

div_cell <- pasta::mapdiv(shape, data, unity="cell")
testthat::test_that("test that mapdiv with cell output is a RasterLayer", {
  testthat::expect_that(div_cell@class[1], equals("RasterLayer"))
})

rm(div_cell)

 
####latdivgrad#####
testthat::context("latdivgrad")
latrich_species <- pasta::latdivgrad(shape, data, method="richness", rank="species")
names_species <- base::names(latrich_species)
testthat::test_that("test that column names are correct", {
  testthat::expect_true("paleolat" %in% names_species)
  testthat::expect_true("div" %in% names_species)
})

testthat::test_that("test that range of paleolat is correct", {
  testthat::expect_true(base::min(latrich_species$paleolat)>=-90 && base::max(latrich_species$paleolat)<=90)
})

testthat::test_that("test that diversity has only numeric values", {
  nums_latrich <- base::as.vector(t(latrich_species$div))
  testthat::expect_true(base::is.numeric(nums_latrich))
})

rm(latrich_species, nums_latrich, names_species)

# #get max and mean latitudinal diversity
latdiv_max <- latdivgrad(shape, data, method="shannon", fun=max)
latdiv_mean <- latdivgrad(shape, data, method="shannon", fun=mean)
names_max <- base::names(latdiv_max)
names_mean <- base::names(latdiv_mean)
testthat::test_that("test that output has columns minlat, maxlat, div", {
  testthat::expect_true("paleolat" %in% names_max && "paleolat" %in% names_mean)
  testthat::expect_true("div" %in% names_max && "div" %in% names_mean)
})


testthat::test_that("test that paleolat value ranges are correct", {
  testthat::expect_true(base::min(latdiv_mean$paleolat)>=-90 && base::max(latdiv_mean$paleolat)<=90
              && base::min(latdiv_max$paleolat)>=-90 && base::max(latdiv_max$paleolat)<=90)
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