context("pm_occraster")
test_that("test on pm_occraster, if output is raster")
  #get the data
  data(testdata)
  #create raster
  ras <- pm_occraster(shape, data)
  #test if raster is a RasterLayer
  expect_that(ras@class[1], equals("RasterLayer"))
  rm(shape, data, ras)
  

context("pm_richraster")
test_that("test on pm_richraster, if output is raster")
  #get data and shape file
  data(testdata)
  #create species richness raster
  spras <- pm_richraster(shape, data)
  #test if class of raster is RasterLayer
  expect_that(spras@class[1], equals("RasterLayer"))
  rm(shape, data, spras)
  
  
context("pm_occ")
  #get data
  data(testdata)
  #create ocurrence matrices
  occ_species <- pm_occ(data, rank="species") 
  occ_genus <- pm_occ(data, rank="genus")
  occ_family <- pm_occ(data, rank="family")
  occ_order <- pm_occ(data, rank="order")
  occ_class <- pm_occ(data, rank="class")
  occ_phylum <-pm_occ(data, rank="phylum")
  #test if paleolat and paleolng are inside and correct
test_that("test that pm_occ gives correct output for species")
  coln_species <- colnames(occ_species)
  nums_species <- as.vector(t(occ_species[3:length(occ_species)]))
  expect_true(is.numeric(nums_species))
  expect_true("paleolat" %in% coln_species && "paleolng" %in% coln_species)
  expect_true(min(occ_species$paleolat)>=-90 && max(occ_species$paleolat)<=90 
              && min(occ_species$paleolng)>=-180 && max(occ_species$paleolng<=180))
test_that("test that pm_occ gives correct output for genus")
  coln_genus <- colnames(occ_genus)
  nums_genus <- as.vector(t(occ_genus[3:length(occ_genus)]))
  expect_true(is.numeric(nums_genus))
  expect_true("paleolat" %in% coln_genus && "paleolng" %in% coln_genus)
  expect_true(min(occ_genus$paleolat)>=-90 && max(occ_genus$paleolat)<=90 
              && min(occ_genus$paleolng)>=-180 && max(occ_genus$paleolng<=180))
test_that("test that pm_occ gives correct output for family")
  coln_family <- colnames(occ_family)
  nums_family <- as.vector(t(occ_family[3:length(occ_family)]))
  expect_true(is.numeric(nums_family))
  expect_true("paleolat" %in% coln_family && "paleolng" %in% coln_family)
  expect_true(min(occ_family$paleolat)>=-90 && max(occ_family$paleolat)<=90 
              && min(occ_family$paleolng)>=-180 && max(occ_family$paleolng<=180))
test_that("test that pm_occ gives correct output for order")
  coln_order <- colnames(occ_order)
  expect_true("paleolat" %in% coln_order && "paleolng" %in% coln_order)
  expect_true(min(occ_order$paleolat)>=-90 && max(occ_order$paleolat)<=90 
              && min(occ_order$paleolng)>=-180 && max(occ_order$paleolng<=180))
test_that("test that pm_occ gives correct output for class")
  coln_class <- colnames(occ_class)
  nums_class <- as.vector(t(occ_class[3:length(occ_class)]))
  expect_true(is.numeric(nums_class))
  expect_true("paleolat" %in% coln_class && "paleolng" %in% coln_class)
  expect_true(min(occ_class$paleolat)>=-90 && max(occ_class$paleolat)<=90 
              && min(occ_class$paleolng)>=-180 && max(occ_class$paleolng<=180))
test_that("test that pm_occ gives correct output for phylum")
  coln_phylum <- colnames(occ_phylum)
  nums_phylum <- as.vector(t(occ_phylum[3:length(occ_phylum)]))
  expect_true(is.numeric(nums_phylum))
  expect_true("paleolat" %in% coln_phylum && "paleolng" %in% coln_phylum)
  expect_true(min(occ_phylum$paleolat)>=-90 && max(occ_phylum$paleolat)<=90 
              && min(occ_phylum$paleolng)>=-180 && max(occ_phylum$paleolng<=180))
  rm(occ_species, occ_genus, occ_family, occ_order, occ_class, occ_phylum)
  rm(coln_species, coln_genus, coln_family, coln_order, coln_class, coln_phylum)
  rm(nums_species, nums_genus, nums_family, nums_order, nums_class, nums_phylum)

    
context("pm_occ_cell")
#get output of function
occ_cell_species <- pm_occ_cell(data, rank="species") 
occ_cell_genus <- pm_occ_cell(data, rank="genus")
occ_cell_family <- pm_occ_cell(data, rank="family")
occ_cell_order <- pm_occ_cell(data, rank="order")
occ_cell_class <- pm_occ_cell(data, rank="class")
occ_cell_phylum <-pm_occ_cell(data, rank="phylum")
#test if paleolat and paleolng are inside and correct
test_that("test that pm_occ_cell gives correct output for species")
coln_cell_species <- colnames(occ_cell_species)
nums_cell_species <- as.vector(t(occ_cell_species[3:length(occ_cell_species)]))
expect_true(is.numeric(nums_cell_species))
expect_true("paleolat" %in% coln_cell_species && "paleolng" %in% coln_cell_species)
expect_true(min(occ_cell_species$paleolat)>=-90 && max(occ_cell_species$paleolat)<=90 
            && min(occ_cell_species$paleolng)>=-180 && max(occ_cell_species$paleolng<=180))
test_that("test that pm_occ_cell gives correct output for genus")
coln_cell_genus <- colnames(occ_cell_genus)
nums_cell_genus <- as.vector(t(occ_cell_genus[3:length(occ_cell_genus)]))
expect_true(is.numeric(nums_cell_genus))
expect_true("paleolat" %in% coln_cell_genus && "paleolng" %in% coln_cell_genus)
expect_true(min(occ_cell_genus$paleolat)>=-90 && max(occ_cell_genus$paleolat)<=90 
            && min(occ_cell_genus$paleolng)>=-180 && max(occ_cell_genus$paleolng<=180))
test_that("test that pm_occ_cell gives correct output for family")
coln_cell_family <- colnames(occ_cell_family)
nums_cell_family <- as.vector(t(occ_cell_family[3:length(occ_cell_family)]))
expect_true(is.numeric(nums_cell_family))
expect_true("paleolat" %in% coln_cell_family && "paleolng" %in% coln_cell_family)
expect_true(min(occ_cell_family$paleolat)>=-90 && max(occ_cell_family$paleolat)<=90 
            && min(occ_cell_family$paleolng)>=-180 && max(occ_cell_family$paleolng<=180))
test_that("test that pm_occ_cell gives correct output for order")
coln_cell_order <- colnames(occ_cell_order)
expect_true("paleolat" %in% coln_cell_order && "paleolng" %in% coln_cell_order)
expect_true(min(occ_cell_order$paleolat)>=-90 && max(occ_cell_order$paleolat)<=90 
            && min(occ_cell_order$paleolng)>=-180 && max(occ_cell_order$paleolng<=180))
test_that("test that pm_occ_cell gives correct output for class")
coln_cell_class <- colnames(occ_cell_class)
nums_cell_class <- as.vector(t(occ_cell_class[3:length(occ_cell_class)]))
expect_true(is.numeric(nums_cell_class))
expect_true("paleolat" %in% coln_cell_class && "paleolng" %in% coln_cell_class)
expect_true(min(occ_cell_class$paleolat)>=-90 && max(occ_cell_class$paleolat)<=90 
            && min(occ_cell_class$paleolng)>=-180 && max(occ_cell_class$paleolng<=180))
test_that("test that pm_occ_cell gives correct output for phylum")
coln_cell_phylum <- colnames(occ_cell_phylum)
nums_cell_phylum <- as.vector(t(occ_cell_phylum[3:length(occ_cell_phylum)]))
expect_true(is.numeric(nums_cell_phylum))
expect_true("paleolat" %in% coln_cell_phylum && "paleolng" %in% coln_cell_phylum)
expect_true(min(occ_cell_phylum$paleolat)>=-90 && max(occ_cell_phylum$paleolat)<=90 
            && min(occ_cell_phylum$paleolng)>=-180 && max(occ_cell_phylum$paleolng<=180))
rm(occ_cell_species, occ_cell_genus, occ_cell_family, occ_cell_order, occ_cell_class, occ_cell_phylum)
rm(coln_cell_species, coln_cell_genus, coln_cell_family, coln_cell_order, coln_cell_class, coln_cell_phylum)
rm(nums_cell_species, nums_cell_genus, nums_cell_family, nums_cell_order, nums_cell_class, nums_cell_phylum)



context("rank_filter")
test_that("test if output is a dataframe and if there are only species in the data frame")
  #get data
  data(testdata)
  #filter data
  filter_data <- rank_filter(data, res=10, rank="species")
  #test if it is a data frame
  expect_true(is.data.frame(filter_data))
  #test if most important columns with the correct names are in the data frame
  expect_true(grep("paleolat", names(filter_data))!=0)
  expect_true(grep("paleolng", names(filter_data))!=0)
  expect_true(grep("matched_name", names(filter_data))!=0)
  expect_true(grep("matched_rank", names(filter_data))!=0)
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
  taxon_rank <- as.vector(filter_data$matched_rank)
  bool <- TRUE
  for (i in 1: length(matched_rank)){
    bool <- (bool && (matched_rank[i]=="species"))
  }
  expect_true(bool)
  
