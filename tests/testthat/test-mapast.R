#get the data
base::load(base::system.file("testdata", "testdata.rda", package = "mapast"))

df <- formatdata(data)
#####formatdata#####
#check if after formatdata df is a data.frame and has all columns needed
testthat::context("formatdata")
testthat::test_that("test on formatdata, if output has all columns needed.", {
  coln_df <- colnames(df)
  testthat::expect_true("avg_age" %in% coln_df && "species" %in% coln_df && "genus" %in% coln_df && "class" %in% coln_df
                        && "family" %in% coln_df && "order" %in% coln_df && "phylum" %in% coln_df)
  rm(coln_df)
})



#####paleocoords#####
#check if has recon_age, paleolat and paleolng and is data.frame
df_avg <- paleocoords(df, time="average")
df_auto <- paleocoords(df, time="automatic")
df_vec <- paleocoords(df, time="timevector", timevector=c(0,5))
testthat::context("paleocoords")
testthat::test_that("test on paleocoords, if output has all columns needed.", {
  coln_avg <- colnames(df_avg)
  coln_auto <- colnames(df_auto)
  coln_vec <- colnames(df_vec)
  testthat::expect_true("recon_age" %in% coln_avg && "paleolat" %in% coln_avg && "paleolng" %in% coln_avg)
  testthat::expect_true("recon_age" %in% coln_auto && "paleolat" %in% coln_auto && "paleolng" %in% coln_auto)
  testthat::expect_true("recon_age" %in% coln_vec && "paleolat" %in% coln_vec && "paleolng" %in% coln_vec)
  rm(coln_avg, coln_auto, coln_vec)
})

#####mapocc#####
testthat::context("mapocc")
testthat::test_that("test on mapocc, if output is raster", {
  #create raster
  ras <- mapast::mapocc(df_auto, model="SETON2012", rank="genus", map=map)
  #test if raster is a RasterLayer
  testthat::expect_that(ras@class[1], equals("RasterLayer"))
  rm(ras)
})
testthat::test_that("test on mapocc, if output is rasterstack", {
  #create raster
  ras <- mapast::mapocc(df_avg, model="SETON2012", rank="genus", map=maps)
  #test if raster is a RasterStack
  testthat::expect_that(ras@class[1], equals("RasterStack"))
  rm(ras)
})

#####maprich#####
testthat::context("maprich")
testthat::test_that("test on maprich, if output is raster",{
  #create species richness raster
  spras <- mapast::maprich(df_auto, model="SETON2012", rank="genus", map=map)
  #test if class of raster is RasterLayer
  testthat::expect_that(spras@class[1], equals("RasterLayer"))
  rm(spras)
})
testthat::test_that("test on maprich, if output is rasterstack",{
  #create species richness raster
  spras <- mapast::maprich(df_avg, model="SETON2012", rank="genus", map=maps)
  #test if class of raster is RasterLayer
  testthat::expect_that(spras@class[1], equals("RasterStack"))
  rm(spras)
})

######spsite#####
testthat::context("spsite")
  #create ocurrence matrices
  occ_species <- mapast::spsite(df_auto, unity="fossilsite", rank="species")
  occ_genus <- mapast::spsite(df_auto, unity="fossilsite", rank="genus")
  occ_family <- mapast::spsite(df_auto, unity="fossilsite", rank="family")
  occ_order <- mapast::spsite(df_auto, unity="fossilsite", rank="order")
  occ_class <- mapast::spsite(df_auto, unity="fossilsite", rank="class")
  occ_phylum <-mapast::spsite(df_auto, unity="fossilsite", rank="phylum")
  occ_genus_list <- mapast::spsite(df_avg, unity="fossilsite", rank="genus")
  #test if returned object is data.frame
testthat::test_that("function returns data.frame or list of data frames", {
  testthat::expect_that(class(occ_genus), equals("data.frame"))
  testthat::expect_that(class(occ_genus_list), equals("list"))
  testthat::expect_that(class(occ_genus_list[[1]]), equals("data.frame"))
})
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

testthat::test_that("test that spsite gives correct output for phylum", {
  coln_phylum <- base::colnames(occ_phylum)
  nums_phylum <- base::as.vector(t(occ_phylum[3:base::length(occ_phylum)]))
  testthat::expect_true(base::is.numeric(nums_phylum))
  testthat::expect_true("paleolat" %in% coln_phylum && "paleolng" %in% coln_phylum)
  testthat::expect_true(base::min(occ_phylum$paleolat)>=-90 && base::max(occ_phylum$paleolat)<=90
              && base::min(occ_phylum$paleolng)>=-180 && base::max(occ_phylum$paleolng<=180))
})
rm(occ_genus, occ_family, occ_order, occ_class, occ_phylum, occ_genus_list)
rm(coln_species, coln_genus, coln_family, coln_order, coln_class, coln_phylum)
rm(nums_species, nums_genus, nums_family, nums_order, nums_class, nums_phylum)

######spsite######
testthat::context("spsite")
# #get output of function
occ_cell_species <- mapast::spsite(df_auto, unity="cell", rank="species")
occ_cell_genus <- mapast::spsite(df_auto, unity="cell", rank="genus")
occ_cell_family <- mapast::spsite(df_auto, unity="cell", rank="family")
occ_cell_order <- mapast::spsite(df_auto, unity="cell", rank="order")
occ_cell_class <- mapast::spsite(df_auto, unity="cell", rank="class")
occ_cell_phylum <-mapast::spsite(df_auto, unity="cell", rank="phylum")

occ_cell_genus_list <- mapast::spsite(df_avg, unity="cell", rank="genus")
  
  testthat::test_that("function returns data.frame or list of data frames", {
    testthat::expect_that(class(occ_cell_genus), equals("data.frame"))
    testthat::expect_that(class(occ_cell_genus_list), equals("list"))
    testthat::expect_that(class(occ_cell_genus_list[[1]]), equals("data.frame"))
  })
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

rm(occ_cell_genus, occ_cell_family, occ_cell_order, occ_cell_class, occ_cell_phylum, occ_cell_genus_list)
rm(coln_cell_species, coln_cell_genus, coln_cell_family, coln_cell_order, coln_cell_class, coln_cell_phylum)
rm(nums_cell_species, nums_cell_genus, nums_cell_family, nums_cell_order, nums_cell_class, nums_cell_phylum)


#####mapdiv#####
testthat::context("mapdiv")
div_mean <- mapast::mapdiv(map=map, data=df_auto, unity="fossilsite", fun=mean)

div_stack <- mapast::mapdiv(map=maps, data=df_avg, unity="fossilsite")
testthat::test_that("test that mapdiv with fossilsite output is a RasterLayer", {
  testthat::expect_that(div_mean@class[1], equals("RasterLayer"))
  testthat::expect_that(div_stack@class[1], equals("RasterStack"))
})
rm(div_mean, div_stack)

div_cell <- mapast::mapdiv(map=map, data=df_auto, unity="cell")
div_stack <- mapast::mapdiv(map=maps, data=df_avg, unity="cell")
testthat::test_that("test that mapdiv with cell output is a RasterLayer", {
  testthat::expect_that(div_cell@class[1], equals("RasterLayer"))
  testthat::expect_that(div_stack@class[1], equals("RasterStack"))
})

rm(div_cell, div_stack)


####latdivgrad#####
testthat::context("latdivgrad")
latrich_species <- mapast::latdivgrad(data=df_auto, map=map, method="richness", rank="species")
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

#get max and mean latitudinal diversity
latdiv_max <- mapast::latdivgrad(data=df_auto, map=map, method="shannon", fun=max)
latdiv_mean <- mapast::latdivgrad(data=df_auto, map=map, method="shannon", fun=mean)
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


rm(map, data, df)