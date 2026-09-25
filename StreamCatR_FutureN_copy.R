library(StreamCatR)
library(arrow)
library(future.apply)
library(tictoc)
library(data.table)
library(fst)
# https://github.com/usepa/streamcatR

# For Summative Accumulation Approach 

future_dir2 = "C:/Users/MPennino/OneDrive - Environmental Protection Agency (EPA)/Projects/OASES/Data/Future_NO3/"


# 1. Download base files ---------------------------------------------------------
# These files contain the network information necessary to construct the 
# medium-resolution NHDPlus igraph object (see step 3 below)
StreamCatR::sr_fetch_accum_data(dest_dir = "./base_files",
                                force = TRUE)

# 2. Download zonal raster artifacts -----------------------------------------------
# This function downloads optimized catchment rasters for the zonal statistics
# and tabulate area functions
StreamCatR::sr_fetch_zonal_data(dest_dir = "./cat_rasters",
                                force = TRUE,
                                quiet = TRUE)

# 3. Build accumulation network --------------------------------------------------

ft  <- setDT(read_parquet("base_files/full_from_to_conus.parquet"))
fty <- setDT(read_parquet("base_files/ftype_conus.parquet"))
sp  <- setDT(read_parquet("base_files/special_comid_handling.parquet"))
tr  <- setDT(read_parquet("base_files/gridcode_comid_translation_conus.parquet"))

# #Don't need to do this a second time if already saved to netowrk. In sr_write_network() step
# tic()
# net <- sr_prepare_network(
#   from           = ft$FROMCOMID,
#   to             = ft$TOCOMID,
#   preset         = "nhdplus_mr",
#   coast_ids      = fty[FTYPE == "Coastline"]$COMID,     # 23,585
#   special_remove = sp$removeFROMCOMID,                  # 4
#   all_ids        = tr$FEATUREID                         # seeds isolated vertices
# )
# toc()
# #4.31 sec elapsed
# 
# #Seems like it isn't necessary to re-run?
# #Error: A network bundle already exists at 'networks/nhdplus_mr_conus'. Use overwrite = TRUE to replace it.
# 
# tic()
# sr_write_network(net, "networks/nhdplus_mr_conus")
# toc()
# #1.92 sec elapsed

# test the integrity of the network 
# should say: ACYCLIC - safe to accumulate.
#sr_network_report(net)

#---------------------------------------------------------------------------
# Read in catchment variable data
future_dir = 'C:/Users/MPennino/OneDrive - Environmental Protection Agency (EPA)/Projects/OASES/Data/Future_NO3/'
#filename = 'Dataset_Future_Catchment_4.5GISS.parquet'
filename = 'Future_NO3_predictors_catchments.parquet'

varData <- read_parquet(paste0(future_dir,filename))
dep_N_Cat = read.fst(paste0(future_dir2,'Future_Data_COMID_N_Deposition.fst'))
popden_cat = read.fst(paste0(future_dir2,'Future_Data_COMID_PopDensity.fst'))
temp_Cat = read.fst(paste0(future_dir2,'Future_Data_COMID_Temperature.fst'))
prec_Cat = read.fst(paste0(future_dir2,'Future_Data_COMID_Precipitation.fst'))
imprv_cat = read.fst(paste0(future_dir2,'Future_Data_COMID_Impervious.fst'))

varData = merge(varData,prec_Cat,by='COMID')
varData = merge(varData,temp_Cat,by='COMID')
varData = merge(varData,imprv_cat,by='COMID')

DataAreas = varData[,c('COMID','CatAreaSqKm','WsAreaSqKm')]
vars = c('COMID','CatAreaSqKm','WsAreaSqKm',
         'n_surp_kgsqkm_RCP4.5G','n_manure_kgsqkm_RCP4.5G',
         'Pct_crop_pasture_land_RCP4.5G','n_farm_fert_kgsqkm_RCP4.5G',
         'pop_den_RCP4.5G','impervious_2050_RCP4.5GISS',
         'n_crop_fix_kgsqkm_RCP4.5G','Pct_forest_RCP4.5G',
         'temp_2050_degC_RCP4.5GISS','precip_2050_mm_RCP4.5GISS',
         'n_crop_rem_kgsqkm_RCP4.5G','n_human_food_waste_kgsqkm_RCP4.5G',
         'totalN_2050ref_kgNha','totalN_2050ctl_kgNha',
         
         'n_surp_kgsqkm_RCP8.5G','n_manure_kgsqkm_RCP8.5G',
         'Pct_crop_pasture_land_RCP8.5G','n_farm_fert_kgsqkm_RCP8.5G',
         'impervious_2050_RCP8.5GISS',
         'n_crop_fix_kgsqkm_RCP8.5G','Pct_forest_RCP8.5G',
         'temp_2050_degC_RCP8.5GISS','precip_2050_mm_RCP8.5GISS',
         'n_crop_rem_kgsqkm_RCP8.5G',
         #'n_human_food_waste_kgsqkm_RCP8.5G','pop_den_RCP8.5G', # due to issues with raster, was not able to generate these variables
         
         'n_surp_kgsqkm_RCP4.5H','n_manure_kgsqkm_RCP4.5H',
         'Pct_crop_pasture_land_RCP4.5H','n_farm_fert_kgsqkm_RCP4.5H',
         'pop_den_RCP4.5H','impervious_2050_RCP4.5Hadgem',
         'n_crop_fix_kgsqkm_RCP4.5H','Pct_forest_RCP4.5H',
         'temp_2050_degC_RCP4.5Hadgem','precip_2050_mm_RCP4.5Hadgem',
         'n_crop_rem_kgsqkm_RCP4.5H','n_human_food_waste_kgsqkm_RCP4.5H',
         
         'n_surp_kgsqkm_RCP8.5H','n_manure_kgsqkm_RCP8.5H',
         'Pct_crop_pasture_land_RCP8.5H','n_farm_fert_kgsqkm_RCP8.5H',
         'pop_den_RCP8.5H','impervious_2050_RCP8.5Hadgem',
         'n_crop_fix_kgsqkm_RCP8.5H','Pct_forest_RCP8.5H',
         'temp_2050_degC_RCP8.5Hadgem','precip_2050_mm_RCP8.5Hadgem',
         'n_crop_rem_kgsqkm_RCP8.5H','n_human_food_waste_kgsqkm_RCP8.5H')

vars2 = c('COMID',
          'n_surp_kg_RCP4.5G','n_manure_kg_RCP4.5G',
          'Pct_crop_pasture_land_RCP4.5G','n_farm_fert_kg_RCP4.5G',
          'pop_den_RCP4.5G','impervious_2050_RCP4.5GISS',
          'n_crop_fix_kg_RCP4.5G','Pct_forest_RCP4.5G',
          'temp_2050_degC_RCP4.5GISS','precip_2050_mm_RCP4.5GISS',
          'n_crop_rem_kg_RCP4.5G','n_human_food_waste_kg_RCP4.5G',
          'totalN_2050ref_kgNha','totalN_2050ctl_kgNha',
          
          'n_surp_kg_RCP8.5G','n_manure_kg_RCP8.5G',
          'Pct_crop_pasture_land_RCP8.5G','n_farm_fert_kg_RCP8.5G',
          'impervious_2050_RCP8.5GISS',
          'n_crop_fix_kg_RCP8.5G','Pct_forest_RCP8.5G',
          'temp_2050_degC_RCP8.5GISS','precip_2050_mm_RCP8.5GISS',
          'n_crop_rem_kg_RCP8.5G',
          #'n_human_food_waste_kg_RCP8.5G','pop_den_RCP8.5G', # due to issues with raster, was not able to generate these variables
          
          'n_surp_kg_RCP4.5H','n_manure_kg_RCP4.5H',
          'Pct_crop_pasture_land_RCP4.5H','n_farm_fert_kg_RCP4.5H',
          'pop_den_RCP4.5H','impervious_2050_RCP4.5Hadgem',
          'n_crop_fix_kg_RCP4.5H','Pct_forest_RCP4.5H',
          'temp_2050_degC_RCP4.5Hadgem','precip_2050_mm_RCP4.5Hadgem',
          'n_crop_rem_kg_RCP4.5H','n_human_food_waste_kg_RCP4.5H',
          
          'n_surp_kg_RCP8.5H','n_manure_kg_RCP8.5H',
          'Pct_crop_pasture_land_RCP8.5H','n_farm_fert_kg_RCP8.5H',
          'pop_den_RCP8.5H','impervious_2050_RCP8.5Hadgem',
          'n_crop_fix_kg_RCP8.5H','Pct_forest_RCP8.5H',
          'temp_2050_degC_RCP8.5Hadgem','precip_2050_mm_RCP8.5Hadgem',
          'n_crop_rem_kg_RCP8.5H','n_human_food_waste_kg_RCP8.5H')

setdiff(vars,colnames(varData)) # gives not matching

varData = varData[,vars]




# test = varData[,c('COMID','Pct_crop_pasture_land_RCP4.5G')]
# summary(test$Pct_crop_pasture_land_RCP4.5G)
# write.csv(test, file = paste0(future_dir2,'Dataset_Pct_crop_pasture_land_RCP4.5G.csv'), row.names=F)


# 4. zonal statistics (if continuous raster data) ---------------------------------------------------------

# define zones
#zones <- sr_zones(zone_dir = "cat_rasters")            # all 21 regions
# zones <- sr_zones("NHDPlus16", "cat_rasters")        # just one

# tic()
# bfi <- sr_zonal(
#   predictor = "landscape_rasters/bfi.tif",
#   zones     = zones,
#   stats     = c("sum", "count"),
#   workers   = 21L
# )
# toc()
# 65.28 sec elapsed





# 5. tabulate area (if categorical raster data) ---------------------------------------------------------

# tic()
# nlcd <- sr_tab_area(
#   predictor = "landscape_rasters/Annual_NLCD_LndCov_2023_CU_C1V0.tif",
#   zones     = zones,
#   workers   = 21L
# )
# toc()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PreProcessing Data -------------------------------------------------------------
# Converting percents the Area and area normalized to Kg
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# !! For kgsqkm fields !! --------------------------
kgsqkm_cols <- grep("kgsqkm", names(varData), value = TRUE) # pull out kgsqkm fields
area_col <- "CatAreaSqKm"  # replace with the actual column name
kg_cols <- gsub("kgsqkm", "kg", kgsqkm_cols)
temp_kgsqkm = varData[,c('COMID','CatAreaSqKm',kgsqkm_cols)]

for (i in seq_along(kg_cols)) {
  temp_kgsqkm[[kg_cols[i]]] <-
    temp_kgsqkm[[kgsqkm_cols[i]]] *  temp_kgsqkm[["CatAreaSqKm"]]
}
options(scipen = 999)

# Pull out just the "kg" fields
temp_kg = temp_kgsqkm[,c('COMID',kg_cols)]

# !! For kgha fields !!--------------------------
kgNha_cols <- grep("kgNha", names(varData), value = TRUE) # pull out kgsqkm fields
area_col <- "CatAreaSqKm"  # replace with the actual column name
kg_cols <- gsub("kgNha", "kg", kgNha_cols)
temp_kgNha = varData[,c('COMID','CatAreaSqKm',kgNha_cols)]

for (i in seq_along(kg_cols)) {
  temp_kgNha[[kg_cols[i]]] <-
    temp_kgNha[[kgNha_cols[i]]] * (temp_kgNha[["CatAreaSqKm"]]*100)
}
options(scipen = 999)

# Pull out just the "kg" fields
temp_kg_dep = temp_kgNha[,c('COMID',kg_cols)]

# !! For Percent fields !!--------------------------
library(dplyr)
varData <- varData %>% rename(
  Pct_impervious_2050_RCP4.5GISS = impervious_2050_RCP4.5GISS,
  Pct_impervious_2050_RCP8.5GISS = impervious_2050_RCP8.5GISS,
  Pct_impervious_2050_RCP4.5Hadgem = impervious_2050_RCP4.5Hadgem,
  Pct_impervious_2050_RCP8.5Hadgem = impervious_2050_RCP8.5Hadgem
)

pct_cols <- grep("Pct_", names(varData), value = TRUE) # pull out kgsqkm fields
area_cols <- gsub("Pct_", "area_sqkm_", pct_cols)
temp_Pct = varData[,c('COMID','CatAreaSqKm',pct_cols)]

for (i in seq_along(area_cols)) {
  temp_Pct[[area_cols[i]]] <-
    temp_Pct[[pct_cols[i]]] * temp_Pct[["CatAreaSqKm"]]/ 100
}
options(scipen = 999)

# Pull out just the "kg" fields
temp_area = temp_Pct[,c('COMID',area_cols)]
summary(temp_area$area_sqkm_forest_RCP4.5G)
summary(temp_area$area_sqkm_crop_pasture_land_RCP4.5G)
summary(varData$CatAreaSqKm)


# !! For Pop Density fields (#/sqkm) !!--------------------------
pop_den_cols <- grep("pop_den", names(varData), value = TRUE) # pull out kgsqkm fields
area_col <- "CatAreaSqKm"  # replace with the actual column name
pop_num_cols <- gsub("pop_den", "pop_num", pop_den_cols)
temp_pop_den = varData[,c('COMID','CatAreaSqKm',pop_den_cols)]

for (i in seq_along(pop_num_cols)) {
  temp_pop_den[[pop_num_cols[i]]] <-
    temp_pop_den[[pop_den_cols[i]]] * (temp_pop_den[["CatAreaSqKm"]])
}
options(scipen = 999)

# Pull out just the "kg" fields
temp_pop_num = temp_pop_den[,c('COMID',pop_num_cols)]
summary(temp_pop_num$pop_num_RCP4.5G)
summary(temp_pop_num$pop_num_RCP4.5H)
summary(temp_pop_num$pop_num_RCP8.5H)

# !! For Precip fields (mm)  !!--------------------------
precip_cols <- grep("precip_", names(varData), value = TRUE) # pull out kgsqkm fields
temp_precip = varData[,c('COMID',precip_cols)]

# 6. Accumulate ---------------------------------------------------------

# only run this line if network already saved, but not in memory yet:

net <- sr_read_network("networks/nhdplus_mr_conus")
# 
# tic()
# bfi_acc <- sr_accumulate(net, bfi)
# toc()

dim(varData)

summary(varData$Pct_crop_pasture_land_RCP4.5G)
summary(varData$Pct_forest_RCP4.5G)
summary(varData$Pct_crop_pasture_land_RCP8.5G)
summary(varData$Pct_forest_RCP8.5G)
summary(varData$Pct_crop_pasture_land_RCP4.5H)
summary(varData$Pct_forest_RCP4.5H)
summary(varData$Pct_crop_pasture_land_RCP8.5H)
summary(varData$Pct_forest_RCP8.5H)

tic()
#futureN_acc <- sr_accumulate(net, varData)
toc()

tic()
future_kg_acc <- sr_accumulate(net, temp_kg)
toc()

tic()
future_kg_dep_acc <- sr_accumulate(net, temp_kg_dep)
toc()

tic()
future_area_acc <- sr_accumulate(net, temp_area)
toc()

tic()
future_pop_acc <- sr_accumulate(net, temp_pop_num)
toc()

tic()
future_precip_acc <- sr_accumulate(net, temp_precip)
toc()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# POST PROCESSING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# !! For kgsqkm fields !! --------------------------
future_NNI = merge(future_kg_acc,varData[,c('COMID','CatAreaSqKm','WsAreaSqKm')],by='COMID')

kg_cols <- grep("kg", names(future_NNI), value = TRUE) # pull out fields
ws_kg_cols = grep("ws_", kg_cols, value = TRUE)
#area_col <- "WsAreaSqKm"  # replace with the actual column name
kgsqkm_cols <- gsub("kg", "kgsqkm", ws_kg_cols)

for (i in seq_along(kgsqkm_cols)) {
  future_NNI[[kgsqkm_cols[i]]] <-
    future_NNI[[ws_kg_cols[i]]] /  future_NNI[["WsAreaSqKm"]]
}
options(scipen = 999)

# Pull out just the "kg" fields
future_NNI2 = future_NNI[,c('COMID','n_up',kgsqkm_cols)]
summary(future_NNI2$ws_n_surp_kgsqkm_RCP4.5G)
summary(varData$n_surp_kgsqkm_RCP4.5G)


# !! For Dep fields !! --------------------------
future_dep = merge(future_kg_dep_acc,varData[,c('COMID','CatAreaSqKm','WsAreaSqKm')],by='COMID')
kg_cols <- grep("kg", names(future_dep), value = TRUE) # pull out kgsqkm fields
ws_kg_cols = grep("ws_", kg_cols, value = TRUE)
kgsqkm_cols <- gsub("kg", "kgsqkm", ws_kg_cols)

for (i in seq_along(kgsqkm_cols)) {
  future_dep[[kgsqkm_cols[i]]] <-
    future_dep[[kg_cols[i]]] / future_dep[["WsAreaSqKm"]]
}
options(scipen = 999)

# Pull out just the "kg" fields
future_dep2 = future_dep[,c('COMID','n_up',kgsqkm_cols)]
summary(future_dep2$ws_totalN_2050ctl_kgsqkm)
summary(varData$totalN_2050ctl_kgNha)


# !! For Pct fields !! --------------------------
future_Pct = merge(future_area_acc,varData[,c('COMID','CatAreaSqKm','WsAreaSqKm')],by='COMID')
area_cols <- grep("area", names(future_area_acc), value = TRUE) # pull out kgsqkm fields
ws_area_cols = grep("ws_", area_cols, value = TRUE)
Pct_cols <- gsub("area_sqkm", "Pct", ws_area_cols)

for (i in seq_along(Pct_cols)) {
  future_Pct[[Pct_cols[i]]] <-
    100*future_Pct[[area_cols[i]]] / future_Pct[["WsAreaSqKm"]]
}

# Pull out just the "kg" fields
future_Pct2 = future_Pct[,c('COMID','n_up',Pct_cols)]
summary(future_Pct2$ws_Pct_forest_RCP4.5G)
summary(future_Pct2$ws_Pct_crop_pasture_land_RCP8.5H)
summary(future_Pct2$ws_Pct_impervious_2050_RCP4.5GISS)
summary(varData$Pct_impervious_2050_RCP4.5GISS)

# !! For Pop fields !! --------------------------
future_Pop = merge(future_pop_acc,varData[,c('COMID','CatAreaSqKm','WsAreaSqKm')],by='COMID')
ws_pop_cols <- grep("ws_pop", names(future_pop_acc), value = TRUE) # pull out kgsqkm fields
Pop_den_cols <- gsub("pop_num", "pop_den", ws_pop_cols)

for (i in seq_along(Pop_den_cols)) {
  future_Pop[[Pop_den_cols[i]]] <-
    future_Pop[[ws_pop_cols[i]]] / future_Pop[["WsAreaSqKm"]]
}

# Pull out just the "kg" fields
future_Pop2 = future_Pop[,c('COMID',Pop_den_cols)]
summary(future_Pop2$ws_pop_den_RCP4.5G)
summary(varData$pop_den_RCP4.5G)

# !! For Precip fields !! --------------------------
future_precip = merge(future_precip_acc,varData[,c('COMID','CatAreaSqKm','WsAreaSqKm')],by='COMID')
ws_precip_cols <- grep("ws_precip", names(future_precip_acc), value = TRUE) # pull out kgsqkm fields


# Pull out just the "kg" fields
future_precip2 = future_precip[,c('COMID','n_up',ws_precip_cols)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVING 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(future_NNI2, file = paste0(future_dir2,'Ws_Accum_Future_NNI_Vars.csv'), row.names=F)
write.csv(future_dep2, file = paste0(future_dir2,'Ws_Accum_Future_N_Dep.csv'), row.names=F)
write.csv(future_Pct2, file = paste0(future_dir2,'Ws_Accum_Future_Land_Use_Pct.csv'), row.names=F)
write.csv(future_Pop2, file = paste0(future_dir2,'Ws_Accum_Future_Pop_Dens.csv'), row.names=F)
write.csv(future_precip2, file = paste0(future_dir2,'Ws_Accum_Future_Precip.csv'), row.names=F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# The above doesn't work for precentage variables - trying with them separately.
pct_cols <- c(
  "Pct_crop_pasture_land_RCP4.5G","Pct_forest_RCP4.5G",
  'Pct_crop_pasture_land_RCP8.5G','Pct_forest_RCP8.5G',
  'Pct_crop_pasture_land_RCP4.5H','Pct_forest_RCP4.5H',
  'Pct_crop_pasture_land_RCP8.5H','Pct_forest_RCP8.5H'
)

futureN_acc2 <- sr_accumulate(
  net,
  varData,
  metric_cols = pct_cols
)
summary(futureN_acc2$cat_Pct_crop_pasture_land_RCP4.5G)
summary(futureN_acc2$ws_Pct_crop_pasture_land_RCP4.5G)

summary(futureN_acc$cat_temp_2050_degC_RCP4.5GISS)
summary(futureN_acc$ws_temp_2050_degC_RCP4.5GISS)

summary(futureN_acc$cat_n_farm_fert_kgsqkm_RCP4.5G)
summary(futureN_acc$ws_n_farm_fert_kgsqkm_RCP4.5G)

summary(futureN_acc$cat_precip_2050_mm_RCP4.5GISS)
summary(futureN_acc$ws_precip_2050_mm_RCP4.5GISS)


dim(futureN_acc)

# categorical as long table
tic()
nlcd_acc  <- sr_accumulate(net, nlcd, by = "VALUE", value_col = "area")
toc()

# categorical as wide table
tic()
nlcd_acc  <- sr_accumulate(net, nlcd, by = "VALUE", value_col = "area", wide = TRUE)
toc()

