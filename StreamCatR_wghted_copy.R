# For Weighted Average Accumulation Approach

library(StreamCatR)
library(arrow)
library(future.apply)
library(tictoc)
library(data.table)
library(fst)
library(dplyr)
# https://github.com/usepa/streamcatR

future_dir2 = "C:/Users/MPennino/OneDrive - Environmental Protection Agency (EPA)/Projects/OASES/Data/Future_NO3/"

# 1. Download base files ---------------------------------------------------------
# These files contain the network information necessary to construct the 
# medium-resolution NHDPlus igraph object (see step 3 below)
StreamCatR::sr_fetch_accum_data(dest_dir = "./base_files",
                                force = TRUE)


# Specify path to catchment_areas.parquet (will be weights)
areas <- file.path("base_files", "catchment_areas.parquet")

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


# Read in stored network file
net <- StreamCatR::sr_read_network("networks/nhdplus_mr_conus")

#---------------------------------------------------------------------------
# Read in catchment variable data
filename = 'Future_NO3_predictors_catchments.parquet'

varData <- read_parquet(paste0(future_dir2,filename))
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

setdiff(vars,colnames(varData)) # gives not matching

varData = varData[,vars]

varData <- varData %>% rename(
  Pct_impervious_2050_RCP4.5GISS = impervious_2050_RCP4.5GISS,
  Pct_impervious_2050_RCP8.5GISS = impervious_2050_RCP8.5GISS,
  Pct_impervious_2050_RCP4.5Hadgem = impervious_2050_RCP4.5Hadgem,
  Pct_impervious_2050_RCP8.5Hadgem = impervious_2050_RCP8.5Hadgem
)


areas = varData[,c('COMID','CatAreaSqKm')]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset out data types
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

temp_cols <- grep("temp", names(varData), value = TRUE) # pull out kgsqkm fields
data_tempC = varData[,c('COMID',temp_cols)]

kgsqkm_cols <- grep("kgsqkm", names(varData), value = TRUE) # pull out kgsqkm fields
data_kgsqkm = varData[,c('COMID','CatAreaSqKm',kgsqkm_cols)]

kgNha_cols <- grep("kgNha", names(varData), value = TRUE) # pull out kgsqkm fields
data_kgNha = varData[,c('COMID','CatAreaSqKm',kgNha_cols)]

pct_cols <- grep("Pct_", names(varData), value = TRUE) # pull out kgsqkm fields
data_Pct = varData[,c('COMID','CatAreaSqKm',pct_cols)]

pop_den_cols <- grep("pop_den", names(varData), value = TRUE) # pull out kgsqkm fields
data_pop_den = varData[,c('COMID','CatAreaSqKm',pop_den_cols)]

precip_cols <- grep("precip_", names(varData), value = TRUE) # pull out kgsqkm fields
data_precip = varData[,c('COMID',precip_cols)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Accumulate pasture with areas as weight
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tic()
precip_acc <- StreamCatR::sr_accumulate(
  net,
  data_precip,
  metric_cols = "precip_2050_mm_RCP4.5GISS",
  weight_data = areas
)
toc()

tempC_acc <- StreamCatR::sr_accumulate(
  net,
  data_tempC,
  metric_cols = "temp_2050_degC_RCP4.5GISS",
  weight_data = areas
)