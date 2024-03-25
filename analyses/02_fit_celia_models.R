# This script run the six biomass prediction models (glm, gam, rf, sprf, spamm and brt)

source("R/02_glm_celia_function_SCV.R")
source("R/02_gam_celia_function_SCV.R")
source("R/02_rf_celia_function_SCV.R")
source("R/02_spatialrf_celia_function_SCV.R")
source("R/02_spamm_celia_function_CV.R")
source("R/02_brt_celia_function_SCV.R")

# load fish biomass data and covariates
load("data/derived_data/med_covariates_no_na.RData")
load("data/derived_data/occ_bcv_2.RData")
load("data/derived_data/biodivmed_occ_2.RData")
load("data/derived_data/med_biodiv_2.RData")

base_dir <- "outputs/occurrence_prediction/"



# run glm without or with cross validation process depending on the function
print("glm biomass prediction")
glm_function(occurrence = med_biodiv_2,
             covariates = med_covariates_no_na,
             species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
             base_dir = base_dir)
# glm_function_cv(occurrence = occ_bcv_2,
#              covariates = med_covariates_no_na,
#              species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
#              base_dir = base_dir)




# run gam
print("gam biomass prediction")
gam_function(occurrence = med_biodiv_2,
             covariates = med_covariates_no_na,
             species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
             base_dir = base_dir)
# gam_function_cv(occurrence = occ_bcv_2,
#                 covariates = med_covariates_no_na,
#                 species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
#                 base_dir = base_dir)



# run random forest
print("rf biomass prediction")
# rf_function_cv(occurrence = occ_bcv_2,
#             covariates = med_covariates_no_na,
#             species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
#             base_dir = base_dir)
rf_function(occurrence = med_biodiv_2,
            covariates = med_covariates_no_na,
            species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
            base_dir = base_dir)



# run spatial random forest
print("sprf biomass prediction")
# spatialrf_function_cv(occurrence = occ_bcv_2,
#                    covariates = med_covariates_no_na,
#                    species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
#                    base_dir = base_dir)

spatialrf_function(occurrence = med_biodiv_2,
                   covariates = med_covariates_no_na,
                   species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                   base_dir = base_dir)



# run spamm (GLMM)
print("spamm biomass prediction")
# spamm_function_cv(occurrence = occ_bcv_2,
#                covariates = med_covariates_no_na,
#                species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
#                base_dir = base_dir)

spamm_function(occurrence = med_biodiv_2,
                  covariates = med_covariates_no_na,
                  species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                  base_dir = base_dir)

# run boosted regression trees
print("brt biomass prediction")
# brt_function_cv(occurrence = occ_bcv_2,
#              covariates = med_covariates_no_na,
#              species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
#              n.cores = 1,
#              base_dir = base_dir)

brt_function(occurrence = med_biodiv_2,
             covariates = med_covariates_no_na,
             species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
             n.cores = 1,
             base_dir = base_dir)
