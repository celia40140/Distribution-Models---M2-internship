# This script run the six occurrence contribution models (glm, gam, rf, sprf, spamm and brt)

source("R/03_glm_cont_var.R")
source("R/03_rf_cont_var.R")
source("R/03_gam_cont_var.R")
source("R/03_sprf_cont_var.R")
source("R/03_spamm_cont_var.R")
source("R/03_brt_cont_var.R")


# load fish occurrence data and covariates
load("data/derived_data/med_biodiv_2.RData")
load("data/derived_data/med_covariates_no_na.RData")
load("data/derived_data/occ_bcv_2.RData")
load("data/derived_data/biodivmed_occ_2.RData")

base_dir <- "outputs/occurrence_contribution/"

# run glm for covariates contribution
print("glm occurrence contribution")

glm_function_cont(occurrence = med_biodiv_2,
                  covariates = med_covariates_no_na,
                  species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                  base_dir_cont = base_dir)

# run random Forest for covariates contribution
print("rf occurrence contribution")
rf_function_cont(occurrence = med_biodiv_2,
                 covariates = med_covariates_no_na,
                 species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                 base_dir_cont = base_dir)

# run spatial Random Forest for covariates contribution
print("sprf occurrence contribution")
spatialrf_function_cont(occurrence = med_biodiv_2,
                        covariates = med_covariates_no_na,
                        species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                        base_dir_cont = base_dir)

# run gam for covariates contribution
print("gam occurrence contribution")
gam_function_cont(occurrence = med_biodiv_2,
                  covariates = med_covariates_no_na,
                  species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                  base_dir_cont = base_dir)

# run spamm for covariates contribution
print("spamm occurrence contribution")
spamm_function_cont(occurrence = med_biodiv_2,
                    covariates = med_covariates_no_na,
                    species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                    base_dir_cont = base_dir)

# run gbm for covariates contribution
print("gbm occurrence contribution")
brt_function_cont(occurrence = med_biodiv_2,
                  covariates = med_covariates_no_na,
                  species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")],
                  n.cores = 1,
                  base_dir_cont = base_dir)


