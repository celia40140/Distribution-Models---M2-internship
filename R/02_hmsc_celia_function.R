# code for a hmsc univariate model function without spatial :


load("data/derived_data/med_covariates_no_na.RData")
load("data/derived_data/occ_bcv_2.RData")
load("data/derived_data/biodivmed_occ_2.RData")
load("data/derived_data/med_biodiv_2.RData")

base_dir <- "outputs/occurrence_prediction/"

occurrence = occ_bcv_2
covariates = med_covariates_no_na
species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
base_dir = base_dir

# hmsc_univariate_function_cv <- function(occurrence,
#                              covariates,
#                              species_name,
#                              base_dir){
#   
#   
#   occ <- list()
#   for(i in 1:length(occurrence)) {   
#     # for all CV : create a matrix with species names as column name and occurrence in the rows
#     
#     print(paste0("cv ", i))
#     
#     # create raw occurrence object and select cross validation set i
#     raw_occurrence <- occurrence[[i]]
#     
#     species_j[[i]] <- pbmcapply::pbmclapply(1:length(species_name), function(j){
#       
#       # select occurrences from species in the fitting set
#       fitting <- raw_occurrence$fitting[,c("Row.names", species_name[j])]
#       
#       # add covariates
#       fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")
#       
#       # select occurrences from species in the validation set
#       validation <- raw_occurrence$validation[,c("Row.names", species_name[j])]
#       
#       # add covariates
#       validation <- dplyr::inner_join(validation, covariates, by = "Row.names")
#       
#       # get occurrence data
#       occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
#       occurrence_only_val <- validation[which(validation[,species_name[j]] > 0),]
#       # keep only two times more absences than observation
#       # get absence
#       n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2   # how much absences can i put in my model
#       n_subsample_val <- nrow(validation[which(validation[, species_name[j]] == 0),]) * 2   # same
#       absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]     # all the absences
#       absence_val <- validation[which(validation[, species_name[j]] == 0),]   # same
#       if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
#         absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
#       }
#       if(nrow(absence_val) > n_subsample_val) {
#         absence_val <- absence_val[sample(which(absence_val[, species_name] == 0), n_subsample_val, replace = FALSE),]
#       }
#       # combine absence and presence
#       occurrence_final <- rbind(occurrence_only, absence_fit)
#       occurrence_validation <- rbind(occurrence_only_val, absence_val)
#       names(occurrence_final)[names(occurrence_final) == species_name[j]] <- "occurrence"
#       names(occurrence_validation)[names(occurrence_validation) == species_name[j]] <- "occurrence"
# 
#       # Fit model:
#       # Create X matrix
#       X <- fitting |>
#         dplyr::select(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")]) 
# 
#       # Create Y matrix
#       Y_matrix <- fitting |>
#         dplyr::select(-c("Row.names", "latitude_start_DD", "longitude_start_DD", "protection", "habitat_div", "mean_bathy", "logland", "chloroDay" , "tempDay")) |>
#         data.matrix()
# 
#       # # add spatial residuals (create coordinates and use spatial distance matrix to draw autocorrelated residuals for each species)
#       # XYcoords <- fitting  |>
#       #   dplyr::select("latitude_start_DD", "longitude_start_DD") |>
#       #   data.matrix()
#       # colnames(XYcoords) = c("XX", "YY")
#       
#       #fit the model
#       model_fit = Hmsc::Hmsc(Y = Y_matrix, 
#                            XData = X,
#                            XFormula = ~ protection + habitat_div + mean_bathy + logland + chloroDay + tempDay, 
#                            distr = "probit") 
#       
#       model_fit = Hmsc::sampleMcmc(model_fit,
#                                    thin = 1,
#                                    samples = 1000,
#                                    transient = 500,
#                                    nChains = 2,
#                                    nParallel = 2,
#                                    verbose = 500)
#       
#       if(!any(is.na(model_fit) == TRUE)){
#         
#       validation_prediction = Hmsc::computePredictedValues(model_fit)
#       
#       # Create data frame with predictions
#               validation_predict <- data.frame(Row.names = occurrence_validation$Row.names,
#                                                validation_predict = validation_predict)
#               # Extract observed values from the validation set
#               validation_observed <- occurrence_validation[,c("Row.names", "occurrence")]
# 
#               # Rename columns for consistency
#               validation_observed <- validation_observed |>
#                 dplyr::rename(validation_observed = occurrence)
# 
#               # Inner join observed and predicted values
#               validation_obs_prd <- validation_predict |>
#                 dplyr::inner_join(validation_observed, multiple = "first")
# 
#               validation_obs_prd
# 
#             }else{
# 
#               validation_obs_prd  <- NA
# 
#             }
# 
#           }, mc.cores = parallel::detectCores() - 1)
# 
#         }
# 
#         validation_prediction <- parallel::mclapply(1:length(species_j[[1]]), function(i){
# 
#           species_i <- lapply(species_j, `[[`, i)
# 
#           species_i_bind <- do.call(rbind, species_i)
# 
#         }, mc.cores = 10)
# 
# 
#         extracted_predictions <- dplyr::tibble(species_name = species_name,
#                                                fitted_model = 'cv_HMSC',
#                                                validation_observed = lapply(validation_prediction, '[[', "validation_observed"),
#                                                validation_predict = lapply(validation_prediction, '[[', "validation_predict"))
# 
#         # save prediciton output in same file structure
# 
#         model_dir <- "cv_hmsc"
# 
#         dir.create(base_dir, recursive = T)
# 
#         save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))
# 
#         rm(list=ls())
#         gc()
# 
# }
# 
# 
#       Hmsc::evaluateModelFit(model_fit,
#                              predY = validation_prediction)
# 
# 
# 

      set_credentials <- function(given = Celia, family = Bertrand, email = celia-bertrand@laposte.net, 
                                  orcid = 000000000, protocol = ssh) {
          
          
          credentials <- as.list(match.call())[-1]
          
          if (length(credentials)) {
            
            r_prof <- "## RCompendium Credentials ----"
            
            
            ## Check remote protocol ----
            
            protocol <- credentials["protocol"]$protocol
            
            if (!is.null(protocol)) {
              
              stop_if_not_string(protocol)
              
              if (!(protocol %in% c("https", "ssh"))) {
                
                protocol <- NULL
                stop("Argument 'protocol' must one among 'https' and 'ssh'.")
              }
              
              
              if (!is.null(protocol)) {
                
                usethis_protocol <- getOption("usethis.protocol")
                
                if (!is.null(usethis_protocol)) {
                  
                  if (usethis_protocol == protocol) {
                    
                    ui_oops("Protocol is already set to {ui_value(protocol)}")
                    protocol <- NULL
                  }
                }
              }
            }
            


# code for a hmsc univariate model function with spatial :


# code for hmsc multivariate model function with spatial :
            
            
            
            