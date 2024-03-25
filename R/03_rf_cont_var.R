# function to fit Random Forest and assess covariates relative importance


occurrence = med_biodiv_2
covariates = med_covariates_no_na
species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
base_dir = base_dir

rf_function_cont <- function(occurrence, 
                             covariates, 
                             species_name, 
                             base_dir_cont){
  
  # create raw occurrence object 
  raw_occurrence <- occurrence
  
  # create formula with new covariate names
  covnames_new <- names(covariates)
  covnames_new <- covnames_new[-which(covnames_new %in% c("Row.names", "latitude_start_DD", "longitude_start_DD"))]
  
  contribution <- pbmcapply::pbmclapply(1:length(species_name), function(j){
    
    # select the jth species from the fitting set
    fitting <- raw_occurrence[,c("Row.names", species_name[j])]
    
    # add covariates
    fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")
    
    # get occurrence data
    occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
    
    
    # keep only two times more absences than observation
    n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2   # how much absences can i put in my model
    
    absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]     # all the absences
    
    if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
      
      absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
      
    }
    
    
    # combine absence and presence
    occurrence_final <- rbind(occurrence_only, absence_fit)
    
    names(occurrence_final)[names(occurrence_final) == species_name[j]] <- "occurrence"
    
    # Fit model: (you don't need to explicitly mention that you're building a classification tree within the randomForest function)
    # but occurrence_final$occurrence must be in factor for a classification
    
    # occurrence_final$occurrence <- as.factor(occurrence_final$occurrence)
    
    model_fit <- tryCatch(randomForest::randomForest(x = occurrence_final[covnames_new],
                                                     y = occurrence_final$occurrence,
                                                     ntree = 1000,
                                                     importance = FALSE), error = function(e) NA)
    
    
    # Use the package DALEX to assess covariates relative importance
    # First create an explain object (a representation of your model, depend on the structure of the algorithm used)
    explainer_rf <- DALEX::explain(model = model_fit,
                                    data = occurrence_final[covnames_new],
                                    y = occurrence_final[,"occurrence"],
                                    label = "randomForest",
                                   type = "classification")
                                 

        # Compute a 25-permutation-based value of the RMSE for all explanatory variables
    vip.25_rf <- DALEX::model_parts(explainer = explainer_rf, 
                                     observed = occurrence_final[,"occurrence"], 
                                     predicted = explainer_rf[["model"]][["predicted"]],
                                     loss_function = DALEX::loss_one_minus_auc,
                                     B = 25,
                                     type = "difference")
    
    # From the model_parts function you get 25 RMSE values for each covariates. 
    # Take the mean and assess the standard-deviation of the RMSE for each covariates to assess the error of the permutation method
    vip.25_rf <- vip.25_rf |> 
      dplyr::group_by(variable) |> 
      dplyr::summarise(Dropout_loss = mean(dropout_loss),
                       sd_dropout_loss = sd(dropout_loss))
    
    vip.25_rf <- vip.25_rf |> 
      dplyr::filter(!variable %in% c("_baseline_", "_full_model_"))
    
  }, mc.cores = 15)
  
  extracted_contributions <- dplyr::tibble(species_name = species_name, 
                                           fitted_model = "RF", 
                                           # estimate contribution
                                           contributions_and_sd = contribution)
  
  # save contribution output in same file structure
  
  model_dir <- "rf"
  
  dir.create(base_dir_cont)
  
  save(extracted_contributions, file = paste0(base_dir_cont, model_dir, "_extracted_contributions.RData"))
  
  rm(list=ls())
  gc()
  
}
    