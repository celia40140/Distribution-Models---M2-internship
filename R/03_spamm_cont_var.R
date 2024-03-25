# function to fit glmm (SPAMM) and assess covariates relative importance

spamm_function_cont <- function(occurrence, 
                                covariates,
                                species_name,
                                base_dir_cont){
  # create raw occurrence object
  raw_occurrence <- occurrence
  
  # create formula with new covariate names
  covnames_new <- names(covariates)
  covnames_new <- covnames_new[-which(covnames_new %in% c("Row.names", "latitude_start_DD", "longitude_start_DD"))]
  
  fmla <- as.formula(paste0("occurrence ~ ", paste0(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")], collapse = " + ")))
  
  contribution <- pbmcapply::pbmclapply(1:length(species_name), function(j){
    # select the jth species from the fitting set
    fitting <- raw_occurrence[,c("Row.names", species_name[j])]
    
    # add covariates
    fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")
    
    fitting <- fitting |>  
      dplyr::select("Row.names", "longitude_start_DD", "latitude_start_DD", species_name[j], colnames(med_covariates_no_na)[!colnames(med_covariates_no_na) %in% c("Row.names")]) |> 
      dplyr::rename(X = longitude_start_DD,
                    Y = latitude_start_DD)
    
    # get occurrence data
    occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
    
    # keep only two times more absences than observation
    # get absence
    
    n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2
    
    absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]
    
    if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
      
      absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
      
    }
    
    # combine absence and presence
    occurrence_final <- rbind(occurrence_only, absence_fit)
    
    names(occurrence_final)[names(occurrence_final) == species_name[j]] <- "occurrence"
    
    
    #Fit the model :
    model_fit <- spaMM::fitme(fmla, data = occurrence_final,family = binomial(link = "probit"), method = "ML")
    
    # Use the package DALEX to assess covariates relative importance
    # First create an explain object (a representation of your model, depend on the structure of the algorithm used)
    explainer_spamm <- DALEX::explain(model = model_fit,
                                    data = occurrence_final[covnames_new],
                                    y = occurrence_final[,"occurrence"],
                                    label = "HLfit")
    
    # Compute a 25-permutation-based value of the RMSE for all explanatory variables
    # vip.25_gam <- DALEX::model_parts(explainer = explainer_gam, 
    #                                  observed = y, 
    #                                  predicted = explainer_gam[["model"]][["fitted.values"]],
    #                                  loss_function = DALEX::loss_cross_entropy, # Here we used the RMSE as our loss function
    #                                  B = 25, # Number of permutation
    #                                  type = "difference") #difference returns drop_loss - drop_loss_full_model
    
    vip.25_spamm <- DALEX::model_parts(explainer = explainer_spamm, 
                                     observed = occurrence_final[,"occurrence"], 
                                     predicted = explainer_spamm$y_hat,
                                     loss_function = DALEX::loss_one_minus_auc,
                                     B = 25,
                                     type = "difference")
    
    # From the model_parts function you get 25 RMSE values for each covariates. 
    # Take the mean and assess the standard-deviation of the RMSE for each covariates to assess the error of the permutation method
    vip.25_spamm <- vip.25_spamm |> 
      dplyr::group_by(variable) |> 
      dplyr::summarise(Dropout_loss = mean(dropout_loss),
                       sd_dropout_loss = sd(dropout_loss))
    
    vip.25_spamm <- vip.25_spamm |> 
      dplyr::filter(!variable %in% c("_baseline_", "_full_model_", "X", "Y"))
    
  }, mc.cores = 15)
  
  extracted_contributions <- dplyr::tibble(species_name = species_name, 
                                           fitted_model = "SPAMM", 
                                           # estimate contribution
                                           contributions_and_sd = contribution)
  
  # save contribution output in same file structure
  
  model_dir <- "spamm"
  
  dir.create(base_dir_cont)
  
  save(extracted_contributions, file = paste0(base_dir_cont, model_dir, "_extracted_contributions.RData"))
  
  rm(list=ls())
  gc()
  
}
    