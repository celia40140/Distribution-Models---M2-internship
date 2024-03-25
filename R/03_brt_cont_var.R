# function to fit boosted regression tree and assess covariates relative importance

occurrence = med_biodiv_2
covariates = med_covariates_no_na
species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
n.cores = 1
base_dir = base_dir


brt_function_cont <- function(occurrence,
                            covariates,
                            species_name,
                            n.cores,
                            base_dir_cont){
  
  # create raw occurrence object
  raw_occurrence <- occurrence
  
  # create formula with new covariate names
  covnames_new <- names(covariates)
  covnames_new <- covnames_new[-which(covnames_new %in% c("Row.names", "latitude_start_DD", "longitude_start_DD"))]
  
  # create formula with new covariate names
  brt_formula <- as.formula(paste0("occurrence ~ ", paste0(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")], collapse = " + ")))
  
  contribution <- pbmcapply::pbmclapply(1:length(species_name), function(j){
    
    # select the jth species from the fitting set
    fitting <- raw_occurrence[,c("Row.names", species_name[j])]
    
    # add covariates
    fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")
    
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
    model_fit <- tryCatch(gbm::gbm(formula = brt_formula,
                                   data = occurrence_final,
                                   distribution = "adaboost",
                                   n.trees = 10000,
                                   interaction.depth = 3,
                                   shrinkage = 0.001,
                                   bag.fraction = 0.8,
                                   cv.folds = 10,
                                   n.cores = n.cores), error = function(e) NA)
    
    # selecting the best number of trees from cross validations
    gbm.mod.perf <- gbm::gbm.perf(model_fit, method = "cv", plot.it = F)
    
    # fit model to all data
    model_fit <- gbm::gbm(formula = brt_formula,
                          data = occurrence_final,
                          distribution = "adaboost",
                          n.trees = gbm.mod.perf,
                          bag.fraction = 0.8,
                          interaction.depth = 3,
                          shrinkage = 0.001)
    
    # problem with this function is that fit contain the fitted values on the scale of regression function (e.g. log-odds scale for bernoulli).
    # we need to transform that first ?
    
    # Use the package DALEX to assess covariates relative importance
    # First create an explain object (a representation of your model, depend on the structure of the algorithm used)
    explainer_gbm <- DALEX::explain(model = model_fit,
                                      data = occurrence_final[covnames_new],
                                      y = occurrence_final[,"occurrence"],
                                      label = "gbm")
    
    # Compute a 25-permutation-based value of the RMSE for all explanatory variables
    vip.25_gbm <- DALEX::model_parts(explainer = explainer_gbm, 
                                       observed = occurrence_final[,"occurrence"], 
                                       predicted = explainer_gbm$y_hat,
                                       loss_function = DALEX::loss_one_minus_auc,
                                       B = 25,
                                       type = "difference")
    
    
    # From the model_parts function you get 25 RMSE values for each covariates. 
    # Take the mean and assess the standard-deviation of the RMSE for each covariates to assess the error of the permutation method
    vip.25_gbm <- vip.25_gbm |> 
      dplyr::group_by(variable) |> 
      dplyr::summarise(Dropout_loss = mean(dropout_loss),
                       sd_dropout_loss = sd(dropout_loss))
    
    vip.25_gbm <- vip.25_gbm |> 
      dplyr::filter(!variable %in% c("_baseline_", "_full_model_"))
    
  }, mc.cores = 15)
  
  extracted_contributions <- dplyr::tibble(species_name = species_name, 
                                           fitted_model = "GBM", 
                                           # estimate contribution
                                           contributions_and_sd = contribution)
  
  # save contribution output in same file structure
  
  model_dir <- "gbm"
  
  dir.create(base_dir_cont)
  
  save(extracted_contributions, file = paste0(base_dir_cont, model_dir, "_extracted_contributions.RData"))
  
  rm(list=ls())
  gc()
  
} # end of function

    