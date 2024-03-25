# function to fit spatial Random Forest and assess covariates relative importance

occurrence = med_biodiv_2
covariates = med_covariates_no_na
species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
base_dir = base_dir


spatialrf_function_cont <- function(occurrence,
                                    covariates,
                                    species_name,
                                    base_dir_cont){
  
  # create formula with new covariate names
  covnames_new <- names(covariates)
  covnames_new <- covnames_new[-which(covnames_new %in% c("Row.names", "latitude_start_DD", "longitude_start_DD"))]
  
  raw_occurrence <- occurrence
  
  # model formula
  fmla <<- as.formula(paste0("occurrence ~ ", paste0(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")], collapse = " + ")))
  
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
    n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2   # how much absences can i put in my model
    
    absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]     # all the absences
    
    if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
      
      absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
      
    }
    
    
    # combine absence and presence
    occurrence_final <<- rbind(occurrence_only, absence_fit) |>
      as.data.frame()
    
    names(occurrence_final)[names(occurrence_final) == species_name[j]] <<- "occurrence"
    
    coords <<- occurrence_final |>
      dplyr::select(X, Y) |>
      as.data.frame()
    
    # Fit model: (you don't need to explicitly mention that you're building a classification tree within the randomForest function)
    #occurrence_final$occurrence <- as.numeric(occurrence_final$occurrence)   # for model calculation we need this time to have a numeric depend variable
    
    
    model_fit <- tryCatch(SpatialML::grf(formula = fmla,
                                         dframe = occurrence_final,
                                         bw = 20,
                                         kernel = "adaptive",         # look with fixed and adaptative
                                         coords = coords,
                                         ntree = 1000,
                                         geo.weighted = FALSE), error = function(e) NA)
    
    
    # Use the package DALEX to assess covariates relative importance
    # First create an explain object (a representation of your model, depend on the structure of the algorithm used)
    explainer_sprf <- DALEX::explain(model = model_fit[[1]],
                                   data = occurrence_final[covnames_new],
                                   y = occurrence_final[,"occurrence"],
                                   label = "ranger",
                                   type = "classification")
    
    # Compute a 25-permutation-based value of the RMSE for all explanatory variables
    vip.25_sprf <- DALEX::model_parts(explainer = explainer_sprf, 
                                    # observed = occurrence_final[,"occurrence"], 
                                    # predicted = explainer_sprf[["model"]][["predicted"]],
                                    loss_function = DALEX::loss_one_minus_auc,
                                    B = 25,
                                    type = "difference")
    
    # From the model_parts function you get 25 RMSE values for each covariates. 
    # Take the mean and assess the standard-deviation of the RMSE for each covariates to assess the error of the permutation method
    vip.25_sprf <- vip.25_sprf |> 
      dplyr::group_by(variable) |> 
      dplyr::summarise(Dropout_loss = mean(dropout_loss),
                       sd_dropout_loss = sd(dropout_loss))
    
    vip.25_sprf <- vip.25_sprf |> 
      dplyr::filter(!variable %in% c("_baseline_", "_full_model_"))
    
  }, mc.cores = 15)
  
  extracted_contributions <- dplyr::tibble(species_name = species_name, 
                                           fitted_model = "SPRF", 
                                           # estimate contribution
                                           contributions_and_sd = contribution)
  
  # save contribution output in same file structure
  
  model_dir <- "sprf"
  
  dir.create(base_dir_cont)
  
  save(extracted_contributions, file = paste0(base_dir_cont, model_dir, "_extracted_contributions.RData"))
  
  rm(list=ls())
  gc()
  
}
