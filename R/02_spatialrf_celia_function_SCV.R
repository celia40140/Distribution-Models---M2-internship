#' Title spatialrf_function
#' 
#' This function fit a spatial random forest using the R package `SpatialML` with a k fold spatial cross validation procedure
#'
#' @param occurrence a list in which each elements is a fold of the spatial cross validation procededure. Each fold is split into two subset, the first one named "fitting" to
#' train the model and the second one named "validation" to test the model
#' @param covariates a datagrame containg all covariates to fit the model
#' @param species_name a vector containg the name of all species contain in @param occurrence
#' @param base_dir the path to save the data
#'
#' @return a dataframe with as many row as the length of @param species_name . Each row is a species with its occurrence observation and prediction from each cross validation fold
#' @export
#'
#' @examples

# occurrence = occ_bcv_2
# covariates = med_covariates_no_na   # data with covariates
# species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
# base_dir = base_dir

spatialrf_function_cv <- function(occurrence,
                               covariates,
                               species_name,
                               base_dir){

  species_j <- list()

  for(i in 1:length(occurrence)) {

    print(paste0("cv ", i))

    # create raw occurrence object and select cross validation set i
    raw_occurrence <- occurrence[[i]]

fmla <<- as.formula(paste0("occurrence ~ ", paste0(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")], collapse = " + ")))

species_j[[i]] <- pbmcapply::pbmclapply(1:length(species_name), function(j){

  # select the jth species from the fitting set
  fitting <- raw_occurrence$fitting[,c("Row.names", species_name[j])]

  # add covariates
  fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")

  # select the jth species from the validation set
  validation <- raw_occurrence$validation[,c("Row.names", species_name[j])]

  # add covariates
  validation <- dplyr::inner_join(validation, covariates, by = "Row.names")

  fitting <- fitting |>
    dplyr::select("Row.names", "longitude_start_DD", "latitude_start_DD", species_name[j], colnames(med_covariates_no_na)[!colnames(med_covariates_no_na) %in% c("Row.names")]) |>
    dplyr::rename(X = longitude_start_DD,
                  Y = latitude_start_DD)

  validation <- validation |>
    dplyr::select("Row.names", "longitude_start_DD", "latitude_start_DD", species_name[j], colnames(med_covariates_no_na)[!colnames(med_covariates_no_na) %in% c("Row.names")]) |>
    dplyr::rename(X = longitude_start_DD,
                  Y = latitude_start_DD)

  # get occurrence data
  occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
  occurrence_only_val <- validation[which(validation[,species_name[j]] > 0),]

  if(nrow(occurrence_only_val) == 0){

    occurrence_only_val <- validation

  }


  # keep only two times more absences than observation
  n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2
  n_subsample_val <- nrow(validation[which(validation[, species_name[j]] > 0),]) * 2

  absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]
  absence_val <- validation[which(validation[, species_name[j]] == 0),]

  if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences

    absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]

  }

  if(nrow(absence_val) > n_subsample_val) {

    absence_val <- absence_val[sample(which(absence_val[, species_name[j]] == 0), n_subsample_val, replace = FALSE),]

  }


# combine absence and presence
occurrence_final <<- rbind(occurrence_only, absence_fit) |>
  as.data.frame()
occurrence_validation <- rbind(occurrence_only_val, absence_val) |>
  as.data.frame()

names(occurrence_final)[names(occurrence_final) == species_name[j]] <<- "occurrence"
names(occurrence_validation)[names(occurrence_validation) == species_name[j]] <- "occurrence"


coords <<- occurrence_final |>
  dplyr::select(X, Y) |>
  as.data.frame()

### FITTING MODELS
# fit the spatial random forests
# occurrence_final$occurrence <- as.factor(occurrence_final$occurrence)

model_fit <- tryCatch(SpatialML::grf(formula = fmla,
                                     dframe = occurrence_final,
                                     bw = 20,
                                     kernel = "adaptive",         # look with fixed or adaptive
                                     coords = coords,
                                     ntree = 1000,
                                     geo.weighted = FALSE), error = function(e) NA)
# model_fit$LocalModelSummary

if(!any(is.na(model_fit) == TRUE)){

  validation_predict  <- tryCatch(SpatialML::predict.grf(object = model_fit,
                                                         new.data = occurrence_validation,
                                                         x.var.name = "X" ,
                                                         y.var.name = "Y",
                                                         local.w = 0.5,
                                                         global.w = 0.5), error = function(e) NA)

  validation_predict <- data.frame(Row.names = occurrence_validation$Row.names,
                                   validation_predict = validation_predict)

  validation_observed <- occurrence_validation[,c("Row.names", "occurrence")]

  validation_observed <- validation_observed |>
    dplyr::rename(validation_observed = occurrence)

  validation_obs_prd <- validation_predict |>
    dplyr::inner_join(validation_observed, multiple = "first")

  validation_obs_prd

}else{

  validation_obs_prd  <- NA

}

return(validation_obs_prd)

rm(model_fit)
gc()

    }, mc.cores = parallel::detectCores() - 1)

  }

  validation_prediction <- parallel::mclapply(1:length(species_j[[1]]), function(i){

    species_i <- lapply(species_j, `[[`, i)

    species_i_bind <- do.call(rbind, species_i)

  }, mc.cores = 10)


  extracted_predictions <- dplyr::tibble(species_name = species_name,
                                         fitted_model = "cv_SPRF",
                                         validation_observed = lapply(validation_prediction, '[[', "validation_observed"),
                                         validation_predict = lapply(validation_prediction, '[[', "validation_predict"))

  # save prediciton output in same file structure

  model_dir <- "cv_sprf"

  dir.create(base_dir, recursive = T)

  save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))

  rm(list=ls())
  gc()

}



# # occurrence = med_biodiv_2
# # covariates = med_covariates_no_na
# # species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
# # base_dir = base_dir
# spatialrf_function <- function(occurrence,
#                         covariates,
#                         species_name,
#                         base_dir){
# 
#   # create raw occurrence object and a list
#   species_j <- list()
#   raw_occurrence <- occurrence
# 
#   # model formula
#   fmla <<- as.formula(paste0("occurrence ~ ", paste0(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")], collapse = " + ")))
# 
#   for(j in 1:length(species_name)) {
# 
#     species_j[[j]] <- pbmcapply::pbmclapply(1:length(species_name), function(j){
# 
#       # select the jth species from the fitting set
#       fitting <- raw_occurrence[,c("Row.names", species_name[j])]
# 
#       # add covariates
#       fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")
# 
#       fitting <- fitting |>
#         dplyr::select("Row.names", "longitude_start_DD", "latitude_start_DD", species_name[j], colnames(med_covariates_no_na)[!colnames(med_covariates_no_na) %in% c("Row.names")]) |>
#         dplyr::rename(X = longitude_start_DD,
#                       Y = latitude_start_DD)
# 
#       # get occurrence data
#       occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
# 
# 
#       # keep only two times more absences than observation
#       n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2   # how much absences can i put in my model
# 
#       absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]     # all the absences
# 
#       if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
# 
#         absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
# 
#       }
# 
# 
#       # combine absence and presence
#       occurrence_final <<- rbind(occurrence_only, absence_fit) |>
#         as.data.frame()
# 
#       names(occurrence_final)[names(occurrence_final) == species_name[j]] <<- "occurrence"
# 
#       coords <<- occurrence_final |>
#         dplyr::select(X, Y) |>
#         as.data.frame()
# 
#       # Fit model: (you don't need to explicitly mention that you're building a classification tree within the randomForest function)
#       #occurrence_final$occurrence <- as.numeric(occurrence_final$occurrence)   # for model calculation we need this time to have a numeric depend variable
# 
# 
#       model_fit <- tryCatch(SpatialML::grf(formula = fmla,
#                                   dframe = occurrence_final,
#                                   bw = 20,
#                                   kernel = "adaptive",         # look with fixed and adaptative
#                                   coords = coords,
#                                   ntree = 1000,
#                                   geo.weighted = FALSE), error = function(e) NA)
# 
# 
#       if(!any(is.na(model_fit) == TRUE)){
# 
#         validation_predict  <- tryCatch(SpatialML::predict.grf(object = model_fit,
#                                                       new.data = occurrence_final,    # change that with the dataset you want to test your model with
#                                                       x.var.name = "X" ,
#                                                       y.var.name = "Y",
#                                                       local.w = 0.5,
#                                                       global.w = 0.5), error = function(e) NA)
# 
#         validation_predict <- data.frame(Row.names = occurrence_final$Row.names,
#                                          validation_predict = validation_predict)
# 
#         validation_observed <- occurrence_final[,c("Row.names", "occurrence")]
# 
#         validation_observed <- validation_observed |>
#           dplyr::rename(validation_observed = occurrence)
# 
#         validation_obs_prd <- validation_predict |>
#           dplyr::inner_join(validation_observed, multiple = "first")
# 
#         validation_obs_prd
# 
#       }else{
# 
#         validation_obs_prd  <- NA
# 
#       }
#       
#       return(validation_obs_prd)
#       
#       rm(model_fit)
#       gc()
#       
# 
#     }, mc.cores = parallel::detectCores() - 1)
# 
#   }
# 
#   validation_prediction <- parallel::mclapply(1:length(species_j[[1]]), function(j){
# 
#     species_i <- lapply(species_j, `[[`, j)
# 
#     species_i_bind <- do.call(rbind, species_i)
# 
#   }, mc.cores = 10)
# 
# 
#   extracted_predictions <- dplyr::tibble(species_name = species_name,
#                                          fitted_model = "SPRF",
#                                          validation_observed = lapply(validation_prediction, '[[', "validation_observed"),
#                                          validation_predict = lapply(validation_prediction, '[[', "validation_predict"))
# 
# 
#   # save prediciton output in same file structure
# 
#   model_dir <- "sprf"
# 
#   dir.create(base_dir, recursive = T)
# 
#   save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))
# 
#   rm(list=ls())
#   gc()
# 
# }
# 
# 
# 
