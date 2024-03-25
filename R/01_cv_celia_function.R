
#' Title : scv_function
#' 
#' This function create a k fold spatial cross validation.
#'
#' @param dats a dataframe that is to be split into k folds
#' @param n.folds a numeric corresponding to the number of k fold of the cross validation
#'
#' @return a list with k element. Each element of the list is a fold of the cross validation. In each element, you have two dataframes, one is for model training (named "fitting")
#' the second is for model testing (named "validation")
#' @export
#'
#' @examples



# ajouter conditions : toutes les catégories des var qualitatives représentées dans fitting ; faire une scv en supprimant le site de fitting sauf si présence d'une espèce rare (sinon perte d'info)


#don't forget to set up i=1
# dats = biodivmed_occ    # row.names, lat, long, species occurences
# n.folds = 20

scv_function <- function(dats, 
                         n.folds){
  
  # flexible object for storing folds
  folds <- list()
  
  positive_indices <- which.min(unlist(lapply(dats[,species_name], function(col) length(which(col > 0))))) + 4
  
  fold.size <- nrow(dats)/n.folds
  fold.size.pos <- nrow(dats[which(dats[,positive_indices] > 0),])/n.folds
  fold.size.zero <- nrow(dats[which(dats[,positive_indices] == 0),])/n.folds
  
  # all obs are in
  remain.pos <- which(dats[,positive_indices] > 0)
  remain.zero <- which(dats[,positive_indices] == 0)
  
  for(i in 1:n.folds) {
    
    # randomly sample “fold_size” from the ‘remaining observations’
    select.pos <- sample(remain.pos, fold.size.pos, replace = FALSE)
    select.zero <- sample(remain.zero, fold.size.zero, replace = FALSE)
    
    # store indices
    folds[[i]] <- c(select.pos, select.zero)
    
    if (i == n.folds){
      
      folds[[i]] <- c(remain.pos, remain.zero)
      
    }
    
    # update remaining indices to reflect what was taken out
    remain.pos <- setdiff(remain.pos, select.pos)
    remain.zero <- setdiff(remain.zero, select.zero)
    
  }
  
  train_test <- list()
  
  for(i in 1:n.folds) {
    
    # fold i
    # unpack into a vector
    indis <- folds[[i]]
    
    # split into train and test sets
    train <- dats[-indis,]
    test <- dats[indis,]
    
    train_test[[i]] <- list(train, test)
    names(train_test[[i]]) <- c("fitting", "validation")
    
    # delete from the fitting set, sites that are already in the validation set
    train_test[[i]]$fitting <- train_test[[i]]$fitting |>
      dplyr::filter(!Row.names %in% train_test[[i]]$validation$Row.names)
    
    # Create all combinations of sites between fitting and validation sets
    combinations <- expand.grid(
      fitting_row.names = train_test[[i]]$fitting$Row.names,
      validation_row.names = train_test[[i]]$validation$Row.names
    )

    # Merge the combinations with the fitting and validation datasets
    combined_data <- dplyr::left_join(combinations, train_test[[i]]$fitting, by = c("fitting_row.names" = "Row.names")) |>
      dplyr::left_join(train_test[[i]]$validation, by = c("validation_row.names" = "Row.names"))

    # Select relevant columns (row.names and coordinates)
    combined_data_2 <- combined_data |>
      dplyr::select(fitting_row.names, latitude_start_DD.x, longitude_start_DD.x, validation_row.names, latitude_start_DD.y, longitude_start_DD.y) |>
      dplyr::rename(
        fitting_row.names = fitting_row.names,
        latitude_start_DD_fitting = latitude_start_DD.x,
        longitude_start_DD_fitting = longitude_start_DD.x,
        validation_row.names =validation_row.names,
        latitude_start_DD_validation = latitude_start_DD.y,
        longitude_start_DD_validation = longitude_start_DD.y
      )

    # Calculate distances using geosphere::distVincentySphere
    distances_km <- geosphere::distVincentySphere(
      cbind(combined_data_2$longitude_start_DD_fitting, combined_data_2$latitude_start_DD_fitting),
      cbind(combined_data_2$longitude_start_DD_validation, combined_data_2$latitude_start_DD_validation)
    ) / 1000

    combined_data_2$distances_km <- distances_km

    # Identify sites less than 10 km away in the fitting dataset
    fitting_sites <- combined_data_2 |>
      dplyr::filter(!is.na(distances_km), distances_km <= 5) |>
      dplyr::distinct(fitting_row.names)

    # Keep sites more than 10 km away in the fitting dataset
    filtered_data <- combined_data_2 |>
      dplyr::filter(!is.na(distances_km), distances_km > 5)

    # Delete from the fitting set, sites that are less than 10 km away from the validation site
    train_test[[i]]$fitting <- train_test[[i]]$fitting |>
      dplyr::anti_join(fitting_sites, by = c("Row.names" = "fitting_row.names"))

  }
  
  return(train_test)
}


#' Title : cv_function
#' 
#' This function create a k fold cross validation.
#'
#' @param dats a dataframe that is to be split into k folds
#' @param n.folds a numeric corresponding to the number of k fold of the cross validation
#'
#' @return a list with k element. Each element of the list is a fold of the cross validation. In each element, you have two dataframes, one is for model training (named "fitting")
#' the second is for model testing (named "validation")
#' @export
#'
#' @examples

#don't forget to set up i=1
dats = biodivmed_occ 
n.folds = 20

cv_function <- function(dats, 
                         n.folds){
  
  # flexible object for storing folds
  folds <- list()
  
  positive_indices <- which.min(unlist(lapply(dats[,species_name], function(col) length(which(col > 0))))) + 4
  
  fold.size <- nrow(dats)/n.folds
  fold.size.pos <- nrow(dats[which(dats[,positive_indices] > 0),])/n.folds
  fold.size.zero <- nrow(dats[which(dats[,positive_indices] == 0),])/n.folds
  
  # all obs are in
  remain.pos <- which(dats[,positive_indices] > 0)
  remain.zero <- which(dats[,positive_indices] == 0)
  
  for(i in 1:n.folds) {
    
    # randomly sample “fold_size” from the ‘remaining observations’
    select.pos <- sample(remain.pos, fold.size.pos, replace = FALSE)
    select.zero <- sample(remain.zero, fold.size.zero, replace = FALSE)
    
    # store indices
    folds[[i]] <- c(select.pos, select.zero)
    
    if (i == n.folds){
      
      folds[[i]] <- c(remain.pos, remain.zero)
      
    }
    
    # update remaining indices to reflect what was taken out
    remain.pos <- setdiff(remain.pos, select.pos)
    remain.zero <- setdiff(remain.zero, select.zero)
    
  }
  
  train_test <- list()
  
  for(i in 1:n.folds) {

    # fold i
    # unpack into a vector
    indis <- folds[[i]]
    
    # split into train and test sets
    train <- dats[-indis,]
    test <- dats[indis,]

    train_test[[i]] <- list(train, test)
    names(train_test[[i]]) <- c("fitting", "validation")
    
    # delete from the fitting set, sites that are already in the validation set
    train_test[[i]]$fitting <- train_test[[i]]$fitting |>
      dplyr::filter(!Row.names %in% train_test[[i]]$validation$Row.names)
    
  }
  
  return(train_test)
  
}


#' Title : bcv_function 
#' 
#' This function create a k fold cross validation with balanced fitting set.
#'
#' @param dats a dataframe that is to be split into k folds
#' @param n.folds a numeric corresponding to the number of k fold of the cross validation
#'
#' @return a list with k element. Each element of the list is a fold of the cross validation. In each element, you have two dataframes, one is for model training (named "fitting")
#' the second is for model testing (named "validation")
#' @export
#'
#' @examples

#don't forget to set up i=1
# dats = biodivmed_occ
# n.folds = 20
species_name <- colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]

bcv_function <- function(dats, 
                        n.folds){
    
    # Creating train/test sets : split our data into two subsets called train_set (80%) and test_set (20%) and avoid leakage
    train_set <- groupdata2::fold(biodivmed_occ_2, k = n.folds, cat_col = 'protection', id_col = 'Row.names')
    train_set <- train_set |> dplyr::arrange(.folds)
    
    train_test <- list()
    
    # for(i in 1:n.folds) {
    for (i in 1:n.folds){
      
      train <- train_set[train_set$.folds != i,]
      test <- train_set[train_set$.folds == i,]
      
      # Store train and test sets in the list
      train_test[[i]] <- list(train, test)
      names(train_test[[i]]) <- c("fitting", "validation")
      
      # Identify species with no occurrences in the training set
      missing_species <- colnames(train_test[[i]]$fitting[, species_name])[colSums(train_test[[i]]$fitting[, species_name] == 1) == 0]
      
      # For missing species, add a row with occurrences to 'fitting' from 'validation'
      for (species in missing_species) {
        random_row <- sample(which(train_test[[i]]$validation[, species] == 1), 1)
        train_test[[i]]$fitting <- rbind(train_test[[i]]$fitting, train_test[[i]]$validation[random_row, ])
      }
      
      # delete from the validation set, sites that are already in the validation set (the newly added one) 
      # can be a problem if more than 8sp without occurences in fitting !!!
      train_test[[i]]$validation <- train_test[[i]]$validation |>
      dplyr::filter(!Row.names %in% train_test[[i]]$fitting$Row.names)
    }
    
    return(train_test)
  }
  

  

#' Title : bscv_function 
#' 
#' This function create a k fold spatial cross validation with balanced fitting set.
#'
#' @param dats a dataframe that is to be split into k folds
#' @param n.folds a numeric corresponding to the number of k fold of the cross validation
#'
#' @return a list with k element. Each element of the list is a fold of the cross validation. In each element, you have two dataframes, one is for model training (named "fitting")
#' the second is for model testing (named "validation")
#' @export
#'
#' @examples

#don't forget to set up i=1
dats = biodivmed_occ 
#dats = med_covariates |> dplyr::select(-transect, -habitat_principal, -R, -Chondri, -mean_depth_transect, -logport)
n.folds = 20

bscv_function <- function(dats, 
                         n.folds){
  
  # Creating train/test sets : split our data into two subsets called train_set (80%) and test_set (20%) and avoid leakage
  train_set <- groupdata2::fold(biodivmed_occ, k = n.folds, cat_col = c('Confinement', 'protection'), id_col = 'Row.names')
  train_set <- train_set |> dplyr::arrange(.folds)
  
  
  train_test <- list()
  
  # for(i in 1:n.folds) {
  for (i in 1:n.folds){
    
    train <- train_set[train_set$.folds != i,]
    test <- train_set[train_set$.folds == i,]
    
    train_test[[i]] <- list(train, test)
    names(train_test[[i]]) <- c("fitting", "validation")
    
    # Identify species with no occurrences in the training set
    missing_species <- colnames(train_test[[i]]$fitting[, species_names])[colSums(train_test[[i]]$fitting[, species_names] == 1) == 0]
    
    # For missing species, add a row with occurrences to 'fitting' from 'validation'
    for (species in missing_species) {
      random_row <- sample(which(train_test[[i]]$validation[, species] == 1), 1)
      train_test[[i]]$fitting <- rbind(train_test[[i]]$fitting, train_test[[i]]$validation[random_row, ])
    }
    
    # delete from the validation set, sites that are already in the validation set (the newly added one) 
    # can be a problem if more than 8sp without occurences in fitting !!!
    train_test[[i]]$validation <- train_test[[i]]$validation |>
      dplyr::filter(!Row.names %in% train_test[[i]]$fitting$Row.names)
    
    # Create all combinations of sites between fitting and validation sets
    combinations <- expand.grid(
      fitting_row.names = train_test[[i]]$fitting$Row.names,
      validation_row.names = train_test[[i]]$validation$Row.names
    )
    
    # Merge the combinations with the fitting and validation datasets
    combined_data <- dplyr::left_join(combinations, train_test[[i]]$fitting, by = c("fitting_row.names" = "Row.names"), relationship = "many-to-many") |>
                     dplyr::left_join(train_test[[i]]$validation, by = c("validation_row.names" = "Row.names"), relationship = "many-to-many")
    
    # Select relevant columns (row.names and coordinates)
    combined_data_2 <- combined_data |>
      dplyr::select(fitting_row.names, latitude_start_DD.x, longitude_start_DD.x, validation_row.names, latitude_start_DD.y, longitude_start_DD.y) |>
      dplyr::rename(
        fitting_row.names = fitting_row.names,
        latitude_start_DD_fitting = latitude_start_DD.x,
        longitude_start_DD_fitting = longitude_start_DD.x,
        validation_row.names =validation_row.names,
        latitude_start_DD_validation = latitude_start_DD.y,
        longitude_start_DD_validation = longitude_start_DD.y
      )
    
    # Calculate distances using geosphere::distVincentySphere
    distances_km <- geosphere::distVincentySphere(
      cbind(combined_data_2$longitude_start_DD_fitting, combined_data_2$latitude_start_DD_fitting),
      cbind(combined_data_2$longitude_start_DD_validation, combined_data_2$latitude_start_DD_validation)
    ) / 1000
    
    combined_data_2$distances_km <- distances_km
    
    # Identify sites less than 10 km away in the fitting dataset
    fitting_sites <- combined_data_2 |>
      dplyr::filter(!is.na(distances_km), distances_km <= 4) |>
      dplyr::distinct(fitting_row.names)
    
    # Keep sites more than 10 km away in the fitting dataset
    filtered_data <- combined_data_2 |>
      dplyr::filter(!is.na(distances_km), distances_km > 4)
    
    # Delete from the fitting set, sites that are less than 4 km away from the validation site
    train_test[[i]]$fitting <- train_test[[i]]$fitting |>
      dplyr::anti_join(fitting_sites, by = c("Row.names" = "fitting_row.names"))
    
  }
  
  return(train_test)
}

