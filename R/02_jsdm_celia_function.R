# code for a jsdm function : community 

load("data/derived_data/med_covariates_no_na.RData")
load("data/derived_data/occ_bcv_2.RData")
load("data/derived_data/biodivmed_occ_2.RData")
load("data/derived_data/med_biodiv_2.RData")

base_dir <- "outputs/occurrence_prediction/"

occurrence = occ_bcv_2
covariates = med_covariates_no_na
species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
base_dir = base_dir

jsdm_function_cv <- function(occurrence,
                             covariates,
                             species_name,
                             base_dir){

   
  occ <- list()
  for(i in 1:length(occurrence)) {   
    # for all CV : create a matrix with species names as column name and occurrence in the rows
    
    print(paste0("cv ", i))
    
    # create raw occurrence object and select cross validation set i
    raw_occurrence <- occurrence[[i]]
    
    occ[[i]] <- pbmcapply::pbmclapply(1:length(species_name), function(j){
      
      # select occurrences from species in the fitting set
      fitting <- raw_occurrence$fitting[,c("Row.names", species_name)]
      
      # add covariates
      fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")
      
      # select occurrences from species in the validation set
      validation <- raw_occurrence$validation[,c("Row.names", species_name)]
      
      # add covariates
      validation <- dplyr::inner_join(validation, covariates, by = "Row.names")
      
      # # get occurrence data
      # occurrence_only <- fitting[which(fitting[,species_name] > 0),]
      # occurrence_only_val <- validation[which(validation[,species_name] > 0),]
      # # keep only two times more absences than observation
      # # get absence
      # n_subsample_fit <- nrow(fitting[which(fitting[, species_name] > 0),]) * 2   # how much absences can i put in my model
      # n_subsample_val <- nrow(validation[which(validation[, species_name] == 0),]) * 2   # same
      # absence_fit <- fitting[which(fitting[, species_name] == 0),]     # all the absences
      # absence_val <- validation[which(validation[, species_name] == 0),]   # same
      # if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
      #   absence_fit <- absence_fit[sample(which(absence_fit[, species_name] == 0), n_subsample_fit, replace = FALSE),]
      # }
      # if(nrow(absence_val) > n_subsample_val) {
      #   absence_val <- absence_val[sample(which(absence_val[, species_name] == 0), n_subsample_val, replace = FALSE),]
      # }
      # # combine absence and presence
      # occurrence_final <- rbind(occurrence_only, absence_fit)
      # occurrence_validation <- rbind(occurrence_only_val, absence_val)
      # names(occurrence_final)[names(occurrence_final) == species_name] <- "occurrence"
      # names(occurrence_validation)[names(occurrence_validation) == species_name] <- "occurrence"
    
      
      
      # Fit model:
      # Create X matrix
      X_matrix <- fitting |>
        dplyr::select(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")]) |>
        data.matrix()
      
      # Create Y matrix
      Y_matrix <- fitting |>
        dplyr::select(-c("Row.names", "latitude_start_DD", "longitude_start_DD", "protection", "habitat_div", "mean_bathy", "logland", "chloroDay" , "tempDay")) |>
        data.matrix()
 
      # add spatial residuals (create coordinates and use spatial distance matrix to draw autocorrelated residuals for each species)
      XYcoords <- fitting  |>
        dplyr::select("latitude_start_DD", "longitude_start_DD") |>
        data.matrix()
      colnames(XYcoords) = c("XX", "YY")
      
      #fit the model
      model = sjSDM::sjSDM(Y = Y_matrix, 
                    env = sjSDM::linear(data = X_matrix, formula = ~ protection + habitat_div + mean_bathy + logland + chloroDay + tempDay), #voir si on ajoute des termes interactions
                    spatial = sjSDM::linear(data = XYcoords, ~0+XX+YY+XX:YY), 
                    family = binomial("probit"),
                    iter = 1L) # bug avec se
      
      #ANOVA (Type II) will separate the three components (environment, associations, and space):
      an = anova(model)
      
      #VENN diagram 
      plot(an)
      
      # Internal metacommunity structure
      
      results = sjSDM::plotInternalStructure(an)
      print(results$data$Species)
      
      
      # Importance
      plot(anova(model), internal = TRUE)
      
      
    
    }
  }
}
    
  
      
      
  
      
      
      
