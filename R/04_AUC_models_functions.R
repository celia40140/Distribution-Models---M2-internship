#' Title AUC_models_functions
#' 
#' This function is for models evaluations with AUC (ROC curve).
#' We want to calculate AUC value for all species using validation set for models with cross validation, or using an independant dataset (another year ?). 
#' AUC per model are calculated with the mean AUC of all species. 
#' Finally, we compare the mean AUC of all models with or without cross validation step.


load("data/derived_data/med_covariates_no_na.RData")
load("data/derived_data/occ_bcv_2.RData")
load("data/derived_data/biodivmed_occ_2.RData")
load("data/derived_data/med_biodiv_2.RData")

species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
base_dir = "outputs/auc/"

# load files 
output_files <- list.files("outputs/occurrence_prediction", full.names = T)

load_outputs <- lapply(1:length(output_files), function(i) {
  
  load(output_files[i])
  assign(unique(extracted_predictions$fitted_model), extracted_predictions)
  
})

          # Create an empty list to store results for each model
          model_i <- list()
          
          # Iterate over each model output
          for(i  in 1:length(load_outputs)) {
            
            # Extract the current model output
            model_output <- load_outputs[[i]]
            
            # Create an empty list to store results for each species
            all_species <- list()
            
            # Iterate over each species
            for(j in 1:length(species_name)) {
              
              # Extract observed and predicted values for the current species
              validation_observed <- unlist(model_output[j, "validation_observed"])
              validation_predict <- unlist(model_output[j, "validation_predict"])
              
              # Create a data frame for the current species
              species_data <- data.frame(
                species_name = rep(species_name[j], length(validation_observed)),
                fitted_model = rep(unique(model_output$fitted_model), length(validation_observed)),
                validation_observed = validation_observed,
                validation_predict = validation_predict,
                row.names = NULL  # Suppress row names
              )
              
              # Compute AUC and CI for the current species
              roc_obj <- pROC::roc(species_data$validation_observed, species_data$validation_predict)
              auc_value <- pROC::auc(roc_obj)
              ci_value <- pROC::ci.auc(roc_obj)
              
              # Store results for the current species
              species_result <- list(
                species_name = unique(species_name[j]),
                fitted_model = unique(model_output$fitted_model),
                auc = auc_value,
                ci = ci_value
              )
              
              # Append results for the current species to the list
              all_species[[j]] <- species_result
            }

            # Compute mean AUC for all species within the current model
            model_auc <- sapply(all_species, function(species) species$auc)
            mean_auc <- mean(model_auc, na.rm = TRUE)
            model_name <- unique(model_output$fitted_model)
            
            # Store results for the current model
            model_i[[i]] <- list(
              all_species = all_species,
              mean_auc = mean_auc,
              model_name = model_name
            )
            }

        library(ggplot2)
        library(dplyr)
          
        # Create an empty list to store combined data for all models
        combined_data <- list()
        
        # Iterate over each model
        for (i in 1:length(model_i)) {
          # Create an empty list to store combined data for the current model
          model_data <- list()
          
          # Iterate over each species within the current model
          for (j in 1:length(model_i[[i]]$all_species)) {
            # Extract data for the current species
            species_data <- model_i[[i]]$all_species[[j]]
            
            # Extract AUC and CI values
            auc_value <- as.numeric(sub(".*: (\\d+\\.\\d+)-.*", "\\1", species_data$auc))
            ci_value <- as.numeric(sub(".*-(\\d+\\.\\d+).*", "\\1", species_data$ci))
            
            # Create a data frame for the current species
            species_df <- data.frame(
              Model = paste0(model_i[[i]]$model_name),
              Species = species_data$species_name,
              AUC = auc_value,
              CI_lower = ci_value[1],
              CI_upper = ci_value[3]
            )
            
            # Append data for the current species to the list
            model_data[[j]] <- species_df
          }
          
          # Combine data for all species within the current model into a single data frame
          model_data <- bind_rows(model_data)
          
          # Add data for the current model to the list
          combined_data[[i]] <- model_data
        }
        
        # Combine data for all models into a single data frame
        combined_data <- bind_rows(combined_data)
        
        # reorganize dataset for ggplot
        combined_data$Model <- factor(combined_data$Model, levels = c("cv_GLM", "GLM", "cv_GAM", "GAM", "cv_SPAMM", "SPAMM", "cv_GBM", "GBM", "cv_RF", "RF", "cv_SPRF", "SPRF" ))
        
        # Group by Model and calculate mean AUC and CI for each group
        model_summary <- combined_data |>
          dplyr::group_by(Model) |>
          summarise(
            mean_AUC = mean(AUC, na.rm = TRUE),
            mean_upper_CI = mean(CI_upper, na.rm = TRUE),
            mean_lower_CI = mean(CI_lower, na.rm = TRUE)
          )
        
        # Define a named vector mapping model names to colors
        model_colors <- c(cv_GAM = "#d18975", GAM = "#d18975", cv_GLM ="lightgrey", GLM ="lightgrey", cv_GBM = "#758bd1", GBM = "#758bd1", cv_RF = "#8fd175", RF = "#8fd175", cv_SPAMM ="#3f2d54", SPAMM ="#3f2d54", cv_SPRF="#DDCC77", SPRF="#DDCC77")  # Add more colors as needed
        ci_colors <- c(cv_GAM = "black", GAM = "black", cv_GLM ="black", GLM ="black", cv_GBM = "black", GBM = "black", cv_RF = "black", RF = "black", cv_SPAMM ="black", SPAMM ="black", cv_SPRF="black", SPRF="black")  # Add more colors as needed
        
        # create a vector with species name we want to highlight
        special_species <- c("Belone_belone", "Boops_boops")
        
        # Plotting
        # ggplot(model_summary, aes(x = Model, y = mean_AUC, fill = Model, color = "Species")) +
        #   geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.6, show.legend = FALSE) + 
        #   geom_errorbar(aes(ymin = mean_lower_CI, ymax = mean_upper_CI), position = position_dodge(width = 0.5), width = 0.2, color = "black", linewidth = 1, alpha = 0.7) +
        #   geom_point(data = combined_data, aes(x = Model, y = AUC), color = "black", size = 1, alpha = 0.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 3.5), show.legend = FALSE) +  # Add points for all species
        #   geom_point(data = subset(combined_data, Species %in% special_species), aes(x = Model, y = AUC), color = "red", size = 1, alpha = 0.8, position = position_jitterdodge(dodge.width = 0.3, jitter.width = 0.5), show.legend = FALSE) +  # Highlight points for special species with jitter
        #   scale_fill_manual(values = model_colors) +  # Assign colors to models
        #   scale_color_manual(values = c("red" = "red", "black" = "black"), guide = "legend", labels = c("Model", "Species")) +  # Set color of special species points to red
        #   labs(x = "Models", y = "AUC", fill = "Model", color = "Species") +
        #   ggtitle("AUC Values with Confidence Intervals by Model") +
        #   theme_minimal() +
        #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
        #         panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        #         panel.grid.minor.x = element_blank())

        
        ggplot(combined_data, aes(x = Model, y = AUC, fill = Model)) +
          geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6) +
          geom_point(data = combined_data, aes(x = Model, y = AUC), 
                     color = "black", size = 1, alpha = 0.5, 
                     position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
          geom_point(data = subset(combined_data, Species %in% special_species), 
                     aes(x = Model, y = AUC), color = "red", size = 1, 
                     alpha = 0.8, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
          scale_fill_manual(values = model_colors) +
          labs(x = "Models", y = "AUC", fill = "Model") +
          ggtitle("AUC Values with Confidence Intervals by Model") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major.x = element_blank(),  
                panel.grid.minor.x = element_blank())
        
        
   
  
        # # Add theme to set background color to white
        # plot <- last_plot() +
        #   theme(
        #     panel.background = element_rect(fill = "white", color = NA)
        #   )
         
        # Save the plot with the adjusted theme
        ggsave(
          filename = "outputs/figures/models_auc/confinement_models_auc.svg", 
          plot = plot, 
          device = "svg"
        )
        

        
        
        #roc(response = validation_observed, predictor = validation_predict, controls = validation_observed, cases = validation_predict, formula = model_formula,
        # levels=base::levels(as.factor(response)), percent=FALSE, na.rm=TRUE,
        # direction=c("auto", "<", ">"), algorithm = 6, quiet = FALSE, 
        # smooth=TRUE, auc=TRUE, ci=TRUE, plot=FALSE, smooth.method="binormal",
        # smooth.n=512, ci.method=NULL, density=NULL, ...)
        
        # roc <- roc(response = validation_observed, predictor = validation_predict, percent=TRUE,
        #             # arguments for auc
        #             partial.auc=c(100, 90), partial.auc.correct=TRUE,
        #             partial.auc.focus="sens",
        #             # arguments for ci
        #             ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
        #             # arguments for plot
        #             plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
        #             print.auc=TRUE, show.thres=TRUE)
        # 
        # library(pROC)
        # data(aSAH)
        # 
        # rocobj <- plot.roc(aSAH$outcome, aSAH$s100b,
        #                    main="Confidence intervals", percent=TRUE,
        #                    ci=TRUE, # compute AUC (of AUC by default)
        #                    print.auc=TRUE) # print the AUC (will contain the CI)
        # ciobj <- ci.se(rocobj, # CI of sensitivity
        #                specificities=seq(0, 100, 5)) # over a select set of specificities
        # plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape
        # plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold
        #        
