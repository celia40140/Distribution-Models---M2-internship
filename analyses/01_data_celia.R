# This script load the needed datasets, (covariates and fish occurrences), 
# select the covariates used in the models 
# plot the data and look for interesting links between R (richness) and other covariates
# create different spatial cross validation dataset

source("R/01_cv_celia_function.R")

#########################################################
############# Load environmental covariates #############
#########################################################

# Upload dataset with covariates and occurences
load("data/raw_data/confinement.RData")
data_biodiv <- data[, 1:160]
data_MED <- data[, 161:285]

#faire un rmd pour traiter le fichier spygen et le fichier covariates séparement pour les combiner en utilisant spygen_code (=Row.names) le télécharger dans raw_data (=biodivmed)
#j'ai bien vérifié qu'on avait des detections sur tous les sites
#j'ai aussi vérifié qu'on avait bient les même codes spygen pour les deux fichiers

# Erased from the dataset species not detected
# Identify columns (=species) with only 0 in every row
cols_to_remove <- colnames(data_biodiv)[apply(data_biodiv == 0, 2, all)]
# Remove identified columns
data_biodiv <- data_biodiv[, !(colnames(data_biodiv) %in% cols_to_remove)]

# Clean dataset
data_MED$Date <- NULL
data_MED$Country <- NULL
data_MED$pkey <- NULL
data_MED$method <- NULL
data_MED$depth <- NULL
data_MED$Time_start <- NULL
data_MED$projet <- NULL
data_MED$Comments <- NULL
data_MED$Teleo <- NULL
data_MED$Mammifere <- NULL
data_MED$filter <- NULL
data_MED$Temporisation <- NULL
data_MED$duration <- NULL
data_MED$Volume_filtered <- NULL
data_MED$protection_reglementation <- NULL
data_MED$latitude_start_raw <- NULL
data_MED$longitude_start_raw <- NULL
data_MED$longitude_turn_raw <- NULL
data_MED$latitude_turn_raw <- NULL
data_MED$longitude_end_raw <- NULL
data_MED$latitude_end_raw <- NULL
data_MED$transect <- NULL
data_MED$habitat_principal <- NULL


# Put covariates in factor
data_MED$Row.names.y <- as.factor(data_MED$Row.names.y)
data_MED$Confinement <- as.factor(data_MED$Confinement)
data_MED$protection <- as.factor(data_MED$protection)
#data_MED$transect <- as.factor(data_MED$transect)
#data_MED$habitat_principal <- as.factor(data_MED$habitat_principal)

# Select quantitatives covariates
variables_quantitatives <- c("R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport", "chloroDay", "chloroYear", "tempDay",  "tempYear") 
# Scale with a mean of 0 and a standard deviation of 1
data_MED_final <- data_MED[c("Row.names.y", "Confinement", "protection", "latitude_start_DD", "longitude_start_DD",  variables_quantitatives)]
#data_MED_final[, variables_quantitatives] <- scale(data_MED_final[, variables_quantitatives])

#  Rename column (for later)
names(data_MED_final)[1] <- "Row.names"
data_MED_final <- data_MED_final|>
dplyr::mutate(Row.names = as.character(Row.names))

# data frame without Na for model calculation (test)
variables_quantitatives <- c("habitat_div", "mean_bathy", "logland","chloroDay", "tempDay")
med_covariates_no_na <- data_MED_final|>
  dplyr::select("Row.names","latitude_start_DD", "longitude_start_DD", "latitude_start_DD", "protection","habitat_div", "mean_bathy", "logland","chloroDay", "tempDay")
med_covariates_no_na <- med_covariates_no_na |>
  dplyr::mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
med_covariates_no_na[, variables_quantitatives] <- scale(med_covariates_no_na[, variables_quantitatives])
save(med_covariates_no_na, file = "data/derived_data/med_covariates_no_na.RData")

#####################################################################
############# Create a cross validation dataset (Cyril) #############
#####################################################################
# #/!\attention mes colonnes sont en format "int" (set as.numeric comme cyril? données occurences)
# species_name <- colnames(data_biodiv)[!colnames(data_biodiv) %in% c("Row.names")]
# biodivmed_occ <- merge(data_biodiv, data_MED_final, by = "Row.names", all = TRUE)
# # dans version finale utiliser dplyr
# # result2 <- data_biodiv |> 
# #   dplyr::inner_join(data_MED_final[,c("Row.names")], copy = TRUE)
# 
# biodivmed_occ <- biodivmed_occ |>
#   dplyr::select(Row.names, 
#                 latitude_start_DD, 
#                 longitude_start_DD, 
#                 species_name)  
# 
# occ_scv <- scv_function(dats = biodivmed_occ,
#                             n.folds = 20)   #20 ???
# 
# names(occ_scv) <- sapply(1:length(occ_scv), function(i) { paste0("cv_", i)})
# 
# # save derived data
# 
# med_covariates <- data_MED_final
#   
# save(med_covariates, file = "data/derived_data/med_covariates.RData")
# save(occ_scv, file = "data/derived_data/occ_scv.RData")  #take into account spatial autocorrelation
# #120 vs 8



#####################################################################
############# Create a cross validation dataset (Cyril) #############
#####################################################################

# species_name <- colnames(data_biodiv)[!colnames(data_biodiv) %in% c("Row.names")]
# biodivmed_occ <- merge(data_biodiv, data_MED_final, by = "Row.names", all = TRUE)
# #dans version finale utiliser dplyr
# result2 <- data_biodiv |>
#   dplyr::inner_join(data_MED_final[,c("Row.names")], copy = TRUE)
# biodivmed_occ <- biodivmed_occ |>
#   dplyr::select(Row.names,
#                 latitude_start_DD,
#                 longitude_start_DD,
#                 species_name)
# save(biodivmed_occ, file = "data/derived_data/biodivmed_occ.RData")
# occ_cv <- cv_function(dats = biodivmed_occ,
#                         n.folds = 20)   #20 ???
# 
# names(occ_cv) <- sapply(1:length(occ_cv), function(i) { paste0("cv_", i)})
# 
# # save derived data
# 
# med_covariates <- data_MED_final
# 
# save(med_covariates, file = "data/derived_data/med_covariates.RData")
# save(occ_cv, file = "data/derived_data/occ_cv.RData")  #take into account spatial autocorrelation
# #163 vs 8
# 


#####################################################################
############ Create a balanced cross validation dataset #############
#####################################################################

#modified version for common species
load("data/derived_data/biodivmed_occ_2.RData")
species_name <- colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]

biodivmed_occ_2 <- biodivmed_occ_2 |>
  dplyr::select(Row.names,
                latitude_start_DD,
                longitude_start_DD,
                species_name, protection) |>
  dplyr::mutate(Row.names = as.factor(Row.names))

occ_bcv_2 <- bcv_function(dats = biodivmed_occ_2,
                          n.folds = 20)   #20 ???

names(occ_bcv_2) <- sapply(1:length(occ_bcv_2), function(i) { paste0("cv_", i)})

# save derived data

save(occ_bcv_2, file = "data/derived_data/occ_bcv_2.RData")  #take into account spatial autocorrelation

med_biodiv_2 <- biodivmed_occ_2 |>
  dplyr::select(Row.names,
                latitude_start_DD,
                longitude_start_DD,
                species_name) |>
  dplyr::mutate(Row.names = as.factor(Row.names))
save(med_biodiv_2, file = "data/derived_data/med_biodiv_2.RData")  #take into account spatial autocorrelation


#####################################################################
############# Create a balanced spatial cross validation dataset #############
#####################################################################
# 
# species_name <- colnames(data_biodiv)[!colnames(data_biodiv) %in% c("Row.names")]
# biodivmed_occ <- merge(data_biodiv, data_MED_final, by = "Row.names", all = TRUE)
# biodivmed_occ <- biodivmed_occ |>
#   dplyr::select(Row.names, 
#               latitude_start_DD, 
#               longitude_start_DD, 
#               species_name, Confinement, protection) |>
#   dplyr::mutate(Row.names = as.factor(Row.names)) 
# 
# occ_bscv <- bscv_function(dats = biodivmed_occ,
#                       n.folds = 20)   #20 ???
# 
# names(occ_bscv) <- sapply(1:length(occ_bscv), function(i) { paste0("cv_", i)})
# 
# # save derived data
# 
# med_covariates <- data_MED_final
# save(med_covariates, file = "data/derived_data/med_covariates.RData")
# 
# save(occ_bscv, file = "data/derived_data/occ_bscv.RData")  #take into account spatial autocorrelation
# #70-120 vs 8-9
# 








#########################################################
############## Plot absences vs presences ###############
#########################################################

# for our models it's more than important to have an idea of the number of occurrences we are working with: the following code will help us understand the proportion of absence compare to presence for all species
# we will also focus on the difference between elasmobranchs (cartilaginous fish) and teleosts
# we will also extract the names of the species (=colnames) of which the numbers of occurrences is less than 30 (global rules is 10 occurences/covariates)


#save for later occurences in a different dataset
med_biodiv<-biodivmed_occ
save(med_biodiv, file = "data/derived_data/med_biodiv.RData")

# Identify columns for occurrences
#species_names <- colnames(med_biodiv)[!colnames(med_biodiv) %in% c("Row.names", "longitude_start_DD", "latitude_start_DD")]
species_name <- colnames(data_biodiv)[!colnames(data_biodiv) %in% c("Row.names")] #sans espèces sans occurences

# Calculate total occurrences for each species
total_occurrences <- colSums(med_biodiv[, species_name], na.rm = TRUE)

# Extract species with less than 40 occurrences
species_less_than_40 <- names(total_occurrences[total_occurrences < 40]) #85
species_more_than_40 <- names(total_occurrences[total_occurrences > 40]) 

# create a new dataset with species with more than 40 occurences 
biodivmed_occ_2 <- biodivmed_occ[, !colnames(biodivmed_occ) %in% species_less_than_40]
biodivmed_occ_2$Row.names <- as.factor(biodivmed_occ_2$Row.names)
save(biodivmed_occ_2, file = "data/derived_data/biodivmed_occ_2.RData")

med_biodiv_2 <- med_biodiv[, !colnames(biodivmed_occ) %in% species_less_than_40]

# Create a data frame for plotting
plot_data <- data.frame(species = names(total_occurrences), total_occurrences = total_occurrences)

# Add a ranking column
plot_data$rank <- rank(-plot_data$total_occurrences)  # Rank in descending order

# Calculate mean occurrences
mean_occurrences <- mean(total_occurrences)

# Calculate percentage of species with occurrences above the mean
plot_data$above_mean <- ifelse(plot_data$total_occurrences > mean_occurrences, 1, 0)
plot_data$below_mean <- ifelse(plot_data$total_occurrences < mean_occurrences, 1, 0)

# Calculate percentages
percentage_above_mean <- (sum(plot_data$above_mean) / length(total_occurrences)) * 100
percentage_below_mean <- (sum(plot_data$below_mean) / length(total_occurrences)) * 100

# Define elasmobranch species to be colored differently
highlight_species <- c("Aetomylaeus_bovinus", "Bathytoshia_lata", "Dasyatis_pastinaca", "Dasyatis_tortonesei",
                        "Etmopterus_spinax", "Galeus_melastomus", "Mobula_mobular", "Mustelus_mustelus",
                        "Myliobatis_aquila", "Prionace_glauca", "Pteroplatytrygon_violacea", "Raja_brachyura",
                        "Raja_undulata", "Rostroraja_alba", "Scyliorhinus_canicula", "Scyliorhinus_stellaris",
                        "Squatina_squatina", "Torpedo_marmorata")

# Plotting using ggplot2
library(ggplot2)

ggplot(plot_data, aes(x = reorder(species, -total_occurrences), y = total_occurrences)) +
  geom_bar(stat = "identity", aes(fill = ifelse(species %in% highlight_species, "pink", "skyblue"))) +
  geom_hline(yintercept = mean_occurrences, linetype = "dashed", color = "black", size = 0.5) +
  geom_text(x = max(seq_along(total_occurrences)) - 34, y = mean_occurrences,
            label = paste("Mean Occurrence:", round(mean_occurrences, 0)),
            vjust = -1, hjust = 0, color = "black", size = 4) +
  geom_text(aes(label = paste(round(percentage_above_mean, 1), "% above mean")),
            x = max(seq_along(total_occurrences)) - 33, y = mean_occurrences -8,
            vjust = 0, hjust = 0, color = "black", size = 4) +
  geom_text(aes(label = paste(round(percentage_below_mean, 1), "% below mean")),
            x = max(seq_along(total_occurrences)) - 33, y = mean_occurrences - 10,
            vjust = 1, hjust = 0, color = "black", size = 4) +
  scale_fill_identity() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(color = "black", size = 10, hjust = 0.5)) +
  labs(title = "Total Occurrences and Ranking for Each Species",
       x = "Species",
       y = "Total Occurrences",
       caption = "Number of occurrences of teleosts (blue) and elasmobranchs (pink) species.")





#########################################################
############## Cartograhie of occurrences ################
#########################################################


# the goal here is to see where species are located, and if, some of the absences can be erased: big issue for us will to have too much absences for our models to be able to work
# for exemple, we already know that Squatina squatina (angel shark) is only located in Corsica so all 0 outside can't be taken for real 0

# For all the occurrences of ALL SPECIES 
#This is for the map of just the Med 
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Melt the dataset to long format for easier plotting
melted_data <- reshape2::melt(med_biodiv, id.vars = c("Row.names", "latitude_start_DD", "longitude_start_DD"))   # for all species

# erased rows when column value = 0 (if you just want to see where species occurs)
data <- melted_data |>
  dplyr::filter(value != 0)

# transform as factor column value
data$value <- factor(data$value)


# Separate elamsobranchs species to create 2 specific melted dataset for teleosts and elasmobranchs
elasmo <- c("Aetomylaeus_bovinus", "Bathytoshia_lata", "Dasyatis_pastinaca", "Dasyatis_tortonesei", 
            "Mobula_mobular", "Mustelus_mustelus", "Myliobatis_aquila", "Prionace_glauca", "Pteroplatytrygon_violacea",
            "Raja_undulata", "Torpedo_marmorata") #11

melted_elasmo <- melted_data |>
  dplyr::filter(variable %in% elasmo)

melted_elasmo$value <- factor(melted_elasmo$value)

melted_elasmo <- melted_elasmo |>
  dplyr::filter(value != 0)               # for elamsobranchs

teleost <- setdiff(species_name, elasmo) #120 = 131 en tout 

melted_teleost <- melted_data |>
  dplyr::filter(variable %in% teleost)

melted_teleost$value <- factor(melted_teleost$value)

melted_teleost <- melted_teleost |>
  dplyr::filter(value != 0)

# Specify the columns representing different species
species_name <- colnames(data_biodiv)[!colnames(data_biodiv) %in% c("Row.names")]

# Create a vector of unique colors for each species group
elasmo_color <- "pink"
teleost_color <- "black"

# Plotting using ggplot2
ggplot2::ggplot(data = world) +
  geom_sf(color = "black", fill = "light grey") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(2.79123253648454, 10.647334299871368), ylim = c(41.1906737866009, 44.44029498045467), expand = FALSE) +
  ggtitle("Mediterranean Sea Occurrences") +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank",
                                        size = 0.2), panel.background = element_rect(fill = "light blue")) +
  geom_point(data = melted_teleost, aes(x = longitude_start_DD, y = latitude_start_DD, color = "Teleosts", fill = "Teleosts"), size = 2, alpha = 0.7) +
  geom_point(data = melted_elasmo, aes(x = longitude_start_DD, y = latitude_start_DD, color = "Elasmobranchs", fill = "Elasmobranchs"), size = 2, alpha = 0.7) +  
  labs(title = "Species Occurrences",
       x = "Longitude",
       y = "Latitude",
       color = "Species",
       fill = "Species") +
  scale_color_manual(values = c("Elasmobranchs" = elasmo_color, "Teleosts" = teleost_color), labels = c("Elasmobranchs", "Teleosts")) +
  scale_fill_manual(values = c("Elasmobranchs" = elasmo_color, "Teleosts" = teleost_color), labels = c("Elasmobranchs", "Teleosts")) +
  guides(shape = guide_legend(title = "Legend Title", override.aes = list(size = 4)))


# Plot for ALL ELASMOBRANCHS
neutral_color <- "grey"
elasmo_highlight <- "pink"

# Specify an absolute path
save_directory <- "/home/celia/Documents/M2_internship/celia_project/celia_project/outputs/figures/elasmobranchs_occurrences/"

plots_list <- list()  # Initialize an empty list to store plots
plots_per_grid <- 12  # Set the number of plots per grid

for (i in seq_along(elasmo)) {
  species <- elasmo[i]
  
  # Filter melted_elasmo for the current species
  subset_data <- melted_elasmo[melted_elasmo$variable == species, ]
  data <- subset(melted_elasmo, variable != species)
  
  # Generate the plot
  plot <- ggplot(data = world) +
    geom_sf(color = "black", fill = "light grey") +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(xlim = c(2.79123253648454, 10.647334299871368), ylim = c(41.1906737866009, 44.44029498045467), expand = FALSE) +
    ggtitle(paste("Occurrences of", species)) +
    theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2),
          panel.background = element_rect(fill = "light blue")) +
    geom_point(data = data, aes(x = longitude_start_DD, y = latitude_start_DD, color = "grey", fill = "grey"), size = 1, alpha = 0.7) +
    geom_point(data = subset_data, aes(x = longitude_start_DD, y = latitude_start_DD, color = "pink", fill = "pink"), size = 3, alpha = 0.7) +
    scale_color_manual(values = c(neutral_color, elasmo_highlight), labels = c("Other Elasmobranchs", species)) +
    scale_fill_manual(values = c(neutral_color, elasmo_highlight), labels = c("Other Elasmobranchs", species)) +
    labs(title = paste("Occurrences of", species),
         x = "Longitude",
         y = "Latitude",
         color = "Presence",
         fill = "Presence") +
    guides(shape = guide_legend(title = "Legend Title", override.aes = list(size = 4)))
  # Add the plot to the list
  plots_list[[length(plots_list) + 1]] <- plot
  
  # Check if we've reached the desired number of plots per grid or it's the last species
  if (length(plots_list) %% plots_per_grid == 0 || i == length(elasmo)) {
    # Arrange and display the plots in a grid
    grid_arrange <- gridExtra::grid.arrange(grobs = plots_list, ncol = 3)
    
    # Save the grid to a file
    save_filename <- paste0(save_directory, "grid_", (i - 1) %/% plots_per_grid + 1, ".svg")
    ggsave(save_filename, grid_arrange, device = "svg", width = 15, height = 10)  # Adjust width and height as needed
    
    # Clear the plots_list for the next grid
    plots_list <- list()
  }
}


# Plot for TELEOSTS
# Create a neutral color for other teleosts
neutral_color <- "grey"
highlight <- "skyblue"

# Specify an absolute path
save_directory <- "/home/celia/Documents/M2_internship/celia_project/celia_project/outputs/figures/teleosts_occurrences/"

plots_list <- list()  # Initialize an empty list to store plots
plots_per_grid <- 12  # Set the number of plots per grid

for (i in seq_along(teleost)) {
  species <- teleost[i]
  
  # Filter melted_elasmo for the current species
  subset_data <- melted_teleost[melted_teleost$variable == species, ]
  data <- subset(melted_teleost, variable != species)
  
  # Generate the plot
  plot <- ggplot(data = world) +
    geom_sf(color = "black", fill = "light grey") +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(xlim = c(2.79123253648454, 10.647334299871368), ylim = c(41.1906737866009, 44.44029498045467), expand = FALSE) +
    ggtitle(paste("Occurrences of", species)) +
    theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2),
          panel.background = element_rect(fill = "light blue")) +
    geom_point(data = data, aes(x = longitude_start_DD, y = latitude_start_DD, color = "grey", fill = "grey"), size = 1, alpha = 0.7) +
    geom_point(data = subset_data, aes(x = longitude_start_DD, y = latitude_start_DD, color = "skyblue", fill = "skyblue"), size = 3, alpha = 0.7) +
    scale_color_manual(values = c(neutral_color, highlight), labels = c("Other Teleosts", species)) +
    scale_fill_manual(values = c(neutral_color, highlight), labels = c("Other Teleosts", species)) +
    labs(title = paste("Occurrences of", species),
         x = "Longitude",
         y = "Latitude",
         color = "Presence",
         fill = "Presence") +
    guides(shape = guide_legend(title = "Legend Title", override.aes = list(size = 4)))
  # Add the plot to the list
  plots_list[[length(plots_list) + 1]] <- plot
  
  # Check if we've reached the desired number of plots per grid or it's the last species
  if (length(plots_list) %% plots_per_grid == 0 || i == length(teleost)) {
    # Arrange and display the plots in a grid
    grid_arrange <- gridExtra::grid.arrange(grobs = plots_list, ncol = 3)
    
    # Save the grid to a file
    save_filename <- paste0(save_directory, "grid_", (i - 1) %/% plots_per_grid + 1, ".svg")
    ggsave(save_filename, grid_arrange, device = "svg", width = 15, height = 10)  # Adjust width and height as needed
    
    # Clear the plots_list for the next grid
    plots_list <- list()
  }
}



  
################################################################
############## Calculate center of our transect ################
################################################################

# Because we extract our covariate using a buffer of 1km in each direction from the midle of our transect we need to: 
#(1)calculate the distance between start and end to see if all of them are >1km (otherwise problem); 
#(2)find a way to calculate the new coordinates


# (1) Transect size > 1km
# reprendre fichier initial data_MED avec latitude start et end : attention ici présence de NA qu'il n'y aura pas dans le fichier final
data_dist <- data_MED |>
  dplyr::select(Row.names.y, latitude_start_DD, longitude_start_DD, latitude_end_DD, longitude_end_DD) |>
  dplyr::rename(
    Row.names = Row.names.y)

# Calculate distances using geosphere::distVincentySphere
distances_km <- geosphere::distVincentySphere(
  cbind(data_dist$longitude_start_DD, data_dist$latitude_start_DD),
  cbind(data_dist$longitude_end_DD, data_dist$latitude_end_DD)
) / 1000

data_dist$distances_km <- distances_km

# Identify transect less than 1 km 
transect_under1km <- data_dist |>
  dplyr::filter(!is.na(distances_km), distances_km <= 1) |>
  dplyr::distinct(Row.names)

# plot number compare to the other 
# Create categories based on distances
data_dist$category <- cut(data_dist$distances_km, breaks = c(0, 0.5, 1, 1.5, 2, Inf), labels = c("0-0.5 km", "0.5-1 km", "1-1.5 km", "1.5-2 km", "Above 2 km"))

# Plot the counts for each category
ggplot(data_dist, aes(x = category)) +
  geom_bar(fill = "#336666", color = "black") +
  labs(title = "Distribution of Distances",
       x = "Distance Categories",
       y = "Count")

# (2) Calculate coordinates transect center 
# Calculate midpoints
data_dist <- data_dist |>
  dplyr::mutate(
    mid_latitude = (latitude_start_DD + latitude_end_DD) / 2,
    mid_longitude = (longitude_start_DD + longitude_end_DD) / 2
  )

# Create a new data frame with midpoint coordinates
midpoints_data <- data_dist |>
  dplyr::select(Row.names, mid_latitude, mid_longitude)



########################################################################
############## Calculate distances between transects ###################
########################################################################


# supprimer les lignes avec latitude/longitude = NA (que pour ici)
midpoints_data2 <- midpoints_data |>
  dplyr::select(Row.names, mid_latitude, mid_longitude) |>
  dplyr::filter(!is.na(mid_latitude) & !is.na(mid_longitude))

# using midpoints data, create all combinations of trasect 
combinations <- expand.grid(midpoints_data2$Row.names, midpoints_data2$Row.names)
combinations <- combinations[combinations$Var1 != combinations$Var2, ]

# Initialize an empty data frame to store distances
distances_data <- data.frame(
  Site1 = character(),
  Lat1 = numeric(),
  Lon1 = numeric(),
  Site2 = character(),
  Lat2 = numeric(),
  Lon2 = numeric(),
  Distance = numeric()
)

# Calculate distances between pairs of sites
for (i in 1:nrow(combinations)) {
  site1 <- combinations[i, 1]
  site2 <- combinations[i, 2]
  
  # Get latitude and longitude values for the two sites
  lat1 <- as.vector(midpoints_data2$mid_latitude[midpoints_data2$Row.names == site1])
  lon1 <- as.vector(midpoints_data2$mid_longitude[midpoints_data2$Row.names == site1])
  lat2 <- as.vector(midpoints_data2$mid_latitude[midpoints_data2$Row.names == site2])
  lon2 <- as.vector(midpoints_data2$mid_longitude[midpoints_data2$Row.names == site2])
  
  
  # Check if both latitude and longitude values are available
  if (!is.na(lat1) && !is.na(lon1) && !is.na(lat2) && !is.na(lon2)) {
  # Calculate distance using geosphere::distVincentySphere
  Distance_km <- geosphere::distVincentySphere(
    c(lon1, lat1),
    c(lon2, lat2)
  ) / 1000
  } else {
    # Set distance to NA if coordinates are missing
    distance_km <- NA
  }
  # Add the data to the distances_data data frame
  distances_data <- rbind(distances_data, data.frame(
    Site1 = site1, Lat1 = lat1, Lon1 = lon1,
    Site2 = site2, Lat2 = lat2, Lon2 = lon2,
    Distance_km = Distance_km
  ))
}

# Create categories based on distances
distances_data$category <- cut(distances_data$Distance_km, breaks = c(0.000000e+00, 1, 2, 4, 6, 12, Inf), labels = c("0-1 km", "1-2 km", "2-4 km", "4-6 km", "6-12 km", "Above 12 km"))

# Count the number of distances below 4 km for each category
count_under_4km <- distances_data |>
  dplyr::filter(Distance_km < 4) |>
  dplyr::group_by(category) |>
  dplyr::summarize(count = dplyr::n())

# Count the number of distances above 4 km for each category
count_above_4km <- distances_data |>
  dplyr::filter(Distance_km > 4) |>
  dplyr::group_by(category) |>
  dplyr::summarize(total_count = dplyr::n())

# Plot the distribution of distances
ggplot2::ggplot(distances_data, aes(x = category)) +
  ggplot2::geom_bar(fill = "#336666", color = "black") +
  ggplot2::geom_text(data = count_above_4km, aes(label = total_count, y = 0.5 * total_count),
            position = position_stack(vjust = 2),
            vjust = -0.5, size = 4, color = "black") +
  ggplot2::geom_text(data = count_under_4km, aes(label = count, y = 0.5 * count),
            position = position_stack(vjust = 2),
            vjust = -0.5, size = 4, color = "red") +
  labs(title = "Distribution of Distances",
       x = "Distance Categories",
       y = "Count") +
  theme_minimal()



##################################################################
############## Study of covariates with VIF & ACP ################
##################################################################

# see if some covariates are too coorelated and choose 6 to 7 predictors (compare with the mean number of occurences)
# dataframe with all occurences and all covariates + Row.names
data_biodiv[, -1] <- as.data.frame(lapply(data_biodiv[, -1], as.numeric))
combined_data <- dplyr::full_join(data_biodiv, data_MED_final, by = "Row.names")

## RDA
# utiliser data_MED_final
RDA2=vegan::capscale(combined_data[,-c(1,133:151)] ~ Confinement + protection + least_dist_reserve + mean_depth_transect + transect + habitat_div + habitat_principal + mean_bathy + logland + logport + chloroDay  + chloroYear + tempDay  + tempYear, combined_data, dist="jaccard", na.action = na.omit, add =TRUE)
summary(RDA2)
plot(RDA2)

## AFC 
afc <- FactoMineR::CA(data_biodiv[,-1], graph = FALSE, axes = c(1,2))
factoextra::fviz_ca_biplot(afc, axes = c(1, 2), col.row = "blue", col.col = "black", repel= FALSE, ellipse.type="confidence", addELLipse=TRUE)


# ACP
# ici problème avec les NA vérifier qu'on a bien aucun NA dans le fichier final et sauter cette étape 
# Create a dataset with all quantitative covariates
data_quantitative <- data_MED_final[c("Row.names", "R","Chondri", "LFI", "habitat_div", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
#data_quantitative <- data_MED_final[c( "habitat_div", "mean_bathy", "logland","chloroDay", "tempDay")]

pca = FactoMineR::PCA(data_quantitative[,-1], scale.unit = TRUE, ncp = 5, graph = FALSE)
# Get eigenvalues
eig.val <- factoextra::get_eigenvalue(pca)
eig.val
# Draw screeplot
factoextra::fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

# Variable contributions to axes
var <- factoextra::get_pca_var(pca)
# Corrplot of the cos2 of variables
corrplot::corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 to Dim.3
factoextra::fviz_cos2(pca, choice = "var", axes = 1:3)
# Contributions of variables to PC1 : 
factoextra::fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2 :
factoextra::fviz_contrib(pca, choice = "var", axes = 2, top = 10)
# Variable contributions to axes
factoextra::fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


# VIF avec package usdm
usdm::vifcor(data_quantitative, th = 0.9, keep = NULL, method = 'pearson')


# Check correlation between quantitative covariates
psych::pairs.panels(scale(data_quantitative),
                    method = "spearman", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = FALSE) # show correlation ellipses
# on peut voir qu'il est pas necessaire de garder toutes les var de température et de chlorophyle (garder seulement pour le jour et l'année)
# atttention chloro année et temp année fortement corrélé aussi !
# attention a moyenne profondeur transect et moyenne bathy corrélées





#############################################################################
#################### Dataset Analyse : PLOTS ################################
#############################################################################
# do all of those plots with the variables chosen (or not if preliminary work) to investigate covariates 
# don't forget to calculate R (=richness) and Chondri (=elasmobranchs diversity) with Alicia Dalongeville indices (https://doi.org/10.1111/1365-2664.14276)

# New dataset with scaled quantitative covariates
data_quantitative <- data_MED_final[c("R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]

# Plot data
#confinement montre une différence de richesse spé
#protection ne montre pas de diff de richesse
#richesse spé semble augmenter en cotier (transect)
cowplot::plot_grid(nrow = 1, ncol = 3,
          ggplot(data_MED_final, aes(y = Confinement, x = R, fill = R)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("richesse spé") +
            ylab("Confinement"),
          ggplot(data_MED_final, aes(y = protection, x = R, fill = R)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("richesse spé") +
            ylab("protection"),
          ggplot(data_MED_final, aes(y = R, x = transect)) +
            geom_point() +
            xlab("transect") +
            ylab("richesse"))

#ploter plus en détail
ggplot(data_MED, aes(x=R, y=LFI, shape=Confinement, colour=Confinement, fill=Confinement)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("richesse") +
  ylab("LFI") +
  ggtitle("Une regression par confinement couleur") #lorsqu'on est en confinement, interieur ou dehors des reserves on a globalement une plus grande richesse associé avec des poissons de plus grande taille
#les transect offshore ont un LFI de 20 max et un R de 25 max (faible du a pb detection? effort d'echantillonnage plus faible?)

ggplot(data_MED_final, aes(x=R, y=Chondri, shape=transect, colour=transect, fill=transect)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("Chondri") +
  ggtitle("Une regression par confinement couleur")# quand richesse augmente richesse en chondri augmente (logique)
#observé pour reserve et en dehors, en confinement et en dehors, en cotier ou offshore

ggplot(data_MED_final, aes(x=R, y=least_dist_reserve, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("distance reserve") +
  ggtitle("Une regression par confinement couleur") #distance à la reserve semble pas expliquer richesse spé

ggplot(data_MED, aes(x=R, y=habitat_div, shape=habitat_principal, colour=habitat_principal, fill=habitat_principal)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("div habitat") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair

ggplot(data_MED, aes(x=R, y=mean_depth_transect, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("profondeur moyenne transect") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair

ggplot(data_MED, aes(x=R, y=logland, shape=Confinement, colour=Confinement, fill=Confinement)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("logland") +
  ggtitle("Une regression par confinement couleur") #en confinement il ne semble pas y avoir de lien entre richesse et distance à la côte, par contre hors confinement une diminution de la distance à la côte entraine une augmentation de la richesse
# la richesse augmente quand le log de la distance a la cote diminue en dehors des reserves (distance cst car reserve côtière) , quand on est offshore ou en cotier, distance à la cote nexplique pas modif de richesse

ggplot(data_MED_final, aes(x=R, y=logport, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("logport") +
  ggtitle("Une regression par confinement couleur") #logport explique pas changement de R

ggplot(data_MED_final, aes(x=R, y=chloroDay, shape=transect, colour=transect, fill=transect)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("chloroDay") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair chloro et richesse

ggplot(data_MED, aes(x=R, y=tempDay, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("tempDay") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair entre temperature et richesse

# close to balanced dataframe
mosaicplot(table(data_MED_final$protection, data$Confinement), main="Mosaic Plot")

# boxplot
boxplot(least_dist_reserve ~ protection, data = data_MED_final,
        xlab = "Protection", ylab = "Least Distance to Reserve",
        main = "Boxplot of Least Distance to Reserve by Protection")


###########################################################################################
######################## Choix du modèle linéaire : ancova ################################
###########################################################################################

#var quanti : "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport", "chloroDay", "chloroYear", "tempDay",  "tempYear"
#var quali : "Confinement", "protection", "transect", "habitat_principal"
#david garde : Confinement  + protection +  logport + logland + habitat_div  + chloroWeek + tempWeek + tempYear + chloroYear + min_bathy
mod1 <- lm(R ~ logland + Chondri + LFI + Confinement + protection + LFI*Confinement + logland*protection + logland*Confinement, data=data_MED_final)
drop1(mod1,.~ ., test="F")
mod2 <- lm(R ~ logland + Chondri + LFI + Confinement + protection + logland*protection +logland*Confinement, data=data_MED_final)
drop1(mod2,.~ ., test="F")
mod3 <- lm(R ~ logland + Confinement + Chondri + LFI  + protection +logland*Confinement, data=data_MED_final)
drop1(mod3,.~ ., test="F")
mod4 <- lm(R ~ logland + Confinement + Chondri + LFI +logland*Confinement, data=data_MED_final)
drop1(mod4,.~ ., test="F")
mod5 <- lm(R ~ logland + Confinement + LFI +logland*Confinement, data=data_MED_final)
drop1(mod5,.~ ., test="F")
summary(mod5)
##tester hypothèses du modèle
par(mfrow=c(2,2))
plot(mod5)
shapiro.test(mod5$res) #p-value = 0.736
#verifier residus
res <-mod5$res
fitted <-fitted(mod5)
resfit <-dplyr::bind_cols(res =res,
                   fit =fitted,
                   PRD =data_MED_final$LFI)
cowplot::plot_grid(nrow= 2, ncol= 2,
          ggplot2::ggplot(dplyr::as_tibble(res), aes(x =res)) +
            geom_histogram(bins = 30,
                           fill ="lightblue", color= "black")+
            labs(title= "Histogrammedes residus"),
          ggplot2::ggplot(dplyr::as_tibble(res), aes(sample= res)) +
            stat_qq()+
            stat_qq_line()+
            labs(title= "Normal Q-QPlot"),
          ggplot2::ggplot(resfit,aes(y = res, x = fit)) +
            geom_point()+
            labs(title= "Valeurs ajustees"),
          ggplot2::ggplot(resfit,aes(y = res, x = PRD)) +
            geom_point()+
            xlab(" ") +
            labs(title= "En fonctionde LFI"))

# Ancova bis
#var quanti : "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport", "chloroDay", "chloroYear", "tempDay",  "tempYear"
#var quali : "Confinement", "protection", "transect", "habitat_principal"
#david garde : Confinement  + protection +  logport + logland + habitat_div  + chloroWeek + tempWeek + tempYear + chloroYear + min_bathy
#on ne peut pas avoir l'interaction de least_dist_reserve * reserve
mod1bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*protection + logland*Confinement + least_dist_reserve*Confinement + mean_bathy*Confinement + mean_bathy*protection + chloroDay*Confinement + chloroDay*protection + tempDay*Confinement + tempDay*protection, data=data_MED_final)
drop1(mod1bis,.~ ., test="F")
#chloroDay:protection=0.824978
mod2bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*protection + logland*Confinement + least_dist_reserve*Confinement + mean_bathy*Confinement + mean_bathy*protection + chloroDay*Confinement + tempDay*Confinement + tempDay*protection, data=data_MED_final)
drop1(mod2bis,.~ ., test="F")
#least_dist_reserve:Confinement  1    0.0395 81.857 -52.480  0.0623 0.803298
mod3bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*protection + logland*Confinement + mean_bathy*Confinement + mean_bathy*protection + chloroDay*Confinement + tempDay*Confinement + tempDay*protection, data=data_MED_final)
drop1(mod3bis,.~ ., test="F")
#tempDay:Confinement            1    0.0427 81.900 -54.404  0.0678 0.7950050
mod4bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*protection + logland*Confinement + mean_bathy*Confinement + mean_bathy*protection + chloroDay*Confinement + tempDay*protection, data=data_MED_final)
drop1(mod4bis,.~ ., test="F")
#mean_bathy:protection          1    0.2907 82.191 -55.886  0.4650 0.4964885
mod5bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*protection + logland*Confinement + mean_bathy*Confinement + chloroDay*Confinement + tempDay*protection, data=data_MED_final)
drop1(mod5bis,.~ ., test="F")
#tempDay:protection      1    0.1939 82.385 -57.542  0.3114 0.5777479
mod6bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*protection + logland*Confinement + mean_bathy*Confinement + chloroDay*Confinement, data=data_MED_final)
drop1(mod6bis,.~ ., test="F")
#mean_bathy:Confinement  1    1.0083 83.393 -57.766  1.6278 0.2042250
mod7bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*protection + logland*Confinement + chloroDay*Confinement, data=data_MED_final)
drop1(mod7bis,.~ ., test="F")
#logland:protection     1    1.4755 84.868 -57.206  2.3710 0.125968
mod8bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + protection + logland*Confinement + chloroDay*Confinement, data=data_MED_final)
drop1(mod8bis,.~ ., test="F")
#protection             1    0.0550 84.923 -59.111  0.0875 0.767794
mod9bis <- lm(R ~ logland + least_dist_reserve + habitat_div + mean_bathy + chloroDay + tempDay + Confinement + logland*Confinement + chloroDay*Confinement, data=data_MED_final)
drop1(mod9bis,.~ ., test="F")
#habitat_div            1    0.1656 85.089 -60.827  0.2651 0.607454
mod10bis <- lm(R ~ logland + least_dist_reserve + mean_bathy + chloroDay + tempDay + Confinement + logland*Confinement + chloroDay*Confinement, data=data_MED_final)
drop1(mod10bis,.~ ., test="F")
#least_dist_reserve     1    0.3651 85.454 -62.201  0.5879 0.4445690
mod11bis <- lm(R ~ logland + mean_bathy + chloroDay + tempDay + Confinement + logland*Confinement + chloroDay*Confinement, data=data_MED_final)
drop1(mod11bis,.~ ., test="F")
#tempDay                1    0.8731  97.922 -81.331  1.4664 0.2276752
mod12bis <- lm(R ~ logland + mean_bathy + chloroDay + Confinement + logland*Confinement + chloroDay*Confinement, data=data_MED_final)
drop1(mod12bis,.~ ., test="F")
##tester hypothèses du modèle
par(mfrow=c(2,2))
plot(mod12bis)
shapiro.test(mod12bis$res) #p-value = 3.73e-06, strong evidence against H0=normally distributed
#verifier residus
res <-mod12bis$res
fitted <-fitted(mod12bis)
resfit <-dplyr::bind_cols(res =res,
                   fit =fitted,
                   PRD =data_MED_final$chloroDay)
cowplot::plot_grid(nrow= 2, ncol= 2,
          ggplot2::ggplot(dplyr::as_tibble(res), aes(x =res)) +
            geom_histogram(bins = 30,
                           fill ="lightblue", color= "black")+
            labs(title= "Histogrammedes residus"),
          ggplot2::ggplot(dplyr::as_tibble(res), aes(sample= res)) +
            stat_qq()+
            stat_qq_line()+
            labs(title= "Normal Q-QPlot"),
          ggplot2::ggplot(resfit,aes(y = res, x = fit)) +
            geom_point()+
            labs(title= "Valeurs ajustees"),
          ggplot2::ggplot(resfit,aes(y = res, x = PRD)) +
            geom_point()+
            xlab(" ") +
            labs(title= "En fonctionde chloroday"))
#modèle pas bon ni pour la normalité, homoscedasticité et indépendance...




###########################################################################################
#################### Calculate Alicia Dalongeville indices ################################
###########################################################################################
# https://github.com/AliciaDalongeville/eDNA_indicators_med/blob/main/01_indicators/indicators.R

## Load the eDNA data (matrix species per sample)
adne <- read.csv("Data/eDNA.csv", header=T, row.names = 1 )

# list of species
species <- rownames(adne)
# list of samples
samples <- colnames(adne)

## Calculate the indicators for each sample
##########################################################
## 1 - Species Richness R
##########################################################
indicators[,1] <- apply(adne, 2, sum)

##########################################################
## 2 - Functional diversity FD
##########################################################
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Get the functional groups of these species
  fd_i <- as.factor(traits[which(traits$Species %in% s_i), "GF"])
  
  # Number of unique functional groups
  indicators[i,2] <- nlevels(fd_i)
}

##########################################################
## 3 - Large Reef Fish Index - LFI
## 5 - Ratio Demerso-pelagic / benthic
## 7 - Chondrichtyen species
## 8 - Commercial species
## 9 - Highly commercial species 
##########################################################
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Calculate indicators
  indicators[i,"LFI"] <- sum(traits[which(traits$Species %in% s_i), "LRFI"]) 
  indicators[i,"DP_B_ratio"] <- sum(traits[which(traits$Species %in% s_i), "DP"]) / (sum(traits[which(traits$Species %in% s_i), "B"])+1)
  indicators[i,"Chondri"] <- sum(traits[which(traits$Species %in% s_i), "SHarK"])
  indicators[i,"Commercial"] <- sum(traits[which(traits$Species %in% s_i), "all_commercial_level"])
  indicators[i,"High_commerc"] <- sum(traits[which(traits$Species %in% s_i), "highly_commercial_only"])
}

###########################################################
## 4 Cryptobenthic (definition Brandl et al. 2018)
# Brandl SJ, Goatley CHR, Bellwood DR, Tornabene L. 2018 The hidden half: ecology and evolution of cryptobenthic fishes on coral reefs. Biol. Rev. 93, . (doi:10.1111/brv.12423))
###########################################################
crypto_families = c("Tripterygiidae", "Grammatidae", "Creediidae", "Aploactinidae", "Gobiidae", 
                    "Chaenopsidae", "Gobiesocidae", "Labrisomidae", "Pseudochromidae", "Bythitidae", 
                    "Plesiopidae", "Blenniidae", "Apogonidae", "Callionymidae", "Opistognathidae", "Syngnathidae")

traits <- traits %>%
  mutate(crypto_Brandl = if_else(Family %in% crypto_families, 1,0))

for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Calculate indicator
  indicators[i,"Crypto"] <- sum(traits[which(traits$Species %in% s_i), "crypto_Brandl"]) 
}

##########################################################
## 11 - Vulnerability
##########################################################
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # calculate indicators
  indicators[i,"Vulner"] <- mean(traits[which(traits$Species %in% s_i), "Vulnerability"], na.rm=T)
}

##########################################################
## 6 - Red List IUCN
##########################################################
## Count all species listed VU, EN or CR on the Red List of Threatened Species

### Make a dummy variable for IUCN categories
traits <- dummy_cols(traits, select_columns = 'IUCN_Red_List_Category')

## Calculate indicator
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Number of species per category
  VU <- sum(traits[which(traits$Species %in% s_i), "IUCN_Red_List_Category_VU"], na.rm=T)
  EN <- sum(traits[which(traits$Species %in% s_i), "IUCN_Red_List_Category_EN"], na.rm=T)
  CR <- sum(traits[which(traits$Species %in% s_i), "IUCN_Red_List_Category_CR"], na.rm=T)
  
  # Calculate indicator
  indicators[i,"RedList"] <- VU + EN + CR
}

##########################################################
## 10 - Phylogenetic Diversity - PD
##########################################################
# Retrieve the phylogeny of only native reef species across all three oceans.
phy <- fishtree_phylogeny(species = species)

plot(phy, show.tip.label = FALSE)
tiplabels(tip = which(phy$tip.label %in% species),
          pch=19, cex=2)

rownames(adne) <- gsub(" ", "_", rownames(adne), fixed = TRUE)

## check that phylogeny and data have matching names
#nc <- geiger::name.check(phy, adne) # 24 species not in tree (chondrychtyans + synonyms)

# Manually check synonyms and find the species
species[species == "Mullus barbatus"] <- "Mullus barbatus barbatus"
rownames(adne)[rownames(adne) == "Mullus_barbatus"] <- "Mullus_barbatus_barbatus"

species[species == "Diplodus sargus"] <- "Diplodus sargus sargus"
rownames(adne)[rownames(adne) == "Diplodus_sargus"] <- "Diplodus_sargus_sargus"

species[species == "Diplodus cervinus"] <- "Diplodus cervinus cervinus"
rownames(adne)[rownames(adne) == "Diplodus_cervinus"] <- "Diplodus_cervinus_cervinus"

species[species == "Chelon auratus"] <- "Liza aurata"
rownames(adne)[rownames(adne) == "Chelon_auratus"] <- "Liza_aurata"

species[species == "Chelon ramada"] <- "Liza ramada"
rownames(adne)[rownames(adne) == "Chelon_ramada"] <- "Liza_ramada"

# Retrieve the missing phylogeny 
phy <- fishtree_phylogeny(species = species, type="phylogram")
nc <- geiger::name.check(phy, adne) # 19 species not in the tree

# Remove from the data the species that are not in the tree
adne2 <- adne[which(rownames(adne) %in% nc$data_not_tree == F),]

# Transpose the ADNe matrix 
adne2 <- t(adne2)

# prune the tree
prunedTree <- prune.sample(adne2,phy)

# Calculate Faith's PD
pd.result <- pd(adne2, prunedTree, include.root=T)

# Add PD to indicator dataframe
indicators[,"PD"] <- pd.result$PD/pd.result$SR  #percentage of species richness with only species that are in the tree

# Save the indicator table
write.csv(indicators, file="01_indicators/indicators_updated.csv")


