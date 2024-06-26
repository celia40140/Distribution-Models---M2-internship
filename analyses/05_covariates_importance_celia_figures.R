# source functions ----

# plot_data = bind_files
# fitted_model = "GLM"
# color = pal_contribution
# labs_y = ""
# labs_fill = ""
# ylim = c(0,3)
# legend.position = "none"


library(patchwork)

source("R/05_contributions_figures_celia_functions.R")

pal_contribution <- PNWColors::pnw_palette("Bay", 3, type = "discrete")

# load("outputs/best_models.Rdata")

#### Covariates contribution plot ####

list_files_path <- list.files("outputs/occurrence_contribution", full.names = T)
bind_files <- lapply(1:length(list_files_path), function(i) {
  
  load(list_files_path[i])
  assign(paste0("model_", i), extracted_contributions)
  
})
bind_files <- do.call(rbind, bind_files)

##### Relative importance by mean

covariates_importance_GLM <- covariates_importance_function(plot_data = bind_files,
                                                            fitted_model = "GLM",
                                                            color = pal_contribution,
                                                            labs_y = "",
                                                            labs_fill = "",
                                                            ylim = c(0,0.15),
                                                            legend.position = "none")

covariates_importance_GAM <- covariates_importance_function(plot_data = bind_files,
                                                            fitted_model = "GAM",
                                                            color = pal_contribution,
                                                            labs_y = "",
                                                            labs_fill = "",
                                                            ylim = c(0,0.15),
                                                            legend.position = "none")

covariates_importance_SPAMM <- covariates_importance_function(plot_data = bind_files,
                                                              fitted_model = "SPAMM",
                                                              color = pal_contribution,
                                                              labs_y = "",
                                                              labs_fill = "",
                                                              ylim = c(0,0.15),
                                                              legend.position = "none")

covariates_importance_RF <- covariates_importance_function(plot_data = bind_files,
                                                           fitted_model = "RF",
                                                           color = pal_contribution,
                                                           labs_y = "",
                                                           labs_fill = "",
                                                           ylim = c(0,0.15),
                                                           legend.position = "none")

covariates_importance_GBM <- covariates_importance_function(plot_data = bind_files,
                                                            fitted_model = "GBM",
                                                            color = pal_contribution,
                                                            labs_y = "",
                                                            labs_fill = "",
                                                            ylim = c(0,0.15),
                                                            legend.position = "none")

covariates_importance_SPRF <- covariates_importance_function(plot_data = bind_files,
                                                             fitted_model = "SPRF",
                                                             color = pal_contribution,
                                                             labs_y = "",
                                                             labs_fill = "",
                                                             ylim = c(0,0.15),
                                                             legend.position = "none")

# covariates_importance_all <- (covariates_importance_GLM + covariates_importance_GAM) / (covariates_importance_SPAMM + covariates_importance_RF) / (covariates_importance_GBM + covariates_importance_SPRF)

# ggsave("figures/covariates_importance_all.pdf", covariates_importance_all, height = 15, width = 11)
# ggsave("figures/covariates_importance_all.png", covariates_importance_all, height = 15, width = 11)
# 
# ggsave("outputs/figures/covariates_importance/covariates_importance_glm.svg", covariates_importance_GLM, height = 10, width = 11)
# 


# Arrange plots in a grid
grid_arrange <- gridExtra::grid.arrange(
  covariates_importance_GLM,
  covariates_importance_GAM,
  covariates_importance_SPAMM,
  covariates_importance_RF,
  covariates_importance_GBM,
  covariates_importance_SPRF,
  ncol = 2  # Adjust the number of columns as needed
)

# Save the grid of plots
save_directory <- "/home/celia/Documents/M2_internship/celia_project/celia_project/outputs/figures/covariates_importance/"

save_filename <- paste0(save_directory, "all_plots_grid" , ".svg")
ggsave(save_filename, grid_arrange, device = "svg", width = 15, height = 10)

save_filename_bis <- paste0(save_directory, "all_plots_grid", ".pdf")
ggsave(save_filename_bis, grid_arrange, device = "pdf", width = 15, height = 10)


# ##### Relative importance by median
# 
# merged_covariates_importance_GLM <- merged_covariates_importance_function(plot_data = bind_files,
#                                                                           fitted_model = "GLM",
#                                                                           color = pal_contribution,
#                                                                           labs_y = "",
#                                                                           labs_fill = "",
#                                                                           legend.position = "none",
#                                                                           mul = 2)
# 
# merged_covariates_importance_GAM <- merged_covariates_importance_function(plot_data = bind_files,
#                                                                           fitted_model = "GAM",
#                                                                           color = pal_contribution,
#                                                                           labs_y = "",
#                                                                           labs_fill = "",
#                                                                           legend.position = "none",
#                                                                           mul = 2)
# 
# merged_covariates_importance_SPAMM <- merged_covariates_importance_function(plot_data = bind_files,
#                                                                             fitted_model = "SPAMM",
#                                                                             color = pal_contribution,
#                                                                             labs_y = "",
#                                                                             labs_fill = "",
#                                                                             legend.position = "none",
#                                                                             mul = 2)
# 
# merged_covariates_importance_RF <- merged_covariates_importance_function(plot_data = bind_files,
#                                                                          fitted_model = "RF",
#                                                                          color = pal_contribution,
#                                                                          labs_y = "",
#                                                                          labs_fill = "",
#                                                                          legend.position = "none",
#                                                                          mul = 3)
# 
# merged_covariates_importance_GBM <- merged_covariates_importance_function(plot_data = bind_files,
#                                                                           fitted_model = "GBM",
#                                                                           color = pal_contribution,
#                                                                           labs_y = "Relative importance (RMSE)",
#                                                                           labs_fill = "",
#                                                                           legend.position = "none",
#                                                                           mul = 3)
# 
# merged_covariates_importance_SPRF <- merged_covariates_importance_function(plot_data = bind_files,
#                                                                            fitted_model = "SPRF",
#                                                                            color = pal_contribution,
#                                                                            labs_y = "Relative importance (RMSE)",
#                                                                            labs_fill = "",
#                                                                            legend.position = c(0.8, 0.18),
#                                                                            mul = 3)

# merged_covariates_importance <- (merged_covariates_importance_GLM + merged_covariates_importance_GAM) / (merged_covariates_importance_SPAMM + merged_covariates_importance_RF) / (merged_covariates_importance_GBM + merged_covariates_importance_SPRF)
# 
# ggsave("figures/merged_covariates_importance.pdf", merged_covariates_importance, height = 15, width = 11)
# ggsave("figures/merged_covariates_importance.png", merged_covariates_importance, height = 15, width = 11)
# 


