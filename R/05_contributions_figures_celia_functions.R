# function for evaluating covariates importance

pal_contribution <- PNWColors::pnw_palette("Bay", 1, type = "discrete")
plot_data = bind_files
fitted_model = "GLM"
color = pal_contribution
labs_y = ""
labs_fill = ""
ylim = c(0,3)
legend.position = "none"

covariates_importance_function <- function(plot_data,
                                           fitted_model,
                                           color,
                                           labs_y,
                                           labs_fill,
                                           ylim,
                                           legend.position
){
  
  require(ggplot2)
  
  # covariates relative importance by mean
  
  plot_level <- fitted_model
  only_model <- plot_data |>
    dplyr::filter(fitted_model == plot_level)
  
  # only_model <- bind_files |>
  #   dplyr::filter(species_name %in% bind_files[fitted_model == plot_level,1]$species_name)
  
  ENV <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("protection", "tempDay", "chloroDay", "logland", "habitat_div", "mean_bathy"),]$Dropout_loss})
  ENV_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("protection", "tempDay", "chloroDay", "logland", "habitat_div", "mean_bathy"),]$sd_dropout_loss})
  ENV <- do.call(rbind, ENV)
  ENV_sd <- do.call(rbind, ENV_sd)
  ENV <- dplyr::tibble(value = colMeans(ENV),
                       sd = colMeans(ENV_sd),
                       var = c("protection", "tempDay", "chloroDay", "logland", "habitat_div", "mean_bathy"),
                       VAR = rep("ENV", 6),
                       plot_level = rep(plot_level, 6))
  
  
  # SOC <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),]$Dropout_loss})
  # SOC_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),]$sd_dropout_loss})
  # SOC <- do.call(rbind, SOC)
  # SOC_sd <- do.call(rbind, SOC_sd)
  # SOC <- dplyr::tibble(value = colMeans(SOC),
  #                      sd = colMeans(SOC_sd),
  #                      var = c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),
  #                      VAR = rep("HUM", 8),
  #                      plot_level = rep(plot_level, 8))
  # SOC[SOC$var == "n_fishing_vessels",3]$var <- "fishing_vessels_density"
  # 
  # HAB <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),]$Dropout_loss})
  # HAB_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),]$sd_dropout_loss})
  # HAB <- do.call(rbind, HAB)
  # HAB_sd <- do.call(rbind, HAB_sd)
  # HAB <- dplyr::tibble(value = colMeans(HAB),
  #                      sd = colMeans(HAB_sd),
  #                      var = c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),
  #                      VAR = rep("HAB", 8),
  #                      plot_level = rep(plot_level, 8))

  
  
  cont <- ENV 
  # |>  
  #   dplyr::full_join(HAB) |> 
  #   dplyr::full_join(SOC)
  
  # importance_plot <- ggplot(cont) +
  #   geom_col(aes(x = reorder(var, value), y = value, fill = VAR)) +
  #   geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
  #                 position = position_dodge(.9)) +
  #   scale_fill_manual(values = c("ENV" = color[1],
  #                                "HUM" = color [3],
  #                                "HAB" = color [2])) +
  #   theme_bw() +
  #   coord_flip(ylim = ylim) +
  #   facet_grid(~ plot_level) +
  #   labs(y = labs_y, x = "", fill = labs_fill) +
  #   theme(legend.position = legend.position) +
  #   theme(title = element_text(size = 18),
  #         axis.text.x = element_text(size = 10),
  #         axis.text.y = element_text(size = 12),
  #         axis.title = element_text(size = 14),
  #         legend.text = element_text(size = 10),
  #         strip.text.x = element_text(size = 20),
  #         strip.text.y = element_text(size = 20),
  #         strip.background = element_blank(),
  #         panel.background = element_rect(fill = "white", colour = "grey50",
  #                                         size = 1, linetype = "solid"),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank())
  
  
  importance_plot <- ggplot(cont) +
    geom_col(aes(x = reorder(var, value), y = value, fill = VAR)) +
    geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
                  position = position_dodge(.9)) +
    scale_fill_manual(values = color[1]) +
    theme_bw() +
    coord_flip(ylim = ylim) +
    facet_grid(~ plot_level) +
    labs(y = labs_y, x = "", fill = labs_fill) +
    theme(legend.position = legend.position) +
    theme(title = element_text(size = 18),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 10),
          strip.text.x = element_text(size = 20),
          strip.text.y = element_text(size = 20),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50",
                                          size = 1, linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
}

# 
# var_max_function <- function(plot_data,
#                              fitted_model
# ){
#   
#   plot_level <- fitted_model
#   only_model <- plot_data |> 
#     dplyr::filter(fitted_model == plot_level)
#   only_model <- only_model |> 
#     dplyr::filter(species_name %in% best_models[best_models$best_model == plot_level,1]$species_name)
#   
#   ENV <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("max_1year_analysed_sst", "max_5year_degree_heating_week", "mean_1year_chl", "mean_1year_so_mean", "mean_7days_analysed_sst", "mean_7days_chl", "min_1year_analysed_sst", "min_5year_ph"),]$Dropout_loss})
#   ENV_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("max_1year_analysed_sst", "max_5year_degree_heating_week", "mean_1year_chl", "mean_1year_so_mean", "mean_7days_analysed_sst", "mean_7days_chl", "min_1year_analysed_sst", "min_5year_ph"),]$sd_dropout_loss})
#   ENV <- do.call(rbind, ENV)
#   ENV_sd <- do.call(rbind, ENV_sd)
#   ENV <- dplyr::tibble(species_name = only_model$species_name,
#                        value = rowMeans(ENV),
#                        sd = rowMeans(ENV_sd),
#                        var = rep("ENV",nrow(ENV)),
#                        plot_level = rep(plot_level, nrow(ENV)))
  
  # SOC <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),]$Dropout_loss})
  # SOC_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),]$sd_dropout_loss})
  # SOC <- do.call(rbind, SOC)
  # SOC_sd <- do.call(rbind, SOC_sd)
  # SOC <- dplyr::tibble(species_name = only_model$species_name,
  #                      value = rowMeans(SOC),
  #                      sd = rowMeans(SOC_sd),
  #                      var = rep("HUM",nrow(SOC)),
  #                      plot_level = rep(plot_level, nrow(SOC)))
  # 
  # HAB <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),]$Dropout_loss})
  # HAB_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),]$sd_dropout_loss})
  # HAB <- do.call(rbind, HAB)
  # HAB_sd <- do.call(rbind, HAB_sd)
  # HAB <- dplyr::tibble(species_name = only_model$species_name,
  #                      value = rowMeans(HAB),
  #                      sd = rowMeans(HAB_sd),
  #                      var = rep("HAB",nrow(HAB)),
  #                      plot_level = rep(plot_level,nrow(HAB)))
  # 
  # cont <- ENV 
  # |> 
  #   dplyr::full_join(HAB) |> 
  #   dplyr::full_join(SOC)
  
  # best <- dplyr::tibble(species_name = unique(cont$species_name),
  #                       ENV = cont[cont$var == "ENV",2],
  #                       SOC = cont[cont$var == "HUM",2],
  #                       HAB = cont[cont$var == "HAB",2])
  # best <- dplyr::inner_join(best, best_models, by = "species_name")
  # best <- as.matrix(best)
  # 
  # best <- lapply(1:nrow(best), function(i) {
  #   test <- best[i,]
  #   testt <- which.max(test[2:4])
  #   testtt <- dplyr::tibble(species_name = test[1],
  #                           varmax = testt,
  #                           plot_level = test[5])
  # })
  
  # best <- do.call(rbind, best)
  # best$varmax <- as.character(best$varmax)
  # best[best$varmax == "1" ,2] <- "ENV"
  # best[best$varmax == "2" ,2] <- "HUM"
  # best[best$varmax == "3" ,2] <- "HAB"
  # 
  # cont <- cont |> 
  #   dplyr::inner_join(best, by = c("species_name", "plot_level"))
  # 
#   var_max <- cont |> 
#     dplyr::group_by(plot_level, varmax) |> 
#     dplyr::summarise(n = dplyr::n()/3)
#   var_max <- var_max |> 
#     dplyr::rename(VAR = varmax)
#   
# }

# merged_covariates_importance_function <- function(plot_data,
#                                                   fitted_model,
#                                                   color,
#                                                   labs_y,
#                                                   labs_fill,
#                                                   legend.position,
#                                                   mul
# ){
  
  # require(ggplot2)
  
  # # covariates relative importance by median
  # 
  # plot_level <- fitted_model
  # only_model <- plot_data |> 
  #   dplyr::filter(fitted_model == plot_level)
  # only_model <- only_model |> 
  #   dplyr::filter(species_name %in% best_models[best_models$best_model == plot_level,1]$species_name)
  # 
  # ENV <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("max_1year_analysed_sst", "max_5year_degree_heating_week", "mean_1year_chl", "mean_1year_so_mean", "mean_7days_analysed_sst", "mean_7days_chl", "min_1year_analysed_sst", "min_5year_ph"),]$Dropout_loss})
  # ENV_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("max_1year_analysed_sst", "max_5year_degree_heating_week", "mean_1year_chl", "mean_1year_so_mean", "mean_7days_analysed_sst", "mean_7days_chl", "min_1year_analysed_sst", "min_5year_ph"),]$sd_dropout_loss})
  # ENV <- do.call(rbind, ENV)
  # ENV_sd <- do.call(rbind, ENV_sd)
  # ENV <- dplyr::tibble(value = colMeans(ENV),
  #                      sd = colMeans(ENV_sd),
  #                      var = c("max_1year_analysed_sst", "max_5year_degree_heating_week", "mean_1year_chl", "mean_1year_so_mean", "mean_7days_analysed_sst", "mean_7days_chl", "min_1year_analysed_sst", "min_5year_ph"),
  #                      VAR = rep("ENV", 8),
  #                      plot_level = rep(plot_level, 8))
  # 
  # SOC <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),]$Dropout_loss})
  # SOC_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),]$sd_dropout_loss})
  # SOC <- do.call(rbind, SOC)
  # SOC_sd <- do.call(rbind, SOC_sd)
  # SOC <- dplyr::tibble(value = colMeans(SOC),
  #                      sd = colMeans(SOC_sd),
  #                      var = c("effectiveness", "gdp", "gravtot2", "hdi", "n_fishing_vessels", "natural_ressource_rent", "neartt", "ngo"),
  #                      VAR = rep("HUM", 8),
  #                      plot_level = rep(plot_level, 8))
  # 
  # HAB <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),]$Dropout_loss})
  # HAB_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),]$sd_dropout_loss})
  # HAB <- do.call(rbind, HAB)
  # HAB_sd <- do.call(rbind, HAB_sd)
  # HAB <- dplyr::tibble(value = colMeans(HAB),
  #                      sd = colMeans(HAB_sd),
  #                      var = c("Rock_500m", "Rubble_500m", "Sand_500m", "coral", "coral_algae_500m", "coralline_algae", "depth", "reef_extent"),
  #                      VAR = rep("HAB", 8),
  #                      plot_level = rep(plot_level, 8))
  # 
  # cont <- ENV |>  
  #   dplyr::full_join(HAB) |> 
  #   dplyr::full_join(SOC)
  # 
  # var_max <- var_max_function(plot_data = bind_files,
  #                             fitted_model = plot_level)
  
  # #merge contribution per var and model
  #
  # cont_merge <- cont |>
  #   dplyr::group_by(VAR, plot_level) |>
  #   dplyr::summarise(value = mean(value),
  #                    sd = mean(sd))
  #
  # cont_merge <- dplyr::inner_join(cont_merge, var_max, by = c("plot_level", "VAR"))
  #
  # merged_importance_plot <- ggplot(cont_merge) +
  #   geom_col(aes(x = reorder(VAR, value), y = value, fill = VAR)) +
  #   geom_errorbar(aes(x = VAR, y = value, ymin=value-sd, ymax=value+sd), width=.1,
  #                 position=position_dodge(.9)) +
  #   geom_text(aes(x = VAR, y = value+mul*sd, label = n), size = 5) +
  #   scale_fill_manual(values = c("ENV" = color[1],
  #                                "HUM" = color[3],
  #                                "HAB" = color[2])) +
  #   theme_bw() +
  #   coord_flip() +
  #   facet_grid(~plot_level) +
  #   labs(y = labs_y, x = "", fill = labs_fill) +
  #   theme(legend.position = legend.position) +
  #   theme(title = element_text(size = 18),
  #         axis.text.x = element_text(size = 10),
  #         axis.text.y = element_text(size = 12),
  #         axis.title = element_text(size = 15),
  #         legend.text = element_text(size = 10),
  #         strip.text.x = element_text(size = 20),
  #         strip.text.y = element_text(size = 20),
  #         strip.background = element_blank(),
  #         panel.background = element_rect(fill = "white", colour = "grey50",
  #                                         size = 1, linetype = "solid"),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank())

# }
