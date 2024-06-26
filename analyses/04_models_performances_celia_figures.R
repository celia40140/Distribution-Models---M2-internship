# script to produce figures that evaluate model performances

library(ggplot2)

# source functions ----

source("R/04_model_performance_functions.R")

# Set palette colors for performance figures

pal_best <- PNWColors::pnw_palette("Bay", 6, type = "continuous")
pal_perf <- PNWColors::pnw_palette("Bay", 6, type = "continuous")

# select best fitted model for each model type based on a concensus metrics ----

metrics <- c("Intercept", "Slope", "Pearson", "Spearman")

# read data

load("outputs/model_assessment_validation/validation.Rdata")
all_assessments_SCV <- model_assessment
all_assessments_SCV <- do.call(rbind, all_assessments_SCV)

# select only the columns to be used later 
all_assessments_SCV <- all_assessments_SCV |> 
  
  dplyr::select(fitted_model, species_name, metrics)

# estimate for each species the best model based on performance metrics  
best_models <- all_assessments_SCV |> 
  aggregate_metrics(., 
                    metrics = c("Intercept", "Slope", "Pearson", "Spearman")) |>
  # find the best fitting model for each species within each fitted_model
  dplyr::group_by(species_name) |> 
  dplyr::do(best_model = .$fitted_model[which.max(.$discrimination)]) |> 
  tidyr::unnest(cols = c('best_model'))

# save(best_models, file = "outputs/best_models.Rdata")

#### Best Model plot ####

best_models_pr <- best_models |>  
  dplyr::group_by(best_model) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::mutate(pr = (n*100)/sum(n))

best_model <- best_models_pr |> 
  dplyr::mutate(best_model = forcats::fct_relevel(best_model, "GLM", "GAM", "SPAMM", "GBM", "RF", "SPRF")) |> 
  ggplot(aes(x = best_model, y = pr, fill = best_model)) +
  geom_bar(width = 0.8, stat = 'identity') +
  scale_fill_manual(values = pal_best) +
  labs(x = "Statistic methods", y = "Best model (%)", fill = "Method", title = "B") +
  theme(title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 25),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# produce histograms of model performance for best models ----

p_level <- unique(all_assessments_SCV$fitted_model)

perf_models_all <- all_assessments_SCV |> 
  dplyr::summarise_at(vars(Intercept:Spearman), list(function(x) list(Q0.05 = round(quantile(x, 0.05, na.rm = T), 2),
                                                                      IQR0.25 = round(quantile(x, 0.25, na.rm = T), 2),
                                                                      median  = round(median(x, na.rm = T), 2),
                                                                      IQR0.75 = round(quantile(x, 0.75, na.rm = T), 2),
                                                                      Q0.95 = round(quantile(x, 0.95, na.rm = T), 2)))) |> 
  dplyr::mutate(summary_value = c('Q0.05','IQR0.25', 'median', 'IQR0.75', 'Q0.95')) |> 
  tidyr::unnest() |> 
  dplyr::group_by(summary_value) |> 
  dplyr::select(Intercept:Spearman) |> 
  t() |> 
  data.frame()

perf_models_all <- perf_models_all |> 
  dplyr::mutate(metric = rownames(perf_models_all)) |>
  dplyr::select(metric, X1:X5)

perf_models_details <- parallel::mclapply(1:length(unique(all_assessments_SCV$fitted_model)), function(i) {
  
  p_level <- p_level[i]
  
  perf_models <- all_assessments_SCV |> 
    dplyr::filter(fitted_model == p_level) |> 
    dplyr::summarise_at(vars(Intercept:Spearman), list(function(x) list(Q0.05 = round(quantile(x, 0.05, na.rm = T), 2),
                                                                        IQR0.25 = round(quantile(x, 0.25, na.rm = T), 2),
                                                                        median  = round(median(x, na.rm = T), 2),
                                                                        IQR0.75 = round(quantile(x, 0.75, na.rm = T), 2),
                                                                        Q0.95 = round(quantile(x, 0.95, na.rm = T), 2)))) |> 
    dplyr::mutate(summary_value = c('Q0.05', 'IQR0.25', 'median', 'IQR0.75', 'Q0.95')) |> 
    tidyr::unnest() |> 
    dplyr::group_by(summary_value) |> 
    dplyr::select(Intercept:Spearman) |> 
    t() |> 
    data.frame() 
  
  perf_models <- perf_models |> 
    dplyr::mutate(metric = rownames(perf_models)) |> 
    dplyr::select(metric, X1:X5)
  
}, mc.cores = 1)
names(perf_models_details) <- p_level

# Select only the best model for each species

best_assessments_SCV <- dplyr::inner_join(all_assessments_SCV, best_models, by = "species_name")
best_assessments_SCV <- best_assessments_SCV[best_assessments_SCV$fitted_model == best_assessments_SCV$best_model,]

perf_models_all_best <- best_assessments_SCV |> 
  dplyr::summarise_at(vars(Intercept:Spearman), list(function(x) list(Q0.05 = round(quantile(x, 0.05, na.rm = T), 2),
                                                                      IQR0.25 = round(quantile(x, 0.25, na.rm = T), 2),
                                                                      median  = round(median(x, na.rm = T), 2),
                                                                      IQR0.75 = round(quantile(x, 0.75, na.rm = T), 2),
                                                                      Q0.95 = round(quantile(x, 0.95, na.rm = T), 2)))) |> 
  dplyr::mutate(summary_value = c('Q0.05','IQR0.25', 'median', 'IQR0.75', 'Q0.95')) |> 
  tidyr::unnest() |> 
  dplyr::group_by(summary_value) |> 
  dplyr::select(Intercept:Spearman) |> 
  t() |> 
  data.frame() 

perf_models_all_best <- perf_models_all_best |> 
  dplyr::mutate(metric = rownames(perf_models_all_best)) |> 
  dplyr::select(metric, X1:X5)

# Manage data for performance plot

# performance_all <- all_assessments_SCV[,c(1,2,4:7)]
performance_all <- dplyr::tibble(species_name = rep(all_assessments_SCV$species_name, 4),
                                 value = c(all_assessments_SCV$Intercept, all_assessments_SCV$Slope, all_assessments_SCV$Pearson, all_assessments_SCV$Spearman),
                                 metrics = c(rep("Intercept", nrow(all_assessments_SCV)), 
                                             rep("Slope", nrow(all_assessments_SCV)),
                                             rep("Pearson", nrow(all_assessments_SCV)), 
                                             rep("Spearman", nrow(all_assessments_SCV))),
                                 model = rep(all_assessments_SCV$fitted_model, 4))

performance_all[performance_all$metrics == "Intercept",2] <- log10(performance_all[performance_all$metrics == "Intercept",2]$value + 1)
performance_all[performance_all$metrics == "Slope",2] <- log10(performance_all[performance_all$metrics == "Slope",2]$value + 1)

performance_best <- dplyr::tibble(species_name = rep(best_assessments_SCV$species_name, 4),
                                  value = c(best_assessments_SCV$Intercept, best_assessments_SCV$Slope, best_assessments_SCV$Pearson, best_assessments_SCV$Spearman),
                                  metrics = c(rep("Intercept", nrow(best_assessments_SCV)), 
                                              rep("Slope", nrow(best_assessments_SCV)),
                                              rep("Pearson", nrow(best_assessments_SCV)), 
                                              rep("Spearman", nrow(best_assessments_SCV))),
                                  best_model = rep(best_assessments_SCV$fitted_model, 4))

performance_best[performance_best$metrics == "Intercept",2] <- log10(performance_best[performance_best$metrics == "Intercept",2]$value + 1)
performance_best[performance_best$metrics == "Slope",2] <- log10(performance_best[performance_best$metrics == "Slope",2]$value + 1)

performance_all_best <- dplyr::full_join(performance_all, performance_best)
performance_all_best$cat <- NA

performance_all_best[which(performance_all_best$model == performance_all_best$best_model),6] <- "Best models"
performance_all_best[which(is.na(performance_all_best$cat) == TRUE),6] <- "All models"

plot_pearson <- performance_plot(performance_all_best,
                                 metrics_sel = "Pearson",
                                 slope = 0,
                                 intercept = 1,
                                 color = pal_perf,
                                 ylim = c(-0.5, 1),
                                 legend.position = 'none',
                                 plot_title = "")

plot_spearman <- performance_plot(performance_all_best,
                                  metrics_sel = "Spearman",
                                  slope = 0,
                                  intercept = 1,
                                  color = pal_perf,
                                  ylim = c(-0.5, 1),
                                  legend.position = c(0.5, 0.1),
                                  plot_title = "")

plot_slope <- performance_plot(performance_all_best,
                               metrics_sel = "Slope",
                               slope = 0,
                               intercept = 0.30103,
                               color = pal_perf,
                               ylim = c(-0.05, 0.4),
                               legend.position = "none",
                               plot_title = "")

plot_intercept <- performance_plot(performance_all_best,
                                   metrics_sel = "Intercept",
                                   slope = 0,
                                   intercept = 0,
                                   color = pal_perf,
                                   ylim = c(-1,10),
                                   legend.position = 'none',
                                   plot_title = "A")

all_plots <- patchwork::wrap_plots(plot_intercept, plot_slope, plot_pearson, plot_spearman)
all_plots <- all_plots / best_model

# ggplot2::ggsave("figures/plot_perf_best.pdf", all_plots, height = 18, width = 13)
# ggplot2::ggsave("figures/plot_perf_best.png", all_plots, height = 18, width = 13)

################## Plot performance-traits relationship

load("data/new_derived_data/species_count.Rdata")
sp_count <- sp_count |> 
  dplyr::rename(occurence = count)
sp_car <- read.csv("data/new_raw_data/Traits_tropical_spp_1906.csv", header = TRUE)
sp_car <- sp_car |> 
  dplyr::rename(species_name = Species)

sp_car <- sp_car[which(is.na(sp_car$MaxLength) == FALSE),]
sp_car <- sp_car[which(is.na(sp_car$Trophic_guild_name) == FALSE),]

sp_car$ML_cat <- NA
sp_car[sp_car$MaxLength > 0 & sp_car$MaxLength <= 20,]$ML_cat <- "0-20 cm"
sp_car[sp_car$MaxLength > 20 & sp_car$MaxLength <= 40,]$ML_cat <- "20-40 cm"
sp_car[sp_car$MaxLength > 40 & sp_car$MaxLength <= 60,]$ML_cat <- "40-60 cm"
sp_car[sp_car$MaxLength > 60 & sp_car$MaxLength <= 80,]$ML_cat <- "60-80 cm"
sp_car[sp_car$MaxLength > 80 & sp_car$MaxLength <= 300,]$ML_cat <- "80-300 cm"

sp_car[sp_car$Water.column == "Demersal",]$Water.column <- "demersal"
sp_car[sp_car$Water.column == "pelagic non-site attached",]$Water.column <- "pelagic"
sp_car[sp_car$Water.column == "pelagic site attached",]$Water.column <- "pelagic"

sp_car[sp_car$Habitat == "Coral",]$Habitat <- "coral"

sp_car[sp_car$Trophic_guild_name == "Herbivores Microvores Detritivores",]$Trophic_guild_name <- "herbivores"

best_count <- best_assessments_SCV |> 
  dplyr::inner_join(sp_count) |> 
  dplyr::inner_join(sp_car[,colnames(sp_car) %in% c("species_name", "MaxLength", "IUCN_status")])

plot_spear_count <- ggplot(best_count, aes(x = log10(occurence), y = Spearman)) +
  geom_point() +
  geom_smooth(method = "lm")

# ggsave(plot_spear_count, file = "figures/spearman_occurence.png")

summary(lm(formula = "Spearman ~ occurence", data = best_count))

plot_pear_count <- ggplot(best_count, aes(x = log10(occurence), y = Pearson)) +
  geom_point() +
  geom_smooth(method = "lm")

# ggsave(plot_pear_count, file = "figures/pearson_occurence.png")

summary(lm(formula = "Pearson ~ occurence", data = best_count))

best_trait <- best_assessments_SCV |> 
  dplyr::inner_join(sp_car)

best_trait |> 
  dplyr::group_by(ML_cat) |> 
  dplyr::summarise(n = dplyr::n())

best_trait |> 
  dplyr::group_by(Water.column) |> 
  dplyr::summarise(n = dplyr::n())

best_trait |> 
  dplyr::group_by(Habitat) |> 
  dplyr::summarise(n = dplyr::n())

best_trait |> 
  dplyr::group_by(Trophic_guild_name) |> 
  dplyr::summarise(n = dplyr::n())

kruskal_ML <- kruskal_test_function_perf(best_trait,
                                         "ML_cat",
                                         "Spearman")

if(kruskal_ML$statistics$p.chisq > 0.05){
  
  stop(print("No statistical differences among groups"))
  
}else{
  
  print("p.chisq < 0.05")
  
}

plot_MLclass_Spear <- ggplot(best_trait, aes(x = ML_cat, y = Spearman), group = ML_cat) +
  geom_boxplot() +
  geom_text(data = kruskal_ML$groups, aes_string(x = "trait", y = "quant", label = "groups"), size = 5, vjust = -0.5, hjust = -0.55) +
  labs(x = "Size class")

plot_ML_Spear <- ggplot(best_count, aes(x = MaxLength, y = Spearman), group = ML_cat) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Maximum length")

# ggsave(plot_MLclass_Spear, file = "figures/maximumlengthclass_spearman.png")
# ggsave(plot_ML_Spear, file = "figures/maximumlength_spearman.png")

kruskal_WC <- kruskal_test_function_perf(best_trait,
                                         "Water.column",
                                         "Spearman")

if(kruskal_WC$statistics$p.chisq > 0.05){
  
  stop(print("No statistical differences among groups"))
  
}else{
  
  print("p.chisq < 0.05")
  
}

plot_WC_Spear <- ggplot(best_trait, aes(x = Water.column, y = Spearman), group = Water.column) +
  geom_boxplot() +
  geom_text(data = kruskal_WC$groups, aes_string(x = "trait", y = "quant", label = "groups"), size = 5, vjust = -0.5, hjust = -0.55) +
  labs(x = "Water column")

# ggsave(plot_WC_Spear, file = "figures/watercolumn_spearman.png")

kruskal_HAB <- kruskal_test_function_perf(best_trait,
                                          "Habitat",
                                          "Spearman")

if(kruskal_HAB$statistics$p.chisq > 0.05){
  
  stop(print("No statistical differences among groups"))
  
}else{
  
  print("p.chisq < 0.05")
  
}

plot_HAB_Spear <- ggplot(best_trait, aes(x = Habitat, y = Spearman), group = Habitat) +
  geom_boxplot() +
  geom_text(data = kruskal_HAB$groups, aes_string(x = "trait", y = "quant", label = "groups"), size = 5, vjust = -0.5, hjust = -0.55)

# ggsave(plot_HAB_Spear, file = "figures/habitat_spearman.png")

kruskal_TR <- kruskal_test_function_perf(best_trait,
                                         "Trophic_guild_name",
                                         "Spearman")

if(kruskal_TR$statistics$p.chisq > 0.05){
  
  stop(print("No statistical differences among groups"))
  
}else{
  
  print("p.chisq < 0.05")
  
}

plot_TR_Spear <- ggplot(best_trait, aes(x = Trophic_guild_name, y = Spearman), group = Trophic_guild_name) +
  geom_boxplot() +
  geom_text(data = kruskal_TR$groups, aes_string(x = "trait", y = "quant", label = "groups"), size = 5, vjust = -0.5, hjust = -0.55)

# ggsave(plot_TR_Spear, file = "figures/trophicgroup_spearman.png", width = 10)


occ_ml_sp <- ggplot(best_count, aes(x = log10(occurence), y = MaxLength, color = Spearman)) + 
  geom_point() +
  geom_point(data = best_count[best_count$IUCN_status %in% "Thr",], colour = "red", pch = 21, size = 4, stroke = 1) +
  scale_colour_viridis_c()

best_count_log <- best_count |> 
  dplyr::mutate(occurence = log10(occurence))

glm_occ_ml_sp <- glm(formula = "Spearman ~ occurence * MaxLength", data = best_count_log)
summary(glm_occ_ml_sp)
with(summary(glm_occ_ml_sp), 1 - deviance/null.deviance)

vis_plot <- visreg::visreg2d(glm_occ_ml_sp, "occurence", "MaxLength", scale = "response", plot.type = "gg") + 
  geom_contour(aes(z = z), color = "black") +
  labs(title = "Spearman ~ occurence x maximum lenght", y = "Maximum length (cm)", x = "log10(Occurrence)", fill = "Spearman") +
  scale_fill_viridis_c(option = "viridis") +
  theme(title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave(vis_plot, file = "figures/visreg_occ_ml_sp.png", width = 8)
