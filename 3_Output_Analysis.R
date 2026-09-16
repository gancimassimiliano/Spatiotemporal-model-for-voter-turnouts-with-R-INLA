library(scales)
library(ggplot2)
library(dplyr)
library(sf)

# ==============================================================================
# 3.1: MODEL SELECTION
# ==============================================================================

load("C:/Users/massi/OneDrive/Desktop/Spatiotemporal-model-for-voter-turnouts-with-R-INLA/Outputs.RData")

# 1. Store the models in a named list for clean extraction
model_list <- list(
  "Baseline Model (Baseline No Space-Time Effects)" = model_baseline,
  "Model 0 (Spatiotemporal No Interactions)" = model0,
  "Model 1 (TYPE I: iid x iid)" = model1,
  "Model 2 (Type II: iid x ar1)" = model2,
  "Model 3 (Type III: besag x iid)" = model3,
  "Model 4 (Type IV: besag x ar1)" = model4,
  "Model 5 (Bernardinelli)" = model5,
  "Model 6 (Bernardinelli (No Laws))" = model6)


### 3.1.1. WAIC and DIC ----

# 2. Extract the metrics into a data frame
selection_table <- data.frame(
  Model = names(model_list),
  
  # Extract DIC and its effective number of parameters
  DIC = sapply(model_list, function(m) m$dic$dic),
  p_eff_DIC = sapply(model_list, function(m) m$dic$p.eff),
  
  # Extract WAIC and its effective number of parameters
  WAIC = sapply(model_list, function(m) m$waic$waic),
  p_eff_WAIC = sapply(model_list, function(m) m$waic$p.eff)
)

# 4. Remove rownames for a cleaner print in the console
rownames(selection_table) <- NULL

# Print the final table
print(selection_table)

### 3.1.2. RESIDUAL ANALYSIS ----

## 1. Extract CPO failures for all models
# A failure value > 0 indicates INLA struggled to compute the cross-validated predictive probability
cpo_summary <- data.frame(
  Model = names(model_list),
  CPO_Failures = sapply(model_list, function(m) sum(m$cpo$failure > 0, na.rm = TRUE))
)

rownames(cpo_summary) <- NULL
print("Summary of CPO Failures:")
print(cpo_summary)

## 2. Dunn-Smith Quantile Residuals and Q-Q Plot (FIT / CALIBRATION)

library(ggplot2)
library(dplyr)

model_list <- list(
  #"Baseline Model (Baseline No Space-Time Effects)" = model_baseline,
  #"Model 0 (Spatiotemporal No Interactions)" = model0,
  "Model 1 (TYPE I: iid x iid)" = model1,
  "Model 2 (Type II: iid x ar1)" = model2,
  "Model 3 (Type III: besag x iid)" = model3,
  "Model 4 (Type IV: besag x ar1)" = model4,
  "Model 5 (Bernardinelli)" = model5,
  "Model 6 (Bernardinelli (No Laws))" = model6)

diagnostic_df <- data.frame()

for (m in names(model_list)) {
  model_obj <- model_list[[m]]
  
  # Extract fitted values subset to the observed data
  fitted_vals <- model_obj$summary.fitted.values$mean[1:nrow(df_continuous)]
  
  # Extract PIT values, squeeze to prevent infinite bounds, and convert to Quantile Residuals
  pit_values <- model_obj$cpo$pit[1:nrow(df_continuous)]
  pit_squeezed <- pmax(pmin(pit_values, 0.9999), 0.0001)
  quantile_residuals <- qnorm(pit_squeezed)
  
  # Bind into a temporary dataframe
  temp_df <- data.frame(
    Fitted = fitted_vals, 
    Residuals = quantile_residuals, # Using quantile residuals instead of raw response residuals
    Model = m
  )
  
  diagnostic_df <- rbind(diagnostic_df, temp_df)
}

plot_qq_all <- ggplot(diagnostic_df, aes(sample = Residuals)) +
  stat_qq(color = "grey30", alpha = 0.4, size = 1) +
  stat_qq_line(color = "black", linewidth = 1) +
  facet_wrap(~ Model, ncol = 3) +
  theme_minimal() +
  labs(
    x = "Theoretical Quantiles",
    y = "Randomized Quantiles (Residuals)"
  ) +
  theme(strip.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(face = "bold", size = 12))

print(plot_qq_all)

## 3. Plotting Residuals vs Fitted values (HOMOSCEDASTICITY & DISPERSION)

# Preserve the exact order of the models for plotting
diagnostic_df$Model <- factor(diagnostic_df$Model, levels = names(model_list))

# Generate a single faceted plot using the Quantile Residuals already in diagnostic_df
plot_homo_all <- ggplot(diagnostic_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "grey30", size = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 1) + # Changed to solid to contrast with boundaries
  geom_hline(yintercept = c(-1.96, 1.96), color = "black", linetype = "dashed", linewidth = 0.8) + # Added +/- 2 dashed lines
  geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 1) +
  facet_wrap(~ Model, ncol = 3) +
  theme_minimal() +
  labs(x = "Fitted Values (Posterior Mean)", 
       y = "Randomized Quantiles (Residuals)") +
  theme(strip.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(face = "bold", size = 12))

print(plot_homo_all)

# 3. Calculate Empirical Coverage (The Dispersion Check)
dispersion_check <- diagnostic_df %>%
  group_by(Model) %>%
  summarise(
    Total_Observations = n(),
    
    # Check 95% Interval
    Outside_2SD = sum(abs(Residuals) > 1.96),
    Percent_Outside_2SD = round((Outside_2SD / Total_Observations) * 100, 2),
    
    # Check 99.7% Interval (Extreme Outliers)
    Outside_3SD = sum(abs(Residuals) > 3.0),
    Percent_Outside_3SD = round((Outside_3SD / Total_Observations) * 100, 3)
  )

print("Empirical Dispersion Check (Expected: ~5.00% outside 2SD, ~0.3% outside 3SD)")
print(dispersion_check)

## 4. SPATIAL & TEMPORAL AUTOCORRELATION TESTS
library(spdep)

# Compile the quantile residuals with strict spatial and temporal identifiers
res_data <- df_continuous %>%
  select(id_space_main, ANNO) %>%
  # Duplicate the layout for each model by joining the diagnostic_df
  # Assumes diagnostic_df was built sequentially mirroring df_continuous
  mutate(RowID = row_number()) %>%
  left_join(
    diagnostic_df %>% 
      group_by(Model) %>% 
      mutate(RowID = row_number()) %>% 
      ungroup(),
    by = "RowID"
  ) %>%
  select(-RowID)

# --- SPATIAL AUTOCORRELATION: Year-by-Year Moran's I ---
listw_prov <- nb2listw(nb_prov, style = "W", zero.policy = TRUE)
election_years <- unique(sort(df_continuous$ANNO))

spatial_ac_results <- data.frame()

for (m in names(model_list)) {
  for (yr in election_years) {
    df_yr <- res_data %>% 
      filter(Model == m, ANNO == yr) %>% 
      arrange(id_space_main)
    
    m_test <- moran.test(df_yr$Residuals, listw_prov, zero.policy = TRUE)
    
    spatial_ac_results <- bind_rows(spatial_ac_results, data.frame(
      Model = m,
      Year = yr,
      Morans_I = m_test$estimate[1],
      p_value = m_test$p.value
    ))
  }
}

spatial_summary <- spatial_ac_results %>%
  group_by(Model) %>%
  summarise(
    Avg_Morans_I = round(mean(Morans_I), 4),
    Years_Significant = sum(p_value < 0.05) 
  )

print("Spatial Autocorrelation (Moran's I) Summary:")
print(spatial_summary)

# --- TEMPORAL AUTOCORRELATION: Lag-1 Correlation ---
temporal_summary <- res_data %>%
  arrange(Model, id_space_main, ANNO) %>%
  group_by(Model, id_space_main) %>%
  mutate(Residuals_lag1 = lag(Residuals)) %>%
  ungroup() %>%
  filter(!is.na(Residuals_lag1)) %>%
  group_by(Model) %>%
  summarise(
    Lag1_Correlation = round(cor(Residuals, Residuals_lag1, use = "complete.obs"), 4)
  )

print("Temporal Autocorrelation (Lag-1) Summary:")
print(temporal_summary)

# ==============================================================================
# 3.2: THE OPTIMAL SPATIO-TEMPORAL MODEL
# ==============================================================================

# Now we focus on Model 4: Type IV Knorr-Held interaction

### A. FIXED EFFECTS ----
fixed_raw <- as.data.frame(model4$summary.fixed)

fixed_table <- fixed_raw %>%
  # Convert rownames to a column so we can filter by the covariate names
  tibble::rownames_to_column(var = "Covariate") %>%
  filter(Covariate %in% c("Density_z", "Aging_z", "Education_z", "Employment_z")) %>%
  select(Covariate, mean, sd, `0.025quant`, `0.975quant`, mode) %>%
  rename(
    `Posterior Mean` = mean,
    `Posterior SD` = sd,
    `0.025 Quantile` = `0.025quant`,
    `0.975 Quantile` = `0.975quant`,
    `Posterior Mode` = mode
  )

print(fixed_table %>% mutate_if(is.numeric, round, 2))

### B. HYPERPARAMETERS ----
hyper_raw <- model4$summary.hyperpar

hyper_table <- hyper_raw %>%
  select(mean, sd, `0.025quant`, `0.975quant`, mode) %>%
  rename(
    `Posterior Mean` = mean,
    `Posterior SD` = sd,
    `0.025 Quantile` = `0.025quant`,
    `0.975 Quantile` = `0.975quant`,
    `Posterior Mode` = mode
  )

print(round(hyper_table, 2))


### C . RANDOM EFFECTS: Continuous Temporal Main Effect (OU Process) ----
time_df <- as.data.frame(model4$summary.random$Time_Cont) %>%
  mutate(Real_Year = ID + 1987) # Convert continuous index back to actual years

plot_time <- ggplot(time_df, aes(x = Real_Year, y = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), fill = "steelblue", alpha = 0.3) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 2, color = "darkblue") +
  scale_x_continuous(breaks = seq(1987, 2022, by = 5)) +
  theme_minimal() +
  labs(
    x = "Election Year",
    y = "Latent Temporal Effect"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(plot_time)

### D. POSTERIOR PREDICTIONS: Observed vs. Fitted Density Overlay ----
pred_df <- data.frame(
  Observed = df_continuous$Y_turnout,
  Fitted = model4$summary.fitted.values$mean[1:nrow(df_continuous)]
) %>%
  tidyr::pivot_longer(cols = c(Observed, Fitted), names_to = "Type", values_to = "Turnout")

plot_pred <- ggplot(pred_df, aes(x = Turnout, fill = Type, color = Type)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values = c("Observed" = "gray50", "Fitted" = "darkorange")) +
  scale_color_manual(values = c("Observed" = "gray30", "Fitted" = "darkorange3")) +
  theme_minimal() +
  labs(
    x = "Voter Turnout",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.15, 0.8),
    legend.title = element_blank()
  )

print(plot_pred)

### E. MAP THE SPATIAL EFFECT (BYM2)

num_provinces <- nrow(map_prov_2022)
spatial_main_df <- data.frame(
  id_space_main = 1:num_provinces,
  Spatial_Effect = model4$summary.random$id_space_main$mean[1:num_provinces]
)

# Join with the spatial map 
map_spatial_effect <- map_prov_2022 %>%
  mutate(id_space_main = row_number()) %>%
  left_join(spatial_main_df, by = "id_space_main")

# Plot the BYM2 Spatial Effect
plot_spatial <- ggplot(map_spatial_effect) +
  geom_sf(aes(fill = Spatial_Effect), color = "black", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "indianred1", mid = "white", high = "steelblue2", midpoint = 0,
    name = "Log-Odds\nDeviation"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )

print(plot_spatial)

### F. MAP THE POSTERIOR PREDICTIVE TURNOUTS (FITTED VALUES)

fitted_values_df <- df_continuous %>%
  select(id_space_main, ANNO) %>%
  mutate(
    Fitted_Turnout = model4$summary.fitted.values$mean[1:n()]
  )

# Join the complete dataset to the spatial map to include ALL 10 years
map_predictive <- map_prov_2022 %>%
  mutate(id_space_main = row_number()) %>%
  # Without filtering, this automatically expands the geometries for all 10 years
  left_join(fitted_values_df, by = "id_space_main")

# Plot the Posterior Predictive Turnouts
plot_predictive <- ggplot(map_predictive) +
  geom_sf(aes(fill = Fitted_Turnout), color = "black", linewidth = 0.05) +
  
  # Keep the continuous scale exactly as you had it
  scale_fill_distiller(
    palette = "YlGnBu", 
    direction = 1,      
    labels = label_percent(accuracy = 1),
    name = "Predicted\nTurnout",
    limits = c(min(fitted_values_df$Fitted_Turnout), max(fitted_values_df$Fitted_Turnout))
  ) +
  
  # Use 5 columns to create a balanced 2x5 grid
  facet_wrap(~ ANNO, ncol = 5) +
  theme_void() +
  
  theme(
    # --- GRID & BOX STYLING ---
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(color = "black", fill = "#faebd7", linewidth = 0.8), 
    strip.text = element_text(face = "bold", color = "black", margin = margin(t = 4, b = 4)),
    panel.spacing = unit(0, "lines"), # Seamless grid packing
    
    # --- LEGEND STYLING ---
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.title = element_text(face = "bold", vjust = 0.8),
    legend.margin = margin(t = 15) # Adds a little breathing room between the grid and legend
  )

print(plot_predictive)

# Export as an ultra-high-definition PNG to eliminate PDF viewer lag
ggsave(
  filename = "posterior_predictive_turnout_map.png", 
  plot = plot_predictive, 
  width = 11.7,      
  height = 8.3,      
  units = "in",      
  dpi = 300,         # Upgraded to 600 for flawless pixel density
  bg = "white"       
)

### G. ODDS RATIO AND MARGINAL EFFECTS ----

# 1. Calculate the Odds Ratios
odds_ratios <- fixed_table %>%
  mutate(`Odds Ratio` = exp(`Posterior Mean`)) %>%
  select(Covariate, `Posterior Mean`, `Odds Ratio`)

print("Odds Ratios (Multiplicative impact of 1 SD increase):")
print(odds_ratios %>% mutate_if(is.numeric, round, 3))

# 2. Plot the Marginal Effect Curves
# Extract the national intercept to calculate true probabilities
intercept <- model4$summary.fixed["(Intercept)", "mean"]

# Create a sequence from -3 to +3 Standard Deviations
sd_seq <- seq(-3, 3, length.out = 100)

marginal_df <- data.frame()

# Loop through each covariate to calculate its isolated effect
for(i in 1:nrow(fixed_table)) {
  cov_name <- fixed_table$Covariate[i]
  beta <- fixed_table$`Posterior Mean`[i]
  
  # Logit equation: hold all other variables at 0 (their mean), vary the target variable
  logit_p <- intercept + (beta * sd_seq)
  # Convert log-odds back to probability
  prob <- exp(logit_p) / (1 + exp(logit_p))
  
  marginal_df <- bind_rows(marginal_df, data.frame(
    Covariate = cov_name,
    SD_Change = sd_seq,
    Predicted_Turnout = prob
  ))
}

# Ensure label_percent is available[cite: 1]
library(scales) 

plot_marginal <- ggplot(marginal_df, aes(x = SD_Change, y = Predicted_Turnout, color = Covariate)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Marginal Effects of Standardized Covariates",
    subtitle = "Predicted turnout variations holding all other factors at their mean (0)",
    x = "Standard Deviations from the Mean",
    y = "Predicted Turnout Probability",
    color = "Socio-Economic Driver"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(plot_marginal)

### H. MAPPING SPACE-TIME INTERACTIONS (ALL 4 KNORR-HELD TYPES) ----

# Create a list defining the 4 models and their specific temporal architectures
interaction_models <- list(
  list(obj = model1, grid_type = "discrete"),
  list(obj = model2, grid_type = "yearly"),
  list(obj = model3, grid_type = "discrete"),
  list(obj = model4, grid_type = "yearly")
)

# Loop through each model to extract, join, and plot
for (m in interaction_models) {
  
  # 1. Extract the raw INLA interaction matrix
  int_raw <- as.data.frame(m$obj$summary.random$id_space_int)
  
  # 2. Reconstruct the Kronecker structure based on the model's grid type
  if (m$grid_type == "discrete") {
    int_raw <- int_raw %>%
      mutate(
        id_space_main = rep(1:107, times = 10),
        id_time_discrete = rep(1:10, each = 107)
      ) %>%
      select(id_space_main, id_time_discrete, Interaction_Effect = mean)
    
    int_df <- df_continuous %>%
      select(id_space_main, id_time_discrete, COD_PROV_22, ANNO) %>%
      left_join(int_raw, by = c("id_space_main", "id_time_discrete"))
    
  } else if (m$grid_type == "yearly") {
    int_raw <- int_raw %>%
      mutate(
        id_space_main = rep(1:107, times = 36),
        id_time_yearly = rep(1:36, each = 107)
      ) %>%
      select(id_space_main, id_time_yearly, Interaction_Effect = mean)
    
    int_df <- df_continuous %>%
      select(id_space_main, id_time_yearly, COD_PROV_22, ANNO) %>%
      left_join(int_raw, by = c("id_space_main", "id_time_yearly"))
  }
  
  # 3. Join back to the spatial polygons and DISCRETIZE the interaction effect
  map_interactions <- map_prov_2022 %>%
    mutate(id_space_main = row_number()) %>%
    left_join(int_df, by = "id_space_main") %>%
    # Create the strict 3-class categorical variable
    mutate(Shock_Class = cut(
      Interaction_Effect,
      breaks = c(-Inf, -0.01, 0.01, Inf),
      labels = c("[-1, 0.01]", "(-0.01, 0.01]", "(0.01, 1]"),
      right = TRUE # Ensures intervals are closed on the right (..., x]
    ))
  
  # 4. Generate the Plot (Boxed Grid + Categorical Legend)
  plot_int <- ggplot(map_interactions) +
    # Map the fill to our newly created categorical variable
    geom_sf(aes(fill = Shock_Class), color = "black", linewidth = 0.05) +
    
    scale_fill_manual(
      values = c(
        "[-1, 0.01]" = "steelblue4",      
        "(-0.01, 0.01]" = "grey98", 
        "(0.01, 1]" = "orangered4"        
      ),
      name = "Local Shock\n(Log-Odds)",
      drop = FALSE,
      guide = guide_legend(reverse = FALSE) 
    ) +
    
    facet_wrap(~ ANNO, ncol = 5) +
    theme_void() + 
    
    theme(
      # --- GRID & BOX STYLING ---
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      strip.background = element_rect(color = "black", fill = "#faebd7", linewidth = 0.8), 
      strip.text = element_text(face = "bold", color = "black", margin = margin(t = 4, b = 4)),
      panel.spacing = unit(0, "lines"), 
      
      # --- CATEGORICAL LEGEND STYLING ---
      legend.position = "right",
      # Adds a black border around the individual legend color boxes so the "white" box doesn't vanish
      legend.key = element_rect(color = "black", linewidth = 0.5),
      legend.key.size = unit(0.7, "cm"), 
      legend.title = element_text(face = "bold", hjust = 0),
      legend.text = element_text(size = 10)
    )
  
  print(plot_int)
}

### I. ELECTORAL ANOMALIES (OUTLIERS & RAW PREDICTION ERROR) ----

# 1. Calculate the raw percentage point difference between Observed and Fitted
outliers_df <- df_continuous %>%
  mutate(
    Fitted_Turnout = model4$summary.fitted.values$mean[1:n()],
    # Calculate Error (Positive = voted higher than expected, Negative = voted lower)
    Error = Y_turnout - Fitted_Turnout,
    Abs_Error = abs(Error)
  )

# 2. Extract the Top 20 Over-performing Anomalies 
top_positive <- outliers_df %>%
  arrange(desc(Error)) %>%
  select(COD_PROV_22, ANNO, Y_turnout, Fitted_Turnout, Error) %>%
  head(20)

# 3. Extract the Top 20 Under-performing Anomalies
top_negative <- outliers_df %>%
  arrange(Error) %>% # Sorts from most negative to least
  select(COD_PROV_22, ANNO, Y_turnout, Fitted_Turnout, Error) %>%
  head(20)

print("Top 20 Unpredicted Surges in Turnout (Positive Outliers):")
print(top_positive %>% mutate_if(is.numeric, round, 3))

print("Top 20 Unpredicted Crashes in Turnout (Negative Outliers):")
print(top_negative %>% mutate_if(is.numeric, round, 3))

# 1. Use the safely mapped interaction_df we just created
shocks_df <- interaction_df %>%
  select(COD_PROV_22, ANNO, Interaction_Effect)

# 2. Extract Top 20 Positive Shocks (Absorbed Surges)
top_positive_shocks <- shocks_df %>%
  arrange(desc(Interaction_Effect)) %>%
  head(20)

print("Top 20 Positive Local Shocks (Log-Odds):")
print(top_positive_shocks %>% mutate_if(is.numeric, round, 4))

# 3. Extract Top 20 Negative Shocks (Absorbed Crashes)
top_negative_shocks <- shocks_df %>%
  arrange(Interaction_Effect) %>%
  head(20)

print("Top 20 Negative Local Shocks (Log-Odds):")
print(top_negative_shocks %>% mutate_if(is.numeric, round, 4))
