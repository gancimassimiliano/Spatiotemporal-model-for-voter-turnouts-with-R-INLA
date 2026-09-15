# ==============================================================================
# 0. INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================
# Install missing packages if necessary
required_packages <- c("brms", "ggplot2", "dplyr", "tidyr", "sf", "spdep", "Matrix", "scales")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(brms)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(spdep)
library(Matrix)
library(scales) 
library(stats)

# Force brms to use base rstan to avoid Windows C++ compiler/CmdStan space errors
options(brms.backend = "rstan")


# ==============================================================================
# 1. LOAD WORKSPACE & PREPARE TYPE III INTERACTION MATRIX
# ==============================================================================
# Load your existing INLA workspace containing df_continuous, W, nb_prov, map_prov_2022, and model3
# load("C:/Users/massi/OneDrive/Desktop/Spatiotemporal-model-for-voter-turnouts-with-R-INLA/Outputs.RData")

# 1. Prepare the Spatial Weight Matrix
# INLA uses a graph file, but brms requires a binary adjacency matrix (W)
W <- spdep::nb2mat(nb_prov, style = "B", zero.policy = TRUE)
rownames(W) <- spatial_dict_prov$COD_PROV_22
colnames(W) <- spatial_dict_prov$COD_PROV_22

# 2. Define the Simplified MCMC Priors
mcmc_priors <- c(
  prior(normal(0, 5), class = "b"),          # Fixed effects (Beta coefficients)
  prior(student_t(3, 0, 2.5), class = "sd")  # Variances for random effects
)


# 1. Build the Spatial Precision Matrix (Diagonal of neighbors minus Adjacency Matrix)
D <- diag(rowSums(W))
Q_s <- D - W

# 2. Add a tiny ridge penalty to make the singular ICAR matrix positive-definite for brms
Q_s_ridge <- Q_s + diag(0.001, nrow(Q_s))
Sigma_s <- solve(Q_s_ridge) # Invert to get the Spatial Covariance Matrix

# 3. Create the Type III Space-Time Matrix (Kronecker product of Space and Time)
T_years <- length(unique(df_continuous$ANNO)) # 10 election years
Sigma_TypeIII <- kronecker(diag(T_years), Sigma_s)

# brms requires exact rownames/colnames to match the data index length (1,070)
rownames(Sigma_TypeIII) <- 1:nrow(df_continuous)
colnames(Sigma_TypeIII) <- 1:nrow(df_continuous)

# 4. Create a unique 1-to-N index in the data to match the matrix dimensions
df_continuous <- df_continuous %>%
  arrange(ANNO, COD_PROV_22) %>%
  mutate(prov_year_id = as.character(row_number()))

# ==============================================================================
# 2. DEFINE MCMC MODELS (TYPE I vs TYPE III)
# ==============================================================================

# --- TYPE I INTERACTION MODEL (COMMENTED OUT) ---
mcmc_formula_type1 <- bf(
   Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
     (1 | id_time_discrete) + 
     car(W, gr = COD_PROV_22, type = "bym2") + 
     (1 | COD_PROV_22:id_time_discrete)
 )

# --- TYPE III INTERACTION MODEL (ACTIVE) ---
#mcmc_formula_type3 <- bf(
#  Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
#    (1 | id_time_discrete) + 
#    car(W, gr = COD_PROV_22, type = "bym2") + 
#    (1 | gr(prov_year_id, cov = Sigma_TypeIII))
#)

mcmc_priors <- c(
  prior(normal(0, 5), class = "b"),          
  prior(student_t(3, 0, 2.5), class = "sd")  
)

# Execute the Type III Model
mcmc_model3_type3 <- brm(
  formula = mcmc_formula_type3,
  data = df_continuous,
  data2 = list(W = W, Sigma_TypeIII = Sigma_TypeIII), 
  family = Beta(link = "logit"),
  prior = mcmc_priors,
  chains = 4,             
  cores = 4,              
  iter = 2000,            
  warmup = 1000,          
  control = list(adapt_delta = 0.95, max_treedepth = 12) 
)

# Execute the Type I Model
mcmc_model3_type1 <- brm(
  formula = mcmc_formula_type1,
  data = df_continuous,
  data2 = list(W = W),                         # ONLY W is needed here
  family = Beta(link = "logit"),
  prior = mcmc_priors,
  chains = 4,             
  cores = 4,              
  iter = 2000,            
  warmup = 1000,          
  control = list(adapt_delta = 0.95, max_treedepth = 12) 
)

# ==============================================================================
# 3. OUTPUT ANALYSIS: INLA vs MCMC COMPARISON
# ==============================================================================

### A. Information Criteria (WAIC)
inla_waic <- model3$waic$waic
mcmc_waic_obj <- waic(mcmc_model3_type3)

cat("INLA WAIC:", inla_waic, "\n")
cat("MCMC WAIC:", mcmc_waic_obj$estimates["waic", "Estimate"], "\n")

### B. Extract Quantile Residuals for INLA
inla_fitted <- model3$summary.fitted.values$mean[1:nrow(df_continuous)]
inla_pit <- model3$cpo$pit[1:nrow(df_continuous)]
inla_pit_sqz <- pmax(pmin(inla_pit, 0.9999), 0.0001)

diagnostic_inla <- data.frame(
  COD_PROV_22 = df_continuous$COD_PROV_22,
  ANNO = df_continuous$ANNO,
  Observed = df_continuous$Y_turnout,
  Fitted = inla_fitted,
  Residuals = qnorm(inla_pit_sqz),
  Engine = "INLA (Type III)"
)

### C. Extract Quantile Residuals for MCMC
mcmc_fitted <- fitted(mcmc_model3_type3)[, "Estimate"]
mu_draws <- posterior_epred(mcmc_model3_type3)
phi_draws <- as_draws_df(mcmc_model3_type3)$phi

y_obs <- df_continuous$Y_turnout
N_obs <- length(y_obs)
pit_mcmc <- numeric(N_obs)

for (i in 1:N_obs) {
  shape1_i <- mu_draws[, i] * phi_draws
  shape2_i <- (1 - mu_draws[, i]) * phi_draws
  pit_mcmc[i] <- mean(pbeta(y_obs[i], shape1 = shape1_i, shape2 = shape2_i))
}

mcmc_pit_sqz <- pmax(pmin(pit_mcmc, 0.9999), 0.0001)

diagnostic_mcmc <- data.frame(
  COD_PROV_22 = df_continuous$COD_PROV_22,
  ANNO = df_continuous$ANNO,
  Observed = y_obs,
  Fitted = mcmc_fitted,
  Residuals = qnorm(mcmc_pit_sqz),
  Engine = "MCMC (Type III)"
)

# Combine for plotting
diagnostic_df <- bind_rows(diagnostic_inla, diagnostic_mcmc)

# ==============================================================================
# 4. DIAGNOSTIC PLOTS
# ==============================================================================

### A. Q-Q Plot Comparison
plot_qq_comp <- ggplot(diagnostic_df, aes(sample = Residuals)) +
  stat_qq(color = "grey40", alpha = 0.5, size = 1) +
  stat_qq_line(color = "firebrick", linewidth = 1) +
  facet_wrap(~ Engine) +
  theme_minimal() +
  labs(    x = "Theoretical Quantiles",
    y = "Randomized Quantile Residuals"
  ) +
  theme(strip.text = element_text(face = "bold", size = 11))

print(plot_qq_comp)

### B. Residuals vs. Fitted Plot Comparison (Homoscedasticity)
plot_homo_comp <- ggplot(diagnostic_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "grey40", size = 1) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = c(-1.96, 1.96), color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "firebrick", se = FALSE, linewidth = 1) +
  facet_wrap(~ Engine) +
  theme_minimal() +
  labs(    x = "Fitted Values (Posterior Mean)", 
    y = "Randomized Quantile Residuals"
  ) +
  theme(strip.text = element_text(face = "bold", size = 11))

print(plot_homo_comp)

### C. Empirical Dispersion Check
dispersion_check <- diagnostic_df %>%
  group_by(Engine) %>%
  summarise(
    Total_Observations = n(),
    Outside_2SD = sum(abs(Residuals) > 1.96),
    Percent_Outside_2SD = round((Outside_2SD / Total_Observations) * 100, 2),
    Outside_3SD = sum(abs(Residuals) > 3.0),
    Percent_Outside_3SD = round((Outside_3SD / Total_Observations) * 100, 3)
  )

print("Empirical Dispersion Check (Expected: ~5.00% outside 2SD, ~0.3% outside 3SD)")
print(dispersion_check)

# ==============================================================================
# 5. AUTOCORRELATION TESTS (SPATIAL & TEMPORAL)
# ==============================================================================
listw_prov <- nb2listw(nb_prov, style = "W", zero.policy = TRUE)
election_years <- unique(sort(df_continuous$ANNO))

### A. Spatial Autocorrelation (Moran's I)
spatial_ac_results <- data.frame()

for (eng in unique(diagnostic_df$Engine)) {
  for (yr in election_years) {
    df_yr <- diagnostic_df %>% filter(Engine == eng, ANNO == yr) %>% arrange(COD_PROV_22)
    m_test <- moran.test(df_yr$Residuals, listw_prov, zero.policy = TRUE)
    
    spatial_ac_results <- bind_rows(spatial_ac_results, data.frame(
      Engine = eng,
      Year = yr,
      Morans_I = m_test$estimate[1],
      p_value = m_test$p.value
    ))
  }
}

spatial_summary <- spatial_ac_results %>%
  group_by(Engine) %>%
  summarise(
    Avg_Morans_I = round(mean(Morans_I), 4),
    Years_Significant = sum(p_value < 0.05) 
  )

print("Spatial Autocorrelation (Moran's I) Summary:")
print(spatial_summary)

### B. Temporal Autocorrelation (Lag-1)
temporal_summary <- diagnostic_df %>%
  arrange(Engine, COD_PROV_22, ANNO) %>%
  group_by(Engine, COD_PROV_22) %>%
  mutate(Residuals_lag1 = lag(Residuals)) %>%
  ungroup() %>%
  filter(!is.na(Residuals_lag1)) %>%
  group_by(Engine) %>%
  summarise(
    Lag1_Correlation = round(cor(Residuals, Residuals_lag1, use = "complete.obs"), 4)
  )

print("Temporal Autocorrelation (Lag-1) Summary:")
print(temporal_summary)

# ==============================================================================
# 6. GLOBAL CALIBRATION & PREDICTIVE MAPPING
# ==============================================================================

### A. Density Overlay (Observed vs INLA vs MCMC)[cite: 1]
df_density <- data.frame(
  Observed = df_continuous$Y_turnout,
  INLA = inla_fitted,
  MCMC = mcmc_fitted
) %>%
  pivot_longer(cols = everything(), names_to = "Source", values_to = "Turnout") %>%
  mutate(Source = factor(Source, levels = c("Observed", "INLA", "MCMC")))

plot_density_comparison <- ggplot(df_density, aes(x = Turnout, fill = Source, color = Source)) +
  geom_density(alpha = 0.28, linewidth = 0.8) +
  scale_fill_manual(values = c("Observed" = "#7F7F7F", "INLA" = "#E66101", "MCMC" = "#2B83BA")) +
  scale_color_manual(values = c("Observed" = "#333333", "INLA" = "#B34700", "MCMC" = "#1C587D")) +
  scale_x_continuous(labels = label_percent(accuracy = 1), limits = c(0.45, 1.0)) +
  labs(
    title = "Global Calibration: Observed vs. INLA vs. MCMC Posterior Distributions",
    x = "Voter Turnout", y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = c(0.18, 0.82), plot.title = element_text(face = "bold"))

print(plot_density_comparison)

### B. MCMC Posterior Predictive Maps[cite: 1]
map_predictive_mcmc <- map_prov_2022 %>%
  left_join(diagnostic_mcmc, by = "COD_PROV_22")

plot_predictive_mcmc <- ggplot(map_predictive_mcmc) +
  geom_sf(aes(fill = Fitted), color = "black", linewidth = 0.05) +
  scale_fill_distiller(
    palette = "YlGnBu", direction = 1, 
    labels = label_percent(accuracy = 1),
    name = "Predicted\nTurnout"
  ) +
  facet_wrap(~ ANNO, ncol = 5) +
  theme_void() +
  labs(
    title = "MCMC Posterior Predictive Turnout Map",
    subtitle = "Model 3 Equivalent (brms Implementation)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm")
  )

print(plot_predictive_mcmc)

# Save the Maps
ggsave("plot_homo_comp.png", plot = plot_homo_comp, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave("plot_qq.png", plot = plot_qq_comp, width = 9, height = 5.5, dpi = 300, bg = "white")

ggsave("density_comparison.png", plot = plot_density_comparison, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave("posterior_predictive_map_mcmc.png", plot = plot_predictive_mcmc, width = 11.7, height = 8.3, dpi = 300, bg = "white")

