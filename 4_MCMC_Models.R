# ==============================================================================
# 0. INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================
required_packages <- c("brms", "dplyr", "tidyr", "spdep", "Matrix")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(brms)
library(dplyr)
library(tidyr)
library(spdep)
library(Matrix)

# Force brms to use base rstan to avoid Windows C++ compiler errors
options(brms.backend = "rstan")

# ==============================================================================
# 1. PREPARE THE FOUR KNORR-HELD MATRICES
# ==============================================================================
# This loads back the INLA outputs and database constructed
load("C:/Users/massi/OneDrive/Desktop/Spatiotemporal-model-for-voter-turnouts-with-R-INLA/Outputs.RData")

# A. Spatial Weight Matrix (W) & ICAR Precision (Sigma_s)
W <- spdep::nb2mat(nb_prov, style = "B", zero.policy = TRUE)
rownames(W) <- spatial_dict_prov$COD_PROV_22
colnames(W) <- spatial_dict_prov$COD_PROV_22

N_space <- nrow(W)
D <- diag(rowSums(W))
Q_s <- D - W
Q_s_ridge <- Q_s + diag(0.001, N_space) # Tiny ridge penalty for brms stability
Sigma_s <- solve(Q_s_ridge) 

# B. Temporal AR(1) Correlation Matrix (Sigma_t)
T_years <- length(unique(df_continuous$ANNO))
rho <- 0.90 # Structural proxy for temporal correlation
Sigma_t <- matrix(0, nrow = T_years, ncol = T_years)
for (i in 1:T_years) {
  for (j in 1:T_years) {
    Sigma_t[i, j] <- rho^(abs(i - j))
  }
}

# C. Identity Matrices for Unstructured Components
I_S <- diag(1, N_space)
I_T <- diag(1, T_years)

# D. Build Knorr-Held Covariance Matrices (Time Outer, Space Inner)
# TYPE II: Structured Time x Unstructured Space
Sigma_TypeII <- kronecker(Sigma_t, I_S)
# TYPE III: Unstructured Time x Structured Space
Sigma_TypeIII <- kronecker(I_T, Sigma_s)
# TYPE IV: Structured Time x Structured Space
Sigma_TypeIV <- kronecker(Sigma_t, Sigma_s)

# Ensure correct data sorting (Time first, Space second to match Kronecker order)
df_continuous <- df_continuous %>%
  arrange(ANNO, COD_PROV_22) %>%
  mutate(prov_year_id = as.character(row_number()))

rownames(Sigma_TypeII) <- 1:nrow(df_continuous)
colnames(Sigma_TypeII) <- 1:nrow(df_continuous)
rownames(Sigma_TypeIII) <- 1:nrow(df_continuous)
colnames(Sigma_TypeIII) <- 1:nrow(df_continuous)
rownames(Sigma_TypeIV) <- 1:nrow(df_continuous)
colnames(Sigma_TypeIV) <- 1:nrow(df_continuous)

# ==============================================================================
# 2. MCMC MODEL DEFINITIONS AND EXECUTION
# ==============================================================================
mcmc_priors <- c(
  prior(normal(0, 5), class = "b"),          
  prior(student_t(3, 0, 2.5), class = "sd")  
)

mcmc_results <- data.frame(Engine = character(), Model = character(), WAIC = numeric(), Runtime_Sec = numeric())

# ------------------------------------------------------------------------------
# MODEL 1 (TYPE I: Unstructured Space x Unstructured Time)
# brms handles this easily with a standard i.i.d random effect: (1 | prov_year_id)
formula_type1 <- bf(Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
                      (1 | id_time_discrete) + car(W, gr = COD_PROV_22, type = "bym2") + 
                      (1 | prov_year_id))

cat("Starting MCMC Model 1 (Type I)...\n")
t0 <- Sys.time()
mcmc_mod1 <- brm(formula_type1, data = df_continuous, data2 = list(W = W), 
                 family = Beta(link = "logit"), prior = mcmc_priors,
                 chains = 1, cores = 4, iter = 2000, warmup = 500, 
                 control = list(adapt_delta = 0.95, max_treedepth = 12))
t1 <- Sys.time()

mcmc_results <- rbind(mcmc_results, data.frame(
  Engine = "MCMC", Model = "Type I", 
  WAIC = waic(mcmc_mod1)$estimates["waic", "Estimate"], 
  Runtime_Sec = as.numeric(difftime(t1, t0, units = "secs"))
))

# ------------------------------------------------------------------------------
# MODEL 2 (TYPE II: Structured Time x Unstructured Space)
formula_type2 <- bf(Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
                      (1 | id_time_discrete) + car(W, gr = COD_PROV_22, type = "bym2") + 
                      (1 | gr(prov_year_id, cov = Sigma_TypeII)))

cat("Starting MCMC Model 2 (Type II)...\n")
t0 <- Sys.time()
mcmc_mod2 <- brm(formula_type2, data = df_continuous, data2 = list(W = W, Sigma_TypeII = Sigma_TypeII), 
                 family = Beta(link = "logit"), prior = mcmc_priors,
                 chains = 1, cores = 4, iter = 2000, warmup = 500, 
                 control = list(adapt_delta = 0.95, max_treedepth = 12))
t1 <- Sys.time()

mcmc_results <- rbind(mcmc_results, data.frame(
  Engine = "MCMC", Model = "Type II", 
  WAIC = waic(mcmc_mod2)$estimates["waic", "Estimate"], 
  Runtime_Sec = as.numeric(difftime(t1, t0, units = "secs"))
))

# ------------------------------------------------------------------------------
# MODEL 3 (TYPE III: Unstructured Time x Structured Space)
formula_type3 <- bf(Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
                      (1 | id_time_discrete) + car(W, gr = COD_PROV_22, type = "bym2") + 
                      (1 | gr(prov_year_id, cov = Sigma_TypeIII)))

cat("Starting MCMC Model 3 (Type III)...\n")
t0 <- Sys.time()
mcmc_mod3 <- brm(formula_type3, data = df_continuous, data2 = list(W = W, Sigma_TypeIII = Sigma_TypeIII), 
                 family = Beta(link = "logit"), prior = mcmc_priors,
                 chains = 1, cores = 4, iter = 2000, warmup = 500, 
                 control = list(adapt_delta = 0.95, max_treedepth = 12))
t1 <- Sys.time()

mcmc_results <- rbind(mcmc_results, data.frame(
  Engine = "MCMC", Model = "Type III", 
  WAIC = waic(mcmc_mod3)$estimates["waic", "Estimate"], 
  Runtime_Sec = as.numeric(difftime(t1, t0, units = "secs"))
))

# ------------------------------------------------------------------------------
# MODEL 4 (TYPE IV: Structured Time x Structured Space)
formula_type4 <- bf(Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
                      (1 | id_time_discrete) + car(W, gr = COD_PROV_22, type = "bym2") + 
                      (1 | gr(prov_year_id, cov = Sigma_TypeIV)))

cat("Starting MCMC Model 4 (Type IV)...\n")
t0 <- Sys.time()
mcmc_mod4 <- brm(formula_type4, data = df_continuous, data2 = list(W = W, Sigma_TypeIV = Sigma_TypeIV), 
                 family = Beta(link = "logit"), prior = mcmc_priors,
                 chains = 1, cores = 4, iter = 2000, warmup = 500, 
                 control = list(adapt_delta = 0.95, max_treedepth = 12))
t1 <- Sys.time()

mcmc_results <- rbind(mcmc_results, data.frame(
  Engine = "MCMC", Model = "Type IV", 
  WAIC = waic(mcmc_mod4)$estimates["waic", "Estimate"], 
  Runtime_Sec = as.numeric(difftime(t1, t0, units = "secs"))
))

# ==============================================================================
# 3. INLA PERFORMANCE EXTRACTION
# ==============================================================================
# Extract WAIC and Runtimes directly from the loaded R-INLA environment objects
inla_results <- data.frame(
  Engine = rep("INLA", 4),
  Model = c("Type I", "Type II", "Type III", "Type IV"),
  WAIC = c(model1$waic$waic, model2$waic$waic, model3$waic$waic, model4$waic$waic),
  Runtime_Sec = c(
    model1$cpu.used["Total"],
    model2$cpu.used["Total"],
    model3$cpu.used["Total"],
    model4$cpu.used["Total"]
  )
)

# ==============================================================================
# 4. FINAL COMPARISON TABLE
# ==============================================================================
final_comparison <- bind_rows(inla_results, mcmc_results) %>%
  mutate(
    WAIC = round(WAIC, 2),
    Runtime_Sec = round(Runtime_Sec, 2),
    Runtime_Minutes = round(Runtime_Sec / 60, 2)
  ) %>%
  arrange(Model, desc(Engine))

print("=========================================================")
print("  FINAL WAIC & RUNTIME COMPARISON: INLA vs MCMC (brms)   ")
print("=========================================================")
print(final_comparison)

# Save the final table as a CSV 
write.csv(final_comparison, "INLA_vs_MCMC_Comparison.csv", row.names = FALSE)
