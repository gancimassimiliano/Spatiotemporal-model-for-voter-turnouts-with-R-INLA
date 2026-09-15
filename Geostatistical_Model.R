load("C:/Users/massi/OneDrive/Desktop/TESI/R codes/df_2.RData")

library(INLA)
library(sf)        
library(spdep)     
library(dplyr)
library(tidyr) 
library(ggplot2)
library(scales)

# ==============================================================================
# PHASE 0: HARMONIZING THE DATA (CENTROID JOIN)
# ==============================================================================
map_2022 <- st_make_valid(conf_comuni_22) %>%
  select(PRO_COM_22 = PRO_COM, COD_PROV_22 = COD_PROV, COD_REG_22 = COD_REG)

create_spatial_crosswalk <- function(hist_shapefile, shape_year_label) {
  hist_points <- st_point_on_surface(st_make_valid(hist_shapefile)) %>%
    select(PRO_COM_HIST = PRO_COM)
  crosswalk <- st_join(hist_points, map_2022, join = st_intersects) %>%
    st_drop_geometry() %>%
    mutate(SHAPE_REF = shape_year_label) 
  return(crosswalk)
}

cw_91 <- create_spatial_crosswalk(conf_comuni_91, "91")
cw_01 <- create_spatial_crosswalk(conf_comuni_01, "01")
cw_06 <- create_spatial_crosswalk(conf_comuni_06, "06")
cw_08 <- create_spatial_crosswalk(conf_comuni_08, "08")
cw_13 <- create_spatial_crosswalk(conf_comuni_13, "13")
cw_18 <- create_spatial_crosswalk(conf_comuni_18, "18")
cw_22 <- create_spatial_crosswalk(conf_comuni_22, "22") 

master_crosswalk <- bind_rows(cw_91, cw_01, cw_06, cw_08, cw_13, cw_18, cw_22) %>%
  distinct() %>%
  mutate(PRO_COM_HIST = as.numeric(PRO_COM_HIST))

df_storico_master <- df_storico_master %>%
  mutate(
    SHAPE_REF = case_when(
      ANNO %in% c(1987, 1992, 1994, 1996) ~ "91",
      ANNO == 2001 ~ "01",
      ANNO == 2006 ~ "06",
      ANNO == 2008 ~ "08",
      ANNO == 2013 ~ "13",
      ANNO == 2018 ~ "18",
      ANNO == 2022 ~ "22",
      TRUE ~ NA_character_
    ),
    PRO_COM = as.numeric(PRO_COM)
  ) %>%
  left_join(master_crosswalk, by = c("PRO_COM" = "PRO_COM_HIST", "SHAPE_REF" = "SHAPE_REF")) %>%
  select(-SHAPE_REF)


# ==============================================================================
# PHASE 1: AGGREGATION & STANDARDIZATION
# ==============================================================================

electoral_laws_timeline <- df_storico_master %>%
  select(ANNO, LEGGE_ELETTORALE) %>%
  distinct() %>%
  drop_na(LEGGE_ELETTORALE) %>%
  arrange(ANNO)

# 1A. Aggregate up to the Province Level
df_model <- df_storico_master %>%
  filter(TIPO_ELEZIONE == "CAMERA" | TIPO_ELEZIONE == "Camera") %>%
  group_by(PRO_COM_22, COD_PROV_22, ANNO) %>%
  summarise(
    Elettori_Com = max(ELETTORI, na.rm = TRUE),
    Votanti_Com  = max(VOTANTI, na.rm = TRUE),
    P7_Density   = mean(P7_Density, na.rm = TRUE),
    P13_Aging    = mean(P13_Aging, na.rm = TRUE),
    SS4_Education= mean(SS4_Education, na.rm = TRUE),
    L12_Employment= mean(L12_Employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(COD_PROV_22, ANNO) %>%
  summarise(
    ELETTORI = sum(Elettori_Com, na.rm = TRUE),
    VOTANTI  = sum(Votanti_Com, na.rm = TRUE),
    P7_Density     = weighted.mean(P7_Density, w = Elettori_Com, na.rm = TRUE),
    P13_Aging      = weighted.mean(P13_Aging, w = Elettori_Com, na.rm = TRUE),
    SS4_Education  = weighted.mean(SS4_Education, w = Elettori_Com, na.rm = TRUE),
    L12_Employment = weighted.mean(L12_Employment, w = Elettori_Com, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Y_turnout = VOTANTI / ELETTORI,
    Y_turnout = ifelse(Y_turnout >= 1, 0.9999, Y_turnout),
    Y_turnout = ifelse(Y_turnout <= 0, 0.0001, Y_turnout)
  )

# 1B. Standardize Covariates STRICTLY on observed elections
df_model_scaled <- df_model %>%
  mutate(
    Density_z    = scale(P7_Density)[,1],
    Aging_z      = scale(P13_Aging)[,1],
    Education_z  = scale(SS4_Education)[,1],
    Employment_z = scale(L12_Employment)[,1]
  ) %>%
  # ADD THIS: Bring the electoral laws back into the final dataset!
  left_join(electoral_laws_timeline, by = "ANNO")

# ==============================================================================
# PHASE 2: BUILDING THE GRAPH AND THE SPATIAL DICTIONARY
# ==============================================================================
map_prov_2022 <- st_make_valid(conf_province_22) %>%
  mutate(COD_PROV_22 = as.numeric(COD_PROV))

spatial_dict_prov <- data.frame(
  COD_PROV_22 = map_prov_2022$COD_PROV_22,
  id_space_main = 1:nrow(map_prov_2022) 
)

nb_prov <- poly2nb(map_prov_2022, queen = TRUE)

# Inject the Ferry Routes (The Island Problem)
idx_messina <- which(map_prov_2022$COD_PROV_22 == 83)
idx_reggio  <- which(map_prov_2022$COD_PROV_22 == 80)
idx_sassari <- which(map_prov_2022$COD_PROV_22 == 90)
idx_roma    <- which(map_prov_2022$COD_PROV_22 == 58)
idx_genova  <- which(map_prov_2022$COD_PROV_22 == 10) 

nb_prov[[idx_messina]] <- sort(unique(as.integer(c(nb_prov[[idx_messina]], idx_reggio))))
nb_prov[[idx_reggio]]  <- sort(unique(as.integer(c(nb_prov[[idx_reggio]], idx_messina))))
nb_prov[[idx_sassari]] <- sort(unique(as.integer(c(nb_prov[[idx_sassari]], idx_roma, idx_genova))))
nb_prov[[idx_roma]]    <- sort(unique(as.integer(c(nb_prov[[idx_roma]], idx_sassari))))
nb_prov[[idx_genova]]  <- sort(unique(as.integer(c(nb_prov[[idx_genova]], idx_sassari))))

cat("After fix, disconnected regions:", n.comp.nb(nb_prov)$nc, "\n")

nb2INLA("italy_province.graph", nb_prov)
g_prov <- inla.read.graph("italy_province.graph")

# ==============================================================================
# PHASE 3: FINAL SPATIOTEMPORAL DATASET FOR INLA
# ==============================================================================

df_continuous <- df_model_scaled %>%
  # 1. Attach the spatial dictionary created in Phase 2
  left_join(spatial_dict_prov, by = "COD_PROV_22") %>%
  
  # 2. Create the precise indices for INLA
  mutate(
    # Set Proportional as the reference category for Electoral Law (Adjust exact string if needed)
    LEGGE_ELETTORALE = as.factor(LEGGE_ELETTORALE),
    LEGGE_ELETTORALE = relevel(LEGGE_ELETTORALE, ref = "Proporzionale"),
    
    # Continuous Time for the OU Main Effect (Anchored to 1987 = 0)
    Time_Cont = ANNO - 1987, 
    
    # Discrete Time (1 to T) for the Interaction Grouping
    id_time_discrete = as.numeric(as.factor(ANNO)),
    
    # Duplicate the spatial index for the interaction effect
    id_space_int = id_space_main
  )

# Extract the exact continuous time points for the OU process
target_time_cont <- unique(sort(df_continuous$Time_Cont))

# ==============================================================================
# PHASE 4: THE SPATIO-TEMPORAL MODELS
# ==============================================================================

### 4.1: The Hyperpriors

# Define PC Priors for the BYM2 spatial main effect
pc_prior_bym2 <- list(
  prec = list(prior = "pc.prec", param = c(1, 0.01)),
  phi  = list(prior = "pc", param = c(0.5, 0.5))
)

pc_prior_ou <- list(
  prec = list(prior = "pc.prec", param = c(1, 0.01))
)


### 4.2.: Define the formulas for the linear predictor

# BASELINE (Model 0): Beta GLM with no Spatial nor Time effect
formula0 <- Y_turnout ~ 1 + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE

# NO INTERACTION (Model 1): Main Time (OU) + Main Space (BYM2)
formula1 <- Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
  f(Time_Cont, model = "ou", values = target_time_cont, hyper = pc_prior_ou) +
  f(id_space_main, model = "bym2", graph = g_prov, scale.model = TRUE, hyper = pc_prior_bym2)

# TYPE I (Model 2): Base + Unstructured Space x Unstructured Time (iid x iid)
formula2 <- Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
  f(Time_Cont, model = "ou", values = target_time_cont, hyper = pc_prior_ou) + 
  f(id_space_main, model = "bym2", graph = g_prov, scale.model = TRUE, hyper = pc_prior_bym2) +
  f(id_space_int, model = "iid", group = id_time_discrete, control.group = list(model = "iid"))

# TYPE III (Model 3): Base + Structured Space x Unstructured Time (besag x iid)
formula3 <- Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
  f(Time_Cont, model = "ou", values = target_time_cont, hyper = pc_prior_ou) + 
  f(id_space_main, model = "bym2", graph = g_prov, scale.model = TRUE, hyper = pc_prior_bym2) +
  f(id_space_int, model = "besag", graph = g_prov, group = id_time_discrete, control.group = list(model = "iid"))

# BERNARDINELLI model (Model 4)
formula4 <- Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z + LEGGE_ELETTORALE +
  Time_Cont + 
  f(id_space_main, model = "bym2", graph = g_prov, scale.model = TRUE, hyper = pc_prior_bym2) +
  # ADDED pc_prior_ou to the random slope to ensure consistency with Chapter 2!
  f(id_space_int, Time_Cont, model = "besag", graph = g_prov, scale.model = TRUE, hyper = pc_prior_ou)

# BERNARDINELLI No Laws (Model 5)
formula5 <- Y_turnout ~ 1 + Density_z + Aging_z + Education_z + Employment_z +
  Time_Cont + 
  f(id_space_main, model = "bym2", graph = g_prov, scale.model = TRUE, hyper = pc_prior_bym2) +
  f(id_space_int, Time_Cont, model = "besag", graph = g_prov, scale.model = TRUE, hyper = pc_prior_ou)

# ==============================================================================
# PHASE 5: EXECUTING THE MODELS
# ==============================================================================

# Centralize controls so they are easy to change globally if needed
ctrl_compute <- list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
ctrl_pred    <- list(compute = TRUE, link = 1)

# Model 0: Simple Beta GLM with no Spatial-Temporal effects
model0 <- inla(formula0, family = "beta", data = df_continuous,
               control.compute = ctrl_compute, control.predictor = ctrl_pred, verbose = FALSE)

# Model 1: Spatiotemporal model with no interactions
model1 <- inla(formula1, family = "beta", data = df_continuous,
               control.compute = ctrl_compute, control.predictor = ctrl_pred, verbose = FALSE)

# Model 2: Type I Interaction
model2 <- inla(formula2, family = "beta", data = df_continuous,
               control.compute = ctrl_compute, control.predictor = ctrl_pred, verbose = FALSE)

# Model 3: Type III Interaction
model3 <- inla(formula3, family = "beta", data = df_continuous,
               control.compute = ctrl_compute, control.predictor = ctrl_pred, verbose = FALSE)

# Model 4: Bernardinelli
model4 <- inla(formula4, family = "beta", data = df_continuous,
  control.compute = ctrl_compute, control.predictor = ctrl_pred, verbose = FALSE)

# Model 5: Bernardinelli No Laws
model5 <- inla(formula5, family = "beta", data = df_continuous,
               control.compute = ctrl_compute, control.predictor = ctrl_pred, verbose = FALSE)



