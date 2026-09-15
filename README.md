# Spatiotemporal-model-for-voter-turnouts-with-R-INLA
This repository contains the R codes used for my bachelor thesis named "The Dynamics of Political Disengagement in Italy: A Spatiotemporal Bayesian Model of Voter Turnout". The main goal of this analysis is to build a Spatiotemporal Model using the INLA approximation method, as theoretically formulated in Section 2. The scripts present here follows the complete pipeline developed with additional temporary tests (not included in the final thesis). Only the main models of interest (Model 3 and 4) are analyzed in their specific outputs to obtain the final considerations about the 'abstensionism' phenomenon in Italy. 

## Prerequisites and Libraries
Spiega quale versione di R hai usato e quali pacchetti sono necessari per far girare il codice.
* R (version 4.5.2) has been used. 
* `INLA` and `brms` are the key packages to install for the INLA and MCMC models.
  
  -tutorial to install INLA at: "https://www.r-inla.org/download/".
  
  -`brms` package info at: "https://cran.r-project.org/web/packages/brms/index.html".
* `sf` for spatial data, to manage ISTAT limits.
* other other packages for dataframe management, plots and visualization are all specified in the scripts: (`dplyr`, `stringi`, `stringr`, `viridis`, `ggplot2`, `scales`, `tifyr`).

## How to reproduce the analysis
The general workflow is the following:
1. Execute `0_DataBase_Creation.R` to build the final-complete database (df_continuous). The script shows the steps to manage and import the ISTAT covariates, limits, provincial codes matching and solving errors and issues. (SECTION 1.3)
2. Execute `1_Exploratory_Analysis.R` to generate maps and initial observations on the data. (SECTION 1.4)
3. Execute `2_Geostatistical_Model.R` to estimate the INLA models and their formulations. (CHAPTER 2)
4. Execute `3_Output_Analysis.R` to evaluate fit, residual analysis for model-selection. Extract the posterior estimates, the random effect and hyperparameters. (SECTIONS 3.1 to 3.5)
5. Execute `4_MCMC_Output.R` to execute the MCMC-based models and compare them with the INLA-based one. (SECTION 3.6).

## The repository structure
* `Confini/`: contains the Shapefile for municipalities, provincial and regional spatial shapes and limits.
* `Covariate/`: contains the ISTAT indicators used as covariates in the regression models.
* `Dataset/`: contains the actual electoral data from ELIGENDO.
* `Scripts/`: contains R codes numbered in order of execution.
* `Outputs/`: contains Model charts and exports from the scripts or used in the thesis.
