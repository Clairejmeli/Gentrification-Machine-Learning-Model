


> Written with [StackEdit](https://stackedit.io/).

# README File for QSS Senior One Quarter Major Project
##### *Claire Meli, 23W*

## Overview
This project builds a Random Forest classifier to predict whether a U.S. census tract gentrified between 2000 and 2010, using Census/ACS socioeconomic and land use features. The target variable was constructed in R using a two-step methodology: tracts were first identified as "gentrifiable" based on below-median household income in 2000, then classified as gentrified if their composite socioeconomic change score — built from PCA on five indicators including income, rent, and educational attainment — exceeded the 75th percentile of change across gentrifiable tracts in the same city. The final model was tuned via randomized grid search with 5-fold cross-validation and evaluated on accuracy, F1 score, ROC/AUC, and feature importance. 

Key finding: features related to neighborhood disadvantage — poverty rate, proportion of renters, and foreclosure rate — were most predictive of gentrification risk.

## Files to run
1. `load_data.R`: load data from files in directory
2. `modify_data.R`: combine data into data frames for all variables, one data frame for 2000-2003 period and one data frame for 2010-2013 period
3. `define_gentrified.R`: create "gentrifiable" binary variable for each census tract; use PCA to create gentrification index; compute binary "gentrified" variable for each tract based on index
4. `rename_vars.R`: rename variables so they make more sense; save data frames to be loaded into python for ML model
5. `map_gentrifiable.R`: create plots to represent "gentrifiable" census tracts in 2000-2003 and 2010-2013 for an example city
6. `random_forest_ML.ipynb`: run random forest model, metrics, feature importances, and visualizations
7. `map_gentrified.R`: create plots of data define and ML model defined "gentrified" tracts in 2010-2013 and future (2020-2023)

**The files should be run in the order listed above.**

## Data files
None of the Data files were included due to the size of the data.

The R scripts create and save two files: `final_data_clean_2000.csv` and `final_data_clean_2010.csv` which hold all the features used to train the Random Forest model in their respective year. The former file contains the "gentrified" label (1 or 0) for each census tract.

The `random_forest_ML.ipynb` file creates and saves two data files: `2010_gentrified_model.csv` and `future_gentrified_model.csv` which hold all feature variables and model predicted gentrification classifications (1 or 0).


