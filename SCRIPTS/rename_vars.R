# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# FILE 4
# use: Rename variables so they make more sense
#      Save data frames to be loaded into python for ML model
#
# QSS Senior Major One Quarter Project
# author: Claire Meli
# date: March 2023
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# load libraries ----------------------------------------------------------

library(tidyverse)

# rename variables (for Python) -------------------------------------------

# remove unnecessary variables
df_2000_final_clean <- df_2000_final %>% 
  filter(gentrifiable_num_2000 == 1) %>%
  select(-medinc_2000, -gentrifiable_num_2000, -gentrifiable_2000, -gentrified)

df_2010_final_clean <- df_2010_final %>% 
  filter(gentrifiable_num_2010 == 1) %>%
  select(-medinc_2010, -gentrifiable_num_2010, -gentrifiable_2010, )

# create new variable names
new_names <- c("FIPS", 
               "CITY_ID", 
               "Unemployment rate", 
               "Female headed households", 
               "High school grads", 
               "Poverty rate",
               "White", 
               "Hispanic", 
               "Minority", 
               "Racial heterogeneity", 
               "Foreign born", 
               "Low wage jobs",
               "High wage jobs",
               "Population density", 
               "Developed land, low intensity", 
               "Developed land, medium intensity",
               "Developed land, high intensity", 
               "Under 18", 
               "18 to 24", 
               "25 to 34", 
               "35 to 44", 
               "45 to 54", 
               "55 to 64", 
               "65 and older",
               "Family households", 
               "Non-family households", 
               "Group quarters",
               "Full-service restaurants", 
               "Limited service (fast food)", 
               "Coffee shops", 
               "Bars", 
               "Performing arts organizations",
               "Spectator sports organizations", 
               "Museums, historical sites",
               "Libraries", 
               "Casinos", 
               "Golf, skiing, boating, fitness, bowling, other", 
               "Gyms, fitness centers", 
               "Grocery stores",
               "Specialty food stores", 
               "Supercenters", 
               "Redlining",
               "Housing units", 
               "Occupied housing units", 
               "Owner occupied housing units", 
               "Renters", 
               "Vacant housing", 
               "Foreclosure rate", 
               "Law enforcement", 
               "Loan applications - Refinancing",
               "Loan applications - owner occupancy", 
               "Loan applications - non-owner occupancy",
               "Loan applications - conventional", 
               "Loan applications - FHA", 
               "Loan applications - VA", 
               "Barber shops", 
               "Beauty salons",
               "Nail salons", 
               "Diet/weight loss centers", 
               "Other personal care",
               "Laundromats", 
               "Polluting sites", 
               "Religious sites", 
               "Civic/social organizations", 
               "Non-recreation child/youth services", 
               "Elderly/disability services", 
               "Other individual/family services", 
               "Temporary shelters",
               "Emergency relief services", 
               "Vocational rehabilitation services",
               "Day care services", 
               "Commuting - car, truck, van", 
               "Commuting - public transportation", 
               "Commuting - motorcylce", 
               "Commuting - bicycle", 
               "Commuting - walking", 
               "Commuting - other",
               "Commuting - WFM", 
               "Average work commute", 
               "Average household size",
               "Median year structure built", 
               "Gentrified")

# reassign variable names
colnames(df_2000_final_clean) <- new_names
colnames(df_2010_final_clean) <- new_names

# don't actually need "gentrified" variable for 2010 data
df_2010_final_clean <- df_2010_final_clean %>% 
  select(-`Gentrified`)

# save data frame ---------------------------------------------------------

write_csv(df_2000_final_clean, "final_data_clean_2000.csv", append = FALSE)
write_csv(df_2010_final_clean, "final_data_clean_2010.csv", append = FALSE)

