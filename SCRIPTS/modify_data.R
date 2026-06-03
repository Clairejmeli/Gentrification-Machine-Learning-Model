# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# FILE 2
# use: Combine data into data frames for all variables, one dataframe for 
#      2000-2003 period and one dataframe for 2010-2013 period
#
# QSS Senior Major One Quarter Project
# author: Claire Meli
# date: March 2023
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# load libraries ----------------------------------------------------------

library(tidyverse)

# define functions --------------------------------------------------------

# make each tract ID the same name, 11 char string padded with 0 if necessary
adjust_FIPS <- function(df) {
  
  df <- df %>% 
    mutate(FIPS = as.character(df[[1]]),
           FIPS = str_pad(FIPS, 11, pad = 0)) %>% 
    relocate(FIPS)
  
  return(df)
  
}

# make lists of all data frames for each time period ----------------------

dfs_2000 <- list(ses_demo_2000, area_2001, demo_housingtypes_2000, eating_2003,
                 entertainment_2003, grocery_2003, healthcare_2003, redlining,
                 housing_stock_2000, lawenf_2003, liqtob_2003,
                 loans_2000, pers_care_2003, pobank_2003, pollut_2000, 
                 rel_2003, rentinc_2000, retail_2003, soc_serv_2003, 
                 transpo_2000, house_size_2000, year_built_2000)

dfs_2010 <- list(ses_demo_2010, area_2011, demo_housingtypes_2010, eating_2013,
                 entertainment_2013, grocery_2013, healthcare_2013, redlining,
                 housing_stock_2010, lawenf_2013, liqtob_2013,
                 loans_2010, pers_care_2013, pobank_2013, pollut_2010, 
                 rel_2013, rentinc_2010, retail_2013, soc_serv_2013, 
                 transpo_2010, house_size_2010, year_built_2010)

# join data frames for each time period -----------------------------------

### 2000
# adjust census tract ID column for each data frame

for (i in seq_along(dfs_2000)) {
  
  dfs_2000[[i]] <- adjust_FIPS(dfs_2000[[i]])
  
}

# extract first data frame (something to join on)
temp <- dfs_2000[[1]]

# adjust the rest of the list
dfs_2000 <- dfs_2000[2:22]

# initiate joined data frame with crime data frame
df_2000 <- temp

# join all data frames
for (i in seq_along(dfs_2000)) {
  
  df_2000 <- left_join(df_2000, dfs_2000[[i]], by = "FIPS", `copy` = TRUE)
  
}

# remove repeated variables
df_2000 <- df_2000 %>% 
  select(-contains("."))

### 2010
# adjust census tract ID column for each data frame

for (i in seq_along(dfs_2010)) {
  
  dfs_2010[[i]] <- adjust_FIPS(dfs_2010[[i]])
  
}

# extract first data frame (something to join on)
temp <- dfs_2010[[1]]

# adjust the rest of the list
dfs_2010 <- dfs_2010[2:22]

# initiate joined data frame with crime data frame
df_2010 <- temp

# join all data frames
for (i in seq_along(dfs_2010)) {
  
  df_2010 <- left_join(df_2010, dfs_2010[[i]], by = "FIPS", `copy` = TRUE)
  
}

# remove repeated variables
df_2010 <- df_2010 %>% 
  select(-contains("."))

# remove variables with too many missing values ---------------------------

### 2000
df_2000_remove <- df_2000 %>% 
  select(-starts_with("Geo_"), -T_AREMI2, -geoid10)

### 2010
df_2010_remove <- df_2010 %>% 
  select(-starts_with("Geo_"), -T_AREMI2, -geoid10)

# uncomment to check missing values by column
# colSums(is.na(df_2000_fixed))

# remove remaining missing values by case ---------------------------------

# 2000
df_2000_fixed <- df_2000_remove %>% 
  filter(!is.na(SE_T001_005) & !is.na(SE_T001_007) & !is.na(area_D) &
           !is.na(TN_HSVL1))

# 2010
df_2010_fixed <- df_2010_remove %>% 
  filter(!is.na(SE_T001_005) & !is.na(SE_A10057_001) &
           !is.na(SE_T001_007) & !is.na(T_HSVL2) & !is.na(area_D) &
           !is.na(SE_A18009_001) & !is.na(SE_A09003_001))

# uncomment to check missing values by column
# colSums(is.na(df_2010_fixed))

# make sure both data frames have the same tracts -------------------------

# create list of tracts from both data frames
# filter by lists to make sure only tracts in both lists are in the data frames

tracts_2000 <- df_2000_fixed %>% 
  select(FIPS) %>% 
  pull()

df_2010_fixed <- df_2010_fixed %>% 
  filter(FIPS %in% tracts_2000) %>% 
  distinct(FIPS, .keep_all = TRUE)

tracts_2010 <- df_2010_fixed %>% 
  select(FIPS) %>% 
  pull()

df_2000_fixed <- df_2000_fixed %>% 
  filter(FIPS %in% tracts_2010) %>% 
  distinct(FIPS, .keep_all = TRUE)


  