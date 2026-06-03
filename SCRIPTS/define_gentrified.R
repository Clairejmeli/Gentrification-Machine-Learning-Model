# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# FILE 3
# use: Create "gentrifiable" binary variable for each census tract
#      Use PCA to create gentrification index
#      Compute binary "gentrified" variable for each tract based on index
#
# QSS Senior Major One Quarter Project
# author: Claire Meli
# date: March 2023
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# load libraries ----------------------------------------------------------

library(tidyverse)
library(rgdal)
library(knitr)
library(ggcorrplot)
library("FactoMineR")
library("devtools")
library("factoextra")

# define "gentrifiable" tracts --------------------------------------------

# 2000-2003 period
df_2000_gentrifiable <- df_2000_fixed %>% 
  group_by(CITY_ID) %>% 
  mutate(medinc_2000 = median(SE_T091_001, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gentrifiable_num_2000 = ifelse(SE_T091_001 < medinc_2000, 1, 0),
         gentrifiable_2000 = ifelse(SE_T091_001 < medinc_2000, 
                                    "gentrifiable", 
                                    "not gentrifiable"))

# 2010-2013 period
df_2010_gentrifiable <- df_2010_fixed %>% 
  group_by(CITY_ID) %>% 
  mutate(medinc_2010 = median(SE_A14006_001, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gentrifiable_num_2010 = ifelse(SE_A14006_001 < medinc_2010, 1, 0),
         gentrifiable_2010 = ifelse(SE_A14006_001 < medinc_2010, 
                                    "gentrifiable", 
                                    "not gentrifiable"))

# get vars from 2000/2010 used for gentrification classification ----------

# 2000-2003
gent_2000 <- df_2000_gentrifiable %>% 
  filter(gentrifiable_num_2000 == 1) %>%
  select(FIPS, CITY_ID,
         SE_T091_001, # median income
         TN_CLGD1,    # proportion of college graduates
         TN_PCPF1,    # proportion professional or managers
         SE_T167_001, # median gross rent
         TN_HSVL1)    # median value of owner-occupied housing

# 2010-2013
gent_2010 <- df_2010_gentrifiable %>% 
  filter(gentrifiable_num_2010 == 1) %>%
  select(FIPS, CITY_ID,
         SE_A14006_001, # median income
         T_CLGD2,       # proportion of college graduates
         T_PCPF2,       # proportion professional or managers
         SE_A18009_001, # median gross rent
         T_HSVL2)       # median value of owner-occupied housing

# conduct PCA to create gentrification index ------------------------------

### 2000

# normalize data for better results
data_normalized_2000 <- scale(gent_2000 %>% select(3:7))

# compute correlation matrix between variables
corr_matrix_2000 <- cor(data_normalized_2000)
# plot the correlation matrix (if you want to see)
ggcorrplot(corr_matrix_2000)

# call the principal component analysis
data_2000.pca <- princomp(corr_matrix_2000)
# check components and the amount of variation they account for
summary(data_2000.pca)

# draw out the values of each components to use in creating new index
loadings_2000 <- as.data.frame(data_2000.pca$loadings[,1])

# extract values into temporary variables for linear combination
comp_income  <- loadings_2000[[1]][1]
comp_college <- loadings_2000[[1]][2]
comp_prof    <- loadings_2000[[1]][3]
comp_rent    <- loadings_2000[[1]][4]
comp_hsval   <- loadings_2000[[1]][5]

# uncomment to see visualization of variable correlation
# fviz_pca_var(data_2000.pca, col.var = "black")

# create gentrification index based on PCA loadings -----------------------
# basically, linear combination of loadings by old vars.

# 2000
gent_2000_index <- df_2000_gentrifiable %>% 
  mutate(gent_score_2000 = (comp_income  * SE_T091_001) +
                           (comp_college * TN_CLGD1) +
                           (comp_prof    * TN_PCPF1) +
                           (comp_rent    * SE_T167_001) +
                           (comp_hsval   * TN_HSVL1)) %>% 
  select(FIPS, CITY_ID, gentrifiable_num_2000, gent_score_2000)

# 2000
gent_2010_index <- df_2010_gentrifiable %>% 
  mutate(gent_score_2010 = (comp_income  * SE_A14006_001) +
                           (comp_college * T_CLGD2) +
                           (comp_prof    * T_PCPF2) +
                           (comp_rent    * SE_A18009_001) +
                           (comp_hsval   * T_HSVL2)) %>% 
  select(FIPS, CITY_ID, gentrifiable_num_2010, gent_score_2010)

# compute gent index for rest of city for comparison ----------------------

# join 2000 and 2010 data frames
gent_index <- left_join(gent_2000_index, gent_2010_index, 
                        by = c("FIPS", "CITY_ID")) %>% 
  # calculate change in each tract between time periods
  mutate(gent_index_change = gent_score_2010 - gent_score_2000) %>% 
  # calculate median gentrification index for citywide change
  group_by(CITY_ID) %>% 
  mutate(gent_index_city = quantile(gent_index_change, 0.75)) %>%
  ungroup() %>% 
  # if tract is "gentrifiable" AND tract-level change is greater than 
  # city-level change, the tract GENTRIFIED
  mutate(gentrified_num = ifelse(gentrifiable_num_2000 == 1 &
                               gent_index_change > gent_index_city, 
                               1, 0),
         gentrified = ifelse(gentrified_num == 1, 
                             "gentrified", 
                             "not gentrified")) %>% 
  select(FIPS, gentrified_num, gentrified)

# join gentrified index data frame with rest of the data ------------------

df_2000_final <- left_join(df_2000_gentrifiable, gent_index, by = "FIPS") %>% 
  # remove vars used to create gent index
  select(-SE_T091_001,  # median income
         -TN_CLGD1,     # proportion of college graduates
         -TN_PCPF1,     # proportion professional or managers
         -SE_T167_001,  # median gross rent
         -TN_HSVL1) %>% # median house value
  filter(!is.na(gentrified_num)) 
  
# remove from 2010 data frame also
df_2010_final <- df_2010_gentrifiable %>% 
  select(-SE_A14006_001, # median incomes
         -T_CLGD2,       # proportion of college graduates
         -T_PCPF2,       # proportion professional or managers
         -SE_A18009_001, # median gross rent
         -T_HSVL2) %>%   # median house value %>% 
  mutate(gentrified_num = 0)

