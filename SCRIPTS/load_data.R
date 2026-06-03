# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# FILE 1
# use: Load data files into data frames
#
# QSS Senior Major One Quarter Project
# author: Claire Meli
# date: March 2023
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# set working directory (if necessary) ------------------------------------

# setwd("/Users/clairejmeli/Desktop/QSS82/CapstoneProject")

# load necessary libraries ------------------------------------------------

library(tidyverse)
library(readr)
library(rgdal)
library(knitr)
library(readxl)
library(stringr)

# load data ---------------------------------------------------------------

### amenities

# arts, entertainment, recreation
entertainment <- read_csv("DATA/amenities/arts, ent, recreation/artsentrec.csv") %>% 
  select(tract_fips10, year, aden_7111, aden_7112, aden_712, aden_51912, 
         aden_7132, aden_7139, aden_71394)

entertainment_2003 <- entertainment %>% 
  filter(year == 2003)

entertainment_2013 <- entertainment %>% 
  filter(year == 2013)

# eating, drinking, restaurants
eating <- read_csv("DATA/amenities/eating, drinking/eatdrink.csv") %>% 
  select(tract_fips10, year, aden_722511, aden_722513, aden_722515, aden_722410)

eating_2003 <- eating %>% 
  filter(year == 2003)

eating_2013 <- eating %>% 
  filter(year == 2013)

# grocery
grocery <- read_csv("DATA/amenities/grocery/grocery.csv") %>% 
  select(tract_fips10, year, popden_445110, popden_4452, popden_452311)

grocery_2003 <- grocery %>% 
  filter(year == 2003)

grocery_2013 <- grocery %>% 
  filter(year == 2013)

# personal care services, laundromats
pers_care <- read_csv("DATA/amenities/pers_care/pers_care.csv") %>% 
  select(tract_fips10, year, aden_812111, aden_812112, aden_812113, 
         aden_812191, aden_812199, aden_812310)

pers_care_2003 <- pers_care %>% 
  filter(year == 2003)

pers_care_2013 <- pers_care %>% 
  filter(year == 2013)

# post offices, banks
pobank <- read_csv("DATA/amenities/post offices, banks/pobank.csv") %>% 
  select(tract_fips10, year, popden_522110, popden_522120, popden_522130)

pobank_2003 <- pobank %>% 
  filter(year == 2003)

pobank_2013 <- pobank %>% 
  filter(year == 2013)

# retail by type
retail <- read_csv("DATA/amenities/retail/retail.csv") %>% 
  select(tract_fips10, year, aden_443, aden_444, aden_448, aden_451, 
         aden_452210, aden_453910, aden_453310, aden_446120, aden_446191)

retail_2003 <- pobank %>% 
  filter(year == 2003)

retail_2013 <- pobank %>% 
  filter(year == 2013)

### general demographic and housing data (comparability data)

# built environment and demographic data between 2000 and 2010
demo_housingtypes <- read_csv("DATA/comparability/R13296575_SL140.csv") %>% 
  select(Geo_FIPS,
         SE_T032_001, SE_T032_002, SE_T032_003, SE_T032_004, SE_T032_005, 
         SE_T032_006, SE_T032_007, SE_T032_008, SE_T032_009, SE_T032_010, 
         SE_T032_011, SE_T032_012, SE_T032_013, SE_NV126_001, SE_NV126_002, 
         SE_NV126_003, SE_NV126_004, SE_NV126_005, SE_NV126_006, SE_NV126_007, 
         SE_NV126_008, SE_NV126_009, SE_NV126_010, SE_NV126_011, SE_NV126_012, 
         SE_NV126_013, SE_T059_002, SE_T059_003, SE_T059_016, SE_T059_024, 
         SE_NV234_002, SE_NV234_003, SE_NV234_016, SE_NV234_024) %>% 
  # combine vars and convert to proportions
    # age groups
  mutate(SE_T032_005 = SE_T032_002 + SE_T032_003 + SE_T032_004 + SE_T032_005,
         SE_T032_011 = SE_T032_011 + SE_T032_012 + SE_T032_013,
         SE_NV126_005 = SE_NV126_002 + SE_NV126_003 + SE_NV126_004 + SE_NV126_005,
         SE_NV126_011 = SE_NV126_011 + SE_NV126_012 + SE_NV126_013) %>% 
    # household types, convert to proportions
  mutate(SE_T059_003 = (SE_T059_003 / SE_T059_002) * 100,
         SE_T059_016 = (SE_T059_016 / SE_T059_002) * 100,
         SE_T059_024 = (SE_T059_024 / SE_T059_002) * 100,
         SE_NV234_003 = (SE_NV234_003 / SE_NV234_002) * 100,
         SE_NV234_016 = (SE_NV234_016 / SE_NV234_002) * 100,
         SE_NV234_024 = (SE_NV234_024 / SE_NV234_002) * 100) %>% 
  # remove variables no longer needed
  select(-SE_T032_001, -SE_T032_002, -SE_T032_003, -SE_T032_004, -SE_T032_012, 
         -SE_T032_013, -SE_NV126_001, -SE_NV126_002, -SE_NV126_003, 
         -SE_NV126_004, -SE_NV126_012, -SE_NV126_013,
         -SE_T059_002, -SE_NV234_002)

demo_housingtypes_2000 <- demo_housingtypes %>% 
  select(Geo_FIPS, starts_with("SE_T"))

demo_housingtypes_2010 <- demo_housingtypes %>% 
  select(Geo_FIPS, starts_with("SE_N"))

# average household size
house_size_2000 <- read_csv("DATA/housing/household_size_2000.csv") %>% 
  select(1, 8)

house_size_2010 <- read_csv("DATA/housing/household_size_2010.csv") %>% 
  select(1, 56)

# median year structure built
year_built_2000 <- read_csv("DATA/housing/year_built_2000.csv") %>% 
  select(Geo_FIPS, SE_T160_001)
  
year_built_2010 <- read_csv("DATA/housing/year_built_2010.csv") %>% 
  select(Geo_FIPS, SE_A10057_001)

### basic demographic and SES data
df <- load("DATA/crime/crime_1999-2013/DS0001/38483-0001-Data.rda")

ses_demo <- da38483.0001 %>% 
  select(STATE, T_COUNTY, TRACT2, CITY_ID, T_AREMI2, TN_POP01, T_POP082,
         TN_UNWA1, T_UNWA2, TN_PCPF1, T_PCPF2, TN_FMHD1, T_FMHD2, TN_HSGD1, 
         T_HSGD2, TN_CLGD1, T_CLGD2, TN_PVTY1, T_PVTY2, TN_PWHT1, T_PWHT2, 
         TN_PHSP1, T_PHSP2, TN_PMIN1, T_PMIN2, TN_RCHT1, T_RCHT2, 
         TN_FRBR1, T_FRBR2, TN_SSLW1, T_SSLW2, TN_HIWG1, T_HIWG2) %>% 
  mutate(STATE = str_extract(STATE, "\\d\\d")) %>% 
  unite("FIPS", STATE, T_COUNTY, TRACT2, sep="", remove=TRUE) 

ses_demo_2000 <- ses_demo %>% 
  select(FIPS, CITY_ID, T_AREMI2, ends_with("1")) %>% 
  mutate(pop_den_2000 = TN_POP01 / T_AREMI2) %>% 
  select(-TN_POP01, -T_AREMI2)

ses_demo_2010 <- ses_demo %>% 
  select(FIPS, CITY_ID, T_AREMI2, ends_with("2")) %>% 
  mutate(pop_den_2010 = T_POP082 / T_AREMI2) %>% 
  select(-T_POP082, -T_AREMI2)

### housing stock
housing_stock <- da38483.0001 %>% 
  select(STATE, T_COUNTY, TRACT2, T_AREMI2,
         TN_HU01, T_HU102, TN_OCH01, T_OCH102, TN_OOH01, T_OOH102, TN_PRNT1, 
         T_PRNT2, TN_PVAC1, T_PVAC2, TN_HSVL1, T_HSVL2, T_FRT00, T_FRT10) %>% 
  mutate(STATE = str_extract(STATE, "\\d\\d")) %>% 
  unite("FIPS", STATE, T_COUNTY, TRACT2, sep="", remove=TRUE) 

housing_stock_2000 <- housing_stock %>% 
  select(FIPS, ends_with("1"), T_FRT00, T_AREMI2) %>%
  # convert to number per square mile (housing units)
  mutate(TN_HU01 = (TN_HU01 / T_AREMI2) / 100,
         TN_OCH01 = (TN_OCH01 / T_AREMI2) / 100,
         TN_OOH01 = (TN_OOH01 / T_AREMI2) / 100)

housing_stock_2010 <- housing_stock %>% 
  select(FIPS, ends_with("2"), T_FRT10, T_AREMI2) %>% 
  # convert to number per square mile (housing units)
  mutate(T_HU102 = (T_HU102 / T_AREMI2) / 100,
         T_OCH102 = (T_OCH102 / T_AREMI2) / 100,
         T_OOH102 = (T_OOH102 / T_AREMI2) / 100)

### loan applications
loans_2000 <- da38483.0001 %>% 
  select(STATE, T_COUNTY, TRACT2, TN_HRFN1, TN_HOOC1, TN_HNOC1, TN_HCNV1, 
         TN_HFHA1, TN_HVA1) %>% 
  mutate(STATE = str_extract(STATE, "\\d\\d")) %>% 
  unite("FIPS", STATE, T_COUNTY, TRACT2, sep="", remove=TRUE) 

loans_2010 <- da38483.0001 %>% 
  select(STATE, T_COUNTY, TRACT2, T_HRFN2, T_HOOC2, T_HNOC2, T_HCNV2, 
         T_HFHA2, T_HVA2) %>% 
  mutate(STATE = str_extract(STATE, "\\d\\d")) %>% 
  unite("FIPS", STATE, T_COUNTY, TRACT2, sep="", remove=TRUE) 

### environment related to health

# healthcare
healthcare <- read_csv("DATA/health environment/healthcare/healthcare.csv") %>% 
  select(tract_fips10, year, popden_621111, popden_621112, popden_6212, 
         popden_446110, popden_621330, popden_6214)

healthcare_2003 <- healthcare %>% 
  filter(year == 2003)

healthcare_2013 <- healthcare %>% 
  filter(year == 2013)

# land area cover
area_2001 <- read_csv("DATA/health environment/land_area_cover/environment_area_2001.csv") %>% 
  select(Geo_FIPS, SE_T001_005, SE_T001_006, SE_T001_007)

area_2011 <- read_csv("DATA/health environment/land_area_cover/environment_area_2011.csv") %>% 
  select(Geo_FIPS, SE_T001_005, SE_T001_006, SE_T001_007)

# liquor and tobacco access
liqtob <- read_csv("DATA/health environment/liquor, tobacco/lqtbcon.csv") %>% 
  select(tract_fips10, year, aden_4453, aden_453991, aden_445120, aden_447110)

liqtob_2003 <- healthcare %>% 
  filter(year == 2003)

liqtob_2013 <- healthcare %>% 
  filter(year == 2013)

# number of polluting sites
pollut <- read_csv("DATA/health environment/polluting_sites/pollutst.csv")

pollut_2000 <- pollut %>% 
  filter(year == 2000)

pollut_2010 <- pollut %>% 
  filter(year == 2010)

### historical processes

redlining <- read_csv("DATA/history/HOLC_2010_census_tracts/HOLC_2010_census_tracts.csv") %>% 
  select(geoid10, area_D)

###  <- pation and industry

# job_sector_2002 <- read_csv("DATA/industry_occupation/jobsector_2002.csv") %>% 
#   select(-(2:7)) %>% 
#   # merge job sectors into overarching sectors, convert to proportions
#   mutate(primary_sector = ((ORGRACN_B007_002 + ORGRACN_B007_003) / 
#                              ORGRACN_B007_001) * 100,
#          secondary_sector = ((ORGRACN_B007_004 + ORGRACN_B007_005 +
#                             ORGRACN_B007_006 + ORGRACN_B007_007) / 
#                               ORGRACN_B007_001) * 100,=
#          tertiary_sector = ((ORGRACN_B007_008 + ORGRACN_B007_009 +
#                            ORGRACN_B007_012 + ORGRACN_B007_015 +
#                            ORGRACN_B007_017 + ORGRACN_B007_018 + 
#                            ORGRACN_B007_019) / ORGRACN_B007_001) * 100,
#          quartenary_sector = ((ORGRACN_B007_010 + ORGRACN_B007_011 + 
#                              ORGRACN_B007_013 + ORGRACN_B007_014 + 
#                              ORGRACN_B007_016) / ORGRACN_B007_001) * 100,
#          public_other_sector = ((ORGRACN_B007_020 + ORGRACN_B007_021) / 
#                             ORGRACN_B007_001) * 100) %>% 
#   select(-(2:22))

# job_sector_2012 <- read_csv("DATA/industry_occupation/jobsector_2012.csv") %>% 
#   select(-(2:7)) %>% 
#   # merge job sectors into overarching sectors, convert to proportions
#   mutate(primary_sector = ((ORGRACN_B007_002 + ORGRACN_B007_003) / 
#                              ORGRACN_B007_001) * 100,
#          secondary_sector = ((ORGRACN_B007_004 + ORGRACN_B007_005 +
#                                 ORGRACN_B007_006 + ORGRACN_B007_007) / 
#                                ORGRACN_B007_001) * 100,
#          tertiary_sector = ((ORGRACN_B007_008 + ORGRACN_B007_009 +
#                                ORGRACN_B007_012 + ORGRACN_B007_015 +
#                                ORGRACN_B007_017 + ORGRACN_B007_018 + 
#                                ORGRACN_B007_019) / ORGRACN_B007_001) * 100,
#          quartenary_sector = ((ORGRACN_B007_010 + ORGRACN_B007_011 + 
#                                  ORGRACN_B007_013 + ORGRACN_B007_014 + 
#                                  ORGRACN_B007_016) / ORGRACN_B007_001) * 100,
#          public_other_sector = ((ORGRACN_B007_020 + ORGRACN_B007_021) / 
#                                   ORGRACN_B007_001) * 100) %>% 
#   select(-(2:22))

### socioeconomic status and demographic conditions

# NaNDA data set (2000-2010) 
ses_dem <- load("DATA/ses_dem/NaNDA_SES_DEM/DS0001/38528-0001-Data.rda")

ses_dem_2000 <- da38528.0001 %>% 
  select(ends_with("00"))

ses_dem_2010 <- da38528.0001 %>% 
  select(ends_with("10"))

# median income and rent

rentinc_2000 <- read_csv("DATA/ses_dem/rent_income_2000.csv") %>% 
  select(Geo_FIPS, SE_T167_001, SE_T091_001)

rentinc_2010 <- read_csv("DATA/ses_dem/rent_income_2010.csv") %>% 
  select(Geo_FIPS, SE_A14006_001, SE_A18009_001)

### social environment

# law enforcement
lawenf <- read_csv("DATA/social environment/law enforcement/lawenf.csv") %>% 
  select(tract_fips10, year, aden_922)

lawenf_2003 <- lawenf %>% 
  filter(year == 2003)

lawenf_2013 <- lawenf %>% 
  filter(year == 2013)

# religious/social sites
rel <- read_csv("DATA/social environment/religious and social sites/relcivsoc.csv") %>% 
  select(tract_fips10, year, popden_8131, popden_8134)

rel_2003 <- rel %>% 
  filter(year == 2003)

rel_2013 <- rel %>% 
  filter(year == 2013)

# social services
soc_serv <- read_csv("DATA/social environment/social services/socsvcs.csv") %>% 
  select(tract_fips10, year, popden_624110, popden_624120, 
         popden_624190, popden_624221, popden_624230, 
         popden_624310, popden_624410)

soc_serv_2003 <- soc_serv %>% 
  filter(year == 2003)

soc_serv_2013 <- soc_serv %>% 
  filter(year == 2013)

# transportation, commuting -----------------------------------------------

transpo_2000 <- read_csv("DATA/transpo, commuting/transpo_2000.csv") %>% 
  select(Geo_FIPS, SE_T195_001, SE_T195_002, SE_T195_003, SE_T195_004, 
         SE_T195_004, SE_T195_005, SE_T195_006, SE_T195_007, SE_T195_008, 
         SE_T198_001) %>% 
  # convert to proportions
  mutate(SE_T195_002 = (SE_T195_002 / SE_T195_001) * 100,
         SE_T195_003 = (SE_T195_003 / SE_T195_001) * 100,
         SE_T195_004 = (SE_T195_004 / SE_T195_001) * 100,
         SE_T195_005 = (SE_T195_005 / SE_T195_001) * 100,
         SE_T195_006 = (SE_T195_006 / SE_T195_001) * 100,
         SE_T195_007 = (SE_T195_007 / SE_T195_001) * 100,
         SE_T195_008 = (SE_T195_008 / SE_T195_001) * 100) %>% 
  select(-SE_T195_001)

transpo_2010 <- read_csv("DATA/transpo, commuting/transpo_2010.csv") %>% 
  select(Geo_FIPS, SE_A09005_001, SE_A09005_002, SE_A09005_003, SE_A09005_004, 
         SE_A09005_005, SE_A09005_006, SE_A09005_007, SE_A09005_008, 
         SE_A09003_001) %>% 
  mutate(SE_A09005_002 = (SE_A09005_002 / SE_A09005_001) * 100,
         SE_A09005_003 = (SE_A09005_003 / SE_A09005_001) * 100,
         SE_A09005_004 = (SE_A09005_004 / SE_A09005_001) * 100,
         SE_A09005_005 = (SE_A09005_005 / SE_A09005_001) * 100,
         SE_A09005_006 = (SE_A09005_006 / SE_A09005_001) * 100,
         SE_A09005_007 = (SE_A09005_007 / SE_A09005_001) * 100,
         SE_A09005_008 = (SE_A09005_008 / SE_A09005_001) * 100) %>% 
  select(-SE_A09005_001)
  
