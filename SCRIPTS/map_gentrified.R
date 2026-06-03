# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# FILE 6
# use: Create plots of data define and ML model defined "gentrified" tracts
# in 2010-2013 and future (2020-2023)
#
# QSS Senior Major One Quarter Project
# author: Claire Meli
# date: March 2023
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# load libraries ----------------------------------------------------------

library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
family = "Times New Roman"

# load data from python ---------------------------------------------------

# data for model predictions of gentrified in 2010-2013
model_2010 <- read_csv("2010_gentrified_model.csv")
model_2010 <- adjust_FIPS(model_2010)

# join with geometries
tracts_2010_model <- tracts("PA", "Philadelphia", year = 2010) %>% 
  unite("FIPS", STATEFP10, COUNTYFP10, TRACTCE10, sep="", remove=TRUE) %>% 
  left_join(model_2010, by = "FIPS") %>% 
  filter(!is.na(FIPS)) %>% 
  # filter(!is.na(`Gentrified - Model`)) %>%
  mutate(gentrified_data = ifelse(Gentrified == 1,
                                  "gentrified",
                                  "not gentrified"),
         gentrified_model = ifelse(`Gentrified - Model` == 1, 
                                   "gentrified", 
                                   "not gentrified"))

# map gentrified tracts 2010-2013 (based on data) -------------------------

# plot the tracts that were defined as gentrified in 2010-2013
ggplot(data = tracts_2010_model) +
  ggtitle("Census Tracts in Philadelphia, PA that Gentrified in 2010-2013",
          subtitle = "Based on data definitions of \"gentrified\"") +
  geom_sf(color = NA, aes(geometry = geometry, fill = gentrified_data)) +
  scale_fill_manual(values = c("#002060", "#3D83A1"), 
                    na.value = "lightgrey",
                    name = "Gentrified") +
  theme_void() +
  theme(plot.title = element_text(size=32, family=family),
        plot.subtitle = element_text(size = 22, family = family),
        legend.title = element_text(size = 25, family = family),
        legend.text = element_text(size = 25, family = family))

# map gentrified tracts 2010-2013 (based on model) ------------------------

# plot the model-defined gentrified tracts
ggplot(data = tracts_2010_model) +
  ggtitle("Census Tracts in Philadelphia, PA that Gentrified in 2010-2013",
          subtitle = "Based on data from 2000-2003 and Random Forest model definitions of \"gentrified\"") +
  geom_sf(color = NA, aes(geometry = geometry, fill = gentrified_model)) +
  scale_fill_manual(values = c("#002060", "#3D83A0"), 
                    na.value = "lightgrey",
                    name = "Gentrified") +
  theme_void() +
  theme(plot.title = element_text(size=32, family=family),
        plot.subtitle = element_text(size = 22, family = family),
        legend.title = element_text(size = 25, family = family),
        legend.text = element_text(size = 25, family = family))

# map future gentrified tracts (based on model) ---------------------------

# load file (saved in python model)
model_future <- read_csv("future_gentrified_model.csv")
model_future <- adjust_FIPS(model_future)

# join with geometries
tracts_future_model <- tracts("PA", "Philadelphia", year = 2010) %>% 
  unite("FIPS", STATEFP10, COUNTYFP10, TRACTCE10, sep="", remove=TRUE) %>% 
  left_join(model_future, by = "FIPS") %>% 
  filter(!is.na(FIPS)) %>% 
  mutate(gentrified_model = ifelse(`Gentrified` == 1, 
                                   "gentrified", 
                                   "not gentrified"))

# plot the model-defined gentrified tracts
ggplot(data = tracts_future_model) +
  ggtitle("Census Tracts in Philadelphia, PA Predicted to Gentrify in 2020-2023",
          subtitle = "Based on Random Forest model definitions of \"gentrified\"") +
  geom_sf(color = NA, aes(geometry = geometry, fill = gentrified_model)) +
  scale_fill_manual(values = c("#002060", "#3D83A0"), 
                    na.value = "lightgrey",
                    name = "Gentrified") +
  theme_void() +
  theme(plot.title = element_text(size=32, family=family),
        plot.subtitle = element_text(size = 22, family = family),
        legend.title = element_text(size = 25, family = family),
        legend.text = element_text(size = 25, family = family))
