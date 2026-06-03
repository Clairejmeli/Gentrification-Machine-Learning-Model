# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# FILE 5
# use: Create plots to represent "gentrifiable" census tracts in 2000-2003
# and 2010-2013 for an example city
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
family <- "Times New Roman"

# "gentrifiable" tracts in both 2000-2003 period --------------------------

### get tracts for example city (Philadelphia, PA)
# the 2000 vars are normalized to 2010 boundaries, so map by 2010 boundaries
tracts_2000_chi <- tracts("PA", "Philadelphia", year = 2010) %>% 
  unite("FIPS", STATEFP10, COUNTYFP10, TRACTCE10, sep="", remove=TRUE) %>% 
  left_join(df_2000_final, by = "FIPS") %>% 
  filter(!is.na(FIPS)) 

# plot the tracts for 2000-2003 "gentrifiable" data
ggplot(data = tracts_2000_chi) +
  ggtitle("\"Gentrifiable\" Census Tracts in Philadelphia, PA (2000-2003)") +
  geom_sf(color = NA, aes(geometry = geometry, fill = gentrifiable_2000)) +
  scale_fill_manual(values = c("#002060", "#3D83A1"), 
                    na.value = "lightgrey",
                    name = "Gentrifiable") +
  theme_void() +
  theme(plot.title = element_text(size=32, family=family),
        legend.title = element_text(size = 25, family = family),
        legend.text = element_text(size = 22, family = family))

# "gentrifiable" tracts in 2010-2013 period -------------------------------

### get tracts for example city (Chicago, Il)
tracts_2010_chi <- tracts("PA", "Philadelphia", year = 2010) %>% 
  unite("FIPS", STATEFP10, COUNTYFP10, TRACTCE10, sep="", remove=TRUE) %>% 
  left_join(df_2010_final, by = "FIPS") %>% 
  filter(!is.na(FIPS))

# plot the tracts for 2010-2013 "gentrifiable" data
ggplot(data = tracts_2010_chi) +
  labs(title = "\"Gentrifiable\" Census Tracts in Philadelphia, PA (2010-2013)",
       fill = "Gentrifiable") +
  geom_sf(color = NA, aes(geometry = geometry, fill = gentrifiable_2010)) +
  scale_fill_manual(values = c("#002060", "#3D83A0"), 
                    na.value = "lightgrey") +
  theme_void() +
  theme(plot.title = element_text(size=32, family=family),
        legend.title = element_text(size = 25, family = family),
        legend.text = element_text(size = 22, family = family))
