

setwd("C:/Users/19106/Dropbox (Personal)/BOEM_data/Papers/sediment_chl/data")
#install.packages(c("tidyverse", "lubridate", "janitor", "readxl"))
library(tidyverse) # for all the tidying needs (mostly dplyr)
library(lubridate) # for dealing with dates if necessary
library(janitor) # for the clean_names function
library(readxl)
library(openxlsx) # for write.xlsx

# Load in the data with surface and bottom chlorophyll
water_data = read.csv('boem_merged.csv', header = T) 

# replace the site number in the boem_merged with a murie_id without space
water_data = water_data %>% 
  mutate(site = str_replace(water_data$murie_id, " ", ""))

# load in data for the sediment chlorophyll
# use read_excel because read.xlsx treats dates weird
sed_data = read_excel('sediment_chl_clean.xlsx', sheet = 'Sheet 1')

water_sites <-  unique(water_data$site)
sed_sites <- unique(sed_data$site)

# create a vector of values to select from the water_data
select_vector = c('date', 'site','region','int_bot','topo', 'depth', 'code', 'sp_desc', 'genera', 'fxn_group',
                  'cells_ml','carbon_biomass','unc_chl', 'tp', 'tn', 'wtem', 'turb', 'secchi', 'do', 'ph', 'sal')

# filter the water sites to only include sites that the sediment sites have
water_data_filtered <- water_data %>% 
  filter(site %in% sed_sites) %>% 
  select(all_of(select_vector))

  
sed_data_filtered <-sed_data %>% 
  mutate(date = as.Date(date))
  

head(water_data_filtered)
