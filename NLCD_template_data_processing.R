# Pre-process data for use in Shiny app

# packages ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(here)
library(mapview)
library(readxl)
library(janitor)
library(httr)
library(esri2sf) # install using remotes::install_github("yonghah/esri2sf"); for more info see: https://github.com/yonghah/esri2sf 
library(tigris)

## conflicts ----
library(conflicted)
conflicts_prefer(dplyr::filter)



# CA boundary -------------------------------------------------------------
## get CA boundary ----
ca_boundary <- states(year = 2020, cb = TRUE) %>% 
    filter(STUSPS == 'CA') %>%
    st_transform(3310)

## write CA boundary ----
st_write(ca_boundary, 
         here('data_processed', 
              'ca_boundary.gpkg'), 
         append = FALSE)
