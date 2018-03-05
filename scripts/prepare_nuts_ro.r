#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script to prepare administrative units shapefiles for Romania
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load packages ---------------------------------------------------------------

library(sf)
library(dplyr)
library(data.table)


# Read data ---------------------------------------------------------------

# Read EU administrative units (NUTS)
nuts_eu <- sf::st_read(dsn = "data/NUTS_2013_01M_SH/data/NUTS_RG_01M_2013.shp",
                       stringsAsFactors = FALSE)
# Shapefile downloaded from:
# http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts

# Read NUTS names
nuts_eu_data <- fread("data/NUTS_2013_01M_SH/data/NUTS_AT_2013.csv", 
                      colClasses = "character")


# Clean data --------------------------------------------------------------
# Select NUTS referring to Romania and add names --------------------------

nuts_ro <- 
  # From EU admin units, 
  nuts_eu %>% 
  # select admin units (polygons) referring to Romania
  filter(NUTS_ID %like% "RO") %>% 
  # and add units names to each polygon.
  merge(nuts_eu_data, by = "NUTS_ID")

# Save as shapefile
dir.create("data/shp_ro/nuts_ro", recursive = TRUE)
sf::write_sf(obj    = nuts_ro, 
             dsn    = "data/shp_ro/nuts_ro/nuts_ro.shp", 
             driver = "ESRI Shapefile")


# Select counties only ----------------------------------------------------

nuts_ro_county <- filter(nuts_ro, STAT_LEVL_ == 3)

# Save as shapefile
dir.create("data/shp_ro/nuts_ro_county")
sf::write_sf(obj    = nuts_ro_county, 
             dsn    = "data/shp_ro/nuts_ro_county/nuts_ro_county.shp", 
             driver = "ESRI Shapefile")
