
packages <- c('shiny',
              'shinydashboard',
              'tidyverse',
              'sf',
              'RColorBrewer',
              'viridis',
              'GADMTools',
              'tmap',
              'leaflet',
              'here',
              'rnaturalearthdata',
              'lubridate',
              'plotly',
              'htmltools',
              'raster',
              'maptools',
              'rgdal',
              'spatstat',
              'sp',
              'CGPfunctions')

for (p in packages){
  if (!require(p,character.only=T)){
    install.packages(p)
  }
  library(p, character.only=T)
}

# Reading the raw csv file as a tbl_df
ACLED_SA <- read_csv("Data/2016-01-01-2019-12-31-Southern_Asia.csv")

# Inspecting the structure of the dataset
str(ACLED_SA)

# Read in aspatial dataframe
SA_df <- readRDS("Data/prepared_files/SA_df.rds")

# Read in sf object
SA_sf <- readRDS("Data/prepared_files/SA_sf.rds")


# read in South Asia shapefiles
SA_sh <- readRDS("Data/prepared_files/SA_sh.rds")


SA_tm <- tm_shape(SA_sh) +
  tm_text("name")+
  tm_fill() +
  tm_borders("black", lwd = 1) +
  tm_shape(SA_sf) +  
  tm_dots(col="event_type", palette="Spectral", alpha= 0.5,
          id= "data_id",
          popup.vars= c("Country:"="country", "State/Province:"="admin1","Event Type"="event_type","Sub-Event Type"="sub_event_type","Primary actor"="actor1"))

tmap_leaflet(SA_tm)   

