#packages <- c('shiny','shinydashboard','tidyverse','sf','RColorBrewer','viridis','GADMTools','tmap','leaflet','here','rnaturalearthdata','lubridate','plotly','htmltools','raster','maptools','rgdal','spatstat','sp','ggplot2','anytime','plyr','zoo','DT',
#              'TH.data','coin','matrixStats','modeltools','multcomp','party','sandwich','strucchange','oompaBase')

#for (p in packages){
#    if (!require(p,character.only=T)){
#        install.packages(p)
#    }
#    library(p, character.only=T)
#}

#install.packages('CGPfunctions', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(devtools)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(viridis)
library(GADMTools)
library(tmap)
library(leaflet)
library(leaflet.extras)
library(here)
library(rnaturalearthdata)
library(lubridate)
library(plotly)
library(htmltools)
library(raster)
library(maptools)
library(rgdal)
library(spatstat)
library(sp)
library(ggplot2)
library(anytime)
library(plyr)
library(zoo)
library(DT)
library(CGPfunctions)
library(shinyBS)
library(geoshaper)
library(ggthemes)
library(rsconnect)
library(shinycssloaders)
library(shinyWidgets)
library(BiocManager)
library(rsconnect)

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


# Slope Chart
# read in South Asia shapefiles and assign to another variable
SA_sh_df <- readRDS("Data/prepared_files/SA_sh.rds")

SA_sh_df$area <- st_area(SA_sh_df)
st_geometry(SA_sh_df) <- NULL


SA_sh_df <- SA_sh_df %>% dplyr::select(name,area) %>% dplyr::mutate(country=name)

SA_df_area <- dplyr::left_join(SA_df, SA_sh_df, by = "country")


SA_agg_country <- SA_df_area %>%
  dplyr::select(c("year","country","area","fatalities")) %>%
  dplyr::group_by(year, country,area) %>% 
  dplyr::summarise(fatalities=sum(fatalities),count=n()) %>%
  dplyr::mutate(area= as.numeric(area))%>%
  dplyr::mutate(intensity= signif(count/area, 2)) %>%
  dplyr::ungroup(year,country,area) %>%
  dplyr::mutate(year = factor(year))
  
  

newggslopegraph(SA_agg_country, year, intensity, country,
                XTextSize = 10, YTextSize = 3, WiderLabels=TRUE,
                #LineColor = colorvect,
                ThemeChoice = "gdocs",
                TitleTextSize = 16,
                TitleJustify = "center") +
  labs(title="Conflict Risk Ranking",
       subtitle= NULL, caption=NULL)

