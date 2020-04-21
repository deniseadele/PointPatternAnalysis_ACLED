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
              'sp')

for (p in packages){
  if (!require(p,character.only=T)){
    install.packages(p)
  }
  library(p, character.only=T)
}


# Prepare SA_df
ACLED_SA <- read_csv("Data/2016-01-01-2019-12-31-Southern_Asia.csv")
SA_df <- ACLED_SA %>%
  mutate(event_date=parse_date(event_date, "%d %B %Y"))%>%
  mutate(month=month(event_date)) %>%
  mutate(monyear = as.Date(paste0(year,"-",month, "-01"),"%Y-%m-%d"))

if (!file.exists(paste0(here::here(),"/Data/prepared_files/SA_df.csv"))){
  saveRDS(SA_df, paste0(here::here(),"/Data/prepared_files/SA_df.rds"))
  
}


# Prepare SA_sf

SA_sf <- st_as_sf(SA_df, 
                  coords = c("longitude", "latitude"),
                  crs=4326)

# use projected coordinate system 24313, and convert units from m to km
SA_sf <- st_transform(SA_sf, "+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs")

if (!file.exists(paste0(here::here(),"/Data/prepared_files/SA_sf.rds"))){
  saveRDS(SA_sf, paste0(here::here(),"/Data/prepared_files/SA_sf.rds"))
}



# Prepare SA_sp
xy <- SA_df[,c("longitude","latitude")]
SA_sp <- SpatialPointsDataFrame(coords = xy, data=SA_df, proj4string =CRS("+init=epsg:4326"))
SA_sp <- spTransform(SA_sp, CRS=CRS("+init=epsg:24313"))
SA_sp <- spTransform(SA_sp, CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))

if (!file.exists(paste0(here::here(),"/Data/prepared_files/SA_sp.rds"))){
  saveRDS(SA_sp, paste0(here::here(),"/Data/prepared_files/SA_sp.rds"))
}


# Prepare SA ppp object 
lon <- SA_sp@coords[,1]
lat <- SA_sp@coords[,2]

xrange <- range(lon, na.rm=T)
yrange <- range(lat, na.rm=T)

SA_ppp <- ppp(lon, lat, xrange, yrange, data=SA_sp, marks=as.factor(SA_sp$event_type))

if (!file.exists(paste0(here::here(),"/Data/prepared_files/SA_ppp.rds"))){
  saveRDS(SA_ppp, paste0(here::here(),"/Data/prepared_files/SA_ppp.rds"))
}

# Prepare SA_sh
# obtain shapefiles from rearthnaturaldata
# convert the geospatial data to sf object
SA_sh <- st_as_sf(rnaturalearthdata::countries50)%>%
  filter(adm0_a3 %in% c("IND","BGD","LKA","NPL","PAK"))

SA_sh <- st_transform(SA_sh, 24313)
#convert units from m to km
SA_sh <- st_transform(SA_sh, "+proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs")


if (!file.exists(paste0(here::here(),"/Data/prepared_files/SA_sh.rds"))){
  saveRDS(SA_sh, paste0(here::here(),"/Data/prepared_files/SA_sh.rds"))
}



# PREPARE PPP FOR EACH COUNTRY
PAK_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_PAK.gpkg"), layer="gadm36_PAK_1")
BGD_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_BGD.gpkg"), layer="gadm36_BGD_1")
LKA_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_LKA.gpkg"), layer="gadm36_LKA_1")
NPL_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_NPL.gpkg"), layer="gadm36_NPL_1")
IND_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_IND.gpkg"), layer="gadm36_IND_1")

# convert to spatial polygons object
PAK_poly <- as(PAK_sh, "SpatialPolygons")
BGD_poly <- as(BGD_sh, "SpatialPolygons")
LKA_poly <- as(LKA_sh, "SpatialPolygons")
NPL_poly <- as(NPL_sh, "SpatialPolygons")
IND_poly <- as(IND_sh, "SpatialPolygons")

# Transform the CRS (source is using WGS84 datum), and convert units from m to km
PAK_poly <- spTransform(PAK_poly, CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))

BGD_poly <- spTransform(BGD_poly, CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))

LKA_poly <- spTransform(LKA_poly, CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))

NPL_poly <- spTransform(NPL_poly, CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))

IND_poly <- spTransform(IND_poly, CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))


# convert to owin
PAK_owin <- maptools::as.owin.SpatialPolygons(PAK_poly)
BGD_owin <- maptools::as.owin.SpatialPolygons(BGD_poly)
LKA_owin <- maptools::as.owin.SpatialPolygons(LKA_poly)
NPL_owin <- maptools::as.owin.SpatialPolygons(NPL_poly)
IND_owin <- maptools::as.owin.SpatialPolygons(IND_poly)


#combining point patterns and the study area (countries)
PAK_ppp <- SA_ppp[PAK_owin]
BGD_ppp <- SA_ppp[BGD_owin]
LKA_ppp <- SA_ppp[LKA_owin]
NPL_ppp <- SA_ppp[NPL_owin]
IND_ppp <- SA_ppp[IND_owin]


# individual geospatial files are already available for reading..
if (!file.exists(paste0(here::here(),"/Data/prepared_files/PAK_ppp.rds"))){
  saveRDS(PAK_ppp, paste0(here::here(),"/Data/prepared_files/PAK_ppp.rds"))
}

if (!file.exists(paste0(here::here(),"/Data/prepared_files/BGD_ppp.rds"))){
  saveRDS(BGD_ppp, paste0(here::here(),"/Data/prepared_files/BGD_ppp.rds"))
}

if (!file.exists(paste0(here::here(),"/Data/prepared_files/LKA_ppp.rds"))){
  saveRDS(LKA_ppp, paste0(here::here(),"/Data/prepared_files/LKA_ppp.rds"))
}

if (!file.exists(paste0(here::here(),"/Data/prepared_files/NPL_ppp.rds"))){
  saveRDS(NPL_ppp, paste0(here::here(),"/Data/prepared_files/NPL_ppp.rds"))
}

if (!file.exists(paste0(here::here(),"/Data/prepared_files/IND_ppp.rds"))){
  saveRDS(IND_ppp, paste0(here::here(),"/Data/prepared_files/IND_ppp.rds"))
}
