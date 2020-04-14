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

# Reading the csv file as a tbl_df
ACLED_SA <- read_csv("Data/2016-01-01-2019-12-31-Southern_Asia.csv")

# Inspecting the structure of the dataset
str(ACLED_SA)

# Read in aspatial dataframe
SA_df <- readRDS("Data/prepared_files/SA_df.rds")

# Read in sp object
SA_sp <- readRDS("Data/prepared_files/SA_sp.rds")


# read in SA geopackage and convert to sp object
PAK_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_PAK.gpkg"), layer="gadm36_PAK_1")
BGD_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_BGD.gpkg"), layer="gadm36_BGD_1")
LKA_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_LKA.gpkg"), layer="gadm36_LKA_1")
NPL_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_NPL.gpkg"), layer="gadm36_NPL_1")
IND_sh <- readOGR(dsn = paste0(here::here(), "/Data/geopackage/gadm36_IND.gpkg"), layer="gadm36_IND_1")

# read in ppp objects
PAK_ppp <- readRDS("Data/prepared_files/PAK_ppp.rds")
BGD_ppp <- readRDS("Data/prepared_files/BGD_ppp.rds")
LKA_ppp <- readRDS("Data/prepared_files/LKA_ppp.rds")
NPL_ppp <- readRDS("Data/prepared_files/NPL_ppp.rds")
IND_ppp <- readRDS("Data/prepared_files/IND_ppp.rds")

#VISUALISE
plot(PAK_ppp)
plot(BGD_ppp)
plot(LKA_ppp)
plot(NPL_ppp)
plot(IND_ppp)

### FIRST-ORDER ###
# Example using pakistan
sh <- PAK_sh 
poly = as(sh, "SpatialPolygons")
poly <- spTransform(poly, CRS=CRS("+init=epsg:24313"))
owin <- maptools::as.owin.SpatialPolygons(poly)
ppp <- PAK_ppp[owin]
ppp_marks <- subset(ppp, marks == "Protests") 
kd <- density(ppp_marks)
ras <- raster(kd, crs="+init=epsg:24313")

shape <- spTransform(sh, CRS=CRS("+init=epsg:24313"))
tmap_kd <- tm_shape(ras)+tm_raster(col="layer", style = "quantile", n = 20, palette=viridisLite::magma(7)) +
  tm_layout(frame = F, legend.format = list(format="g",digits=1)) +
  tm_shape(shape) +
  tm_borders(alpha=.3, col = "black") +
  tm_fill(col="NAME_1", alpha=0, id="NAME_1", title= "State",legend.show=FALSE)
tmap_leaflet(tmap_kd)


### SECOND-ORDER ###
# is there a need to remove duplicated points?
# what if the records are independent events?
duplicated(PAK_ppp)

PAK_ppp_unique <- unique(PAK_ppp)    #eliminate duplicate point events
duplicated(PAK_ppp_unique)

## Conduct all the 2nd order test using Pakistan ppp object
# Nearest K neighbour
nnd_PAK <- nndist(PAK_ppp_unique)
hist(nnd_PAK, breaks=20)

# second nearest neighbours
nnd2 <- nndist(PAK_ppp_unique, k=2)

# first, second and third nearest
nnd1to3 <- nndist(PAK_ppp_unique, k=1:3)
head(nnd1to3)

# distance to nearest neighbour of each type
nnd_marks <- nndist(PAK_ppp_unique, by=marks(PAK_ppp_unique)) 
hist(nnd_marks, breaks=20)

# _mean_ nearest neighbour distances
aggregate(nnd_marks, by=list(from=marks(PAK_ppp_unique)), mean)

# We can formally test whether the mean nearest distance value indicates significant point clustering by using the Clark and Evan’s R statistic
# The spatstat package has the function clarkevans.test() for calculating this statistic and testing whether it is statistically significant from 1 (which indicates no clustering). 
# An R less than 1 that is statistically significant indicates clustering.
clarkevans.test(PAK_ppp_unique)

unique(marks(PAK_ppp_unique))
for (i in unique(marks(PAK_ppp_unique))){
  assign(paste0(i,"_ppp_u"), subset(PAK_ppp_unique, marks == i, drop=TRUE))
}

# G function
# i think the area is too big... ):
G <- Gest(Protests_ppp_u, correction = "border") 
plot(G, cbind(km, rs, theo) ~ r, xaxt="n", xlim = c(0,13208))
G_csr <- envelope(Protests_ppp_u, Gest, nsim = 49)
plot(G_csr)

# K function

K <- Kest(PAK_ppp, correction="Ripley")   # code taking too long to run and aborted

# guard zone correction, which we specify using correction = "border" in the envelope() function. 
#envK <- envelope(PAK_ppp, fun = Kest, correction="border", nsim = 49)

unique(PAK_sh$GID_1)
for (i in unique(PAK_sh$GID_1)){
  sh <- PAK_sh[PAK_sh@data$GID_1 == i,] 
  poly = as(sh, "SpatialPolygons")
  poly <- spTransform(poly, CRS=CRS("+init=epsg:24313"))
  owin <- maptools::as.owin.SpatialPolygons(poly)
  assign(paste0(i,"_ppp"), PAK_ppp[owin])
}


### Conduct all the 2nd order test###
# Nearest K neighbour
nnd <- nndist(PAK.1_1_ppp)
hist(nnd, breaks=20)

# second nearest neighbours
nnd2 <- nndist(PAK.1_1_ppp, k=2)

# first, second and third nearest
nnd1to3 <- nndist(PAK.1_1_ppp, k=1:3)
head(nnd1to3)

# distance to nearest neighbour of each type
nndm <- nndist(PAK.1_1_ppp, by=marks(PAK.1_1_ppp)) 

# _mean_ nearest neighbour distances
aggregate(nndm, by=list(from=marks(PAK.1_1_ppp)), mean)


# G function
# can use different types of correction? but 
G <- Gest(PAK.1_1_ppp, correction = "border") 
plot(Gcsr, xaxt="n", xlim = c(0,13208)) 
# plot(G, cbind(km,theo,rs) ~ r, xaxt="n", xlim = c(0,13208))

Gcsr <- envelope(PAK.1_1_ppp, Gest, correction = c("best"), nsim = 99)
plot(Gcsr, xaxt="n", xlim = c(0,13208))

G <- Gest(PAK.1_1_ppp, correction = "all") 
plot(G, xaxt="n", xlim = c(0,13208))

# F function
Ftest <- Fest(PAK.1_1_ppp)
plot(Ftest)

Fcsr <- envelope(PAK.1_1_ppp, Fest, nsim = 99)
plot(Fcsr)

# K function
ktest_ <- Kest(PAK.1_1_ppp) # takes awhile to load
plot(ktest_, main ="Plots all")

ktest_R <- Kest(PAK.1_1_ppp, correction="Ripley")
plot(ktest_R, main ="Ripley")

ktest_b <- Kest(PAK.1_1_ppp, correction="border")
plot(ktest_b, main ="border")

ktest_b <- Kest(PAK.1_1_ppp, correction="isotropic")
plot(ktest_b, main ="isotropic") # same as Ripley

Kcsr <- envelope(PAK.1_1_ppp, Kest, nsim = 99)
plot(Kcsr)

# All stats 
# NOTE: this function is only applicable to unmarked patterns
plot(allstats(PAK.1_1_ppp))
