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
              'corrplot')

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
duplicated(PAK_ppp)

PAK_ppp_unique <- unique(PAK_ppp)    #eliminate duplicate point events
duplicated(PAK_ppp_unique)

## Conduct all the 2nd order test using Pakistan ppp object
# Nearest neighbour
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



# 2nd order analysis on one state and one event type
sh2 <- PAK_sh[PAK_sh@data$NAME_1 == "Sind",] 
poly2 = as(sh2, "SpatialPolygons")
poly2 <- spTransform(poly2, CRS=CRS("+init=epsg:24313"))
owin2 <- maptools::as.owin.SpatialPolygons(poly2)
ppp <- PAK_ppp[owin2]
ppp_u <- unique(ppp)
ppp_u_mark <- subset(ppp_u, marks == "Protests")#, drop=TRUE)

# Nearest neighbour
nnd <- nndist(ppp_u_mark )
hist(nnd, breaks=20)

# second nearest neighbours
nnd2 <- nndist(ppp_u_mark , k=2)

# first, second and third nearest
nnd1to3 <- nndist(ppp_u_mark , k=1:3)
head(nnd1to3)

# G function

G <- Gest(ppp_u_mark , correction = "border") 
plot(G, cbind(km, rs, theo) ~ r, xaxt="n", xlim = c(0,13208))
G_csr <- envelope(ppp_u_mark , Gest, nsim = 49)
plot(G_csr)


# F function
Ftest <- Fest(ppp_u_mark)
plot(Ftest)

Fcsr <- envelope(ppp_u_mark, Fest, nsim = 99)
plot(Fcsr)

# K function
ktest_ <- Kest(ppp_u_mark) # takes awhile to load
plot(ktest_, main ="Plots all")

Kcsr <- envelope(ppp_u_mark, Kest, nsim = 99)
plot(Kcsr)

# All stats 
# NOTE: this function is only applicable to unmarked patterns
plot(allstats(ppp_u_mark))


# k-cross function??
ppp_u_marks <- subset(ppp_u, marks %in% c("Protests","Riots"))
kcross <- Kcross(ppp_u, i="Protests",j="Riots") 
plot(kcross)

kmult <- envelope(ppp_u, fun=Kcross, nsim=19, i="Protests",j="Riots")
plot(kmult)


ppp_u_m <- split(ppp_u)
plot(ppp_u[marks(ppp_u)%in% c('Protests','Riots')])
v<-c('Protests','Riots')
i <- ppp_u_m[,"Protests"]
j <- ppp_u_m[,v[2]]

mycolor= c("goldenrod", "darkblue")
X <- superimpose(Protests=ppp_u_m$Battles, Riots=ppp_u_m$Riots, W=owin2)
plot(X, main="Marked Point Patterns",col="white")
points(X, col = alpha(mycolor, 0.4), pch=16)


#ppp_u_rescale <- rescale(ppp_u)
#kcross <- envelope(ppp_u_rescale, Kcross, nsim=19, i="Protests", j="Riots",
#                simulate=expression(rshift(ppp_u_rescale, radius=150)))

# nearest neighbour
nndm <- nndist(X, by=marks(X))
plot(nndm)

nndm2 <- nndist(ppp_u, by=marks(ppp_u))
M <- cor(nndm2)
corrplot(M, method = "ellipse")

nndm_g <- as.data.frame(nndm) %>% gather(key="Event_type",value="dist",1:2)
head(nndm_g,3)


nndm_g <- nndm_g %>% mutate(id = row_number())

nndm_g_bg <- nndm_g[, -1]
ggplot(nndm_g, aes(x = dist, fill = Event_type)) +
  geom_histogram(data = nndm_g_bg, fill = "grey", alpha = .5) +
  geom_histogram(colour = "black") +
  facet_wrap(~ Event_type)


nncorr(X)
