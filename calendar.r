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
              'ggplot2',
              'anytime',
              'plyr',
              'zoo')

for (p in packages){
  if (!require(p,character.only=T)){
    install.packages(p)
  }
  library(p, character.only=T)
}

ACLED_SA <- read_csv("Data/2016-01-01-2019-12-31-Southern_Asia.csv")
ACLED_SA_filter <- ACLED_SA %>%
  filter(country%in% c(as.vector("Pakistan")))
ACLED_clean <- aggregate(ACLED_SA_filter, by = list(ACLED_SA_filter$event_date), FUN = length)

ACLED_clean$weekday = as.POSIXlt(anydate(ACLED_clean$Group.1))$wday
ACLED_clean$weekdayf<-factor(ACLED_clean$weekday,levels=rev(0:6),labels=rev(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),ordered=TRUE) #converting the day no. to factor 
ACLED_clean$monthf<-factor(month(anydate(ACLED_clean$Group.1)),levels=as.character(1:12),
                           labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) # finding the month 
ACLED_clean$yearmonth<- factor(as.yearmon(anydate(ACLED_clean$Group.1))) #finding the year and the month from the date. Eg: Nov 2018 
ACLED_clean$week <- as.numeric(format(anydate(ACLED_clean$Group.1),"%W")) #finding the week of the year for each date                           
ACLED_clean<-ddply(ACLED_clean,.(yearmonth),transform,monthweek=1+week-min(week)) #normalizing the week to start at 1 for every month 

p <- ggplot(ACLED_clean, aes(monthweek, weekdayf, fill = ACLED_clean$data_id)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(anydate(ACLED_clean$Group.1))~monthf) + 
  scale_fill_gradient(low="#8dd3c7", high="#fb8072") + 
  xlab("Week of Month") + ylab("") + 
  ggtitle("Time-Series Calendar Heatmap: ACLED Events in Southern Asia") + 
  labs(fill = "Number of Events") +
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_blank(),
        legend.position="top",
        legend.justification="right",
        legend.direction="horizontal",
        legend.key.size=unit(0.3,"cm"),
        legend.spacing.x=unit(0.1,"cm"))

ggplotly(p)
