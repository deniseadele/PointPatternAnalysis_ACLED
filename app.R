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

# Reading the raw csv file as a tbl_df
ACLED_SA <- read_csv("Data/2016-01-01-2019-12-31-Southern_Asia.csv")

# Read in aspatial dataframe
SA_df <- readRDS("Data/prepared_files/SA_df.rds")

# Read in sf object
SA_sf <- readRDS("Data/prepared_files/SA_sf.rds")


# read in South Asia shapefiles
SA_sh <- readRDS("Data/prepared_files/SA_sh.rds")

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


# CREATING DASHBOARD
##Creating title with clickable image
title<-tags$a(href="https://acleddata.com/#/dashboard",
              tags$img(src="https://acleddata.com/acleddatanew/wp-content/uploads/2019/09/logo-mobile.png", width='120',length='50'),
              'ACLED',style = "font-family: Impact; color: black; font-size: 25px")

##Create dashboardHeader
header<- dashboardHeader(
    title=title,titleWidth = 229,
    dropdownMenu(
        type = "notifications",
        notificationItem(
            text = "Visit ACLED website for more information",
            href = "https://acleddata.com/#/dashboard")
    )
)

##Create sidebars
sidebar <- dashboardSidebar(
    sidebarMenu(
        width = 350,
        menuItem("Home",tabName = "home",icon = icon("home")),
        menuItem("Exploratory",tabName = "explore", icon = icon("chart-bar")),
        menuItem("Point Pattern Analysis",tabName = "pointpattern", icon = icon("globe-asia")),
        menuItem("Spatio-Temporal Analysis",tabName = "time", icon=icon("calendar-alt")),
        menuItem("Load Data",tabName = "data", icon=icon("table"))
    )
)

explore <- tabItem(
    tabName = "explore",
    fluidRow(
        column(width = 9,
               leafletOutput("tmap_overview")
        ),
        column(width = 3,
               box(width = NULL, status = "warning",
                   checkboxGroupInput("select_eventtype1", "Select Conflict Type:",
                                      choices = c(as.vector(sort(unique(SA_df$event_type)))),
                                      selected = c("Protests")
                   )
               ),
               
               box(width = NULL, status = "warning",
                   checkboxGroupInput("select_country", "Filter countries:",
                                      choices = c(as.vector(sort(unique(SA_df$country)))),
                                      selected = c("Pakistan")
                   )
               )
        )
    )
)

pointpattern <- tabItem(
    tabName = "pointpattern",
    fluidRow(
        tabBox(
            width = NULL,
            title = "", height= "650px",
            tabPanel("First-order",
                     
                     h1("First-order analysis"),
                     h2("Kernel Density Estimation"),
                     column(width = 9,
                            leafletOutput("tmap_kd")
                     ),
                     column(width = 3,
                            box(width = NULL, status = "warning",
                                radioButtons("select_eventtype2", "Select Conflict Type:", 
                                             choices = c(as.vector(sort(unique(SA_df$event_type)))),
                                             selected = c("Protests")) 
                            ),
                            box(width = NULL, status = "warning",
                                radioButtons("select_country2", "Select Conflict Type:", 
                                             choices = c(as.vector(sort(unique(SA_df$country)))),
                                             selected = c("Pakistan"))
                            )
                            
                     )
            ),
            tabPanel("Second_order", 
                     h1("Second-order analysis"),
                     fluidRow(
                         column(width = 4,
                                box(width = NULL, status = "warning", title = "Nearest-neighbour", solidHeader = TRUE,
                                    plotOutput("nnd_plot", height = 250)
                                )),
                         column(width = 4,
                                box(width = NULL, status = "warning", title = "G function", solidHeader = TRUE,
                                    plotOutput("Gfunction", height = 250)
                                )),
                         column(width = 4,
                                box(width = NULL, status = "warning",
                                    radioButtons("select_state2", "Filter by states:", 
                                                 choices = c(as.vector(sort(PAK_sh@data$NAME_1))),
                                                 selected = c("Azad Kashmir")) 
                                )
                         )
                         
                     )
            )
        )
    )
)


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "home"),
        explore,
        pointpattern,
        tabItem(tabName = "time"),
        tabItem(tabName = "data")
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    
    
    
    
    output$tmap_overview <- renderLeaflet({
        # Creating a sf object
        SA_sf <- st_as_sf(SA_df, 
                          coords = c("longitude", "latitude"),
                          crs=4326)
        
        SA_sf <- st_transform(SA_sf, 24313)
        
        # obtaining shapefiles from rearthnaturaldata
        # convert the geospatial data to sf object
        
        SA_sh <- st_as_sf(rnaturalearthdata::countries50)%>%
            filter(adm0_a3 %in% c("IND","BGD","LKA","NPL","PAK"))%>%
            filter(name %in% c(as.vector(input$select_country)))
        SA_sh <- st_transform(SA_sh, 24313)
        
        by_eventtype <- SA_sf %>%
            filter(event_type %in% c(as.vector(input$select_eventtype1)))%>%
            filter(country%in% c(as.vector(input$select_country)))
        
        tm_SA <- tm_shape(SA_sh) +
            tm_text("name")+
            tm_fill() +
            tm_borders("black", lwd = 1) +
            tm_shape(by_eventtype) +  
            tm_dots(col="event_type", palette="Spectral", alpha= 0.5,
                    id= "data_id",
                    popup.vars= c("Country:"="country", "State/Province:"="admin1","Event Type"="event_type","Sub-Event Type"="sub_event_type","Primary actor"="actor1"))
        tmap_leaflet(tm_SA)
        
        
    })
    
    
    output$tmap_kd <- renderLeaflet({
        if (input$select_country2=="Pakistan") {
            sh <- PAK_sh
            country_ppp <- PAK_ppp
        } else if (input$select_country2=="Bangladesh") {
            sh <- BGD_sh
            country_ppp <- BGD_ppp
        } else if (input$select_country2=="Sri Lanka") {
            sh <- LKA_sh
            country_ppp <- LKA_ppp
        } else if (input$select_country2=="Nepal") {
            sh <- NPL_sh
            country_ppp <- NPL_ppp
        } else {
            sh <- IND_sh
            country_ppp <- IND_ppp
        }
        
         
        poly = as(sh, "SpatialPolygons")
        poly <- spTransform(poly, CRS=CRS("+init=epsg:24313"))
        owin <- maptools::as.owin.SpatialPolygons(poly)
        ppp <- country_ppp[owin]
        ppp_marks <- subset(ppp, marks == input$select_eventtype2) 
        kd <- density(ppp_marks)
        ras <- raster(kd, crs="+init=epsg:24313")
        
        shape <- spTransform(sh, CRS=CRS("+init=epsg:24313"))
        tmap_kd <- tm_shape(ras)+tm_raster(col="layer", style = "quantile", n = 20, palette=viridisLite::magma(7)) +
            tm_layout(frame = F, legend.format = list(format="g",digits=1)) +
            tm_shape(shape) +
            tm_borders(alpha=.3, col = "black") +
            tm_fill(col="NAME_1", alpha=0, id="NAME_1", title= "State",legend.show=FALSE)
        tmap_leaflet(tmap_kd)
    })
    
    output$nnd_plot <- renderPlot({
        sh <- PAK_sh[PAK_sh@data$NAME_1 %in% c(as.vector(input$select_state2)), ] 
        poly <- as(sh, "SpatialPolygons")
        poly <- spTransform(poly, CRS=CRS("+init=epsg:24313"))
        owin <- maptools::as.owin.SpatialPolygons(poly)
        ppp <- PAK_ppp[owin]
        nnd <- nndist(ppp)
        hist(nnd, breaks=20)
        
    })
    
    output$Gfunction <- renderPlot({
        sh <- PAK_sh[PAK_sh@data$NAME_1 %in% c(as.vector(input$select_state2)), ] 
        poly = as(sh, "SpatialPolygons")
        poly <- spTransform(poly, CRS=CRS("+init=epsg:24313"))
        owin <- maptools::as.owin.SpatialPolygons(poly)
        ppp <- PAK_ppp[owin]
        Gcsr <- envelope(ppp, Gest, correction = c("best"), nsim = 99)
        plot(Gcsr, xaxt="n", xlim = c(0,13208))
    })
    
}
shiny::shinyApp(ui, server)