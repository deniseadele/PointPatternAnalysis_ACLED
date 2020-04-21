#packages <- c('shiny','shinydashboard','tidyverse','sf','RColorBrewer','viridis','GADMTools','tmap','leaflet','here','rnaturalearthdata','lubridate','plotly','htmltools','raster','maptools','rgdal','spatstat','sp','ggplot2','anytime','plyr','zoo','DT')

#for (p in packages){
#    if (!require(p,character.only=T)){
#        install.packages(p)
#    }
#    library(p, character.only=T)
#}


library(rsconnect)
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
        tabBox(
            width = NULL,
            title = "", height= "900px",
            tabPanel("Overview",
                     fluidRow(
                         column(width = 9,
                                h3("Point Symbol Map"),
                                leafletOutput("tmap_overview"),
                                p("Click the points on the map for more information about the event.")
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
                     ),
                     fluidRow(
                         column(width = 12,
                                h4("Explore the data!"),
                                DT::dataTableOutput("datatable")
                         )
                     )
            ),
            tabPanel("Ranking",
                     fluidRow(
                         column(width = 9,
                                plotOutput("slopegraph", height= 500)
                         ),
                         column(width = 3,
                                box(width = NULL, status = "warning",
                                    radioButtons("select_rank", "Select indicators:",
                                                 choices = c("fatalities","number of events"),
                                                 selected = c("number of events")
                                    ),
                                    checkboxInput("normalise", "Normalise", value = TRUE, width = NULL),
                                    bsTooltip(id = "normalise", title = "Check to normalise indicators:<ul><li>fatalities per event</li><li>number of events per unit area (km2)</li></ul>", placement = "bottom",trigger = "hover",options = list(container = "body"))
                                )
                         )
                     )
            ),
            tabPanel("Calendar Chart",
                     fluidRow(
                         column(width = 10,
                                h3("Time-series Calendar Heatmap"),
                                plotlyOutput("calendar_view")),
                         column(width = 2,
                                box(width = NULL, status = "warning",
                                    checkboxGroupInput("filter_country", "Filter countries:",
                                                       choices = c(as.vector(sort(unique(ACLED_SA$country)))),
                                                       selected = c("Bangladesh","India","Nepal","Pakistan","Sri Lanka"))))
                     )
            )
        )
        
    )
)

pointpattern <- tabItem(
    tabName = "pointpattern",
    
    fluidRow(
        column(width=9,
               tabBox(
                   width = NULL,
                   title = "", height= "650px", id = "tabbox",
                   tabPanel("First-order",
                            h3("First-order analysis: "),
                            h4("Kernel Density Estimation"),
                            column(width = 12,
                                   leafletOutput("tmap_kd", height=480))

                            ),
                   tabPanel("Second-order",
                            h3("Second-order analysis"),
                            column(width = 12,
                                   fluidRow(
                                       box(width = NULL, status = "warning", title = "Statistical Inference", solidHeader = TRUE,
                                           plotOutput("env_function"))
                                       )
                                   #fluidRow(
                                   #    box(width = NULL, status = "warning", title = "Nearest-neighbour distance", solidHeader = T, collapsible =  T, collapsed = T,
                                   #        column(width = 6, plotOutput("nnd_plot", height = 250)),
                                   #        column(width=6, plotOutput("Gfunction", height = 250))
                                   #    )
                                   #),
                                   #fluidRow(
                                   #    box(width = NULL, status = "warning", title = "Empty-space distance", solidHeader = TRUE, collapsible =  T, collapsed = T,
                                   #        column(width=6, plotOutput("distmap", height = 250)),
                                   #        column(width=6, plotOutput("Ffunction", height = 250))
                                   #    )
                                   #)
                                   
                                   )

                   ),
                   tabPanel("Cross-type", id=3,
                            h3("Cross-type Point Patterns"),
                            fluidRow(
                                column(width = 6,
                                       box(width = NULL, status = "warning", title = "Marked Point Patterns", solidHeader = TRUE,
                                           plotOutput("mpp_plot", height = 320)
                                       )
                                ),
                                column(width = 6,
                                       box(width = NULL, status = "warning", title = "Summary Functions", solidHeader = TRUE,
                                           plotOutput("summaryfunction", height = 320)
                                       )
                                       )
                                )
                            )
               )
               ),
               
               column(width=3, title="Global Filters:",
                      box(width = NULL, status = "warning", collapsible = T, solidHeader = F, title = "Global Filters",
                          selectInput("select_country2", "Select Country:", 
                                      choices = c(as.vector(sort(unique(SA_df$country)))),
                                      selected = c("Pakistan")),
                          selectInput("select_state", "Select States:",
                                      choices = c(as.vector(sort(unique(SA_df$country)))),
                                      selected = c(as.vector(sort(unique(SA_df$country)))),
                                      multiple = TRUE),
                          selectInput("select_eventtype2", "Select Conflict Type:", 
                                      choices = c(as.vector(sort(unique(SA_df$event_type)))),
                                      selected = c("Protests"))
                      ),
                      conditionalPanel( condition = "input.tabbox == 'First-order'",
                      
                          box(width = NULL, status = "warning",
                              checkboxInput("explore_bw", "Explore bandwidth selection:", value = FALSE, width = NULL),
                              conditionalPanel( condition = "input.explore_bw == true",
                                                radioButtons("bw_selection", "Bandwidth selection:", 
                                                             choices = c("Fixed","Adaptive"),
                                                             selected = c("Fixed")),
                                                conditionalPanel( condition = "input.bw_selection == 'Fixed'",
                                                                  radioButtons("fixed_selection", "Fixed-Bandwidth method:", 
                                                                               choices = c("Automatic","Manual"),
                                                                               selected = c("Automatic")),
                                                                  conditionalPanel( condition = "input.fixed_selection == 'Automatic'",
                                                                                    selectInput("select_autobw", "Cross-validation selection method:", 
                                                                                                choices = c("bw.diggle","bw.CvL","bw.scott","bw.ppl"),
                                                                                                selected = c("bw.diggle"))
                                                                  ),
                                                                  conditionalPanel( condition = "input.fixed_selection == 'Manual'",
                                                                                    sliderInput("select_sigma", "Standard deviation of Gaussian Kernel",
                                                                                                min = 0, max = 50, value = 20, step = 0.1))
                                                                  )
                                                )
                              )
                      ),
                      conditionalPanel( condition = "input.tabbox == 'Second-order'",
                                        
                                        box(width = NULL, status = "warning", 
                                            radioButtons("select_disttype", "Select distance measure:", 
                                                         choices = c("Pairwise","Nearest-neighbour","Empty-space"),
                                                         selected = c("Pairwise")
                                            ),
                                            sliderInput("select_radius", "Distance Range (km)",
                                                        min = 0, max = 50, value = c(0, 20)
                                            ),
                                            sliderInput("select_nsim", "# Monte Carlo Simulation:",
                                                        min = 0, max = 100, value = 49
                                            ) 
                                            )
                                        ),
                      conditionalPanel( condition = "input.tabbox == 'Cross-type'",
                                        
                                        box(width = NULL, status = "warning", 
                                            selectInput("select_pairtypes", "Select second pair of types",
                                                           choices = c(as.vector(sort(unique(SA_df$event_type)))),
                                                           selected = c("Riots")
                                                        ),
                                            radioButtons("select_function", "Select Summary Function:", 
                                                         choices = c("K function",
                                                                     "G function",
                                                                     "J function"),
                                                         selected = c("K function")
                                            ),
                                            sliderInput("select_nsim2", "# Monte Carlo Simulation:",
                                                        min = 0, max = 50, value = 19
                                            ) 
                                        )
                                        )
                      
                      
                      )

    )
)

time <- tabItem(
    tabName = "time",
    fluidRow(
        box(width = NULL, status = "warning", collapsible = T, solidHeader = F, title = "Global Filters: Region",
            column(width=2,
                   box(width = NULL, status = "warning",
                       selectInput("filter_country1", "Filter countries:",
                                   choices = c(as.vector(sort(unique(ACLED_SA$country))),"All"),
                                   selected = c("All")))
            )
        )
    ),
    fluidRow(
        column(width = 6,
               box(width = NULL, status = "warning",
                   leafletOutput("mymap")),
               box(width = NULL, status = "warning",
                   dataTableOutput("mytable"))),
        column(width = 6,
               box(width = NULL, status = "warning",
                   plotlyOutput("stplot")),
               box(width = NULL, status = "warning",
                   plotlyOutput("mylinechart")))
        )
)

data<-tabItem(
    tabName = "data",
    fluidRow(
        box(width = NULL, status = "warning", collapsible = T, solidHeader = F, title = "Load New Dataset",
            column(width = 12,
                   fileInput("file1", "Choose CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"))),
            column(width = 4,
                   checkboxInput("header", "Header", TRUE)),
            column(width = 4,
                   radioButtons("sep", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ",")),
            column(width = 4,
                   radioButtons("quote", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"')))),
    fluidRow(
        box(width = NULL, status = "warning", collapsible = T, solidHeader = F, title = "Data Adjustments",    
            column(width = 4,
                   radioButtons("disp", "Display view",
                                choices = c(Head = "head",
                                            All = "all"),
                                selected = "head")),
            column(width = 4,
                   downloadButton(outputId = "downloadData", label = "Download Datatset")
            )
        )),
    fluidRow(
        width = NULL,
        title = "", height= "1000px",
        tableOutput("contents")
    )
)


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "home"),
        explore,
        pointpattern,
        time,
        data
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
    
    observe({
        
        if (input$select_country2=="Pakistan") {
            x <- as.vector(sort(PAK_sh@data$NAME_1))
        } else if (input$select_country2=="Bangladesh") {
            x <- as.vector(sort(BGD_sh@data$NAME_1))
        } else if (input$select_country2=="Sri Lanka") {
            x <- as.vector(sort(LKA_sh@data$NAME_1))
        } else if (input$select_country2=="Nepal") {
            x <- as.vector(sort(NPL_sh@data$NAME_1))
        } else {
            x <- as.vector(sort(IND_sh@data$NAME_1))
        }
        
        updateSelectInput(session, "select_state",
                          choices = x,
                          selected = head(x,1)
        )
    })
    
    observe({
        
        x <- as.vector(sort(unique(SA_df$event_type)))
        x2 <- x[x != input$select_eventtype2]
        
        updateSelectInput(session, "select_pairtypes",
                          choices = x2,
                          selected = head(x2,1)
        )
    })
    
 
    
    output$tmap_overview <- renderLeaflet({
        # Creating a sf object
        SA_sf <- st_as_sf(SA_df, 
                          coords = c("longitude", "latitude"),
                          crs=4326)
        
        SA_sf <- st_transform(SA_sf, "+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs")
        
        # obtaining shapefiles from rearthnaturaldata
        # convert the geospatial data to sf object
        
        SA_sh <- st_as_sf(rnaturalearthdata::countries50)%>%
            filter(adm0_a3 %in% c("IND","BGD","LKA","NPL","PAK"))%>%
            filter(name %in% c(as.vector(input$select_country)))
        SA_sh <- st_transform(SA_sh, "+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs")
        
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
    
    output$datatable <- DT::renderDataTable({
        DT::datatable(data = SA_df %>% 
                          filter(country %in% input$select_country, event_type %in% input$select_eventtype1)  %>% 
                          dplyr::select(data_id, event_date, event_type, actor1, country, admin1, location, fatalities),
                      options = list(pageLength = 5,
                                     lengthMenu = c(5, 10, 15, 20),
                                     initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                         "}")),
                      selection = list(target = 'row+column'),
                      rownames=FALSE)
        
    })
    
    
    output$slopegraph <- renderPlot({
        SA_sh_df <- readRDS("Data/prepared_files/SA_sh.rds")
        
        SA_sh_df$area <- st_area(SA_sh_df)
        st_geometry(SA_sh_df) <- NULL
        
        SA_sh_df <- SA_sh_df %>% dplyr::select(name,area) %>% dplyr::mutate(country=name)
        SA_df_area <- dplyr::left_join(SA_df, SA_sh_df, by = "country")
        
        
        
        SA_agg_country <- SA_df_area %>%
            dplyr::select(c("year","country","area","fatalities")) %>%
            dplyr::group_by(year, country,area) %>% 
            dplyr::summarise(fatalities=sum(fatalities),`number of events`=n()) %>%
            dplyr::mutate(area= as.numeric(area))%>%
            dplyr::mutate(`number of events_norm`= signif(`number of events`/area,2)) %>%
            dplyr::mutate(fatalities_norm= signif(fatalities/`number of events`,2)) %>%
            dplyr::ungroup(year,country,area) %>%
            dplyr::mutate(year = factor(year)) #%>% 
            #dplyr::rename(x = input$select_rank)
        
        if (input$normalise){
            SA_agg_country <- SA_agg_country %>% 
                dplyr::rename(x=paste0(input$select_rank,"_norm"))
            if (input$select_rank == 'fatalities'){
                title <- 'fatalities per event'
            } else{
                title <- 'number of events per unit area (km2)'
            }
            
        } else {
            SA_agg_country <- SA_agg_country %>% 
                dplyr::rename(x=input$select_rank)
            title= input$select_rank
        }
        
        newggslopegraph(SA_agg_country, year, x, country,
                        DataTextSize = 4.5, 
                        YTextSize = 5, 
                        XTextSize = 16,
                        DataLabelPadding = .9,
                        WiderLabels=TRUE,
                        #LineColor = colorvect,
                        ThemeChoice = "gdocs",
                        TitleTextSize = 18,
                        TitleJustify = "center") +
            labs(title=paste0("Conflict Risk Ranking: By ",title),
                 subtitle= NULL , caption=NULL)
        
    })
    
    output$calendar_view <- renderPlotly({
        ACLED_SA<-uploadData()
        ACLED_SA_filter<-ACLED_SA %>%
            filter(country%in% c(as.vector(input$filter_country)))
        ACLED_clean <- aggregate(ACLED_SA_filter, by = list(ACLED_SA_filter$event_date), FUN = length)
        colnames(ACLED_clean)[grep("data_id", colnames(ACLED_clean))] <-"Events"
        
        ACLED_clean$weekday = as.POSIXlt(anydate(ACLED_clean$Group.1))$wday
        ACLED_clean$Day_of_week<-factor(ACLED_clean$weekday,levels=rev(0:6),labels=rev(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),ordered=TRUE) #converting the day no. to factor 
        ACLED_clean$monthf<-factor(month(anydate(ACLED_clean$Group.1)),levels=as.character(1:12),
                                   labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) # finding the month 
        ACLED_clean$yearmonth<- factor(as.yearmon(anydate(ACLED_clean$Group.1))) #finding the year and the month from the date. Eg: Nov 2018 
        ACLED_clean$week <- as.numeric(format(anydate(ACLED_clean$Group.1),"%W")) #finding the week of the year for each date                           
        ACLED_clean<-ddply(ACLED_clean,.(yearmonth),transform,Week_of_month=1+week-min(week)) #normalizing the week to start at 1 for every month 
        
        ggplot(ACLED_clean, aes(Week_of_month, Day_of_week, fill = Events)) + 
            geom_tile(colour = "white") + 
            facet_grid(year(anydate(ACLED_clean$Group.1))~monthf) + 
            scale_fill_gradient(low="#009ACD", high="#ff0040") + 
            xlab("Week of Month") + ylab("Day of Week") + 
            labs(fill = "Events") +
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
    })
    
    sh <- reactive({
        if (input$select_country2=="Pakistan") {
            PAK_sh
        } else if (input$select_country2=="Bangladesh") {
            BGD_sh
        } else if (input$select_country2=="Sri Lanka") {
            LKA_sh
        } else if (input$select_country2=="Nepal") {
            NPL_sh
        } else {
            IND_sh
        }
    })
    
    country_ppp <- reactive({
        if (input$select_country2=="Pakistan") {
            PAK_ppp
        } else if (input$select_country2=="Bangladesh") {
            BGD_ppp
        } else if (input$select_country2=="Sri Lanka") {
            LKA_ppp
        } else if (input$select_country2=="Nepal") {
            NPL_ppp
        } else {
            IND_ppp
        }
    })
    
    ssh <- reactive({
        sh()[sh()@data$NAME_1 %in% c(as.vector(input$select_state)), ]
    })
    poly <- reactive({
        as(ssh(), "SpatialPolygons")
    })
    
    poly2 <- reactive({
        spTransform(poly(), CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))
    })
    
    owin <- reactive({
        maptools::as.owin.SpatialPolygons(poly2())
    })
    
    ppp <- reactive({
        country_ppp()[owin()]
    })
    
    
  
    
    output$tmap_kd <- renderLeaflet({

        ppp_marks <- subset(ppp(), marks == input$select_eventtype2)
        
        if (input$explore_bw & (input$bw_selection =="Fixed")){
            if (input$fixed_selection =="Automatic"){
                if (input$select_autobw == "bw.diggle"){
                    bw <- bw.diggle(ppp_marks)
                } else if(input$select_autobw  == "bw.CvL"){
                    bw <- bw.CvL(ppp_marks)
                } else if(input$select_autobw  == "bw.scott"){
                    bw <- bw.scott(ppp_marks)
                } else {
                    bw <- bw.ppl(ppp_marks)
                } 
            } else{
                bw <- input$select_sigma
            }
            
            kd <- density(ppp_marks, sigma = bw, adjust = 1, kernel = "gaussian")
            
        } else if (input$explore_bw & (input$bw_selection =="Adaptive")){
            kd <- adaptive.density(ppp_marks, method="kernel")
        } else {
            bw <- NULL
            kd <- density(ppp_marks, sigma = bw, adjust = 1, kernel = "gaussian")
        }
        
        
        ras <- raster(kd, crs="+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs")
        
        shape <- spTransform(sh(), CRS=CRS("+init=epsg:24313 +proj=utm +zone=43 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=km +no_defs"))
        tmap_kd <- tm_shape(ras)+tm_raster(col="layer", style = "quantile", n = 10, palette=viridisLite::magma(7)) +
            tm_layout(frame = F, legend.format = list(format="g",digits=1)) +
            tm_shape(shape) +
            tm_borders(alpha=.3, col = "black") +
            tm_fill(col="NAME_1", alpha=0, id="NAME_1", title= "State",legend.show=FALSE)
        tmap_leaflet(tmap_kd)
    })
    

    
    output$env_function <- renderPlot({
        ppp_marks <- subset(ppp(), marks == input$select_eventtype2)
        ppp_marks_u <- unique(ppp_marks)
        if (input$select_disttype == 'Pairwise'){
            csr <- envelope(ppp_marks_u, Kest, nsim = input$select_nsim)
        } else if (input$select_disttype == 'Nearest-neighbour') {
            csr <- envelope(ppp_marks_u, Gest, correction = c("best"), nsim = input$select_nsim)
        } else {
            csr <- envelope(ppp_marks_u, Fest, nsim = input$select_nsim)
        }
        
        
        plot(csr, main=NULL, xaxt="n", xlim = c(input$select_radius[1],input$select_radius[2]))
    })
    

    output$mpp_plot <- renderPlot({
        ppp_u <- unique(ppp())
        ppp_u_m <- split(ppp_u)
        i <- ppp_u_m[factor=input$select_pairtypes[1]]
        j <- ppp_u_m[factor=input$select_pairtypes[2]]
        X <- superimpose(i,
                         j, 
                         W=owin())
        plot(X, main= NULL)
        
        
    })
    
    output$summaryfunction <- renderPlot({
        ppp_u <- unique(ppp())
        if (input$select_function=="K function") {
            cross_csr <- envelope(ppp_u, fun=Kcross, nsim=input$select_nsim2, i=input$select_pairtypes[1],j=input$select_pairtypes[2])
        } else if (input$select_function=="G function") {
            cross_csr <- envelope(ppp_u, fun=Gcross, nsim=input$select_nsim2, i=input$select_pairtypes[1],j=input$select_pairtypes[2])
        } else {
            cross_csr <- envelope(ppp_u, fun=Jcross, nsim=input$select_nsim2, i=input$select_pairtypes[1],j=input$select_pairtypes[2])
        }
        
        plot(cross_csr, main=paste0(input$select_function))
    })
    
    uploadData <- reactive({
        infile<-input$file1
        df2<-read_csv("data/2016-01-01-2019-12-31-Southern_Asia.csv")
        df2<- subset(df2,select=-c(notes))
        if(is.null(infile))
        {
            return(df2)
        }
        else{
            df<-read.csv(infile$datapath,header=input$header,sep=input$sep,quote=input$quote)
            return(df)
        }
    })
    
    
    output$contents<-renderTable({
        df <- uploadData() 
        if(input$disp=="head")
        {
            return(head(df))
        }
        return(df)
    })
    
    output$downloadData <- downloadHandler(
        filename = "data/2016-01-01-2019-12-31-Southern_Asia.csv",
        content = function(file) {
            # The code for filtering the data is copied from the
            # renderTable() function
            data <- uploadData()
            # Write the filtered data into a CSV file
            write_csv(head(data), file)
        }
    )
    
    # Spatio-temporal
    ################################################# section one #################################################
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())
    
    ################################################# section two #################################################
    ACLED_SA <- subset(ACLED_SA,select=-c(notes))
    ACLED_SA$secondLocationID <- paste(as.character(ACLED_SA$data_id), "_selectedLayer", sep="")
    coordinates <- SpatialPointsDataFrame(ACLED_SA[,c('longitude', 'latitude')] , ACLED_SA)
    
    # base map
    output$mymap <- renderLeaflet({
        if(input$filter_country1=="All"){
            ACLED_SA<-ACLED_SA
            #ACLED_SA$event_date<- as.Date(ACLED_SA$event_date, format = "%d %B %Y")
            #ACLED_SA<-ACLED_SA %>%
            #  filter(event_date %in% c(as.Date(input$date)))
        }
        else{
            ACLED_SA<-ACLED_SA %>%
                filter(country%in% c(as.vector(input$filter_country1)))
            #ACLED_SA$event_date<- as.Date(ACLED_SA$event_date, format = "%d %B %Y")
            #ACLED_SA<-ACLED_SA %>%
            #  filter(event_date %in% c(as.Date(input$date)))
        }
        leaflet() %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircles(data = ACLED_SA,
                       radius = 1000,
                       lat = ACLED_SA$latitude,
                       lng = ACLED_SA$longitude,
                       fillColor = "red",
                       fillOpacity = 1,
                       color = "black",
                       weight = 1,
                       stroke = T,
                       layerId = as.character(ACLED_SA$data_id),
                       highlightOptions = highlightOptions(color = "mediumseagreen",
                                                           opacity = 1.0,
                                                           weight = 2,
                                                           bringToFront = TRUE)) %>%
            addDrawToolbar(
                targetGroup='Selected',
                polylineOptions=FALSE,
                markerOptions = FALSE,
                polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'grey'
                                                                                  ,weight = 2)),
                rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                      ,color = 'grey'
                                                                                      ,weight = 2)),
                circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'grey'
                                                                                  ,weight = 2)),
                editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    })
    
    ############################################### section three #################################################
    
    observeEvent(input$mymap_draw_new_feature,{
        #Only add new layers for bounded locations
        found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature
                                         , location_coordinates = coordinates
                                         , location_id_colname = "data_id")
        
        for(id in found_in_bounds){
            if(id %in% data_of_click$clickedMarker){
                # don't add id
            } else {
                # add id
                data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
            }
        }
        
        # look up airports by ids found
        selected <- subset(ACLED_SA, data_id %in% data_of_click$clickedMarker)
        
        proxy <- leafletProxy("mymap")
        proxy %>% addCircles(data = selected,
                             radius = 1000,
                             lat = selected$latitude,
                             lng = selected$longitude,
                             fillColor = "wheat",
                             fillOpacity = 1,
                             color = "mediumseagreen",
                             weight = 3,
                             stroke = T,
                             layerId = as.character(selected$secondLocationID),
                             highlightOptions = highlightOptions(color = "hotpink",
                                                                 opacity = 1.0,
                                                                 weight = 2,
                                                                 bringToFront = TRUE))
        
        output$mytable <- renderDataTable(
            selected, options = list(scrollX = TRUE))
        
        output$stplot <- renderPlotly({
            
            plot_ly(selected, x = selected$longitude, y = selected$latitude, z = selected$event_date, color=selected$event_type, 
                    text = ~paste("ID:", selected$data_id,
                                  "\nEvent type:", selected$event_type),
                    hovertemplate = paste(
                        "<b>%{text}</b><br>",
                        "Event Date: %{z}",
                        "<extra></extra>")
            ) %>%
                add_markers() %>%
                layout(scene = list(xaxis = list(title = 'Longitude'),
                                    yaxis = list(title = 'Latitude'),
                                    zaxis = list(title = 'Event Date')),showlegend=FALSE)
        })
        
        output$mylinechart <- renderPlotly({
            
            ACLED_clean <- aggregate(selected, by = list(selected$event_date), FUN = length)
            colnames(ACLED_clean)[grep("data_id", colnames(ACLED_clean))] <-"No. of Events"
            ACLED_clean$Group.1<-anydate(ACLED_clean$Group.1)
            colnames(ACLED_clean)[grep("Group.1", colnames(ACLED_clean))] <-"Event Date"
            ggplot(ACLED_clean, aes(x=`Event Date`, y=`No. of Events`)) +
                geom_line( color="steelblue") + 
                geom_point() +
                theme(axis.text.x=element_text(angle=60, hjust=1)) +
                theme_minimal()
        })
        
        
        
    })
    
}
shiny::shinyApp(ui, server)