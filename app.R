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
        box(width = NULL, status = "warning", collapsible = T, solidHeader = F, title = "Global Filters: Region",
            column(width = 6,
                   selectInput("select_country2", "Select Country:", 
                               choices = c(as.vector(sort(unique(SA_df$country)))),
                               selected = c("Pakistan")
                               )
            ),
            column(width = 6,
                   selectInput("select_state", "Select States:",
                               choices = c(as.vector(sort(unique(SA_df$country)))),
                               selected = c(as.vector(sort(unique(SA_df$country)))),
                               multiple = TRUE)
            )
        )
    ),
    
    fluidRow(
        tabBox(
            width = NULL,
            title = "", height= "650px",
            tabPanel("First-order",
                     
                     h3("First-order analysis"),
                     h4("Kernel Density Estimation"),
                     column(width = 9,
                            leafletOutput("tmap_kd")
                     ),
                     column(width = 3,
                            box(width = NULL, status = "warning",
                                radioButtons("select_eventtype2", "Select Conflict Type:", 
                                             choices = c(as.vector(sort(unique(SA_df$event_type)))),
                                             selected = c("Protests")) 
                            )
                     )
            ),
            tabPanel("Second-order", 
                     h3("Second-order analysis"),
                     fluidRow(
                         column(width = 4,
                                box(width = NULL, status = "warning", title = "Nearest-neighbour", solidHeader = TRUE,
                                    plotOutput("nnd_plot", height = 250)
                                    ),
                                box(width = NULL, status = "warning", title = "F function", solidHeader = TRUE,
                                    plotOutput("Ffunction", height = 250)
                                    )
                                ),
                         column(width = 4,
                                box(width = NULL, status = "warning", title = "G function", solidHeader = TRUE,
                                    plotOutput("Gfunction", height = 250)
                                    ),
                                box(width = NULL, status = "warning", title = "K function", solidHeader = TRUE,
                                    plotOutput("Kfunction", height = 250)
                                    )
                                ),
                         column(width = 4,
                                box(width = NULL, status = "warning",
                                    radioButtons("select_eventtype3", "Select Conflict Type:", 
                                                 choices = c(as.vector(sort(unique(SA_df$event_type)))),
                                                 selected = c("Protests")
                                                 )
                                    ),
                                box(width = NULL, status = "warning",
                                    sliderInput("select_nsim", "# Monte Carlo Simulation:",
                                                min = 0, max = 100, value = 49
                                                 )
                                    )
                                )
                         )
            ),
            tabPanel("Multitype", 
                     h3("Multitype Point Patterns"),
                     fluidRow(
                         column(width = 5,
                                box(width = NULL, status = "warning", title = "Marked Point Patterns", solidHeader = TRUE,
                                    plotOutput("mpp_plot", height = 320)
                                )
                         ),
                         column(width = 5,
                                box(width = NULL, status = "warning", title = "Summary Functions", solidHeader = TRUE,
                                    plotOutput("summaryfunction", height = 320)
                                )
                         ),
                         column(width = 2,
                                box(width = NULL, status = "warning",
                                    selectizeInput("select_pairtypes", "Select pairs of types",
                                                options = list(maxItems = 2),
                                                choices = c(as.vector(sort(unique(SA_df$event_type)))),
                                                selected = c("Protests","Riots"),
                                                multiple = TRUE)
                                ),
                                box(width = NULL, status = "warning",
                                    radioButtons("select_function", "Select Summary Function:", 
                                                 choices = c("K function",
                                                             "G function",
                                                             "J function"),
                                                 selected = c("K function")
                                    )
                                ),
                                box(width = NULL, status = "warning",
                                    sliderInput("select_nsim2", "# Monte Carlo Simulation:",
                                                min = 0, max = 50, value = 19
                                    )
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
        spTransform(poly(), CRS=CRS("+init=epsg:24313"))
    })
    
    owin <- reactive({
        maptools::as.owin.SpatialPolygons(poly2())
    })
    
    ppp <- reactive({
        country_ppp()[owin()]
    })

    
    output$tmap_kd <- renderLeaflet({
        
        ppp_marks <- subset(ppp(), marks == input$select_eventtype2) 
        kd <- density(ppp_marks)
        ras <- raster(kd, crs="+init=epsg:24313")
        
        shape <- spTransform(sh(), CRS=CRS("+init=epsg:24313"))
        tmap_kd <- tm_shape(ras)+tm_raster(col="layer", style = "quantile", n = 20, palette=viridisLite::magma(7)) +
            tm_layout(frame = F, legend.format = list(format="g",digits=1)) +
            tm_shape(shape) +
            tm_borders(alpha=.3, col = "black") +
            tm_fill(col="NAME_1", alpha=0, id="NAME_1", title= "State",legend.show=FALSE)
        tmap_leaflet(tmap_kd)
    })
    
    output$nnd_plot <- renderPlot({
        ppp_marks <- subset(ppp(), marks == input$select_eventtype3)
        ppp_marks_u <- unique(ppp_marks)
        nnd <- nndist(ppp_marks_u)
        hist(nnd, breaks=20)
        
    })
    
    output$Gfunction <- renderPlot({
        ppp_marks <- subset(ppp(), marks == input$select_eventtype3)
        ppp_marks_u <- unique(ppp_marks)
        Gcsr <- envelope(ppp_marks_u, Gest, correction = c("best"), nsim = input$select_nsim)
        plot(Gcsr, xaxt="n", xlim = c(0,13208))
    })
    
    
    output$Ffunction <- renderPlot({
        ppp_marks <- subset(ppp(), marks == input$select_eventtype3)
        ppp_marks_u <- unique(ppp_marks)
        Fcsr <- envelope(ppp_marks_u, Fest, nsim = input$select_nsim)
        plot(Fcsr)
    })
    
    output$Kfunction <- renderPlot({
        ppp_marks <- subset(ppp(), marks == input$select_eventtype3)
        ppp_marks_u <- unique(ppp_marks)
        Kcsr <- envelope(ppp_marks_u, Kest, nsim = input$select_nsim)
        plot(Kcsr)
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
    
}
shiny::shinyApp(ui, server)