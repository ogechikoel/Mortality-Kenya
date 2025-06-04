library(tidyverse)
library(readxl)
library(here)
library(sf)
library(shiny)
library(shinydashboard)
library(ggthemes)

##dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Mortality Analysis In kenya (2014, 2022)"),
  #creating the sidebar menu 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "home",
            fluidRow(
      box(width = 12,  status = "success", solidHeader = TRUE,
          title = "Overview",
          h4("My name is Ogechi Daniel Koel, and I’m a Data Scientist and Biostatistician at Dataquest Solutions. 
          I developed this dashboard as a visualization tool to compare neonatal, infant, child, 
          and under-five mortality rates (per 1,000 live births) across Kenyan counties between 2014 and 2022."),
          br(),
          h4("Kenya’s Vision 2030 aims to reduce neonatal mortality to 13, infant mortality to 20, 
          and under-five mortality to 24 per 1,000 live births by the year 2030. \n
        Feel free to interact with the dashboard. For collaborations or further suggestions, please reach out to us using the contact details in the footer.")
          )
    )),
    tabItem(tabName = "dashboard", 
            fluidRow(
              column(width = 6,
                     selectInput("a_2014",  label = "2014 Mortality",
                                 multiple = FALSE, choices = NULL)
              ),
              column(width = 6,
                     selectInput("a_2022",  label = "2022 Mortality",
                                 multiple = FALSE, choices = NULL)
              )
            ),
            plotOutput("map_2014"),
            plotOutput("map_2022")
    )
  ),
  
  tags$br(), tags$br(), tags$br(), tags$br(),
  
  # footer added here
  tags$div(
    class = "footer",
    style = "text-align: center; padding: 10px 20px; background-color: #f9f9f9; position: fixed; bottom: 0; width: 100%; z-index: 1000;",
    HTML(
      paste0(
        '<p>Contact us: ',
        '<a href="mailto:dataquestsolutions2@gmail.com">dataquestsolutions2@gmail.com</a>',
        ' | ',
        '<a href="https://wa.me/254707612395?text=Hello%20I%20am%20interested%20in%20your%20services" target="_blank">',
        '<i class="fab fa-whatsapp"></i> Chat on WhatsApp</a>',
        ' | ',
        '<a href="https://is.gd/WXTtwQ" target="_blank">Visit Our Website</a>',
        ' | Powered By DataQuest Solutions</p>'
      )
    )
  )
  

  )
)

  server<- function(input, output, session){
  #loading the dataset
  data <- read_excel("data.xlsx")
  
  # Extract 2014 and 2022 variable names
  vars_2014 <- grep("14$", names(data), value = TRUE)
  vars_2022 <- grep("22$", names(data), value = TRUE)
  
  observe({
    updateSelectInput(session, "a_2014", 
                      choices = c("Choose a 2014 variable" = "", vars_2014), 
                      selected = "")
    updateSelectInput(session, "a_2022", 
                      choices = c("Choose a 2022 variable" = "", vars_2022), 
                      selected = "")
  })
  
  
  # reading the shapefile 
  kenya <- st_read("kenya_county/ke_county.shp")
  
  #data pre-processing 
  
  #renaming the region column in the mortality dataset
  data  <- data %>% rename(county = Region)
  
  #Joining the datasets
  
  kenya_merged <- kenya %>%
    left_join(data, by = "county")
  
  #plotting the 2014 map
  output$map_2014 <-renderPlot({
    req(input$a_2014)
    
    ggplot()+
      geom_sf(data = kenya_merged, aes_string(fill =input$a_2014))+
      theme_map()+
      geom_sf_text(data = kenya_merged, aes(label = county), 
                   size = 3, color = "black")+
      scale_fill_gradient(low = "yellow", high = "red")+
      labs(title = paste0(input$a_2014, " Mortality in 1000 live births (2014)")
           )+
      theme(plot.title = element_text(face = "bold",
                                      size = 20,
                                      hjust = 0.5))+
      theme(legend.title = element_text(face = "bold", size = 10))
    
  })
  output$map_2022 <- renderPlot({
    req(input$a_2022)
    ggplot()+
      geom_sf(data = kenya_merged, aes_string(fill =input$a_2022))+
      theme_map()+
      geom_sf_text(data = kenya_merged, aes(label = county), 
                   size = 3, color = "black")+
      scale_fill_gradient(low = "yellow", high = "red")+
      labs(title = paste0(input$a_2022,  "  Mortality in 1000 live births (2022)"))+
      theme(plot.title = element_text(face = "bold",
                                      size = 20,
                                      hjust = 0.5))+
      theme(legend.title = element_text(face = "bold", size = 10))
    
    
  })
  
}

shinyApp(ui, server)

