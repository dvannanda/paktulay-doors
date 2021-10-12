#' This is a shiny web application script
#' Aim: To calculate the materials needed for an aluminium project build
#' 
#' Author: Devi Annanda
#' Github: dvannanda

library(shiny)
library(tidyverse)
library(reactable)

library(shinyWidgets)

# import data
df <- read_csv(here::here("data/tidy-data.csv"))

type <- df$tipe %>% unique()
model <- df$model %>% unique()
material <- df$materi %>% unique()

source(here::here("R/countalum.R"))

# UI -------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "flatly"),
    
    navbarPage(
        
        title = "Factory Aluminium Count",
        
        tabPanel(
            "Home",
            
            sidebarLayout(
                sidebarPanel(
                    h2("Input"),
                    tags$br(),
                    
                    pickerInput(
                        inputId = "Id001",
                        label = "Pilih Tipe",
                        choices = type,
                        selected = type,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                    ),
                    
                    pickerInput(
                        inputId = "Id002",
                        label = "Pilih Model",
                        choices = model,
                        selected = model,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                    ),
                    
                    pickerInput(
                        inputId = "Id003",
                        label = "Pilih Materi Aluminium",
                        choices = material,
                        selected = material,
                        options = list(`actions-box` = TRUE),
                        multiple = FALSE
                    ),
                    
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    
                    downloadButton(
                        outputId = "download",
                        label = "Download CSV"
                    )
                    
                ), # sidebarPanel
                mainPanel(tabsetPanel(
                    id = "tabset-output",
                    tabPanel("# Bar Perlu",
                             tags$br(),
                             h5(textOutput("text1")),
                             tags$br(),
                             reactableOutput("output1")),
                    tabPanel("Rincian",
                             reactableOutput("output2"))
                )) # tabsetPanel & mainPanel
            ) # sidebarLayout
        ), # tabPanel$home
        tabPanel(
            "About"
        ) # tabPanel$about
    ) # navbarPage
) # fluidPage
    

# SERVER ---------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
    
    reacDataType <- reactive({
        df %>% 
            dplyr::filter(tipe %in% input$Id001)
    })
    
    reacDataModel <- reactive({
        reacDataType() %>% 
            dplyr::filter(model %in% input$Id002)
    })
    
    reacData <- reactive({
        reacDataModel() %>% 
            dplyr::filter(materi == input$Id003) %>% 
            mutate(total = `perlu per unit` * unit) %>% 
            group_by(ukuran) %>% 
            summarise(total = sum(total))
            
    })

    # change model input acc to type
    observeEvent(input$Id001, {
        updatePickerInput(
            session = session,
            inputId = "Id002",
            choices = reacDataType()$model %>% unique(),
            selected = reacDataType()$model %>% unique())
    })
    
    # change 
    observeEvent(input$Id002, {
        updatePickerInput(
            session = session,
            inputId = "Id003",
            choices = reacDataModel()$materi %>% unique() %>% sort(),
            selected = reacDataModel()$materi %>% unique() %>% sort()
        )
    })
    
    reacNeed <- reactive({
        rep(reacData()$ukuran, times = reacData()$total) %>% 
            sort(decreasing = TRUE) %>% 
            countalum()
    })
    
    output$output1 <- renderReactable({
        reacNeed() %>% 
            reactable(pagination = FALSE)
    })
    
    output$text1 <- renderText({
        paste("No of bars needed for ", input$Id003, " = ", nrow(reacNeed()))
    })
    
    output$output2 <- renderReactable({
        reacDataModel() %>% 
            reactable()
    }) 
    
} # server


# Run the application --------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
