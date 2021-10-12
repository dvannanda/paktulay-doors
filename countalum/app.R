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
df <- read_csv("tidy-data.csv")

type <- df$tipe %>% unique()
model <- df$model %>% unique()
material <- df$materi %>% unique()

source("countalum.R")

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
                    
                    h3("Ringkasan Tipe dan Model"),
                    tags$br(),
                    reactableOutput("output4"),
                    tags$br(),
                    downloadButton(
                        outputId = "download4",
                        label = "Download the table as CSV"
                    )
                    
                ), # sidebarPanel
                mainPanel(tabsetPanel(
                    id = "tabset-output",
                    tabPanel("# Bar Perlu",
                             tags$br(),
                             h5(textOutput("text1")),
                             tags$br(),
                             reactableOutput("output1"),
                             tags$br(),
                             tags$br(),
                             downloadButton(
                                 outputId = "download1",
                                 label = "Download the table as CSV"
                             )),
                    tabPanel("Rincian Materi",
                             reactableOutput("output2"),
                             tags$br(),
                             tags$br(),
                             downloadButton(
                                 outputId = "download2",
                                 label = "Download the table as CSV"
                             )),
                    tabPanel("Rincian Model",
                             reactableOutput("output3"),
                             tags$br(),
                             tags$br(),
                             downloadButton(
                                 outputId = "download3",
                                 label = "Download the table as CSV"
                             ))
                )) # tabsetPanel & mainPanel
            ) # sidebarLayout
        ), # tabPanel$home
        tabPanel(
            "About",
            
            source("about.R")
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
            countalum() %>% 
            rename(
                "potongan (mm)" = "bar_cuts",
                "sisa (mm)" = "remainder"
            )
    })
    
    output$output1 <- renderReactable({
        reacNeed() %>% 
            reactable(pagination = FALSE)
    })
    
    output$text1 <- renderText({
        paste("No bar perlu untuk ", input$Id003, " = ", nrow(reacNeed()))
    })
    
    output$output2 <- renderReactable({
        reacDataModel() %>% 
            dplyr::filter(materi == input$Id003) %>% 
            reactable(pagination = FALSE)
    }) 
    
    output$output3 <- renderReactable({
        reacDataModel() %>% 
            reactable(pagination = FALSE)
    }) 
    
    output$download1 <- downloadHandler(
        filename = function(){paste0(input$Id003, ".csv")},
        content = function(fname) {
            write.csv(reacNeed(), fname)
        }
    )
    
    output$download2 <- downloadHandler(
        filename = function(){paste0(input$Id003, "- RINCIAN MATERI", ".csv")},
        content = function(fname) {
            write.csv(reacNeed(), fname)
        }
    )
    
    output$download3 <- downloadHandler(
        filename = function(){paste0(input$Id002, ".csv")},
        content = function(fname) {
            write.csv(reacNeed(), fname)
        }
    )
    
    reacOut4 <- reactive({
        materi <- reacDataModel()$materi %>% unique() %>% sort()
        barNeeded <- c()
        
        for (i in materi) {
            x <- 
                reacDataModel() %>% 
                dplyr::filter(materi == i) %>% 
                mutate(total = `perlu per unit` * unit)
            
            need <- rep(x$ukuran, times = x$total) %>% sort(decreasing = TRUE)
            
            y <- countalum(need)
            
            index = length(barNeeded) + 1
            barNeeded[index] <- nrow(y)
        }
        
        tibble(materi, barNeeded)
    })
    
    output$output4 <- renderReactable({
         reacOut4() %>% 
            reactable(pagination = FALSE)
    })
    
    output$download4 <- downloadHandler(
        filename = function(){"ringkasan.csv"},
        content = function(fname) {
            write.csv(reacOut4(), fname)
        }
    )
    
} # server


# Run the application --------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
