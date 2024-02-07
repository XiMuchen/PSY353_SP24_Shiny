#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries ----------------------------------------------------------------
# needed libraries
library(shiny)
library(shinyWidgets)
library(here)         # data management
library(dplyr)        # data wrangling
library(ggplot2)      # visualizations
library(lubridate)    # handling of timestamps
library(jtools)       # theme for visualizations
library(summarytools) # freqs (adjust?)
library(profvis)      # optimiziing app
library(qgraph)       # visualization of networks
library(shinycssloaders) # 
library(shinyjs)        #
library(shinyBS)        # 
library(fst)            # faster reading of data
library(data.table)     # faster subsetting
library(shinyvalidate)
library(tidyr)
library(ggdist)
# read data ---------------------------------------------------------------
setwd("~/Library/CloudStorage/Box-Box/2024_Spring/PSY353/Shiny/Shinytest/")
# dat <- fread("sema3_test.csv")
# names(dat)
# dat <- dat %>%
#   select(names(dat)[c(1,12,19:263)])%>%
#   select(-matches("_RT$")) %>%
#   mutate(across(names(.)[-c(1,2)], ~na_if(.,"<no-response>")),
#          across(names(.)[-c(1,2)], as.numeric))
# 
# # difftime(strptime(dat$SCHEDULED_TS[1], format = "%d-%b-%Y %H:%M"),strptime(dat$SCHEDULED_TS[2], format = "%d-%b-%Y %H:%M"))
# 
# dat_per <- dat %>%
#   select(names(dat)[c(1, 8:17, 22,35)]) %>%
#   rowwise() %>%
#   dplyr :: mutate(Extraversion = mean(c(OUTGOING, ASSERTIVE), na.rm =TRUE),
#          Agreeableness = mean(c(COMPASSIONATE, POLITE), na.rm =TRUE),
#          Conscientiousness = mean(c(HARDWORKING, ORGANIZED), na.rm =TRUE),
#          Neuroticism = mean(c(WORRIED, DEPRESSED), na.rm =TRUE),
#          Openness = mean(c(CREATIVE, CURIOUS), na.rm =TRUE))

source("functions.R")
# ui ----------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(theme = "cerulean", 

    # Application title
    # fluidRow(column(12, HTML("<br>"))),
    # titlePanel("Personalized ESM Report"), 
    navbarPage(title = "Personlized Report", 
              # PAGE 1 -------------------
               tabPanel("Log-In",
                        sidebarPanel(
                          tags$h3("Log-In:"), 
                          textInput("PID", "Participant ID:"),
                          actionButton("getData", "Create Report")
                        ),
                        mainPanel(
                          h4("You can use this website to view your personalized data report based
on the ESM data we collected during the last two weeks."),
                          
                          h4("Step 1: Enter your user key to the text field below and click on 'Create Report'."),
                          
                          h4("Step 2: In the navigation bar at the top.")
                          )
                        ),
              # PAGE 2 -------------------
               tabPanel("Completed Surveys", # define tab and title
                        fluidRow(textOutput("missingText")), # personalized text (see above)
                        fluidRow(HTML("<br><br>")),
                        fluidRow(
                          column(2, HTML("<br><br>")),
                          column(8,
                                 plotOutput("missingData") %>% # figure 
                                   withSpinner(color = "#0464A3", type = 6)) # loading wheel
                                )
                        ),
              # PAGE 3 -------------------
              navbarMenu("Personality Change and Variability", 
                         tabPanel("Fluctuation in Personality",
                                  mainPanel(
                                    h4("This page shows the trajectory of your personality change in the past week 
                                       across assessments at different time points."),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("ETrajectory") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("CTrajectory") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("ATrajectory") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("NTrajectory") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("OTrajectory") %>% withSpinner(color = "#0464A3", type = 6)
                                  )
                                  ),
                         
                         tabPanel("Distribution of Personality",
                                  mainPanel(
                                    h4("This page shows the density distributions of your personality states across 
                                       the Big Five domains."),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("traitDistribution") %>% withSpinner(color = "#0464A3", type = 6)
                                  )
                                  )
                         ), 
              
              # PAGE 4 -------------------
              navbarMenu("Personality Distributions in Social Interactions", 
                         tabPanel("Interacting vs. Not",
                                  mainPanel(
                                    h4("This page shows the comparisons of density distributions of personality states 
                                    across the Big Five domains when you engage in social interaction versus not."),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("EInteract") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("CInteract") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("AInteract") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("NInteract") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("OInteract") %>% withSpinner(color = "#0464A3", type = 6)
                                  )
                         ),
                         
                         tabPanel("Interacting with Different Partners",
                                  mainPanel(
                                    h4("This page shows the density distributions of your personality states across 
                                       the Big Five domains when you interactng with different partners."),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("EPart") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("CPart") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("APart") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("NPart") %>% withSpinner(color = "#0464A3", type = 6),
                                    fluidRow(HTML("<br><br>")),
                                    plotOutput("OPart") %>% withSpinner(color = "#0464A3", type = 6)
                                  )
                         )
              ),
              
              # PAGE 5 -------------------
              tabPanel("Personality Distributions in Activities", # define tab and title
                       mainPanel(
                         h4("This page shows the density distributions of your personality states across 
                                       the Big Five domains when you engage in different activities."),
                         fluidRow(HTML("<br><br>")),
                         plotOutput("EAct") %>% withSpinner(color = "#0464A3", type = 6),
                         fluidRow(HTML("<br><br>")),
                         plotOutput("CAct") %>% withSpinner(color = "#0464A3", type = 6),
                         fluidRow(HTML("<br><br>")),
                         plotOutput("AAct") %>% withSpinner(color = "#0464A3", type = 6),
                         fluidRow(HTML("<br><br>")),
                         plotOutput("NAct") %>% withSpinner(color = "#0464A3", type = 6),
                         fluidRow(HTML("<br><br>")),
                         plotOutput("OAct") %>% withSpinner(color = "#0464A3", type = 6)
                       )
              )
              
              )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  PID <- reactive({
    input$PID
  }) %>%
    bindEvent(input$getData)
  
  data_loaded <- reactiveVal(FALSE)
  
  observeEvent(input$getData, {
    print(head(dataset()))
  })
  
  dataset <- reactive({
    # Initialize the dataset as NULL
    data <- NULL
    
    if (!is.null(PID()) && input$getData > 0) {
      tryCatch({ # tryCatch is used to prevent the app from crashing
        # in case the dataset doesn't exist (e.g. typo)
        # Attempt to read the dataset
        data <- fread(paste0(getwd(), "/P_dat/", PID(), ".csv"))
        
        if (!is.null(data)) { # show pop up message if data is succesfully loaded
          data_loaded(TRUE)
          showModal(
            modalDialog(
              "Your data was successfully loaded. You can click anywhere to continue.
            Please navigate throught the options in the naviagation bar",
              footer = NULL,
              easyClose = TRUE
            )
          )
          
        } else {
          data_loaded(FALSE) # show pop up message in case the data loading failed
        }
      }  , error = function(e) {
        showModal(
          modalDialog(
            "Please double-check whether your user key was correct and try again.",
            footer = NULL,
            easyClose = TRUE
          )
        )
      })
    }
    return(data)
  })

# missing data ---------------------------------------------------------
  output$missingData = renderPlot({ # create plot about completed surveys
    # 1. step: create dataframe with info about completed surveys
    df_missing <- create_df_missing(par_dat = dataset())
    # 2. step create plot
    plot_missing_data(df_missing)
  })
  
  output$missingText <- renderText(
    {paste(
      "Hi participant", PID(), "Looking into your data, you completed ",
      nrow(dataset()),
      missing_text_2,
      round((nrow(dataset())/42)*100, 2),
      " %.")
    })
  
# trajectory ---------------------------------------------------------
  output$ETrajectory = renderPlot({
    plot_PxTime(dataset(), "Extraversion", "dodgerblue")
  })
  
  output$CTrajectory = renderPlot({
    plot_PxTime(dataset(), "Conscientiousness", "chartreuse")
  })

  output$ATrajectory = renderPlot({
    plot_PxTime(dataset(), "Agreeableness", "burlywood3")
  })
  
  output$NTrajectory = renderPlot({
    plot_PxTime(dataset(), "Neuroticism", "indianred")
  })
  
  output$OTrajectory = renderPlot({
    plot_PxTime(dataset(), "Openness", "yellow3")
  })

  # distribution ---------------------------------------------------------
  output$traitDistribution = renderPlot({
    plot_TraitDist(dataset())
  })
  
  # distribution at social interaction -----------------------------------
  output$EInteract = renderPlot({
    plot_IntDist(dataset(), "Extraversion")
  })
  
  output$CInteract = renderPlot({
    plot_IntDist(dataset(), "Conscientiousness")
  })
  
  output$AInteract = renderPlot({
    plot_IntDist(dataset(), "Agreeableness")
  })
  
  output$NInteract = renderPlot({
    plot_IntDist(dataset(), "Neuroticism")
  })
  
  output$OInteract = renderPlot({
    plot_IntDist(dataset(), "Openness")
  })

  # distribution with different partners -----------------------------------
  output$EPart = renderPlot({
    plot_PartDist(dataset(), "Extraversion")
  })
  
  output$CPart = renderPlot({
    plot_PartDist(dataset(), "Conscientiousness")
  })
  
  output$APart = renderPlot({
    plot_PartDist(dataset(), "Agreeableness")
  })
  
  output$NPart = renderPlot({
    plot_PartDist(dataset(), "Neuroticism")
  })
  
  output$OPart = renderPlot({
    plot_PartDist(dataset(), "Openness")
  })

  # distribution at different activities -----------------------------------
  output$EAct = renderPlot({
    plot_ActDist(dataset(), "Extraversion")
  })
  
  output$CAct = renderPlot({
    plot_ActDist(dataset(), "Conscientiousness")
  })
  
  output$AAct = renderPlot({
    plot_ActDist(dataset(), "Agreeableness")
  })
  
  output$NAct = renderPlot({
    plot_ActDist(dataset(), "Neuroticism")
  })
  
  output$OAct = renderPlot({
    plot_ActDist(dataset(), "Openness")
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
