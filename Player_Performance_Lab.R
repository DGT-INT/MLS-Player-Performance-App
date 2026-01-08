library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)

offensive_data <- readRDS("data/offensive_data.rds") # old data to delete
data_2025 <- readRDS("data/03 working data season 2025.rds")

# Define UI for application that draws a histogram
ui <- navbarPage("Player Performance Lab",
                 id = "tabs",
                 
    theme = bs_theme(bootswatch = "cyborg", fg = "#239222", bg= "#000600", primary = "#239222"),
    
    tabPanel("Home",
             div(
               style = "text-align: center;",
               h1("Welcome to the Player Performance Lab"),
               h4("Created by Daniel Tshiani")
             ),
             div(
               style = "text-align: center;",
               imageOutput("logo", width = "100%", height = "auto")
             )
             ), # closing panel for Home tab
    
    navbarMenu("Scouting",
               tabPanel("Players",
                        sidebarLayout(
                          sidebarPanel(
                            varSelectInput("players_var1", "What metric are you interested in?", data= data_2025, selected = "goals"),
                            varSelectInput("players_var2", "What other metric are you interested in?", data= data_2025, selected = "assists"),
                            sliderInput("players_age", "How old do you want the player to be?", value= c(17,37), min = 15, max = 45),
                            #verbatimTextOutput("lm_results"),
                            textInput("player_search", "Search for a player using their full name (case sensative):", value = ""),
                            verbatimTextOutput("players_predicted_salary"),
                            verbatimTextOutput("players_actual_salary")
                            
                          ),
                          mainPanel(
                            verbatimTextOutput("players_top10"),
                            plotOutput("players_graph1"),
                            plotOutput("players_graph2")
                          ) # closing mainPanel
                        ) 
                        ), #closing players tab
               
               tabPanel("GoalKeepers"),
               tabPanel("Teams"),
               tabPanel("Referees")
               ), # closing navbarMenu for Players
    
    navbarMenu("Data Frames",
               tabPanel("Players Data",
                        dataTableOutput("players_data_table")
                        ),
               tabPanel("GoalKeepers Data"),
               tabPanel("Team Data"),
               tabPanel("Referee Data")
    ) # closing navbarMenu for Data Frames

)

server <- function(input, output, session) {
  
# Players reactive data
  players_data <- reactive({
    temp <- data_2025 %>%
      arrange(desc(.data[[input$players_var1]]), desc(.data[[input$players_var2]])) %>%
      filter(age >= input$players_age[1], age <= input$players_age[2])%>%
      filter(
        !is.na(.data[[input$players_var1]]),
        !is.na(.data[[input$players_var2]]),
        .data[[input$players_var1]] != 0,
        .data[[input$players_var1]] != 0)
    
    temp
  }) 

# Home Tab
  output$logo <- renderImage({
    list(
      src = "images/logo.png",
      contentType = 'image/png',
      width = 500,        # or any size you prefer
      alt = "App Logo"
    )
  }, deleteFile = FALSE) # DGT-INT logo
  
# Scouting Tab
  # sub-section: Players
  output$players_top10 <- renderPrint({
    head(select(players_data(),`player name`, `team name`, age, position, input$players_var1, input$players_var2), 10)
  }) # Players head
  
  # Players Graph 1
  output$players_graph1 <- renderPlot({
    players_data() %>%
      filter(!is.na(.data[[input$players_var1]])) %>%
      slice_max(order_by = .data[[input$players_var1]], n = 10) %>%
      ggplot(aes(x = reorder(`player name`, .data[[input$players_var1]]),
                 y = .data[[input$players_var1]])) +
      geom_col(fill = "green4") +
      coord_flip() +
      labs(x = "Player", y = input$players_var1,
           title = paste("Top 10 Players by", input$players_var1)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        panel.grid.major = element_line(color = "gray30"),
        panel.grid.minor = element_line(color = "gray20"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", face = "bold")
      )
  }) # Players graph 1
  
  output$offensive_var2_graph <- renderPlot({
    top10 <- attacking_data() %>%
      arrange(desc(.data[[input$offensive_var1]])) %>%
      slice_head(n = 10)
    
    ggplot(top10, aes(x = .data[[input$offensive_var2]], y = .data[[input$offensive_var1]], color = player_name)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = FALSE, color= "white") +
      labs(
        x = input$offensive_var2,
        y = input$offensive_var1,
        title = paste("Top 10 Players by", input$offensive_var2)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        panel.grid.major = element_line(color = "gray30"),
        panel.grid.minor = element_line(color = "gray20"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", face = "bold"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
  }) # Players graph 2
  
  output$lm_results <- renderPrint({

    salary_model <- lm(base_salary ~ goals + primary_assists + age + minutes_played + shots + shots_on_target + key_passes, data = attacking_data())
    summary(salary_model)
    
  })
  
  output$predicted_salary <- renderPrint({
    req(input$player_search)
    
    player_row <- offensive_data %>%
      filter(player_name == input$player_search)
    
    if (nrow(player_row) == 0) {
      return("Player not found.")
    }
    
    salary_model <- lm(base_salary ~ goals + primary_assists + age + minutes_played + shots + shots_on_target + key_passes, data = attacking_data())
    
    prediction <- predict(salary_model, newdata = player_row)
    paste("Estimated Salary:", dollar(prediction))
  })
  
  output$actual_salary <- renderPrint({
    req(input$player_search)
    
    actual <- offensive_data %>%
      filter(player_name == input$player_search) %>%
      pull(base_salary)
    
    if (length(actual) == 0) {
      return("Actual salary not found.")
    }
    
    paste("Actual Salary:", dollar(actual))
  })
  
# Data Frames Tab
  
  # sub-section: Players data
  output$offensive_data_table <- renderDT({
    datatable(offensive_data, options = list(pageLength = 100))
  })
  
  # Pop-up for membership
  observeEvent(input$tabs, {
    if (input$tabs %in% c("GoalKeepers", "Teams", "Referees")) {
      showModal(modalDialog(
        title = paste("The", input$tabs, "tab is a premium feature"),
        paste("The", input$tabs, "tab is only available to premium users. Please upgrade your account if you would like to access this tab. You will now be re-directed to the Player tab."),
        easyClose = TRUE,
        footer = NULL
      ))
      
      updateTabsetPanel(session, inputId = "tabs", selected = "Players")
    }
  })
  
  observeEvent(input$tabs, {
    if (input$tabs %in% c("GoalKeepers Data", "Team Data", "Referee Data")) {
      showModal(modalDialog(
        title = paste("The", input$tabs, "tab is a premium feature"),
        paste("The", input$tabs, "tab is only available to premium users. Please upgrade your account if you would like to access this tab. You will now be re-directed to the Data tab."),
        easyClose = TRUE,
        footer = NULL
      ))
      
      updateTabsetPanel(session, inputId = "tabs", selected = "Players Data")
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
