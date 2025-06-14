library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)

offensive_data <- readRDS("../data/offensive_data.rds")

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
    
    navbarMenu("Players",
               tabPanel("Offensive Players",
                        sidebarLayout(
                          sidebarPanel(
                            varSelectInput("offensive_var1", "What metric are you interested in?", data= offensive_data, selected = "goals"),
                            varSelectInput("offensive_var2", "What other metric are you interested in?", data= offensive_data, selected = "goals"),
                            sliderInput("offensive_age", "Select an age range", value= c(17,37), min = 15, max = 45),
                            verbatimTextOutput("lm_results"),
                            textInput("player_search", "Search for a player using their full name (case sensative):", value = ""),
                            verbatimTextOutput("predicted_salary"),
                            verbatimTextOutput("actual_salary")
                            
                          ),
                          mainPanel(
                            verbatimTextOutput("offensive_top10"),
                            plotOutput("offensive_var1_graph"),
                            plotOutput("offensive_var2_graph")
                          ) # closing mainPanel
                        ) 
                        ), #closing Offensive players tab
               
               tabPanel("Midfield Players"),
               tabPanel("Defensive Players"),
               tabPanel("Goalkeepers")
               ), # closing navbarMenu for Players
    
    navbarMenu("Data Frames",
               tabPanel("Offensive Data",
                        dataTableOutput("offensive_data_table")
                        ),
               tabPanel("Midfield Data"),
               tabPanel("Defensive Data"),
               tabPanel("Goalkeepers Data")
    ) # closing navbarMenu for Players

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
# Offensive reactive data
  attacking_data <- reactive({
    temp <- offensive_data %>%
      #select(player_name, team_name, age, general_position, input$offensive_var1, input$offensive_var2 ) %>%
      arrange(desc(.data[[input$offensive_var1]]), desc(.data[[input$offensive_var2]])) %>%
      filter(age >= input$offensive_age[1], age <= input$offensive_age[2])%>%
      filter(
        !is.na(.data[[input$offensive_var1]]),
        !is.na(.data[[input$offensive_var2]]),
        !is.na(base_salary),
        .data[[input$offensive_var1]] != 0,
        .data[[input$offensive_var1]] != 0)
    
    temp
  })

# Home Tab
  output$logo <- renderImage({
    list(
      src = "../images/logo.png",
      contentType = 'image/png',
      width = 500,        # or any size you prefer
      alt = "App Logo"
    )
  }, deleteFile = FALSE) # DGT-INT logo
  
# Players Tab
  # sub-section: Offensive
  output$offensive_top10 <- renderPrint({
    head(select(attacking_data(),player_name, team_name, age, general_position, input$offensive_var1, input$offensive_var2), 10)
  }) # Offensive head
  
  output$offensive_var1_graph <- renderPlot({
    attacking_data() %>%
      filter(!is.na(.data[[input$offensive_var1]])) %>%
      slice_max(order_by = .data[[input$offensive_var1]], n = 10) %>%
      ggplot(aes(x = reorder(player_name, .data[[input$offensive_var1]]),
                 y = .data[[input$offensive_var1]])) +
      geom_col(fill = "green4") +
      coord_flip() +
      labs(x = "Player", y = input$offensive_var1,
           title = paste("Top 10 Players by", input$offensive_var1)) +
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
  }) # Offensive graph 1
  
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
  }) # Offensive graph 2
  
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
  
  # sub-section: Offensive data
  output$offensive_data_table <- renderDT({
    datatable(offensive_data, options = list(pageLength = 100))
  })
  
  # Pop-up for membership
  observeEvent(input$tabs, {
    if (input$tabs %in% c("Midfield Players", "Defensive Players", "Goalkeepers")) {
      showModal(modalDialog(
        title = paste("The", input$tabs, "tab is a premium feature"),
        paste("The", input$tabs, "tab is only available to premium users. Please upgrade your account if you would like to access this tab. You will now be re-directed to the Offensive Player tab."),
        easyClose = TRUE,
        footer = NULL
      ))
      
      updateTabsetPanel(session, inputId = "tabs", selected = "Offensive Players")
    }
  })
  
  observeEvent(input$tabs, {
    if (input$tabs %in% c("Midfield Data", "Defensive Data", "Goalkeepers Data")) {
      showModal(modalDialog(
        title = paste("The", input$tabs, "tab is a premium feature"),
        paste("The", input$tabs, "tab is only available to premium users. Please upgrade your account if you would like to access this tab. You will now be re-directed to the Offensive Data tab."),
        easyClose = TRUE,
        footer = NULL
      ))
      
      updateTabsetPanel(session, inputId = "tabs", selected = "Offensive Data")
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
