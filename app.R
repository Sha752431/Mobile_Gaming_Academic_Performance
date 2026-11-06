
# ==========================================
# MOBILE GAMING HABITS AND ACADEMIC PERFORMANCE
# INTERACTIVE SHINY DASHBOARD
# ==========================================

library(shiny)
library(ggplot2)
library(dplyr)

# Load dataset
gaming_data <- read.csv("mobile_gaming_academic.csv")

# User Interface
ui <- fluidPage(
  
  titlePanel("Mobile Gaming Habits and Academic Performance"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      selectInput(
        "year",
        "Select Year Level:",
        choices = c("All", unique(gaming_data$Year_Level)),
        selected = "All"
      ),
      
      selectInput(
        "frequency",
        "Select Gaming Frequency:",
        choices = c("All", unique(gaming_data$Gaming_Frequency)),
        selected = "All"
      ),
      
      selectInput(
        "genre",
        "Select Favorite Game Genre:",
        choices = c("All", unique(gaming_data$Favorite_Game_Genre)),
        selected = "All"
      ),
      
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      
      h3("Student Gaming Overview"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Total Students"),
            textOutput("total_students")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Gaming Hours"),
            textOutput("average_gaming")
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Academic Score"),
            textOutput("average_score")
          )
        )
      ),
      
      hr(),
      
      h3("Graph 1: Gaming Frequency and Gaming Hours"),
      plotOutput("graph1"),
      
      hr(),
      
      h3("Graph 2: Gaming Hours vs Academic Performance"),
      plotOutput("graph2"),
      
      hr(),
      
      h3("Graph 3: Academic Performance by Game Genre"),
      plotOutput("graph3"),
      
      hr(),
      
      h3("Filtered Student Records"),
      tableOutput("student_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter dataset
  filtered_data <- reactive({
    
    data <- gaming_data
    
    if (input$year != "All") {
      data <- data[data$Year_Level == input$year, ]
    }
    
    if (input$frequency != "All") {
      data <- data[data$Gaming_Frequency == input$frequency, ]
    }
    
    if (input$genre != "All") {
      data <- data[data$Favorite_Game_Genre == input$genre, ]
    }
    
    data
  })
  
  # Reset filters
  observeEvent(input$reset, {
    
    updateSelectInput(session, "year", selected = "All")
    updateSelectInput(session, "frequency", selected = "All")
    updateSelectInput(session, "genre", selected = "All")
    
  })
  
  # Total students
  output$total_students <- renderText({
    nrow(filtered_data())
  })
  
  # Average gaming hours
  output$average_gaming <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    paste(round(mean(data$Daily_Gaming_Hours), 2), "hours")
  })
  
  # Average academic score
  output$average_score <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    round(mean(data$Academic_Performance_Score), 2)
  })
  
  # Graph 1
  output$graph1 <- renderPlot({
    
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No records match the selected filters."))
    
    gaming_average <- aggregate(
      Daily_Gaming_Hours ~ Gaming_Frequency,
      data = data,
      FUN = mean
    )
    
    ggplot(
      gaming_average,
      aes(
        x = Gaming_Frequency,
        y = Daily_Gaming_Hours,
        fill = Gaming_Frequency
      )
    ) +
      geom_col() +
      labs(
        title = "Average Daily Gaming Hours",
        x = "Gaming Frequency",
        y = "Average Gaming Hours"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Graph 2
  output$graph2 <- renderPlot({
    
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No records match the selected filters."))
    
    ggplot(
      data,
      aes(
        x = Daily_Gaming_Hours,
        y = Academic_Performance_Score
      )
    ) +
      geom_point(color = "blue", size = 3) +
      labs(
        title = "Gaming Hours and Academic Performance",
        x = "Daily Gaming Hours",
        y = "Academic Performance Score"
      ) +
      theme_minimal()
  })
  
  # Graph 3
  output$graph3 <- renderPlot({
    
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No records match the selected filters."))
    
    ggplot(
      data,
      aes(
        x = Favorite_Game_Genre,
        y = Academic_Performance_Score,
        fill = Favorite_Game_Genre
      )
    ) +
      geom_boxplot() +
      labs(
        title = "Academic Performance by Game Genre",
        x = "Favorite Game Genre",
        y = "Academic Performance Score"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Student data table
  output$student_table <- renderTable({
    filtered_data()
  })
  
}

# Run Shiny application
shinyApp(ui = ui, server = server)
