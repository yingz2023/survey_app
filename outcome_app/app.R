library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Load data for all personas
data_scientist <- read.csv("data_scientist.csv")
ai_developer <- read.csv("ai_developer.csv")
mlops <- read.csv("mlops.csv")
combined_data <- read.csv("combined_data.csv")

# Add a column to identify the persona
data_scientist$persona <- "Data Scientist"
ai_developer$persona <- "AI Developer"
mlops$persona <- "MLOps"
combined_data$persona <- "All"

# Combine all data into one data frame
all_data <- rbind(data_scientist, ai_developer, mlops, combined_data)

# Define UI for application
ui <- fluidPage(
  titlePanel("Opportunity Landscape Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("persona", "Select Persona:", choices = c("Data Scientist", "AI Developer", "MLOps", "All")),
      textOutput("pointInfo")
    ),
    mainPanel(
      plotlyOutput("oppScorePlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  op_score <- function(imp, sat) {
    return(imp + max(imp - sat, 0))
  }
  
  count_positives <- function(x, midpoint) {
    x = na.omit(x)
    return(10 * (sum(x > midpoint) / length(x)))
  }
  
  round_df <- function(x, digits) {
    numeric_columns <- sapply(x, class) == 'numeric'
    x[numeric_columns] <- round(x[numeric_columns], digits)
    x
  }
  
  midpoint <- 3
  outcomeName <- "outcome"
  statementName <- "statement"
  impName <- "odd_mydat"
  satName <- "even_mydat"
  removeMissing <- TRUE
  
  filteredData <- reactive({
    persona_data <- all_data[all_data$persona == input$persona, ]
    if (removeMissing) persona_data <- persona_data[rowSums(is.na(persona_data)) == 0,]
    
    inputData <- persona_data[c(outcomeName, statementName, impName, satName)]
    colnames(inputData) <- c("outcome", "statement", "importance", "satisfaction")
    imp <- inputData %>% group_by(outcome, statement) %>%
      summarise(importance = count_positives(importance, midpoint), .groups = 'drop')
    sat <- inputData %>% group_by(outcome, statement) %>%
      summarise(satisfaction = count_positives(satisfaction, midpoint), .groups = 'drop')
    
    values <- left_join(imp, sat, by = c("outcome", "statement"))
    values$oppscore <- mapply(op_score, values$importance, values$satisfaction)
    values <- values[order(-values$oppscore),]
    values <- round_df(values, 2)
    return(values)
    #imp <- aggregate(importance ~ outcome, inputData, count_positives, midpoint = midpoint)
    #sat <- aggregate(satisfaction ~ outcome, inputData, count_positives, midpoint = midpoint)
    #values <- merge(x = imp, y = sat, by = c("outcome", "statement"), all = TRUE)
  })
  
  output$oppScorePlot <- renderPlotly({
    values <- filteredData()
    p <- ggplot(values, aes(x = importance, y = satisfaction, text = paste(outcome, statement, "<br>Opp Score:", oppscore))) +
      geom_point(aes(color = oppscore), size = 3) +
      geom_text(aes(label = seq_along(outcome)), hjust = -0.5, vjust = -0.5) +
      scale_color_gradient(low = "blue", high = "red") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
      labs(x = "Importance", y = "Satisfaction", title = "Opportunity Landscape", color = "Opp Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(clickmode = "event+select")
  })
  
  output$pointInfo <- renderText({
    values <- filteredData()
    event_data <- event_data("plotly_click")
    if (is.null(event_data)) {
      "Click on a point to see details"
    } else {
      paste("Outcome:", values$outcome[event_data$pointNumber + 1], 
            "\nStatement:", values$statement[event_data$pointNumber + 1],
            "\nImportance:", values$importance[event_data$pointNumber + 1], 
            "\nSatisfaction:", values$satisfaction[event_data$pointNumber + 1], 
            "\nOpp Score:", values$oppscore[event_data$pointNumber + 1])
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
