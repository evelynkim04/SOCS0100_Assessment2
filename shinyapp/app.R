# Packages
# =========================
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(shinydashboard)) install.packages("shinydashboard")

library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Read the data as CSV & cleaning data
df <- read.csv("mostwatched_data.csv")

df <- df %>%
  mutate(
    views = as.numeric(gsub(",", "", sub("\\s.*", "", views))),
    hours = as.numeric(gsub(",", "", sub("\\s.*", "", hours))),
    rank = as.numeric(rank)
  )

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "UK Most Watched"),
  
  dashboardSidebar(
    selectInput(
      "variable",
      "Views/hours",
      choices = c("views", "hours"),
      selected = "views"
    ),
    
    sliderInput(
      "top_n",
      "Number of titles",
      min = 5,
      max = 50,
      value = 10,
      step = 5
    )
  ),
  
  dashboardBody(
    box(
      width = 12,
      plotOutput("bar_plot", height = 400)
    )
  )
)

# Server
server <- function(input, output) {
  
  output$bar_plot <- renderPlot({
    
    variable <- switch(input$variable,
                       "views" = "views",
                       "hours" = "hours")
    
    fill_colour <- switch(input$variable,
                          "views" = "steelblue",
                          "hours" = "darkorange")
    
    df %>%
      arrange(rank) %>%
      slice_head(n = input$top_n) %>%
      ggplot(aes(
        x = reorder(title, .data[[variable]]),
        y = .data[[variable]]
      )) +
      geom_col(fill = fill_colour) +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "Movie / TV show Title",
        y = input$variable,
        title = paste("Top", input$top_n, "Most Watched Titles (UK)"),
        subtitle = paste("Metric:", input$variable)
      ) +
      theme_minimal()
  })
}

# Running the app
shinyApp(ui, server)