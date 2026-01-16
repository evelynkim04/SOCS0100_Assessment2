# Installing and loading packages
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(scales)) install.packages("scales")

library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(scales)

# Reading the "mostwatched_data" data as CSV & cleaning the data
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
    sidebarMenu(
      menuItem("Bar Chart", tabName = "bar_tab", icon = icon("bar-chart")),
      menuItem("Scatter Plot", tabName = "scatter_tab", icon = icon("chart-line")),
      menuItem("Donut Chart", tabName = "donut_tab", icon = icon("circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
     
       # Bar chart tab
      tabItem(tabName = "bar_tab",
              box(
                width = 4,
                selectInput(
                  "bar_variable",
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
              box(width = 12,
                  plotOutput("bar_plot", height = 400))
      ),
      
      # Scatter plot tab
      tabItem(tabName = "scatter_tab",
              box(
                width = 4,
                selectInput(
                  "scatter_variable",
                  "Views/hours",
                  choices = c("views", "hours"),
                  selected = "views"
                )
              ),
              box(width = 12,
                  plotOutput("scatter_plot", height = 400))
      ),
      
      # Donut chart tab
      tabItem(tabName = "donut_tab",
              box(
                width = 4,
                selectInput(
                  "type_filter",
                  "Filter by content type",
                  choices = c("All", "Movie", "TV Show"),
                  selected = "All"
                )
              ),
              box(width = 12,
                  plotOutput("donut_plot", height = 400))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # bar chart
  output$bar_plot <- renderPlot({
    
    variable <- switch(input$bar_variable,
                       "views" = "views",
                       "hours" = "hours")
    
    fill_colour <- switch(input$bar_variable,
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
        y = variable,
        title = paste("Top", input$top_n, "Most Watched Titles (UK)"),
        subtitle = paste("Metric:", variable)
      ) +
      theme_minimal()
  })
  
  # scatter plot
  output$scatter_plot <- renderPlot({
    
    df %>%
      ggplot(aes(
        x = runtime,
        y = .data[[input$scatter_variable]]
      )) +
      geom_point(alpha = 0.7, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "runtime",
        y = input$scatter_variable,
        title = paste("Runtime vs", input$scatter_variable),
        subtitle = "Each point represents a title"
      ) +
      theme_minimal()
  })
  
  # donut plot
  output$donut_plot <- renderPlot({
    
    df_filtered <- df
    
    if (input$type_filter != "All") {
      df_filtered <- df %>%
        filter(type == input$type_filter)
    }
    
    df_genre <- df_filtered %>%
      count(genre) %>%
      mutate(prop = n / sum(n),
             label = paste0(genre, " (", scales::percent(prop), ")"))
    
    ggplot(df_genre, aes(x = 2, y = n, fill = genre)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      theme_void() +
      labs(
        title = "Genre Composition of Most Watched Titles (UK)",
        subtitle = paste("Content type:", input$type_filter)
      )
  })
  
}

# Running the app
shinyApp(ui, server)
