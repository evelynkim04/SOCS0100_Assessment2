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
df <- read.csv("mostwatched_data.csv") # reading mostwatched_data as a dataframe called "df". 

df <- df %>%
  mutate(
    views = as.numeric(gsub(",", "", sub("\\s.*", "", views))), # removing anything after he first spac.
    hours = as.numeric(gsub(",", "", sub("\\s.*", "", hours))), # removing any commas.
    rank = as.numeric(rank) # making sure all values are numeric.
  )

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "UK Most Watched"),
  
  dashboardSidebar( # Each tab contains one type of visualisation.
    sidebarMenu(
      menuItem("Bar Chart", tabName = "bar_tab", icon = icon("bar-chart")),
      menuItem("Scatter Plot", tabName = "scatter_tab", icon = icon("chart-line")),
      menuItem("Donut Chart", tabName = "donut_tab", icon = icon("circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
     
       # Bar chart tab
      tabItem(tabName = "bar_tab", # allows user to choose views/hours and how many titles to show. 
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
      tabItem(tabName = "scatter_tab", # user can choose between views/hours using dropdown menu
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
      tabItem(tabName = "donut_tab", # user can choose between "all", "movie" and "TV show" using dropdown filter
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
    
    variable <- switch(input$bar_variable, # this decides which column to plot
                       "views" = "views",
                       "hours" = "hours")
    
    fill_colour <- switch(input$bar_variable,
                          "views" = "steelblue", # making color blue if user chooses "views"
                          "hours" = "darkorange") # making color darkorange if user chooses "hours"
    
    df %>%
      arrange(rank) %>% # sorting by rank
      slice_head(n = input$top_n) %>% # keeping only the top N titles 
      ggplot(aes(
        x = reorder(title, .data[[variable]]), # x-axis is movie/TV show
        y = .data[[variable]] # y-axis is views/hours
      )) +
      geom_col(fill = fill_colour) +
      coord_flip() + # this flips the chart sideways
      scale_y_continuous(labels = scales::comma) + # this adds commas if the value is large
      labs(
        x = "Movie / TV show Title",
        y = variable,
        title = paste("Top", input$top_n, "Most Watched Titles (UK)"),
        subtitle = paste("Metric:", variable)
      ) +
      theme_minimal() # choosing the theme of the plot
  })
  
  # scatter plot
  output$scatter_plot <- renderPlot({
    
    df %>%
      ggplot(aes(
        x = runtime,
        y = .data[[input$scatter_variable]]
      )) +
      geom_point(alpha = 0.7, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") + # this adds a trendline
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "runtime", # x-axis is runtime
        y = input$scatter_variable, # y-axis is views or hours
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
        filter(type == input$type_filter) # this filters data to the selected type (movie/TV show) that the user chooses. 
    }
    
    df_genre <- df_filtered %>% # preparing the data
      count(genre) %>%
      mutate(prop = n / sum(n),
             label = paste0(genre, " (", scales::percent(prop), ")"))
    
    ggplot(df_genre, aes(x = 2, y = n, fill = genre)) + 
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") + # this turns the shape into a circle (donut shape)
      xlim(0.5, 2.5) + # this creates the donut hole
      theme_void() +
      labs(
        title = "Genre Composition of Most Watched Titles (UK)",
        subtitle = paste("Content type:", input$type_filter)
      )
  })
  
}

# Running the app
shinyApp(ui, server)
