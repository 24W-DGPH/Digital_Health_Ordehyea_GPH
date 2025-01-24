#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#
#


library(shiny)
library(ggplot2)
library(scales)


# UI of the app
ui <- fluidPage(
  titlePanel("Diabetes Health Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select Visualization"),
      selectInput("plot_type", 
                  label = "Choose a plot type",
                  choices = c("BMI and Diabetes Status", 
                              "BMI by Gender", 
                              "LDL and Diabetes Status", 
                              "Age by Diabetes Status"))
    ),
    
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# Server logic of the app
server <- function(input, output) {
  
  output$main_plot <- renderPlot({
    
    # Depending on the user input, render different plots
    if (input$plot_type == "BMI and Diabetes Status") {
      ggplot(data = diabetes_clean, aes(x = BMI_group, fill = Diabetes_Status)) +
        geom_bar(position = "fill") +
        labs(
          title = "Proportion of Diabetes Status Across BMI Categories",
          x = "BMI Group",
          y = "Proportion",
          fill = "Diabetes Status"
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal()
    }
    
    else if (input$plot_type == "BMI by Gender") {
      ggplot(data = diabetes_clean, aes(x = BMI_group, fill = Gender)) +
        geom_bar(position = "dodge") +
        labs(
          title = "Counts of BMI Groups by Gender",
          x = "BMI Group",
          y = "Count",
          fill = "Gender"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("M" = "blue", "F" = "pink"))
    }
    
    else if (input$plot_type == "LDL and Diabetes Status") {
      ggplot(data = diabetes_clean, aes(x = LDL_group, fill = Diabetes_Status)) +
        geom_bar(position = "dodge") +
        labs(
          title = "Counts of LDL Categories by Diabetes Status",
          x = "LDL Group",
          y = "Count",
          fill = "Diabetes Status"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("N" = "blue", "P" = "orange", "Y" = "red"))
    }
    
    else if (input$plot_type == "Age by Diabetes Status") {
      ggplot(data = diabetes_clean, aes(x = Diabetes_Status, y = Age_years, color = Diabetes_Status)) +
        geom_jitter(width = 0.2, alpha = 0.7) +
        labs(
          title = "Scatter Plot of Age by Diabetes Status",
          x = "Diabetes Status",
          y = "Age (Years)",
          color = "Diabetes Status"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("N" = "blue", "P" = "yellow", "Y" = "red"))
    }
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)


