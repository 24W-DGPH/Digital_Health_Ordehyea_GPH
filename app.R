#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Shiny 
#Agyeman Serebouh Ordehyea Yaw 
#Diabetes Visualization Shiny
# Digital Health

options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset
 read.csv("diabetes_clean.csv")

# Define UI
ui <- fluidPage(
  
  # Title of the App
  titlePanel("Interactive Diabetes Data Visualizations"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      
      # Dropdown menu to select the visualization
      selectInput("plot_type", 
                  "Select Visualization:", 
                  choices = c("Proportion of Diabetes Status Across BMI Categories", 
                              "Counts of BMI Groups by Gender", 
                              "Counts of LDL Categories by Diabetes Status", 
                              "Scatter Plot of Age by Diabetes Status",
                              "Counts of Cholesterol Categories by Diabetes Status",
                              "Compare Age vs BMI", 
                              "Compare Cholesterol vs Age"),
                  selected = "Proportion of Diabetes Status Across BMI Categories"),
      
      # Sliders for filtering data (only when needed)
      conditionalPanel(
        condition = "input.plot_type == 'Compare Age vs BMI'",
        sliderInput("age_slider", "Select Age Range:", 
                    min = min(diabetes_clean$Age_years, na.rm = TRUE), 
                    max = max(diabetes_clean$Age_years, na.rm = TRUE),
                    value = c(min(diabetes_clean$Age_years, na.rm = TRUE), 
                              max(diabetes_clean$Age_years, na.rm = TRUE))),
        sliderInput("bmi_slider", "Select BMI Range:", 
                    min = min(diabetes_clean$BMI, na.rm = TRUE), 
                    max = max(diabetes_clean$BMI, na.rm = TRUE),
                    value = c(min(diabetes_clean$BMI, na.rm = TRUE), 
                              max(diabetes_clean$BMI, na.rm = TRUE)))
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'Compare Cholesterol vs Age'",
        sliderInput("cholesterol_slider", "Select Cholesterol Range:", 
                    min = min(diabetes_clean$Cholesterol, na.rm = TRUE), 
                    max = max(diabetes_clean$Cholesterol, na.rm = TRUE),
                    value = c(min(diabetes_clean$Cholesterol, na.rm = TRUE), 
                              max(diabetes_clean$Cholesterol, na.rm = TRUE))),
        sliderInput("age_slider2", "Select Age Range:", 
                    min = min(diabetes_clean$Age_years, na.rm = TRUE), 
                    max = max(diabetes_clean$Age_years, na.rm = TRUE),
                    value = c(min(diabetes_clean$Age_years, na.rm = TRUE), 
                              max(diabetes_clean$Age_years, na.rm = TRUE)))
      )
    ),
    
    # Main panel to display the selected plot
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on slider inputs
  filtered_data <- reactive({
    data <- diabetes_clean
    
    if (input$plot_type == "Compare Age vs BMI") {
      data <- data %>%
        filter(Age_years >= input$age_slider[1] & Age_years <= input$age_slider[2],
               BMI >= input$bmi_slider[1] & BMI <= input$bmi_slider[2])
    }
    
    if (input$plot_type == "Compare Cholesterol vs Age") {
      data <- data %>%
        filter(Cholesterol >= input$cholesterol_slider[1] & Cholesterol <= input$cholesterol_slider[2],
               Age_years >= input$age_slider2[1] & Age_years <= input$age_slider2[2])
    }
    
    return(data)
  })
  
  # Reactive expression to generate the selected plot
  output$selected_plot <- renderPlot({
    
    # Based on user selection, render different plots
    if(input$plot_type == "Proportion of Diabetes Status Across BMI Categories") {
      ggplot(data = diabetes_clean, aes(x = BMI_group, fill = Diabetes_status)) +
        geom_bar(position = "fill") +
        labs(title = "Proportion of Diabetes Status Across BMI Categories",
             x = "BMI Group", y = "Proportion", fill = "Diabetes Status") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal()
      
    } else if(input$plot_type == "Counts of BMI Groups by Gender") {
      ggplot(data = diabetes_clean, aes(x = BMI_group, fill = Gender)) +
        geom_bar(position = "dodge") +
        labs(title = "Counts of BMI Groups by Gender", x = "BMI Group", y = "Count", fill = "Gender") +
        theme_minimal()
      
    } else if(input$plot_type == "Counts of LDL Categories by Diabetes Status") {
      ggplot(data = diabetes_clean, aes(x = LDL_group, fill = Diabetes_status)) +
        geom_bar(position = "dodge") +
        labs(title = "Counts of LDL Categories by Diabetes Status", x = "LDL Group", y = "Count", fill = "Diabetes Status") +
        theme_minimal()
      
    } else if(input$plot_type == "Scatter Plot of Age by Diabetes Status") {
      ggplot(data = diabetes_clean, aes(x = Diabetes_status, y = Age_years, color = Diabetes_status)) +
        geom_jitter(width = 0.2, alpha = 0.7) +
        labs(title = "Scatter Plot of Age by Diabetes Status", x = "Diabetes Status", y = "Age (Years)", color = "Diabetes Status") +
        theme_minimal()
      
    } else if(input$plot_type == "Counts of Cholesterol Categories by Diabetes Status") {
      ggplot(data = diabetes_clean, aes(x = Cholesterol_group, fill = Diabetes_status)) +
        geom_bar(position = "dodge") +
        labs(title = "Counts of Cholesterol Categories by Diabetes Status", x = "Cholesterol Group", y = "Count", fill = "Diabetes Status") +
        theme_minimal()
      
    } else if(input$plot_type == "Compare Age vs BMI") {
      ggplot(data = filtered_data(), aes(x = Age_years, y = BMI, color = Diabetes_status)) +
        geom_point() +
        labs(title = "Age vs BMI Comparison", x = "Age (Years)", y = "BMI", color = "Diabetes Status") +
        theme_minimal()
      
    } else if(input$plot_type == "Compare Cholesterol vs Age") {
      ggplot(data = filtered_data(), aes(x = Age_years, y = Cholesterol, color = Diabetes_status)) +
        geom_point() +
        labs(title = "Cholesterol vs Age Comparison", x = "Age (Years)", y = "Cholesterol", color = "Diabetes Status") +
        theme_minimal()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

           


