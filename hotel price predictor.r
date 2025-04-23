library(shiny)
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)

# Load Data
df <- read.csv("C:/Users/Muhammad A El-kufahn/Documents/SIWES/HOTEL PRICE PREDICTOR/Hotels_In_Abuja(in).csv", stringsAsFactors = FALSE)

# Data Cleaning
df$Cost <- as.numeric(gsub("₦|,", "", df$Cost))
df$Rating <- as.numeric(sub(" .*", "", df$Rating))
df <- df %>% drop_na(Cost) # Remove rows with missing Cost

# Handle missing values
df$Location[is.na(df$Location)] <- "Unknown"
df$Rating[is.na(df$Rating)] <- median(df$Rating, na.rm = TRUE)

# Encode categorical variables
df$Location <- as.factor(df$Location)
df$Property_type <- as.factor(df$Property_type)

# Train a predictive model with hyperparameter tuning
set.seed(123)
trainIndex <- createDataPartition(df$Cost, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

tuneGrid <- expand.grid(mtry = c(2, 3, 4))
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model <- train(Cost ~ Location + Property_type + Likes + Rating,
    data = trainData,
    method = "rf",
    trControl = control,
    tuneGrid = tuneGrid,
    importance = TRUE
)

# Define UI
ui <- fluidPage(
    titlePanel("Hotel Price Prediction in Abuja"),
    sidebarLayout(
        sidebarPanel(
            textInput("location", "Location", "Wuse"),
            selectInput("property_type", "Property Type", choices = unique(df$Property_type)),
            numericInput("likes", "Likes", value = 100),
            numericInput("rating", "Rating", value = 7, min = 0, max = 10, step = 0.1),
            actionButton("predict", "Predict Price")
        ),
        mainPanel(
            verbatimTextOutput("prediction"),
            dataTableOutput("raw_data"),
            plotOutput("price_hist"),
            plotOutput("property_bar"),
            plotOutput("rating_cost_scatter"),
            plotOutput("feature_importance")
        )
    )
)

# Define Server
server <- function(input, output) {
    prediction <- eventReactive(input$predict, {
        new_data <- data.frame(
            Location = factor(input$location, levels = levels(df$Location)),
            Property_type = factor(input$property_type, levels = levels(df$Property_type)),
            Likes = input$likes,
            Rating = input$rating
        )
        predict(model, new_data)
    })

    output$prediction <- renderText({
        paste("Predicted Price: ₦", round(prediction(), 2))
    })

    output$raw_data <- renderDataTable({
        df
    })

    # Histogram of hotel prices
    output$price_hist <- renderPlot({
        ggplot(df, aes(x = Cost)) +
            geom_histogram(binwidth = 5000, fill = "blue", color = "white") +
            labs(title = "Distribution of Hotel Prices", x = "Cost (₦)", y = "Count")
    })

    # Bar chart of property types
    output$property_bar <- renderPlot({
        ggplot(df, aes(x = Property_type)) +
            geom_bar(fill = "green") +
            labs(title = "Count of Property Types", x = "Property Type", y = "Count") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # Scatter plot of Rating vs Cost
    output$rating_cost_scatter <- renderPlot({
        ggplot(df, aes(x = Rating, y = Cost)) +
            geom_point(color = "red") +
            labs(title = "Rating vs Cost", x = "Rating", y = "Cost (₦)")
    })

    # Feature importance
    output$feature_importance <- renderPlot({
        importance <- varImp(model, scale = FALSE)
        feature_importance <- data.frame(Feature = rownames(importance$importance), Importance = importance$importance[, 1])
        ggplot(feature_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
            geom_col(fill = "purple") +
            coord_flip() +
            labs(title = "Feature Importance", x = "Feature", y = "Importance")
    })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
