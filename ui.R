# ui.R

# Define UI
ui <- dashboardPage(
  # --- Dashboard Header ---
  dashboardHeader(title = "Abuja Hotel Price Predictor"),

  # --- Dashboard Sidebar ---
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarItemTabs",
      menuItem("Predict Price", tabName = "predict", icon = icon("dollar-sign")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("chart-bar")),
      menuItem("Table ", tabName = "table", icon = icon("table")),
      menuItem("Source Code", href = "https://github.com/KhalifahMB/HOTEL-PRICE-PREDICTOR", newtab = TRUE, icon = icon("github"))
    ),

    # Inputs for Prediction Tab - visible only when 'predict' tab is selected
    conditionalPanel(
      condition = "input.sidebarItemTabs == 'predict'",
      # Use the global variable for GROUPED property types choices
      selectInput(
        inputId = "property_type_input",
        label = "Select Property Type:",
        choices = property_types_grouped, # Use the grouped types
        selected = property_types_grouped[1]
      ),
      selectInput(
        inputId = "location_input",
        label = "Select Location:",
        choices = location_groups, # Use the grouped locations
        selected = location_groups[1]
      ),
      numericInput(
        inputId = "likes_input",
        label = "Enter Number of Likes:",
        value = 10, min = 0
      ),
      actionButton(
        inputId = "predict_button",
        label = "Predict Price",
        icon = icon("calculator"),
        class = "btn-primary"
      )
    )
  ),

  # --- Dashboard Body ---
  dashboardBody(
    tabItems(
      # --- Predict Price Tab Content ---
      tabItem(
        tabName = "predict",
        h2("Hotel Price Prediction"),
        fluidRow(
          box(
            title = "Estimated Price", status = "success", solidHeader = TRUE,
            height = "180px", width = 6,
            div(
              style = "font-size: 36px; font-weight: bold; text-align: center; color: #007bff;",
              textOutput("predicted_price")
            ),
            br(),
            div(
              style = "font-size: 14px; text-align: center; color: #555;",
              textOutput("prediction_timestamp")
            )
          ),
          box(
            title = "About the Predictor", status = "info", solidHeader = TRUE,
            width = 6,
            p("This tool provides an estimated price for a hotel or property type in Abuja based on a machine learning model."),
            p("Factors used for prediction:"),
            tags$ul(
              tags$li("Property Type Group (e.g., Hotel, Guest House/Inn, Apartment/Suites)"), # Updated description
              tags$li("Location (Grouped areas within Abuja or 'Other Location')"), # Updated description
              tags$li("Number of Likes (Positive endorsements)")
            ),
            p("Note: This is a statistical estimate based on the provided dataset and may not reflect real-time pricing or all market factors.")
          )
        )
      ),

      # --- Hotel Table ---
      tabItem(
        tabName = "table",
        h2("Hotel Table"),
        div(
          class = "loading-message",
          shinycssloaders::withSpinner(
            DTOutput("table"),
            type = 4,
            color = "#0dc5c1"
          )
        ),
      ),

      # --- Data Exploration Tab Content ---
      tabItem(
        tabName = "explore",
        h2("Data Exploration"),
        fluidRow(
          box(
            title = "Hotel Cost vs. Likes", status = "info", solidHeader = TRUE,
            width = 6,
            shinycssloaders::withSpinner(
              plotOutput("cost_vs_likes_plot", height = "350px"),
              type = 4,
              color = "#0dc5c1"
            )
          ),
          box(
            title = "Hotel Cost by Grouped Property Type", status = "info", solidHeader = TRUE,
            width = 6,
            shinycssloaders::withSpinner(
              plotOutput("cost_by_property_type_plot", height = "350px"),
              type = 4,
              color = "#0dc5c1"
            )
          ),
          box(
            title = "Hotel Cost by Top Locations", status = "info",
            solidHeader = TRUE,
            width = 6,
            shinycssloaders::withSpinner(
              plotOutput("cost_by_location_plot", height = "350px"),
              type = 4,
              color = "#0dc5c1"
            )
          )
        ),
      )
    )
  )
)
