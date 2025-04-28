# server.R

# Define Server Logic
server <- function(input, output, session) {
  # --- Prediction Logic ---

  predicted_price_value <- eventReactive(input$predict_button,
    {
      prop_type_grouped <- input$property_type_input
      loc_group <- input$location_input
      likes <- input$likes_input

      # Basic input validation (optional but recommended)
      if (is.null(prop_type_grouped) || is.null(loc_group) || is.null(likes) || is.na(likes) || likes < 0) {
        return("Please provide valid inputs.")
      }

      # --- 2. Create a new data frame for prediction ---
      new_data_df <- data.frame(
        Location_Grouped = factor(loc_group, levels = levels(df_cleaned$Location_Grouped)),
        Property_type_Grouped = factor(prop_type_grouped, levels = levels(df_cleaned$Property_type_Grouped)),
        Likes = as.numeric(likes), # Ensure Likes is numeric
        Cost = NA
      )

      # --- 3. Align prediction matrix columns with training matrix columns ---
      # Get column names from the training design matrix (created in global.R)
      train_col_names <- colnames(X_train_matrix)

      # Create an empty matrix of zeros with the correct training columns
      X_predict_aligned <- matrix(0, nrow = 1, ncol = length(train_col_names))
      colnames(X_predict_aligned) <- train_col_names

      # Create the prediction matrix for the new data using the SAME formula and contrasts as training
      X_predict_partial <- tryCatch(
        {
          model.matrix(model_formula, data = new_data_df, contrasts.arg = lapply(new_data_df[sapply(new_data_df, is.factor)], contrasts, contrasts = FALSE))
        },
        error = function(e) {
          message("Error creating prediction matrix for input:", e$message)
          return(NULL)
        }
      )

      # Check if matrix creation failed
      if (is.null(X_predict_partial)) {
        return("Error: Could not process input features for prediction.")
      }

      match_cols <- match(colnames(X_predict_partial), train_col_names)

      X_predict_aligned[1, match_cols[!is.na(match_cols)]] <- X_predict_partial[1, !is.na(match_cols)]

      # --- 4. Make prediction ---
      pred <- predict(rf_model, newdata = X_predict_aligned)

      # --- 5. Format and return output ---
      paste("₦", format(round(pred, 2), big.mark = ","))
    },
    ignoreInit = TRUE
  )

  # Render the predicted price text in the UI
  output$predicted_price <- renderText({
    predicted_price_value()
  })


  output$table <- renderDT({
    datatable(
      df_cleaned,
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
      ),
      rownames = FALSE,
      caption = "Hotal Table Data's"
    )
  })



  # --- Data Exploration Plotting Logic ---
  # Render Cost vs. Likes Plot
  output$cost_vs_likes_plot <- renderPlot({
    create_cost_vs_likes_plot(df_cleaned)
  })
  # Render Cost by Grouped Property Type Plot
  output$cost_by_property_type_plot <- renderPlot({
    create_cost_by_property_type_plot(df_cleaned)
  })

  # Render Cost by Top Locations Plot
  output$cost_by_location_plot <- renderPlot({
    create_cost_by_location_plot(df_cleaned)
  })
}
