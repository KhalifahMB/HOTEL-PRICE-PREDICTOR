library(shiny)
library(shinydashboard)
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
library(ModelMetrics)
library(scales)
library(tidyr)
library(DT)
library(stringr)
library(shinycssloaders)

# --- Data URL ---
# data_url <- "https://raw.githubusercontent.com/KhalifahMB/HOTEL-PRICE-PREDICTOR/master/Hotels_In_Abuja(in).csv"
data_url <- "C:/Users/Muhammad A El-kufahn/Documents/SIWES/HOTEL PRICE PREDICTOR/Hotels_In_Abuja(in).csv"

# --- Data Loading ---
df <- tryCatch(
  {
    read.csv(data_url, stringsAsFactors = FALSE)
  },
  error = function(e) {
    stop(paste("FATAL ERROR: Could not load data from", data_url, ". Error:", e$message))
  }
)
# --- Data Cleaning ---
# Remove index column 'X'
df_cleaned <- df[, !(colnames(df) %in% "X")]

## Cost column cleaning
df_cleaned$Cost <- trimws(df_cleaned$Cost)
df_cleaned <- df_cleaned[df_cleaned$Cost != "", ]
df_cleaned$Cost <- gsub("[₦,NGN ]", "", df_cleaned$Cost, ignore.case = TRUE)
df_cleaned$Cost <- as.numeric(df_cleaned$Cost)
df_cleaned <- df_cleaned %>% filter(!is.na(Cost))

## Remove columns with excessive NAs (Rating, Review) - confirmed this step is desired
df_cleaned <- df_cleaned[, !(colnames(df_cleaned) %in% c("Rating", "Review"))]

## Location Column Cleaning
df_cleaned$Location <- trimws(df_cleaned$Location)
df_cleaned$Location <- str_to_title(df_cleaned$Location)

# --- Manual Location Standardizations using case_when ---
df_cleaned <- df_cleaned %>%
  mutate(
    Location = case_when(
      Location == "" ~ NA_character_,
      Location == "Fct" ~ "Abuja",
      Location == "Wuse Zone 1" ~ "Wuse",
      Location == "Wuse 2" ~ "Wuse Zone 2",
      Location == "Wuse2" ~ "Wuse Zone 2",
      Location == "Wuse 5" ~ "Wuse Zone 5",
      Location == "Wuse 6" ~ "Wuse Zone 6",
      Location == "Wuse, Zone 6" ~ "Wuse Zone 6",
      Location == "Wuse 7" ~ "Wuse Zone 7",
      Location == "Wuse Ii, Abuja" ~ "Wuse Zone 2",
      Location == "Lugbe," ~ "Lugbe",
      Location == "Kuje," ~ "Kuje",
      Location == "Mabuchi" ~ "Mabushi",
      Location == "Mabushi District" ~ "Mabushi",
      Location == "Kado Kuchi" ~ "Kado",
      Location == "Gwarimpa" ~ "Gwarinpa",
      Location == "Gwagwa" ~ "Gwagwalada",
      Location == "Area 2 Garki" ~ "Area 2",
      Location == "Georgio Hotel And Resort 2" ~ NA_character_,
      Location == "Lokoja-Kaduna" ~ NA_character_,
      .default = Location
    )
  )

df_cleaned <- df_cleaned %>% drop_na(Location)

# --- Feature Engineering ---
df_cleaned$Property_type <- as.factor(df_cleaned$Property_type)
df_cleaned$Location <- as.factor(df_cleaned$Location)

# --- Property Type Grouping ---
# Function to categorize property types based on keywords
categorize_property_type <- function(prop_name) {
  if (is.na(prop_name) || as.character(prop_name) == "") {
    return("Other Property Type")
  }
  prop_name_lower <- tolower(as.character(prop_name))
  if (grepl("luxury hotel", prop_name_lower)) {
    return("Luxury Hotel")
  } else if (grepl("boutique hotel", prop_name_lower)) {
    return("Boutique Hotel")
  } else if (grepl("spa hotel", prop_name_lower) || grepl("spa", prop_name_lower)) {
    return("Spa")
  } else if (grepl("budget hotel", prop_name_lower)) {
    return("Budget Hotel")
  } else if (grepl("hotel", prop_name_lower)) {
    return("Hotel")
  } else if (grepl("apartment", prop_name_lower)) {
    return("Apartment")
  } else if (grepl("suite", prop_name_lower)) {
    return("Suites")
  } else if (grepl("guest house", prop_name_lower)) {
    return("Guest House")
  } else if (grepl("guest inn", prop_name_lower)) {
    return("Inn")
  } else if (grepl("resort", prop_name_lower)) {
    return("Resort")
  } else if (grepl("villa", prop_name_lower)) {
    return("Villa")
  } else if (grepl("lodge", prop_name_lower)) {
    return("Lodge")
  } else if (grepl("motel", prop_name_lower)) {
    return("Motel")
  } else if (grepl("house", prop_name_lower)) {
    return("House")
  } else if (grepl("estate", prop_name_lower)) {
    return("Estates")
  } else if (grepl("garden", prop_name_lower)) {
    return("Garden")
  }

  return("Other Property Type")
}

# Get all possible levels by applying the function to the original Property_type column
all_possible_grouped_levels <- unique(sapply(df$Property_type, categorize_property_type))
df_cleaned$Property_type_Grouped <- factor(sapply(df_cleaned$Property_type, categorize_property_type),
  levels = all_possible_grouped_levels
)


# --- Location Grouping ---
location_counts <- table(df_cleaned$Location)
frequent_locations <- names(location_counts[location_counts >= 20])
df_cleaned$Location_Grouped <- ifelse(df_cleaned$Location %in% frequent_locations,
  as.character(df_cleaned$Location),
  "Other_Location"
)
df_cleaned$Location_Grouped <- as.factor(df_cleaned$Location_Grouped)

# --- Select features for the model ---
# --- Prepare Data for Model Matrix Creation ---
data_for_matrix_creation <- df_cleaned[, c("Location_Grouped", "Property_type_Grouped", "Likes", "Cost")]

# cols_to_check_for_na <- c("Location_Grouped", "Property_type_Grouped", "Likes", "Cost")
# data_for_matrix_cresation <- data_for_matrix_creation %>% drop_na(any_of(cols_to_check_for_na))

features_for_training <- data_for_matrix_creation[, c("Location_Grouped", "Property_type_Grouped", "Likes")]
target_for_training <- data_for_matrix_creation$Cost


# --- Define Model Formula ---
# Store the formula used for the model matrix and training
model_formula <- ~ Location_Grouped + Property_type_Grouped + Likes - 1

# --- Create the full design matrix used for training ---
X_train_matrix <- model.matrix(model_formula, data = features_for_training, contrasts.arg = lapply(features_for_training[sapply(features_for_training, is.factor)], contrasts, contrasts = FALSE))

# --- Train Random Forest Model (on all cleaned data for the app predictor) ---
set.seed(123)
# Train Random Forest model using the data prepared above
rf_model <- randomForest(x = X_train_matrix, y = target_for_training, ntree = 500, importance = TRUE)

# --- Source Plot Functions ---
plot_files <- list.files("plots", pattern = "\\.R$", full.names = TRUE)
if (length(plot_files) == 0) {
  stop("FATAL ERROR: No plot files found in the 'plots' subdirectory. Make sure the 'plots' folder exists and contains .R files.")
}
sapply(plot_files, source)

# --- Global Variables for UI and Server ---
# Choices for input dropdowns (use levels from the grouped factors)
property_types_grouped <- levels(df_cleaned$Property_type_Grouped)
location_groups <- levels(df_cleaned$Location_Grouped)

message("Global setup complete. App is ready to start.")
