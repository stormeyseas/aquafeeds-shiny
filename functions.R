# nolint: line_length_linter.
# Function to read CSV files
read_app_data <- function(filepath) {
  tryCatch({
    read.csv(filepath, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop(paste("Error reading file:", e$message))
  })
}

# Function to determine effective composition of ingredient
digestible_composition <- function(dataframe, ingredient_name, component_name) {
  # Check if the ingredient name is in the dataframe
  if (!ingredient_name %in% dataframe$ingredient) {
    stop(paste("Ingredient", ingredient_name, "not found in the dataframe."))
  }
  
  # Calculate effective component composition
  effective_composition <- dataframe$composition[dataframe$ingredient == ingredient_name & dataframe$component == component_name] * dataframe$digestibility[dataframe$ingredient == ingredient_name & dataframe$component == component_name]
  
  # Return the effective composition
  return(effective_composition)
}

# Calculate total nutritional composition of feed from ingredient percentages
calculate_feed_composition <- function(ingredients_df, input_percentages) {
  # Validate inputs
  if (sum(input_percentages) == 0) return(NULL)
  
  # Scale percentages to 100%
  scaled_percentages <- input_percentages / sum(input_percentages) * 100
  
  # Calculate composition
  total_composition <- data.frame(
    component = c("protein", "lipid", "carbohydrate"),
    amount = 0
  )
  
  # Implementation will depend on data structure
  return(total_composition)
}

# Calculate total amount of a specific nutrient from ingredient percentages
calculate_nutrient_total <- function(ingredients_df, ingredient_percentages, target_component) {
  total <- 0
  for (ing in names(ingredient_percentages)) {
    if (ingredient_percentages[ing] > 0) {
      total <- total + ingredient_percentages[ing] * 
        digestible_composition(ingredients_df, ing, target_component) / 100
    }
  }
  return(total)
}

# Function to create a centred title theme for ggplot objects
my_plot_theme <- function() {
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.title = element_text(vjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.title.position = "plot"
  )
}

