# nolint: line_length_linter.
# Function to safely read CSV files with error handling
read_app_data <- function(filepath) {
  tryCatch({
    read.csv(filepath, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop(paste("Error reading file:", e$message))
  })
}

# Function to calculate total composition of an ingredient (digestible or not)
simple_composition <- function(dataframe, ingredient_name, component_name) {
  # Check if ingredient exists
  if (!ingredient_name %in% dataframe$ingredient) {
    stop(paste("Ingredient", ingredient_name, "not found in the dataframe."))
  }
  
  # Calculate component composition
  total_composition <- dataframe$composition[
    dataframe$ingredient == ingredient_name & 
    dataframe$component == component_name
  ]

  return(total_composition)
}

# Function to calculate effective digestible composition of an ingredient
digestible_composition <- function(dataframe, ingredient_name, component_name) {
  # Check if ingredient exists
  if (!ingredient_name %in% dataframe$ingredient) {
    stop(paste("Ingredient", ingredient_name, "not found in the dataframe."))
  }
  
  # Calculate effective component composition
  effective_composition <- dataframe$composition[
    dataframe$ingredient == ingredient_name & 
    dataframe$component == component_name
  ] * dataframe$digestibility[
    dataframe$ingredient == ingredient_name & 
    dataframe$component == component_name
  ]
  
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

# Function to create centered and properly styled plot theme
my_plot_theme <- function() {
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.title.position = "plot",
    axis.line.x = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 14)
  )
}

# Get list of ingredients with non-zero percentages
get_active_ingredients <- function(input, ingredients_df) {
  all_ingredients <- sort(unique(ingredients_df$ingredient))
  all_ingredients <- all_ingredients[!all_ingredients %in% c("Fishmeal trimmings", "Fish oil trimmings")]
  
  active <- sapply(all_ingredients, function(ing) {
    value <- as.numeric(input[[paste0("ing_", make.names(ing))]]) %||% 0
    value > 0
  })
  
  return(names(active)[active])
}

# Get selected countries and their roles
get_selected_countries <- function(input, active_ingredients) {
  countries <- data.frame(
    country = character(),
    role = character(),
    stringsAsFactors = FALSE
  )
  
  for (ing in active_ingredients) {
    source_country <- input[[paste0("source_", make.names(ing))]]
    process_country <- input[[paste0("process_", make.names(ing))]]
    
    if (!is.null(source_country)) {
      countries <- rbind(countries, 
                        data.frame(country = source_country, 
                                 role = "source"))
    }
    if (!is.null(process_country)) {
      countries <- rbind(countries, 
                        data.frame(country = process_country, 
                                 role = "process"))
    }
  }
  
  # Aggregate roles for countries that appear multiple times
  countries <- countries %>%
    group_by(country) %>%
    summarise(role = case_when(
      all(role == "source") ~ "source",
      all(role == "process") ~ "process",
      TRUE ~ "both"
    )) %>% 
    mutate( 
      role = factor(role, levels = c("source", "process", "both"))
    )
  
  return(countries)
}

# Create plotly map with selected countries highlighted
create_sourcing_map <- function(countries_data) {
  # Define colors for different roles
  role_colors <- c(
    "source" = "red",
    "process" = "blue",
    "both" = "purple"
  )
  
  # Create base map
  plot_ly() %>%
    add_trace(
      type = "choropleth",
      locationmode = "country names",
      locations = countries_data$country,
      z = as.numeric(factor(countries_data$role)),
      text = paste0(
        countries_data$country, "<br>",
        "Role: ", countries_data$role
      ),
      colorscale = list(
        list(1, role_colors["source"]),
        list(2, role_colors["process"]),
        list(3, role_colors["both"])
      ),
      showscale = FALSE
    ) %>%
    layout(
      title = NULL,
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'equirectangular')
      )
    )
}

# Function to fix ingredient names into nice labels for UI
nice_ing_names <- function(ing) {
  ing %>% 
    str_remove("ing_") %>% 
    str_replace_all("-", " ") %>% 
    str_to_sentence() %>% 
    str_replace("Bsf", "Black soldier fly") %>% 
    str_replace("Meat bone", "Meat & bone")
}