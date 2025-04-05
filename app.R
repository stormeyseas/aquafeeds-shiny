# nolint: line_length_linter.
# Load required packages
library(rlang)  # Add this line for %||% operator
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(sass)
library(bslib)
library(shinyWidgets)
library(plotly)
library(countrycode)

# Global variables
CLP_fill <- c(carbohydrate = "#FF9999", lipid = "#66B3FF", protein = "#99FF99")

# Compile SCSS to CSS
sass(
  input = sass_file("stylesheet.scss"),
  output = "www/styles.css"
)

# Source functions
source("functions.R")

# UI Definition
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("Sustainable Aquafeeds"),

  tabsetPanel(
    # Tab 1: Feed Composition
    tabPanel("Feed composition",
      sidebarLayout(
        sidebarPanel(
          selectInput("preset_diet", "Select preset diet:", 
                      choices = c("Custom", "Past" = "past", "Reference" = "reference", "Future" = "future")),
          # Ingredient inputs will be generated dynamically
          uiOutput("ingredient_inputs"),
          tags$div(
            style = "margin-top: 15px;",
            textOutput("validation_message")
          )
        ),
        mainPanel(
          # Instructions
          card(
            div(style = "padding: 10px;",
              h4("Instructions"),
              helpText(read_app_data("data/UI.csv") %>% filter(element == "feed_mix_help") %>% pull(text))
            )
          ),
          # Output plot
          div(style = "margin-top: 20px;",
            plotOutput("nutrition_plot")
          ),

          checkboxInput("show_requirements", "Show requirements for salmon", FALSE)
        )
      )
    ),
    # Tab 2: Sourcing
    tabPanel("Sourcing",
      sidebarLayout(
        sidebarPanel(
          # Dynamic UI for ingredient sourcing/processing choices
          uiOutput("sourcing_inputs")
        ),
        mainPanel(
          # Instructions
          card(
            div(style = "padding: 10px;",
              h4("Instructions"),
              helpText(read_app_data("data/UI.csv") %>% filter(element == "sourcing_help") %>% pull(text))
            )
          ),
          # Map output
          div(style = "margin-top: 20px;",
            h4("Map of ingredient sourcing and processing locations"),
            plotlyOutput("sourcing_map", height = "600px")
          )
        )
      )
    ),
    # Placeholder tabs
    tabPanel("Local inputs", "Content coming soon"),
    tabPanel("Global inputs", "Content coming soon"),
    tabPanel("Biodiversity impacts", "Content coming soon"),
    tabPanel("Options", "Content coming soon"),
    tabPanel("Optimise", "Content coming soon")
  )
)

# Server Definition
server <- function(input, output, session) {
  # Load data
  ingredients <- read_app_data("data/ingredients.csv")
  preset_diets <- read_app_data("data/preset_diets.csv")
  sourcing_data <- read_app_data("data/sourcing_processing.csv")

  # Observer for preset diet selection
  observeEvent(input$preset_diet, {
    if (input$preset_diet != "Custom") {
      diet_data <- preset_diets %>%
        filter(feed_name == input$preset_diet)
      
      # Update each ingredient input
      for (i in seq_len(nrow(diet_data))) {
        updateNumericInput(session,
          inputId = paste0("ing_", make.names(diet_data$ingredient[i])),
          value = diet_data$composition[i] * 100
        )
      }
    }
  })

  # Generate ingredient input fields
  output$ingredient_inputs <- renderUI({
    # Get all ingredients except trimmings versions
    all_ingredients <- sort(unique(ingredients$ingredient))
    all_ingredients <- all_ingredients[!all_ingredients %in% c("Fishmeal trimmings", "Fish oil trimmings")]
    ing_names <- paste0("ing_", make.names(all_ingredients))
    mid_point <- ceiling((length(all_ingredients)-2)/2)

    # Create two column layout
    fluidRow(
      column(6,
        lapply(all_ingredients[1:mid_point], function(ing) {
          if (ing %in% c("Fishmeal forage", "Fish oil forage")) {
            # For forage ingredients, add a checkbox for trimmings
            div(
              style = "margin-bottom: 15px;",
              numericInput(
                inputId = paste0("ing_", make.names(ing)),
                label = ing,
                value = 0,
                min = 0,
                max = 100
              ),
              checkboxInput(
                inputId = paste0("use_trimmings_", sub("forage", "trimmings", make.names(ing))),
                label = "Use trimmings",
                value = FALSE
              )
            )
          } else {
            numericInput(
              inputId = paste0("ing_", make.names(ing)),
              label = ing,
              value = 0,
              min = 0,
              max = 100
            )
          }
        })
      ),
      column(6,
        lapply(all_ingredients[(mid_point+1):length(all_ingredients)], function(ing) {
          if (ing %in% c("Fishmeal forage", "Fish oil forage")) {
            # For forage ingredients, add a checkbox for trimmings
            div(
              style = "margin-bottom: 15px;",
              numericInput(
                inputId = paste0("ing_", make.names(ing)),
                label = ing,
                value = 0,
                min = 0,
                max = 100
              ),
              checkboxInput(
                inputId = paste0("use_trimmings_", sub("forage", "trimmings", make.names(ing))),
                label = "Use trimmings",
                value = FALSE
              )
            )
          } else {
            numericInput(
              inputId = paste0("ing_", make.names(ing)),
              label = ing,
              value = 0,
              min = 0,
              max = 100
            )
          }
        })
      )
    )
  })

  # Calculate total percentage of ingredients
  total_percentage <- reactive({
    all_ingredients <- sort(unique(ingredients$ingredient))
    all_ingredients <- all_ingredients[!all_ingredients %in% c("Fishmeal trimmings", "Fish oil trimmings")]
    sum(sapply(all_ingredients, function(ing) {
      as.numeric(input[[paste0("ing_", make.names(ing))]]) %||% 0
    }))
  })

  # Display validation message
  output$validation_message <- renderText({
    total <- total_percentage()
    if (total == 0) {
      return("Please add ingredients to your feed mix")
    } else if (total != 100) {
      return(sprintf("Current total: %.1f%% (ingredients will be scaled to 100%%)", total))
    }
    return(NULL)
  })

  # Calculate nutrition content
  nutrition_data <- reactive({
    total <- total_percentage()
    if (total == 0) return(NULL)

    # Get all ingredient inputs using proper names
    all_ingredients <- sort(unique(ingredients$ingredient))
    all_ingredients <- all_ingredients[!all_ingredients %in% c("Fishmeal trimmings", "Fish oil trimmings")]

    # Create a named vector of input values
    ing_values <- sapply(all_ingredients, function(ing) {
      value <- as.numeric(input[[paste0("ing_", make.names(ing))]]) %||% 0

      # Handle trimmings substitution
      if (ing == make.names("Fishmeal forage") && input$use_trimmings_Fishmeal.trimmings.) {
        ing <- make.names("Fishmeal trimmings")
      }
      if (ing == make.names("Fish oil forage") && input$use_trimmings_Fish.oil.trimmings.) {
        ing <- make.names("Fish oil trimmings")
      }
      value
    })
    names(ing_values) <- all_ingredients

    # Scale inputs to 100%
    scaled_inputs <- ing_values / sum(ing_values) * 100

    # Calculate total nutrients for each component
    components <- unique(ingredients$component)
    result <- data.frame(
      component = components,
      amount = numeric(length(components))
    )

    # Calculate amount for each component
    for (i in seq_along(components)) {
      comp <- components[i]
      result$amount[i] <- sum(sapply(names(scaled_inputs), function(ing) {
        if (scaled_inputs[ing] > 0) {
          scaled_inputs[ing] * digestible_composition(ingredients, ing, comp) / 100
        } else {
          0
        }
      }))
    }

    return(result)
  })

  # Render nutrition plot
  output$nutrition_plot <- renderPlot({
    # Check if we have data to plot
    if (is.null(nutrition_data())) {
      # Create empty plot with message
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data to display yet...") +
        theme_void() +
        xlim(0, 1) +
        ylim(0, 1)
    } else {
      # Create nutrition composition plot
      p <- ggplot(nutrition_data(), aes(x = component, y = amount*100, fill = component)) +
        geom_bar(stat = "identity", colour = "black") +
        scale_x_discrete(limits = c("carbohydrate", "lipid", "protein"),
                        labels = c("Carbohydrate", "Lipid", "Protein")) +
        scale_fill_manual(values = CLP_fill) +
        labs(title = "Nutritional Composition of Aquafeed",
             x = "Component",
             y = "Amount (%)") +
        theme_classic() +
        my_plot_theme()

    # Add salmon requirements if checkbox is checked
    if (input$show_requirements) {
      salmon_requirements <- read_app_data("data/salmon_requirements.csv")
      # Add segments for each component's requirements
      for (comp in c("carbohydrate", "lipid", "protein")) {
        req <- salmon_requirements[salmon_requirements$component == comp, ]
        if (!is.na(req$min)) {
          p <- p + geom_segment(
            data = req,
            aes(x = comp, xend = comp,
                y = min * 100, yend = max * 100),
            linetype = "dashed", size = 1
          )
        }
      }
    }
    p
    }
  })

  # Generate sourcing input fields
  output$sourcing_inputs <- renderUI({
    # Get ingredients with non-zero percentages
    active_ingredients <- get_active_ingredients(input, ingredients)
    
    if (length(active_ingredients) == 0) {
      return(h4("Please select ingredients in the Feed Composition tab first"))
    }


    # Create input fields for each active ingredient
    lapply(active_ingredients, function(ing) {
      # Get unique countries from sourcing data
      source_countries <- sort(unique(sourcing_data$raw_material[sourcing_data$ingredient == ing]))
      process_countries <- sort(unique(sourcing_data$processing[sourcing_data$ingredient == ing]))

      div(
        # style = "margin-bottom: 20px;",
        h4(ing),
        fluidRow(
          column(6,
            selectInput(
              inputId = paste0("source_", make.names(ing)),
              label = NULL,
              choices = source_countries
            )
          ),
          column(6,
            selectInput(
              inputId = paste0("process_", make.names(ing)),
              label = NULL,
              choices = process_countries
            )
          )
        )
      )
    })
  })

  # Create the sourcing map
  output$sourcing_map <- renderPlotly({
    # Get active ingredients and their locations
    active_ingredients <- get_active_ingredients(input, ingredients)
    
    if (length(active_ingredients) == 0) {
      return(plot_ly() %>% 
             layout(title = "Please select ingredients in the Feed Composition tab"))
    }

    # Collect selected countries and their roles
    countries_data <- get_selected_countries(input, active_ingredients)
    
    # Create the map
    create_sourcing_map(countries_data)
  })
}

# Run app
shinyApp(ui = ui, server = server)