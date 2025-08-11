# nolint: line_length_linter.
# Load required packages
library(rlang)  # Add this line for %||% operator
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(sass)
library(bslib)
library(shinyWidgets)
library(plotly)
library(countrycode)
library(DT)
library(RColorBrewer)

# Global variables
CLP_fill <- c(
  protein = "#8B0000", # "darkred"
  P = "#8B0000", 
  carbohydrate = "#4682B4", # "steelblue"
  C = "#4682B4", 
  lipid = "#FFB90F", # "darkgoldenrod1", 
  L = "#FFB90F"
)

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
                      choices = c(
                      "Custom", 
                      "Marine-dominant" = "marine_dominant", 
                      "Plant-dominant" = "plant_dominant", 
                      "Novel-inclusive" = "novel_inclusive"
                    )),
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

          checkboxInput("show_requirements", "Show requirements for salmon", FALSE),
          
          # Ingredient type breakdown table
          div(style = "margin-top: 30px;",
            h4("Feed component breakdown"),
            helpText(read_app_data("data/UI.csv") %>% filter(element == "feed_component_breakdown") %>% pull(text)),
            DTOutput("ingredient_breakdown_table")
          )
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
          value = round(diet_data$composition[i] * 100, 1)
        )
      }
    }
  })

  # Generate ingredient input fields
  output$ingredient_inputs <- renderUI({
    # Get all ingredients and group by category
    all_ingredients <- sort(unique(ingredients$ingredient))
    
    # Create a list to group ingredients by category
    ingredient_categories <- ingredients %>%
      select(ingredient, category) %>%
      distinct() %>%
      arrange(category, ingredient)
    
    # Get unique categories in order
    categories <- unique(ingredient_categories$category)
    
    # Create sections for each category
    category_sections <- lapply(categories, function(cat) {
      # Get ingredients for this category
      cat_ingredients <- ingredient_categories %>%
        filter(category == cat) %>%
        pull(ingredient)
      
      # Calculate midpoint for two-column layout
      mid_point <- ceiling(length(cat_ingredients)/2)
      
      # Create category section with header and two columns
      div(
        style = "margin-bottom: 20px;",
        h4(paste(toupper(substring(cat, 1, 1)), substring(cat, 2), " ingredients", sep = ""),
           style = "color: #666; border-bottom: 1px solid #ddd; padding-bottom: 5px; margin-bottom: 10px;"),
        fluidRow(
          column(6,
            if(length(cat_ingredients) > 0) {
              lapply(cat_ingredients[1:min(mid_point, length(cat_ingredients))], function(ing) {
                numericInput(
                  inputId = paste0("ing_", make.names(ing)),
                  label = nice_ing_names(ing),
                  value = 0,
                  min = 0,
                  max = 100
                )
              })
            }
          ),
          column(6,
            if(length(cat_ingredients) > mid_point) {
              lapply(cat_ingredients[(mid_point+1):length(cat_ingredients)], function(ing) {
                numericInput(
                  inputId = paste0("ing_", make.names(ing)),
                  label = nice_ing_names(ing),
                  value = 0,
                  min = 0,
                  max = 100
                )
              })
            }
          )
        )
      )
    })
    
    # Return all category sections
    do.call(tagList, category_sections)
  })

  # Calculate total percentage of ingredients
  total_percentage <- reactive({
    all_ingredients <- sort(unique(ingredients$ingredient))
    sum(sapply(all_ingredients, function(ing) {
      as.numeric(input[[paste0("ing_", make.names(ing))]]) %||% 0
    }))
  })

  # Display validation message
  output$validation_message <- renderText({
    total <- total_percentage()
    if (total == 0) {
      return("Please add ingredients to your feed mix.")
    } else if (total != 100) {
      return(sprintf("Current total: %.1f%% (ingredients are automatically scaled to 100%%)", total))
    }
    return(NULL)
  })

  # Calculate nutrition content
  nutrition_data <- reactive({
    total <- total_percentage()
    if (total == 0) return(NULL)

    # Get all ingredient inputs using proper names
    all_ingredients <- sort(unique(ingredients$ingredient))

    # Create a named vector of input values
    ing_values <- sapply(all_ingredients, function(ing) {
      as.numeric(input[[paste0("ing_", make.names(ing))]]) %||% 0
    })
    names(ing_values) <- all_ingredients

    # Scale inputs to 100%
    scaled_inputs <- ing_values / sum(ing_values) * 100

    # Calculate total nutrients for each component
    components <- unique(ingredients$component)
    result <- data.frame(
      component = components,
      total = numeric(length(components)),
      digestible = numeric(length(components))
    )

    # Calculate amount for each component
    for (i in seq_along(components)) {
      comp <- components[i]
      result$total[i] <- sum(sapply(names(scaled_inputs), function(ing) {
        if (scaled_inputs[ing] > 0) {
          scaled_inputs[ing] * simple_composition(ingredients, ing, comp) / 100
        } else {
          0
        }}))
      result$digestible[i] <- sum(sapply(names(scaled_inputs), function(ing) {
        if (scaled_inputs[ing] > 0) {
          scaled_inputs[ing] * digestible_composition(ingredients, ing, comp) / 100
        } else {
          0
        }}))
    }
    result <- result %>% 
      pivot_longer(
        names_to = "type", names_transform = list(type = as.factor),
        values_to = "amount",
        cols = c("digestible", "total")
      )
    return(result)
  })

  # Calculate ingredient type breakdown
  ingredient_type_breakdown <- reactive({
    total <- total_percentage()
    if (total == 0) return(NULL)

    # Get all ingredient inputs using proper names
    all_ingredients <- sort(unique(ingredients$ingredient))

    # Create a named vector of input values
    ing_values <- sapply(all_ingredients, function(ing) {
      as.numeric(input[[paste0("ing_", make.names(ing))]]) %||% 0
    })
    names(ing_values) <- all_ingredients

    # Scale inputs to 100%
    scaled_inputs <- ing_values / sum(ing_values) * 100

    # Get unique ingredient types (categories)
    ingredient_types <- c("marine", "plant", "animal", "novel", "other")
    components <- c("protein", "lipid", "carbohydrate")
    
    # Initialize result data frame
    result <- expand.grid(
      Type = factor(ingredient_types, levels = ingredient_types, labels = c("Marine", "Plant", "Animal", "Novel", "Other")),
      component = components,
      stringsAsFactors = FALSE
    )
    result$digestible_pct <- 0
    result$total_pct <- 0

    # Calculate total amounts for each component (for percentage calculation)
    total_amounts <- sapply(components, function(comp) {
      sum(sapply(names(scaled_inputs), function(ing) {
        if (scaled_inputs[ing] > 0) {
          scaled_inputs[ing] * simple_composition(ingredients, ing, comp) / 100
        } else {
          0
        }
      }))
    })
    
    digestible_amounts <- sapply(components, function(comp) {
      sum(sapply(names(scaled_inputs), function(ing) {
        if (scaled_inputs[ing] > 0) {
          scaled_inputs[ing] * digestible_composition(ingredients, ing, comp) / 100
        } else {
          0
        }
      }))
    })

    # Calculate breakdown by ingredient type
    for (i in seq_len(nrow(result))) {
      type <- tolower(as.character(result$Type[i]))
      comp <- result$component[i]
      
      # Get ingredients of this type
      type_ingredients <- ingredients %>%
        filter(category == type) %>%
        pull(ingredient) %>%
        unique()
      
      # Calculate total and digestible amounts for this type and component
      type_total <- sum(sapply(type_ingredients, function(ing) {
        if (ing %in% names(scaled_inputs) && scaled_inputs[ing] > 0) {
          scaled_inputs[ing] * simple_composition(ingredients, ing, comp) / 100
        } else {
          0
        }
      }))
      
      type_digestible <- sum(sapply(type_ingredients, function(ing) {
        if (ing %in% names(scaled_inputs) && scaled_inputs[ing] > 0) {
          scaled_inputs[ing] * digestible_composition(ingredients, ing, comp) / 100
        } else {
          0
        }
      }))
      
      # Calculate percentages
      result$total_pct[i] <- if (total_amounts[comp] > 0) {
        round(type_total / total_amounts[comp] * 100, 1)
      } else {
        0
      }
      
      result$digestible_pct[i] <- if (digestible_amounts[comp] > 0) {
        round(type_digestible / digestible_amounts[comp] * 100, 1)
      } else {
        0
      }
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
      p <- ggplot(nutrition_data(), aes(x = component, y = amount*100, fill = component, alpha = type)) +
        geom_bar(
          position = position_dodge(), 
          width = 0.95,
          stat = "identity", 
          colour = "black"
        )
      # See below for plot display specs

    # Add salmon requirements if checkbox is checked
    if (input$show_requirements) {
      salmon_requirements <- read_app_data("data/salmon_requirements.csv")
      salmon_requirements <- rbind(
        salmon_requirements %>% mutate(type = "digestible"),
        salmon_requirements %>% mutate(type = "total")
      ) %>% 
        mutate(
          min = case_when(type == "total" ~ NA, T ~ min),
          max = case_when(type == "total" ~ NA, T ~ max),
          type = as.factor(type)
      )

      p <- p + 
        geom_errorbar(
            data = salmon_requirements,
            aes(x = component, ymin = min * 100, ymax = max * 100, fill = component, linetype = type),
            position = position_dodge(width = 0.95),
            width = 0.25,
            linewidth = 1, 
            inherit.aes = F
          )
    }
    p +
      scale_x_discrete(limits = c("carbohydrate", "lipid", "protein"),
                      labels = c("Carbohydrate", "Lipid", "Protein")) +
      scale_fill_manual(values = CLP_fill) +
      scale_alpha_manual(values = c("total" = 1, "digestible" = 0.25),
                        labels = c("digestible" = "Digestible", "total" = "Total"),
                        name = NULL) +
      labs(title = "Nutritional Composition of Aquafeed",
            x = "Component", y = "Amount (%)") +
      theme_classic() +
      my_plot_theme() +
      theme(legend.position = "top") +
      guides(fill = "none", linetype = "none")
    }
  })

  # Render ingredient breakdown table
  output$ingredient_breakdown_table <- renderDT({
    breakdown_data <- ingredient_type_breakdown()
    if (is.null(breakdown_data)) {
      return(NULL)
    }
    
    # Reshape data for table display
    table_data <- breakdown_data %>%
      select(Type, component, digestible_pct, total_pct) %>%
      pivot_wider(
        names_from = component,
        values_from = c(digestible_pct, total_pct),
        names_glue = "{component}_{.value}"
      ) %>%
      select(
        Type,
        protein_digestible_pct, lipid_digestible_pct, carbohydrate_digestible_pct,
        protein_total_pct, lipid_total_pct, carbohydrate_total_pct
      )
    
    # Format the table data - add % to numeric columns only
    table_data[, 2:7] <- lapply(table_data[, 2:7], function(x) paste0(x, "%"))
    
    datatable(
      table_data,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:6)
        )
      ),
      container = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, "Type"),
            th(colspan = 3, "Digestible", style = "text-align: center; border-bottom: 1px solid #ddd;"),
            th(colspan = 3, "Total", style = "text-align: center; border-bottom: 1px solid #ddd;")
          ),
          tr(
            th("Protein"), th("Lipid"), th("Carbohydrate"),
            th("Protein"), th("Lipid"), th("Carbohydrate")
          )
        )
      )),
      rownames = FALSE,
      colnames = rep("", 7)
    )
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
        h4(nice_ing_names(ing)),
        fluidRow(
          column(6,
            selectInput(
              inputId = paste0("source_", make.names(ing)),
              label = NULL, # "Source country:",
              choices = source_countries
            )
          ),
          column(6,
            selectInput(
              inputId = paste0("process_", make.names(ing)),
              label = NULL, # "Processing country:",
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