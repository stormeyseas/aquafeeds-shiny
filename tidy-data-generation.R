# Data generation

library(tidyr)
library(dplyr)

# The purpose of this script is to to:
# 1. Pull data straight from the local_to_global_modelling data folder and tidy it up for shiny use
# 2. Generate some placeholder data where needed
# 3. pre-generate data for the shiny app that would otherwise be very cumbersome to generate dynamically

# This script CANNOT be run with the app, only on Tormey's local machine.

# Feed presets and ingredients ----------------------------------------------------
pull_path <- "C:/Users/treimer/Documents/R-temp-files/local_to_global_mariculture_modelling/data/_general_data/diets"

# Read the source data
ingredients_raw <- read.csv(file.path(pull_path, "all_ingredients.csv"), stringsAsFactors = FALSE)

# Define ingredient categories based on the structure file
ingredient_categories <- c(
  "microingredients" = "other",
  # Marine-sourced ingredients
  "fishmeal-forage" = "marine",
  "fishmeal-trimmings" = "marine",
  "fish-oil-forage" = "marine",
  "fish-oil-trimmings" = "marine",
  "krill-meal" = "marine",
  # Animal-sourced ingredients
  "chicken-protein-concentrate" = "animal",
  "feather-meal" = "animal",
  "meat-bone-meal" = "animal",
  "ovine-meal" = "animal",
  "poultry-meal" = "animal",
  # Novel ingredients
  "bsf-meal" = "novel",
  "single-cell-protein" = "novel",
  "yeast-meal" = "novel",
  "spirulina-meal" = "novel",
  # Plant-sourced ingredients
  "camelina-oil" = "plant",
  "canola-meal" = "plant",
  "canola-oil" = "plant",
  "coconut-oil" = "plant",
  "corn-gluten" = "plant",
  "faba-beans" = "plant",
  "guar-meal" = "plant",
  "guar-meal-high-protein" = "plant",
  "linseed-oil" = "plant",
  "pea-flour" = "plant",
  "pea-protein-concentrate" = "plant",
  "rapeseed-oil" = "plant",
  "soybean-meal" = "plant",
  "soybean-oil" = "plant",
  "soy-protein-concentrate" = "plant",
  "sunflower-meal" = "plant",
  "sunflower-meal-high-protein" = "plant",
  "tapioca" = "plant",
  "wheat" = "plant",
  "wheat-gluten" = "plant"
)

# Transform to long format
ingredients_structured <- ingredients_raw %>%
  # Add category column
  mutate(category = ingredient_categories[ingredient]) %>%
  # Select only the columns we need and rename carb to carbohydrate for consistency
  select(ingredient, category,
         protein_comp = protein, lipid_comp = lipid, carbohydrate_comp = carb,
         protein_digest = protein_digestibility, lipid_digest = lipid_digestibility, carbohydrate_digest = carb_digestibility) %>%
  # Transform to long format
  pivot_longer(
    cols = -c(ingredient, category),
    names_to = c("component", ".value"),
    names_sep = "_",
    values_to = c("composition", "digestibility")
  ) %>%
  # Rename columns to match target structure
  rename(composition = comp, digestibility = digest) %>%
  # Arrange by ingredient and component for consistency with structure file
  arrange(ingredient, component)

# Write the structured data
write.csv(ingredients_structured, "data/ingredients.csv", row.names = FALSE)
