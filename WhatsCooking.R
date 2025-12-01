library(jsonlite)
library(dplyr)
library(tidyverse)
library(tidytext)
trainSet <- read_file("C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/train.json") %>%
fromJSON()
testSet <- read_file("C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/test.json") %>%
fromJSON()


training <- trainSet %>%
  unnest(ingredients)


# -----------------------------------------
# Feature Engineering
# -----------------------------------------

# 1. Ingredient Count per recipe
ingredient_counts <- training %>%
  group_by(id) %>%
  summarise(ingredient_count = n())

# 2. Spice Count per recipe
spices <- c("cumin", "turmeric", "coriander", "pepper", "paprika",
            "ginger", "garam masala", "cinnamon", "chili", "cardamom")

spice_counts <- training %>%
  mutate(is_spice = if_else(str_to_lower(ingredients) %in% spices, 1, 0)) %>%
  group_by(id) %>%
  summarise(spice_count = sum(is_spice))

# 3. Has Coconut (binary)
coconut_feature <- training %>%
  mutate(has_coconut = if_else(str_detect(str_to_lower(ingredients), "coconut"), 1, 0)) %>%
  group_by(id) %>%
  summarise(has_coconut = max(has_coconut))

# -----------------------------------------
# Merge all features + target variable (cuisine)
# -----------------------------------------
training_features <- trainSet %>%
  select(id, cuisine) %>%
  left_join(ingredient_counts, by = "id") %>%
  left_join(spice_counts, by = "id") %>%
  left_join(coconut_feature, by = "id")

training_features
