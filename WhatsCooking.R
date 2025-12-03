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
  left_join(cajun_creole_feature, by = "id")

training_features










library(jsonlite)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(randomForest)   # you can switch to xgboost if you want

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
trainSet <- read_file("C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/train.json") %>%
  fromJSON()

testSet <- read_file("C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/test.json") %>%
  fromJSON()

train_long <- trainSet %>% unnest(ingredients)
test_long  <- testSet %>% unnest(ingredients)

# ------------------------------------------------------------
# Feature Engineering Function (Reusable for train + test)
# ------------------------------------------------------------
make_features <- function(long_data, original_data) {
  
  # 1. Ingredient Count
  ingredient_counts <- long_data %>%
    group_by(id) %>%
    summarise(ingredient_count = n(), .groups = "drop")
  
  # 2. Spice Count
  spices <- c("cumin", "turmeric", "coriander", "pepper", "paprika",
              "ginger", "garam masala", "cinnamon", "chili", "cardamom")
  
  spice_counts <- long_data %>%
    mutate(is_spice = if_else(str_to_lower(ingredients) %in% spices, 1, 0)) %>%
    group_by(id) %>%
    summarise(spice_count = sum(is_spice), .groups = "drop")
  
  # 3. Coconut Indicator
  coconut_feature <- long_data %>%
    mutate(has_coconut = if_else(str_detect(str_to_lower(ingredients), "coconut"), 1, 0)) %>%
    group_by(id) %>%
    summarise(has_coconut = max(has_coconut), .groups = "drop")
  
  # Merge
  original_data %>%
    select(id, cuisine = matches("cuisine|^$")) %>%   # handles train (has cuisine) vs test (no cuisine)
    left_join(ingredient_counts, by = "id") %>%
    left_join(spice_counts, by = "id") %>%
    left_join(coconut_feature, by = "id")
}

# ------------------------------------------------------------
# Create train + test feature frames
# ------------------------------------------------------------
train_feat <- make_features(train_long, trainSet)
test_feat  <- make_features(test_long, testSet)

# Remove cuisine from test_feat (it will be NA)
test_feat$cuisine <- NULL
train_feat$cuisine <- as.factor(train_feat$cuisine)

# ------------------------------------------------------------
# Train a model (Random Forest)
# ------------------------------------------------------------
set.seed(123)

rf_model <- randomForest(
  cuisine ~ ingredient_count + spice_count + has_coconut,
  data = train_feat,
  ntree = 300
)

# Predict on test
test_predictions <- predict(rf_model, newdata = test_feat)

# ------------------------------------------------------------
# Create Kaggle Submission File
# ------------------------------------------------------------
submission <- data.frame(
  id = test_feat$id,
  cuisine = test_predictions
)

write.csv(submission, "C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/submission.csv", row.names = FALSE)







#######################
##Plots for ingredients
######################
library(dplyr)
library(ggplot2)
library(stringr)

ingredients_of_interest <- c("coconut", "soy sauce", "cilantro", "ginger", "garlic")

ingredient_freq_df <- train_long %>%
  mutate(ingredient = str_to_lower(ingredients)) %>%
  filter(ingredient %in% ingredients_of_interest) %>%
  group_by(cuisine, ingredient) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cuisine) %>%
  mutate(freq = count / sum(count)) %>%
  arrange(cuisine, desc(freq))

ingredient_freq_df



library(dplyr)
library(ggplot2)
library(stringr)

top5_df <- train_long %>%
  mutate(ingredient = str_to_lower(ingredients)) %>%
  group_by(cuisine, ingredient) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cuisine) %>%
  slice_max(order_by = count, n = 5) %>%
  arrange(cuisine, desc(count))

top5_df







unique_ingredients <- train_long %>%
  mutate(ingredient = str_to_lower(ingredients)) %>%
  
  # Count cuisines per ingredient
  distinct(cuisine, ingredient) %>%
  count(ingredient, name = "num_cuisines") %>% 
  filter(num_cuisines == 1) %>%     # keep only ingredients appearing in exactly one cuisine
  
  # Join back to get the cuisine info
  inner_join(
    train_long %>%
      mutate(ingredient = str_to_lower(ingredients)) %>%
      distinct(cuisine, ingredient),
    by = "ingredient"
  ) %>%
  
  # Add the number of unique recipe IDs containing this ingredient
  left_join(
    train_long %>%
      mutate(ingredient = str_to_lower(ingredients)) %>%
      group_by(ingredient) %>%
      summarise(num_recipes = n_distinct(id), .groups = "drop"),
    by = "ingredient"
  ) %>%
  
  arrange(cuisine, ingredient)

