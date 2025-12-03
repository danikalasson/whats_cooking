# ============================================================
# Libraries
# ============================================================
library(jsonlite)
library(dplyr)
library(tidyverse)
library(stringr)
library(recipes)
library(textrecipes)
library(randomForest)

# ============================================================
# Load Data
# ============================================================
trainSet <- read_file("C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/train.json") %>%
  fromJSON()

testSet <- read_file("C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/test.json") %>%
  fromJSON()

# ============================================================
# Convert ingredient lists → single strings
# ============================================================
collapse_ingredients <- function(df) {
  df %>%
    mutate(
      ingredients = sapply(
        ingredients,
        function(x) str_c(str_to_lower(x), collapse = " ")
      )
    )
}

train_text <- collapse_ingredients(trainSet)
test_text  <- collapse_ingredients(testSet)

train_text$cuisine <- as.factor(train_text$cuisine)

# ============================================================
# TF-IDF RECIPE
# ============================================================
# Convert ingredients list to a single string per recipe
trainSet <- trainSet %>%
  mutate(ingredients_text = sapply(ingredients, paste, collapse = " "))

testSet <- testSet %>%
  mutate(ingredients_text = sapply(ingredients, paste, collapse = " "))

# TF-IDF recipe
rec <- recipe(cuisine ~ ingredients_text, data=trainSet) %>%
  step_tokenize(ingredients_text) %>%
  step_tokenfilter(ingredients_text, max_tokens=500) %>%
  step_tfidf(ingredients_text)

prep_rec <- prep(rec)

# Bake TF-IDF features (ingredients_text is already replaced)
train_tfidf <- bake(prep_rec, new_data = trainSet)
test_tfidf  <- bake(prep_rec, new_data = testSet)

# ============================================================
# Train Random Forest with TF-IDF
# ============================================================
set.seed(123)

# Remove id & non-predictors
train_model_data <- train_tfidf 
test_model_data  <- test_tfidf

rf_model <- randomForest(
  cuisine ~ .,
  data = train_model_data,
  ntree = 300
)

# ============================================================
# Predict on Test
# ============================================================
test_predictions <- predict(rf_model, newdata = test_model_data)

# ============================================================
# Create Kaggle Submission
# ============================================================
submission <- data.frame(
  id = test_text$id,
  cuisine = test_predictions
)

write.csv(
  submission,
  "C:/Users/lasso/OneDrive/Documents/Fall 2025/Stat 348/whats_cooking_new/submission.csv",
  row.names = FALSE
)


