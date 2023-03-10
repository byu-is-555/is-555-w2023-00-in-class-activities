library(tidyverse)
tidyverse_update()
install.packages('tidymodels')
library(tidymodels)


# Super simple scenario: build a regression model to predict `hwy` mpg from `cty` mpg and engine `displ`.
# The `mpg` dataset is preloaded into every R session:
mpg

# The initial split operation will handle the train/test split for you.
mpg_split <- initial_split(mpg,         # <-- dataset to be split
                           prop = 0.75, # <-- default is .75, only needed if you want a different split.
                           strata = hwy) # <-- give the dv to strata so that train/test are similar in distribution 

# Then you get your training and test sets out with appropriately named functions:
mpg_training <- mpg_split %>% training()
mpg_testing <- mpg_split %>% testing()

# Next you'll specify/plan your model:
mpg_model <- linear_reg() %>%  # <-- start with the type of model you're fitting. 
  set_engine('lm') %>%         # <-- "engine" is the estimation technique you'd like to use.
  set_mode('regression')       # <-- "mode" is the type of dv you're predicting (classification vs. regression)

# Now we can fit the model using the training data:
mpg_fit <- mpg_model %>% 
  fit(hwy ~ cty + displ,    # <-- first argument is the model equation.  
      data = mpg_training)  # <-- here you give it the data you're training with.

# Check results with the `tidy()` function
mpg_fit %>% tidy()

# We use `predict()` to get predictions on new data.
# Note: ALWAYS returns the predictions in the same row order as the source data.
mpg_predictions <- mpg_fit %>% 
  predict(new_data = mpg_testing) # <-- new_data is the data "to score" with

# Usually we bind these predictions with a few columns from the data, including the "ground truth" column.
mpg_test_results <- mpg_testing %>% 
  select(hwy, cty, displ) %>% 
  bind_cols(mpg_predictions)

# We can then use these to evaluate whatever metrics feel important to us.
mpg_test_results %>%
  rmse(truth = hwy,       # <-- truth parameter is the thing the model was attempting to predict
       estimate = .pred)  # <-- estimate is the model's prediction 

# Note that all of the tidymodels evaluation functions use the same format:
mpg_test_results %>%
  rsq(truth = hwy,        # <-- here's the r-squared function
      estimate = .pred)
mpg_test_results %>%
  mae(truth = hwy,        # <-- and here's the mean absolute error function
      estimate = .pred)

# When we're done modeling, we use `last_fit()` to "finalize" a model and get associated predictions:
mpg_last_fit <- mpg_model %>% 
  last_fit(hwy ~ cty + displ,  # <-- provide the final equation (in case you dropped variables, etc.)
           split = mpg_split)  # <-- provide that split object from the beginning.

# The last fit return object is a bit complex, so we use various functions to pull things out:
mpg_last_fit %>% 
  collect_metrics()      # <-- extracts various performance metrics
mpg_last_fit %>% 
  collect_predictions()  # <-- extracts predictions from the test data



# Okay let's work through the (cleaned) titanic data from last week:
titanic <- read_csv('https://www.dropbox.com/s/92funarubgk5rzh/titanic_clean.csv?dl=1')


# First let's make sure factors are factors
leo <- titanic %>% 
  mutate(across(c(survived, pclass, had_cabin), ~as.factor(.x)))


# Now let's do a train/test split
set.seed(42)
leo_split <- initial_split(leo, strata = survived)

leo_training <- leo_split %>% training()
leo_testing <- leo_split %>% testing()

# Plan the model setup, including the engine and mode
leo_model <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')

show_engines('logistic_reg')

# relevant model types: logistic_reg(), linear_reg(), decision_tree(), rand_forest(), boost_tree()
# show_engines('logistic_reg')

# Now fit a model, look at output
leo_fit <- leo_model %>% 
  fit(survived ~ .,
      data = leo_training)

leo_fit %>% tidy()

# Calculate predictions
leo_predictions <- leo_fit %>% 
  predict(new_data = leo_testing)

leo_predictions <- leo_fit %>% 
  predict(new_data = leo_testing)

leo_predictions_prob <- leo_fit %>% 
  predict(new_data = leo_testing,
          type = 'prob')

leo_results <- leo_testing %>% 
  bind_cols(leo_predictions, leo_predictions_prob)
  

# Now let's build a confusion matrix and explore a few of the related metrics.
leo_results %>% 
  conf_mat(truth = survived,
           estimate = .pred_class) %>% 
  summary()

leo_results %>% 
  sens(truth = survived,
       estimate = .pred_class)

roc_data <- leo_results %>% 
  roc_curve(truth = survived, .pred_0)

roc_data %>% 
  autoplot()

leo_results %>% 
  roc_auc(truth = survived, .pred_0)

leo_last_fit <- leo_model %>% 
  last_fit(survived ~ .,
         split = leo_split)

leo_last_fit %>% collect_predictions()
leo_last_fit %>% collect_metrics()

# BONUS ROUND -------------------------------------------------------------------------------------------


titanic_raw <- read_csv('https://www.dropbox.com/s/petmujrpxa3qn3p/titanic.csv?dl=1')
# BONUS -------------------------------------------------------------------------------------------------



titanic_2 <- titanic_raw %>% 
  janitor::clean_names() %>% 
  mutate(had_cabin = if_else(is.na(cabin), 0, 1)) %>% 
  select(survived, pclass, sex, age, sib_sp, parch, fare, embarked, had_cabin) %>% 
  mutate(across(c(survived, pclass, had_cabin), ~as.factor(.x)))

bonus_split <- initial_split(titanic_2, strata = survived)

bonus_training <- bonus_split %>% training()
bonus_testing <- bonus_split %>% testing()

bonus_recipe <- recipe(survived ~ ., data = bonus_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_range(fare, min = 1, max = 1000) %>% 
  step_BoxCox(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = .8) %>% 
  step_nzv(all_predictors())
  
bonus_recipe

bonus_prep <- prep(bonus_recipe, training = bonus_training)

bonus_training_ready <- bake(bonus_prep, new_data = bonus_training)
bonus_testing_ready <- bake(bonus_prep, new_data = bonus_testing)



# telecom_recipe <- recipe(canceled_service ~ ., data = telecom_training) %>% 
#   # Removed correlated predictors
#   step_corr(all_numeric(), threshold = 0.8) %>% 
#   # Log transform numeric predictors
#   step_log(all_numeric(), base = 10) %>%
#   # Normalize numeric predictors
#   step_normalize(all_numeric()) %>%
#   # Create dummy variables
#   step_dummy(all_nominal, -all_outcomes())


# # Train logistic model
# logistic_fit <- logistic_model %>% 
#   fit(canceled_service ~ ., data = telecom_training_prep)

# # Create a confusion matrix
# telecom_results %>% 
#   conf_mat(truth = canceled_service, estimate = .pred_class)