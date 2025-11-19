library(tidyverse)
library(tidymodels)
library(vroom)

train <- vroom("train.csv") 

test <- vroom("test.csv")

features <- vroom("features.csv") %>% 
  mutate(across(c(MarkDown1:MarkDown5), ~replace(., is.na(.), 0))) %>%
  mutate(across(c(MarkDown1:MarkDown5), ~replace(., . < 0, 0))) %>% 
  mutate(TotalMarkdown = MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5, 
         MarkdownFlag = ifelse(TotalMarkdown != 0, 1, 0)) %>% 
  mutate(SuperBowl = ifelse(Date %in% c(dmy("12-2-10"), dmy("11-2-11"), dmy("10-2-12"), dmy("8-2-13")), 
                            1, 0), 
         LaborDay = ifelse(Date %in% c(dmy("10-9-10"), dmy("9-9-11"), dmy("7-9-12"), dmy("6-9-13")), 
                           1, 0), 
         Thanksgiving = ifelse(Date %in% c(dmy("26-11-10"), dmy("25-11-11"), dmy("23-11-12"), dmy("29-11-13")), 
                               1, 0), 
         Christmas = ifelse(Date %in% c(dmy("31-12-10"), dmy("30-12-11"), dmy("28-12-12"), dmy("27-12-13")), 
                            1, 0)) %>% 
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5, -IsHoliday)

#####
## Feature recipe
#####
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))

#####
## Join data
#####
joined_train <- train %>% 
  left_join(imputed_features, by = c("Store", "Date")) %>% 
  mutate(StoreDept = paste(Store, Dept, sep="_")) %>% 
  select(-IsHoliday)

joined_test <- test %>% 
  left_join(imputed_features, by = c("Store", "Date")) %>% 
  mutate(StoreDept = paste(Store, Dept, sep="_")) %>% 
  select(-IsHoliday)

rand_stores <- sample(joined_train$StoreDept, size = 3)

train_1 <- joined_train %>% 
  filter(StoreDept == rand_stores[1])

test_1 <- joined_test %>% 
  filter(StoreDept == rand_stores[1])

train_2 <- joined_train %>% 
  filter(StoreDept == rand_stores[2])

test_2 <- joined_test %>% 
  filter(StoreDept == rand_stores[2])

train_3 <- joined_train %>% 
  filter(StoreDept == rand_stores[3])

test_3 <- joined_test %>% 
  filter(StoreDept == rand_stores[3])

#####
## Create the recipe
#####

walmart_recipe <- recipe(Weekly_Sales ~ ., data = train_3) %>%
  step_date(Date, features = "doy") %>%
  step_range(Date_doy, min = 0, max = pi) %>% 
  step_mutate(sinDOY = sin(Date_doy), cosDOY = cos(Date_doy), 
              dec_date = decimal_date(date(Date))) %>% 
  
  step_rm(Date, Store, Dept, StoreDept)

prepped <- prep(walmart_recipe)
baked <- bake(prepped, new_data = train_3)

#####
## Create models
#####

## Random forest
forest_mod <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("regression")

## k-nearest neighbors
knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("regression") %>%
  set_engine("kknn")

#####
## Workflow
#####

## Random forest 
forest_workflow <- workflow() %>%
  add_recipe(walmart_recipe) %>%
  add_model(forest_mod)

## k-nearest neighbors
knn_workflow <- workflow() %>%
  add_recipe(walmart_recipe) %>%
  add_model(knn_model)

#####
## CV
#####

## Grid for random forest
tuning_grid <- grid_regular(mtry(range = c(1, 9)),
                            min_n(), 
                            levels = 5)

## Grid for knn
tuning_grid <- grid_regular(neighbors(),
                            levels = 5)


## Split data
folds <- vfold_cv(train_3, v = 5, repeats = 1)

## Run CV 
CV_results <- tune_grid(
  forest_workflow,
  resamples = folds,
  grid = tuning_grid,
  metrics = metric_set(rmse))

show_best(CV_results, metric = "rmse")




