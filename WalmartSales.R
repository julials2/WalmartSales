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
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Join data
joined_train <- train %>% 
  left_join(features, by = c("Store", "Date")) 

joined_test <- test %>% 
  left_join(features, by = c("Store", "Date"))

#####
## Create the recipe
#####

# walmart_recipe <- recipe(Weekly_Sales ~ ., data = train) %>% 
#   step_mutate(Store = factor(Store), 
#               Dept = factor(Dept), 
#               IsHoliday = factor(IsHoliday)) %>% 
#   step_impute() %>% 
#   step_date(Date, features = c("month", "dow", "mday", "doy", "week", 
#                                "decimal", "quarter", "semester", "year")) %>% 
#   full_join(features) %>% 
#   step_mutate(StoreDept = paste(Store, Dept, sep="_"))
