library(tidyverse)
library(tidymodels)
library(vroom)

train <- vroom("train.csv") %>% 
  mutate(Store = factor(Store), 
         Dept = factor(Dept), 
         IsHoliday = factor(IsHoliday))

test <- vroom("test.csv")%>% 
  mutate(Store = factor(Store), 
         Dept = factor(Dept), 
         StoreDept = paste(Store,Dept,sep="_"))

features <- vroom("features.csv") %>% 
  mutate(Store = factor(Store), 
         IsHoliday = factor(IsHoliday))

## Join data
joined_features <- train %>% 
  left_join(features, by = c("Store", "Date", "IsHoliday")) %>%
  mutate(StoreDept=paste(Store,Dept,sep="_"))

joined_features %>% pull(StoreDept) %>%
  unique()

setdiff(joined_features$StoreDept, test$StoreDept)

## Check missing data
joined_features %>% 
  na.omit() %>% 
  nrow()

nrow(joined_features)

## graphics
ggplot(train, aes(x = IsHoliday, y = Weekly_Sales)) +
  geom_boxplot()
