library(tidyverse) 
library(data.table)
library(recipes)
library(skimr)
library(inspectdf)
library(graphics)
library(Hmisc)
library(glue)
library(highcharter)
library(plotly)
library(h2o)  
library(dplyr)


df <- ggplot2::mpg
df %>% skim()

h2o.init()

h2o_data <- as.h2o(df)

h2o_data <- h2o.splitFrame(h2o_data,ratios = c(0.7,0.15),seed=123)
train<-h2o_data[[1]]
validation<-h2o_data[[2]]
test<-h2o_data[[3]]

outcome <- 'cty'
features <- df %>% select(-model, -manufacturer,-drv,-hwy, -class,-fl,-cty,-trans) %>% names()

model <- h2o.automl(
  x = features,
  y = outcome,
  training_frame    = train,
  validation_frame  = validation,
  leaderboard_frame = test,
  stopping_metric = "RMSE",
  seed = 123,
  max_runtime_secs = 360)


y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred


# Model evaluation ----
test_set <- test %>% as.data.frame()
residuals = test_set$cty - y_pred$predict

#Calculate Root Mean Square Error
RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test_set$cty)

#Calculate total sum of squares
tss = sum((test_set$cty - y_test_mean)^2)

#Calculate residual sum of squares
rss = sum(residuals^2)

#Calculate R-squared
R2 = 1 - (rss/tss)
R2

#Calculate Adjusted R-squared
n <- 234 #sample size
k <- 3 #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))
Adjusted_R2 <- paste0(round(Adjusted_R2*100, 1),"%")

tibble(RMSE = round(RMSE),
       Adjusted_R2)
