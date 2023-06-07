
library(tidyverse)
library(caret)
library(ggplot2)

housing_data <- data.frame(
  average_price = c(200000,240000,400000,150000,130000,500000,100000),
  total_units = c(2000, 2100, 2200,400,1300,2300,4400),
  new_units = c(100, 150, 200, 50, 230, 140,350)
)

set.seed(123)
training_rows <- createDataPartition(housing_data$average_price, p = 0.2, list = FALSE)

training_data <- housing_data[training_rows,]

test_data <- housing_data[-training_rows,]


model <- lm(average_price ~ total_units + new_units, data = training_data)
summary(model)

predictions <- model %>% 
  predict(test_data)

postResample(pred = predictions, obs = test_data$average_price)

new_housing_data <- data.frame(
  total_units = c(2000, 2100, 2200),
  new_units = c(100, 150, 200)
)

predicted_prices <- predict(model, newdata = new_housing_data)

new_housing_data$predicted_prices <- predicted_prices


housing_data <- data.frame(
  average_price = c(200000,240000,400000,150000,130000,500000,100000),
  price = c(230000,270000,420000,170000,160000,550000,160000),
  total_units = c(2000, 2100, 2200,400,1300,2300,4400),
  new_units = c(100, 150, 200, 50, 230, 140,350)
)

# Fit a linear regression model to your data
model <- lm(new_units ~ price + total_units +average_price, data = housing_data)

# The coefficient on 'price' is your estimated price elasticity of supply
summary(model)$coefficients[2, 1]
