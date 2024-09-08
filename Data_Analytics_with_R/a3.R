#libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(patchwork)
library(car)

#load and tidy data
titanic_data <- read.csv("data/titanic.csv", na.strings = c("NA", ""))
titanic_data <- titanic_data |>
  mutate(
    Survived = ifelse(Survived == 0, "no", "yes"),
    across(c(Survived, Sex, Pclass), as.factor)
  )

#EDA

plot1 <- ggplot(titanic_data, aes(x = Survived, fill = Pclass)) +
  geom_bar(position = "dodge") +
  theme_bw()

plot2 <- ggplot(titanic_data, aes(x = Survived, fill = Sex)) +
  geom_bar(position = "dodge") +
  theme_bw()

plot3 <- ggplot(titanic_data, aes(x = Survived, y = Fare, fill = Survived)) +
  geom_boxplot() +
  theme_bw()

plot4 <- ggplot(titanic_data, aes(x = Survived, y = Age, fill = Survived)) +
  geom_boxplot() +
  theme_bw()

plot1 + plot2

plot3 + plot4

# Regression Model
model<- glm(Survived ~ Pclass + Sex + Age, data = titanic_data, family = "binomial")
summary(model)


# VIF, AIC and coefficients
model_vif <- vif(model)
vif_data <- as.data.frame(model_vif)

model_aic <- model$aic
model_coefficient <- model$coefficients


ggplot(model_vif, aes(x = rownames(vif_data), y = GVIF^(1/(2*Df)))) +
  geom_bar(stat = "identity", alpha = 0.5) +
  labs(x = "Attributes",
       y = "VIF") +
 theme_bw()

# Predict model
prediction <- predict(model, newdata = titanic_data, type = "response")

#Accuracy
predicted_outcome <- ifelse(prediction > 0.5, "yes", "no")
correct_predictions <- sum(predicted_outcome == titanic_data$Survived, na.rm = TRUE)
total_predictions <- sum(!is.na(predicted_outcome))
accuracy <- correct_predictions/total_predictions
