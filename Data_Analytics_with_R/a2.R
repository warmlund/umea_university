# 0. Libraries
library(tidyverse)
library(corrplot)
library(patchwork)
library(car)

####

# 1. Correlations
data(iris)

correlations <- iris |> 
  select(where(is.numeric)) |> 
  cor(use="complete.obs")
corrplot(correlations, method = "color")

# Text answer to 1:
# The correlations between Petal length/Petal width, 
# Petal length/Sepal length and Petal width/Sepal length, 
# are positive and high at 0.9628654, which means that the variables increases with each other

# The correlations for sepal width is negative with all other variables and most with Petal length and Petal width, 
# which means when the petal length increases the sepal width decreases. 

# The positive correlations makes sense in case that when the flower grows, all parts of the flower should get larger
# That's does not seem to be the case with the negative correlation of the sepal width. What can be an explanation 
# here is that the petals grow between the sepals, and as the petals grow larger
# It needs more space between the sepals and therefore the width of the sepal decreases. 
# But it seems unlikely when you look at the images of the flowers. The placement of the petals in relation to the sepals differs between species.

####

# 2. Plot Sepal.Width against Sepal.Length
plot1 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point()

# Text to answer 2:
# This scatter plot doesn't say much and seem to be a case of Simsons Paradox, but if you group the points by
# species, you can see a small positive increase of the sepal length as sepal width increases.
# So, according to this scatterplot, the correlation between sepal width and length should
# be positive. But in the earlier made correlation it has a small negative value

####

# 3. Fit a linear model using Sepal.Width as predictor and Sepal.Length as response
model1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)

# Text to answer 3:
# The coefficient is -0.223, which suggest that the width decreases with 0.223 as the length increases with 1. 
# This matches with the negative correlation in task 1 but not with the scatter plot in task 2

####

# 4. Setosa correlations
correlations_setosa <- iris |> 
  filter(Species == "setosa") |>
  select(where(is.numeric)) |> 
  cor(use="complete.obs")

corrplot(correlations_setosa, method = "color")

# Text to answer 4:
# All the correlations are positive. They differ from the overall correlation that most that the negative correlation with the sepal width is gone.
# Now there is only one large positive correlation, and that is between sepal width and sepal length.
# This makes much more sense that both the length and the width incresases as the flower grows.

####

# 5. Plot Sepal.Width against Sepal.Length, color by species
plot2 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color=Species)) +
  geom_point()

# Text to answer 5:
# This plot matches the correlation with the setosa flower, 
# as you can see in the plot that you can sense that the coefficient of a linear growth is positive

####

# 6. Fit second model using species and Sepal.Width as predictors and Sepal.Length as response
model2 <- lm(Sepal.Length ~ Sepal.Width + Species, data=iris)

# Text to answer 6:
# The coefficient for Sepal.Width is 0.8036, which makes more sense than the negative coefficient in model1.
# When we added species as a predictor, the coefficient changed from negative to positive.
# This tells us that the sepal length depends on the species of the flower.

####

# 7. Predict the sepal length of a setosa with a sepal width of 3.6 cm
setosa_36 <- data.frame(Sepal.Width = 3.6, Species = "setosa")

prediction <- predict(model2, newdata = setosa_36)

# Text to answer 7:
# The prediction is that when a setosa iris has the sepal width of 3.6, the sepal length should be 5.14
# When comparing this prediction to the iris data frame, it's a bit higher that the observations, which varies between 4.6-5.0

####

# Download the dataset from Canvas an place in current directory
getwd()

# Load the data
diabetes_data <- read_csv("data/a2_diabetes.csv") # Don't change this line!

# Reflect over important variables
# One important variable that can work as a predictor for diabetes is the variable "Glucose", because
# a person is classified as diabetic if a person had a GTT test, which shows more than 200 mg/dl 2 hours after 75 g carbohydrate ingestion.
# According to the paper, the diabetes variable is registered 5 years after the other variables in the data frame.
# The prediction is therefore if the subjects will likely develop diabetes within 5 years. The glucose variable is therefore measured
# before the classification of diabetes. Other variables that may predict the onset of diabetes within 5 years is Pregnancies and DiabetesPedigreeFunction.
# Diabetes can develop during pregnancies and diabetes type 2 can be inherited.

####

# 8. Recode Outcome as a factor
diabetes_data <- diabetes_data |>
  mutate(Outcome = as.factor(Outcome))

####

# 9. Find a good logistic regression model

#EDA 
#Boxplots for visualizing the relationships between Outcome and different variables
# Variable SkinThickness also seems to be significant as a predictor, because subjects
# diagnosed with diabetes within 5 years have larger skin thickness than subjects not diagnosed with diabetes

boxplot1 <- ggplot(diabetes_data, aes(x=Outcome, y=Glucose)) +
  geom_boxplot()

boxplot2 <- ggplot(diabetes_data, aes(x=Outcome, y=SkinThickness)) +
  geom_boxplot()

boxplot3 <- ggplot(diabetes_data, aes(x=Outcome, y=DiabetesPedigreeFunction)) +
  geom_boxplot()

boxplot4 <- ggplot(diabetes_data, aes(x = Outcome, y = Pregnancies)) +
  geom_boxplot()

boxplot1 + boxplot2 + boxplot3 + boxplot4 + plot_layout(guides = "collect")


# logistic regressions

logistic_model1 <- glm(formula = Outcome ~ Glucose, data = diabetes_data, family = "binomial")
summary(logistic_model1) #AIC 508.25

logistic_model2 <- glm(formula = Outcome ~ Glucose + Pregnancies, data = diabetes_data, family = "binomial")
summary(logistic_model2) #AIC 499.05

logistic_model3 <- glm(formula = Outcome ~ Glucose + Pregnancies + DiabetesPedigreeFunction, data = diabetes_data, family = "binomial")
summary(logistic_model3) #AIC 495.88

logistic_model <- glm(formula = Outcome ~ Glucose + Pregnancies + DiabetesPedigreeFunction + SkinThickness, data = diabetes_data, family = "binomial")
summary(logistic_model) #AIC 323.77

# Trying different logistic regression models and adding variables with a significance level below 0.05
# The variables which are related to diabetes are glucose, pregnancies, diabetespredigreefunction and skinthickness.
# Every variables have a positive coefficient and most values seem reasonable, except the coefficient for variables "diabetespedrigreefunciton",
# which has a significantly larger positive coefficient value than the other variables. 

####

# 10. Improve the model to achieve an accuracy of at least 76.5%

#Calculate the predicted probability for all observations
predicted_prob <- predict(logistic_model, newdata=diabetes_data, type = "response")

#Predict the outcome. if predicted probability of an observation is above 0.5, it's set to 1, else 0
predicted_outcome <- ifelse(predicted_prob > 0.5, 1, 0)

#comparing predictions to actual outcomes and matches how many was predicted correctly
correct_predictions <- sum(predicted_outcome == diabetes_data$Outcome, na.rm = TRUE)

#summarize the total correct protections, excluding na values
total_prediction <- sum(!is.na(predicted_outcome))

#calculating the accuracy in percentage
accuracy <- correct_predictions/total_prediction #accuracy at 78.1%, no need for improvement

# When trying the accuracy for the different logistic models above, the accuracy for "diabetespedigreefunction" does not increase the accuracy value
# Even if it was statistically significant, but it did have a much larger coefficient than the other variables and the significance value closest to 0.05
# Therefore that variable does not seem to bring much significance for prediciting the onset of diabetes within 5 years of the tests.
# 
# Why it's not a good idea to use the same data for fitting a model and evaluate the prediction accuracy, can make the prediction too specified and only work well
# for a small part of data or only work well for that specific data set. It can be prone to overfitting, i.e. it works super on the same data
# it was used for predicting, but worse on new sets of data.
