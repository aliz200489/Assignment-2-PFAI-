library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

# Load iris dataset
data(iris)
iris <- iris %>% mutate(Species = as.factor(Species))

# Train decision tree model
model <- train(Species ~ ., data = iris, method = "rpart",
               trControl = trainControl(method = "cv", number = 5))

# Create grid for Sepal.Length & Sepal.Width
plot_data <- expand.grid(
  Sepal.Length = seq(min(iris$Sepal.Length), max(iris$Sepal.Length), length.out = 100),
  Sepal.Width = seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length.out = 100)
)

# Fill missing features with mean values (for prediction compatibility)
plot_data$Petal.Length <- mean(iris$Petal.Length)
plot_data$Petal.Width <- mean(iris$Petal.Width)

# Predict Species for each grid point
plot_data$Species <- predict(model, newdata = plot_data)

# Scatter plot with decision boundary
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3) + 
  geom_tile(data = plot_data, aes(fill = Species, alpha = 0.3), color = NA) +
  scale_alpha(guide = "none") +
  labs(title = "Decision Tree Classification Boundaries",
       x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()

