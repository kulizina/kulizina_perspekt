
library(caret)

# Подготовка данных
data <- data.frame(
  sepal.length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4, 4.6, 5, 4.4, 4.9),
  sepal.width = c(3.5, 3, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1),
  petal.length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5),
  petal.width = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1),
  variety = c("Setosa", "Setosa", "Setosa", "Setosa", "Setosa", 
              "Setosa", "Setosa", "Setosa", "Setosa", "Setosa")
)

# Преобразование категориальной переменной в числовой формат
data$variety <- as.factor(data$variety)

# Создание тренировочного и тестового наборов
set.seed(123)
train_index <- createDataPartition(data$variety, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Установка библиотеки neuralnet
library(neuralnet)

# Обучение однослойной нейронной сети с использованием caret
model <- train(variety ~ sepal.length + sepal.width + petal.length + petal.width, 
               data = train_data, 
               method = "neuralnet",
               trControl = trainControl(method = "cv"))

# Прогноз на тестовом наборе
predictions <- predict(model, newdata = test_data)

# Вычисление процента ошибок
error_rate <- mean(predictions != test_data$variety)
cat("Процент ошибок на тестовой выборке: ", error_rate * 100, "%\n")
