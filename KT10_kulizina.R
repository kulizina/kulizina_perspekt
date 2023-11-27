install.packages("caret")
install.packages("class")
install.packages("ggplot2")

# Подключение библиотек
library(caret)
library(class)
library(ggplot2)

# Загрузка данных
setwd("/Users/kulizina/Downloads")
iris_data <- read_csv("iris.csv")
View(iris)

# Разделение данных на обучающий и тестовый наборы
set.seed(123)
train_index <- createDataPartition(iris_data$variety, p = 0.8, list = FALSE)
train_data <- iris_data[train_index, ]
test_data <- iris_data[-train_index, ]

# Обучение модели KNN
k <- 3
model_knn <- knn(train = train_data[, 1:4], test = test_data[, 1:4], cl = train_data$variety, k = k)

# Оценка точности модели
accuracy <- sum(model_knn == test_data$variety) / length(test_data$variety)
cat("Accuracy:", accuracy, "\n")

# Визуализация данных
ggplot(iris_data, aes(x = sepal.length, y = sepal.width, color = variety)) +
  geom_point() +
  labs(title = "Scatter Plot of Iris Data", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()


library(mailR) # only sending function
send.mail(from = "kulizinam@yandex.ru",
          to = c("kulizinam@yandex.ru"),
          replyTo = c("Reply to someone else <kulizinam@yandex.ru>"),
          subject = "результаты классификации KNN",
          body = paste("Точность модели KNN:", accuracy),
          smtp = list(host.name = "smtp.yandex.ru", port = 465, user.name = "name@yandex.ru", passwd = rstudioapi::askForPassword(), ssl = TRUE),
          authenticate = TRUE,
          send = TRUE, 
          attach.files = c("./fraud.txt")) # file must be in working dir
