# Определение базового класса "Автомобиль"
Car <- list(
  drive = function() {
    cat("I'm driving\n")
  },
  honk = function() {
    cat("biiip-biiip!!\n")
  }
)

# Создание дочерних классов
DieselCar <- list(
  parent = Car,
  fuel_type = "солярке",
  additional_method = function() {
    cat("Этот метод принадлежит дизельному автомобилю и едет на солярке\n")
  }
)

GasolineCar <- list(
  parent = Car,
  fuel_type = "бензине",
  additional_method = function() {
    cat("Этот метод принадлежит бензиновому автомобилю и едет на бензине\n")
  }
)

ElectricCar <- list(
  parent = Car,
  fuel_type = "электричестве",
  additional_method = function() {
    cat("Этот метод принадлежит электромобилю и едет на электричестве\n")
  }
)

# Функция для создания и вывода методов выбранного класса
create_and_print_car <- function(class_name) {
  car <- switch(
    class_name,
    "Дизельный автомобиль" = DieselCar,
    "Бензиновый автомобиль" = GasolineCar,
    "Электромобиль" = ElectricCar
  )
  
  if (is.null(car)) {
    cat("Неправильный выбор\n")
    return(NULL)
  }
  
  cat("Методы родительского класса и собственные методы:\n")
  car$parent$drive()
  car$parent$honk()
  car$additional_method()
}

# Вызов функции для взаимодействия с пользователем
user_choice <- readline("Выберите класс автомобилей (Дизельный автомобиль, Бензиновый автомобиль, Электромобиль): ")
create_and_print_car(user_choice)
