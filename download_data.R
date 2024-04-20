# Получение данных с гитхаба и предобработка данных----------------------------------------------

# Считываем данные

data <- read_csv("https://raw.githubusercontent.com/taravskayavm/diabetes/main/data/diabetes.xlsx_Sheet_1.csv")

# Конвертируем их в tibble, меняем запятую на точку в дробных числах, заменяем пустые значения на NA

diabetes <- as_tibble(data) %>%
  mutate(across(c(Urea, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI), 
                ~as.numeric(gsub(",", ".", .)))
  )

# Просмотр уникальных значений для предобработки данных
unique(diabetes$Gender)
unique(diabetes$CLASS)

# Заменяем неоднородные значения и убираем пробелы
diabetes$Gender <- toupper(diabetes$Gender)
unique(diabetes$Gender)

# Выводим структуру данных
str(diabetes)

View(diabetes)

















