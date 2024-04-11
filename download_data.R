# Получение данных с гитхаба ----------------------------------------------
data <- read.csv("https://raw.githubusercontent.com/taravskayavm/diabetes/main/data/diabetes.xlsx_Sheet_1.csv",
                 na.strings = c("-", ""))

# Замена запятых на точки в дробных числах
diabetes <- data %>%
  mutate_at(vars(Urea, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI), ~as.numeric(gsub(",", ".", .)))

# Проверка на наличие пропущенных значений во всем датафрейме
any(is.na(diabetes))

# Подсчет количества дубликатов во всем датафрейме
sum(duplicated(diabetes))

str(diabetes)














