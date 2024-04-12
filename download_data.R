# Получение данных с гитхаба и предобработка данных----------------------------------------------

# Считываем данные, конвертируем их в tibble, меняем запятую на точку в дробных числах, заменяем пустые значения на NA

diabetes <- fread("https://raw.githubusercontent.com/taravskayavm/diabetes/main/data/diabetes.xlsx_Sheet_1.csv") %>%
  as_tibble() %>%
  mutate_at(vars(Urea, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI), 
            ~as.numeric(gsub(",", ".", .))) %>%
  mutate(Age_group = factor(case_when(AGE < 20 ~ "< 20",
                                      AGE >= 20 & AGE < 30 ~ "20-29",
                                      AGE >= 30 & AGE < 40 ~ "30-39",
                                      AGE >= 40 & AGE < 50 ~ "40-49",
                                      AGE >= 50 & AGE < 60 ~ "50-59",
                                      AGE >= 60 & AGE < 70 ~ "60-69",
                                      AGE >= 70 & AGE < 80 ~ "70-79",
                                      AGE > 80 ~ "> 80")))

# Просмотр уникальных значений для предобработки данных
unique(diabetes$Gender)
unique(diabetes$CLASS)

# Заменяем неоднородные значения и убираем пробелы
diabetes$Gender <- toupper(diabetes$Gender)
unique(diabetes$Gender)

# Выводим структуру данных
str(diabetes)


















