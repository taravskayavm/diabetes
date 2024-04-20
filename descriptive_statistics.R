# 2. Описательная статистика -------------------------------------------------

str(diabetes)

# Определение функции для вычисления моды
Mode <- function(x) {
  uniqx <- unique(x)
  return(uniqx[which.max(tabulate(match(x, uniqx)))])
}


# Расчет статистик для каждого числового столбца по классу и полу
class_gender_stats <- diabetes %>%
  group_by(CLASS, Gender) %>%
  summarise(
    across(c(AGE, Urea, Cr, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI), 
           list(mean = mean, median = median, mode = ~Mode(.), min = min, max = max))
  )
str(class_gender_stats)
View(class_gender_stats)
write.csv(class_gender_stats, "class_gender_stats.csv", row.names = FALSE)



# Расчет статистик для всех данных
overall_stats <- summarise(diabetes, 
                           across(c(AGE, Urea, Cr, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI), 
                                  list(mean = mean, median = median, mode = ~Mode(.), min = min, max = max))
)
str(overall_stats)
View(overall_stats)
write.csv(overall_stats, "overall_stats.csv", row.names = FALSE)

# Вычисление описательных статистик для всего датасета
overall_stats <- summarise(diabetes, 
                           across(c(AGE, Urea, Cr, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI), 
                                  list(mean = mean, median = median, mode = ~Mode(.), min = min, max = max))
)



# Гистограммы распределения числовых переменных по классу и полу --------


# Гистограмма распределения возраста с разделением по полу и классу
ggplot(data = diabetes, aes(x = AGE)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Распределение возраста по полу и классу", x = "Возраст", y = "Число пациентов")


# Гистограмма распределения уровня мочевой кислоты с разделением по полу и классу
ggplot(data = diabetes, aes(x = Urea)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightgreen", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Urea Distribution by Gender and Class", x = "Urea", y = "Count")


# Гистограмма распределения уровня креатинина с разделением по полу и классу
ggplot(data = diabetes, aes(x = Cr)) +
  geom_histogram(binwidth = 16, color = "black", fill = "lightyellow", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Creatinine Distribution by Gender and Class", x = "Cr", y = "Count")


# Гистограмма распределения уровня гликированного гемоглобина с разделением по полу и классу
ggplot(data = diabetes, aes(x = HbA1c)) +
  geom_histogram(binwidth = 1, color = "black", fill = "pink", alpha = 0.7) +
  facet_grid(Gender ~ CLASS) +
  labs(title = "Distribution of HbA1c by Gender and Class", x = "HbA1c", y = "Count")


# Гистограмма распределения уровня холестерина с разделением по полу и классу
ggplot(data = diabetes, aes(x = Chol)) +
  geom_histogram(binwidth = 1, color = "black", fill = "green", alpha = 0.7) +
  facet_grid(Gender ~ CLASS) +
  labs(title = "Distribution of Cholesterol by Gender and Class", x = "Cholesterol", y = "Count")


# Гистограмма для уровня триглицеридов с разделением по полу и классу
ggplot(data = diabetes, aes(x = TG)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  facet_grid(Gender ~ CLASS) +
  labs(title = "Distribution of Triglycerides by Gender and Class", x = "Triglycerides", y = "Count")


# Гистограмма для уровня HDL с разделением по полу и классу
ggplot(data = diabetes, aes(x = HDL)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple", alpha = 0.7) +
  facet_grid(Gender ~ CLASS) +
  labs(title = "Distribution of HDL by Gender and Class", x = "HDL", y = "Count")


# Гистограмма для уровня LDL с разделением по полу и классу
ggplot(data = diabetes, aes(x = LDL)) +
  geom_histogram(binwidth = 1, color = "black", fill = "yellow", alpha = 0.7) +
  facet_grid(Gender ~ CLASS) +
  labs(title = "Distribution of LDL by Gender and Class", x = "LDL", y = "Count")


# Гистограмма для уровня VLDL с разделением по полу и классу
ggplot(data = diabetes, aes(x = VLDL)) +
  geom_histogram(binwidth = 1, color = "black", fill = "red", alpha = 0.7) +
  facet_grid(Gender ~ CLASS) +
  labs(title = "Distribution of VLDL by Gender and Class", x = "VLDL", y = "Count")


# Гистограмма для уровня BMI с разделением по полу и классу
ggplot(data = diabetes, aes(x = BMI)) +
  geom_histogram(binwidth = 2, color = "black", fill = "orange", alpha = 0.7) +
  facet_grid(Gender ~ CLASS) +
  labs(title = "Distribution of BMI by Gender and Class", x = "BMI", y = "Count")



# Выводы ------------------------------------------------------------------


# 1. Возраст (AGE):
  
# Средний возраст и медиана у пациентов с диабетом класса Y выше, чем у пациентов с классами N и P.
# Мода (наиболее часто встречающееся значение) возраста для пациентов женского пола с классом P составляет 49 лет.

# Создание подмножества данных среднего возраста по классам диабета и полу
mean_age <- aggregate(diabetes$AGE, by = list(diabetes$CLASS, diabetes$Gender), FUN = mean)
colnames(mean_age) <- c("CLASS", "Gender", "Mean_Age")

# График для отображения среднего возраста по классам диабета и полу
ggplot(mean_age, aes(x = CLASS, y = Mean_Age, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Средний возраст пациентов в зависимости от класса и пола",
       x = "Класс диабета", y = "Средний возраст") +
  scale_fill_manual(values = c("pink", "blue")) +
  theme_minimal()



# 2. Мочевина (Urea):

# Создание boxplot для уровня мочевины (Urea) с разделением по классу и полу
ggplot(data = diabetes, aes(x = CLASS, y = Urea, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень мочевины по классу диабета и полу", x = "Класс диабета", y = "Уровень мочевины") +
  scale_fill_manual(values = c("pink", "lightblue"))  # Настройка цветов для разных полов



# 3. Креатинин (Cr)

# Создание boxplot для уровня креатинина (Cr) с разделением по классу и полу
ggplot(data = diabetes, aes(x = CLASS, y = Cr, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень креатинина по классу диабета и полу", x = "Класс диабета", y = "Уровень креатинина") +
  scale_fill_manual(values = c("pink", "lightblue"))  # Настройка цветов для разных полов



# 4. Гликированный гемоглобин

# Создание boxplot для уровня гликированного гемоглобина (HbA1c) с разделением по классу и полу
ggplot(data = diabetes, aes(x = CLASS, y = HbA1c, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень гликированного гемоглобина по классу диабета и полу", x = "Класс диабета", y = "Уровень HbA1c") +
  scale_fill_manual(values = c("pink", "lightblue"))  # Настройка цветов для разных полов


# 5. Холестерин (Chol)

# Создание boxplot для уровня общего холестерина (Chol) с разделением по классу и полу
ggplot(data = diabetes, aes(x = CLASS, y = Chol, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень общего холестерина по классу диабета и полу", x = "Класс диабета", y = "Уровень общего холестерина") +
  scale_fill_manual(values = c("pink", "lightblue"))  # Настройка цветов для разных полов



# 6. Различия в средних значениях триглицеридов (TG), холестерина ХСЛ (HDL), ЛПНП (LDL), ЛПОНП (VLDL) и ИМТ (BMI)

# Создание boxplot для выбранных параметров с разделением по классу и полу
ggplot(data = diabetes, aes(x = CLASS, y = TG, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень триглицеридов по классу диабета и полу", x = "Класс диабета", y = "Уровень триглицеридов") +
  scale_fill_manual(values = c("pink", "lightblue"))


ggplot(data = diabetes, aes(x = CLASS, y = HDL, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень холестерина HDL по классу диабета и полу", x = "Класс диабета", y = "Уровень HDL") +
  scale_fill_manual(values = c("pink", "lightblue"))


ggplot(data = diabetes, aes(x = CLASS, y = LDL, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень холестерина LDL по классу диабета и полу", x = "Класс диабета", y = "Уровень LDL") +
  scale_fill_manual(values = c("pink", "lightblue"))


ggplot(data = diabetes, aes(x = CLASS, y = VLDL, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Уровень холестерина VLDL по классу диабета и полу", x = "Класс диабета", y = "Уровень VLDL") +
  scale_fill_manual(values = c("pink", "lightblue"))


ggplot(data = diabetes, aes(x = CLASS, y = BMI, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Индекс массы тела по классу диабета и полу", x = "Класс диабета", y = "ИМТ") +
  scale_fill_manual(values = c("pink", "lightblue"))



# Группировка графиков ----------------------------------------------------

# Создание графиков для каждого параметра
plot1 <- ggplot(data = diabetes, aes(x = CLASS, y = Urea, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Класс диабета", y = "Уровень мочевины") +
  scale_fill_manual(values = c("pink", "lightblue"))

plot2 <- ggplot(data = diabetes, aes(x = CLASS, y = Cr, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Класс диабета", y = "Уровень креатинина") +
  scale_fill_manual(values = c("pink", "lightblue"))

plot3 <- ggplot(data = diabetes, aes(x = CLASS, y = HbA1c, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Класс диабета", y = "Уровень HbA1c") +
  scale_fill_manual(values = c("pink", "lightblue"))

plot4 <- ggplot(data = diabetes, aes(x = CLASS, y = AGE, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Класс диабета", y = "Возраст") +
  scale_fill_manual(values = c("pink", "lightblue"))

plot5 <- ggplot(data = diabetes, aes(x = CLASS, y = Chol, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Класс диабета", y = "Уровень холестерина") +
  scale_fill_manual(values = c("pink", "lightblue"))

plot6 <- ggplot(data = diabetes, aes(x = CLASS, y = BMI, fill = Gender)) +
  geom_boxplot() +
  labs(x = "Класс диабета", y = "Индекс массы тела") +
  scale_fill_manual(values = c("pink", "lightblue"))

# Собираем все графики на одном изображении
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)

