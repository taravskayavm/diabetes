# Описательная статистика -------------------------------------------------

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



# Гистограмма для распределения возраста с разделением по полу и классу
ggplot(data = diabetes, aes(x = AGE)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Age Distribution by Gender and Class", x = "Age", y = "Count")


# Гистограмма для уровня мочевой кислоты с разделением по полу и классу
ggplot(data = diabetes, aes(x = Urea)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightgreen", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Distribution of Urea Level by Gender and Class", x = "Urea", y = "Count")

# Гистограмма для уровня креатинина с разделением по полу и классу
ggplot(data = diabetes, aes(x = Cr)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "lightyellow", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Distribution of Creatinine Level by Gender and Class", x = "Creatinine Level", y = "Count")

# Гистограмма для уровня гликированного гемоглобина с разделением по полу и классу
ggplot(data = diabetes, aes(x = HbA1c)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Distribution of HbA1c by Gender and Class", x = "HbA1c", y = "Count")

# Гистограмма для уровня холестерина с разделением по полу и классу
ggplot(data = diabetes, aes(x = Chol, fill = CLASS)) +
  geom_histogram(binwidth = 10, color = "black", alpha = 0.7, position = "dodge") +
  facet_grid(.~Gender) +
  labs(title = "Distribution of Cholesterol Level by Gender and Class", x = "Cholesterol Level", y = "Count", fill = "Class")
