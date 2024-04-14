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


# Гистограмма распределения возраста с разделением по полу и классу
ggplot(data = diabetes, aes(x = AGE)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Age Distribution by Gender and Class", x = "Age", y = "Count")

# Гистограмма распределения уровня мочевой кислоты с разделением по полу и классу
ggplot(data = diabetes, aes(x = Urea)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightgreen", alpha = 0.7) +
  facet_grid(CLASS~Gender, scales = "free") +
  labs(title = "Urea Distribution by Gender and Class", x = "Urea", y = "Count")


# Гистограмма распределения уровня креатинина с разделением по полу и классу
ggplot(data = diabetes, aes(x = Cr)) +
  geom_histogram(binwidth = 15, color = "black", fill = "lightyellow", alpha = 0.7) +
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
