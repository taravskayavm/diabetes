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



# Объединение данных по полю CLASS из данных класса с данными из общего датасета по полю ID
merged_data <- merge(overall_stats, class_gender_stats, by.x = "ID", by.y = "CLASS", suffixes = c("_overall", "_class"), all.y = TRUE)

# Вывод результатов
print(merged_data)