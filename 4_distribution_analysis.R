
# 4. Анализ распределения ----------------------------------------------------

# Числовые переменные для анализа
numeric_variables <- c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")

# Создание списка для хранения графиков
qq_plots_list <- lapply(numeric_variables, function(variable) {
  shapiro_test <- shapiro.test(diabetes[[variable]])
  
  cat("Шапиро-Уилк тест для переменной", variable, ":\n")
  print(shapiro_test)
  
  qq_plot <- ggplot(data = data.frame(x = diabetes[[variable]]), aes(sample = x)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste("QQ plot for", variable))
  
  return(qq_plot)
})

# Вывод всех QQ-графиков с помощью grid.arrange
do.call(grid.arrange, qq_plots_list)



# Выводы ------------------------------------------------------------------

# Для переменных Urea, Cr, TG, HDL, VLDL, AGE, Chol, LDL, результаты теста показывают, что данные из этих переменных не следуют нормальному распределению.
# Для переменных BMI и HbA1c  результаты говорят о том, что хотя некоторая вариабельность присутствует, данные также отклонены от нормальности.

# Для переменных с не-нормальным распределением можно использовать непараметрические методы анализа данных или преобразования данных для более точной работы с ними.