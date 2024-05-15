
# 5. Группировка и сравнение -------------------------------------------------

# Создаем кросс-таблицу между Class и Gender
cross_table_Class_Gender <- table(diabetes$CLASS, diabetes$Gender)

# Проведение теста Хи-квадрат
chi_sq_test <- chisq.test(cross_table_Class_Gender)

# Вывод результатов
print(chi_sq_test)




# Вывод -------------------------------------------------------------------

# Значение X-squared (X²) = 16.043: Это значение показывает степень отклонения фактических наблюдаемых частот от ожидаемых частот в кросс-таблице.
# Число степеней свободы df = 2: Это количество независимых категорий данных минус 1.
# Значение p-value = 0.0003284: Очень низкое p-значение, которое меньше уровня значимости 0.05, что говорит о статистической значимости результатов теста.
# Таким образом, на основании результатов теста Хи-квадрат можно сделать вывод, что существует статистически значимая связь между переменными Class и Gender в вашем датасете. То есть есть основания предполагать, что класс диабета зависит от пола пациентов.



# Сравнение показателей между классами Y и P ---------------------

# Двухвыборочный независимый t-тест для оценки различий показателей AGE и Класса
t_test_result <- t.test(AGE ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной AGE между группами P и Y
u_test_result <- wilcox.test(AGE ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей Urea и Класса
t_test_result <- t.test(Urea ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной Urea между группами P и Y
u_test_result <- wilcox.test(Urea ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей Cr и Класса
t_test_result <- t.test(Cr ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной Cr между группами P и Y
u_test_result <- wilcox.test(Cr ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей HbA1c и Класса
t_test_result <- t.test(HbA1c ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной HbA1c между группами P и Y
u_test_result <- wilcox.test(HbA1c ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей Chol и Класса
t_test_result <- t.test(Chol ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной Chol между группами P и Y
u_test_result <- wilcox.test(Chol ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей TG и Класса
t_test_result <- t.test(TG ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной TG между группами P и Y
u_test_result <- wilcox.test(TG ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей HDL и Класса
t_test_result <- t.test(HDL ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной HDL между группами P и Y
u_test_result <- wilcox.test(HDL ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей LDL и Класса
t_test_result <- t.test(LDL ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной LDL между группами P и Y
u_test_result <- wilcox.test(LDL ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей VLDL и Класса
t_test_result <- t.test(VLDL ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной VLDL между группами P и Y
u_test_result <- wilcox.test(VLDL ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей BMI и Класса
t_test_result <- t.test(BMI ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(t_test_result)
# Выполнение U-теста для переменной BMI между группами P и Y
u_test_result <- wilcox.test(BMI ~ factor(CLASS), data = diabetes, subset = CLASS != "N")
print(u_test_result)




# Сравнение показателей между классами Y и N ---------------------

# Двухвыборочный независимый t-тест для оценки различий показателей AGE и Класса
t_test_result <- t.test(AGE ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной AGE между группами Y и N
u_test_result <- wilcox.test(AGE ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей Urea и Класса
t_test_result <- t.test(Urea ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной Urea между группами Y и N
u_test_result <- wilcox.test(Urea ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей Cr и Класса
t_test_result <- t.test(Cr ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной Cr между группами Y и N
u_test_result <- wilcox.test(Cr ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей HbA1c и Класса
t_test_result <- t.test(HbA1c ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной HbA1c между группами Y и N
u_test_result <- wilcox.test(HbA1c ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей Chol и Класса
t_test_result <- t.test(Chol ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной Chol между группами Y и N
u_test_result <- wilcox.test(Chol ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей TG и Класса
t_test_result <- t.test(TG ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной TG между группами Y и N
u_test_result <- wilcox.test(TG ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей HDL и Класса
t_test_result <- t.test(HDL ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной HDL между группами Y и N
u_test_result <- wilcox.test(HDL ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей LDL и Класса
t_test_result <- t.test(LDL ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной LDL между группами Y и N
u_test_result <- wilcox.test(LDL ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей VLDL и Класса
t_test_result <- t.test(VLDL ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной VLDL между группами Y и N
u_test_result <- wilcox.test(VLDL ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)

# Двухвыборочный независимый t-тест для оценки различий показателей BMI и Класса
t_test_result <- t.test(BMI ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(t_test_result)
# Выполнение U-теста для переменной BMI между группами Y и N
u_test_result <- wilcox.test(BMI ~ factor(CLASS), data = diabetes, subset = CLASS != "P")
print(u_test_result)


# ANOVA -------------------------------------------------------------------
# Создание списка для результатов ANOVA
results <- list()

# Перебор каждой колонки и выполнение ANOVA теста
for(col_name in c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")) {
  # Формирование формулы для каждой колонки
  formula_str <- paste(col_name, "~ CLASS")
  formula_obj <- as.formula(formula_str)
  
  # Выполнение ANOVA теста
  anova_result <- aov(formula = formula_obj, data = diabetes)
  
  # Сохранение результатов
  results[[col_name]] <- summary(anova_result)
}

# Вывод результатов
results

# Интерпретация: ----------------------------------------------------------
# Результаты ANOVA теста показывают статистическую значимость различий между группами CLASS для переменных AGE, HbA1c, Chol, TG, VLDL и BMI, так как p-value < 0.05 (уровень значимости). 
# Это означает, что есть статистически значимые различия в этих переменных между разными классами. 
# Для остальных переменных (Urea, Cr, HDL, LDL) различия не являются статистически значимыми.
