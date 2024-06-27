# 1. Исследовательский анализ данных -----------------------------------------------

# Проверка данных на наличие пропущенных значений в датафрейме
any(is.na(diabetes))

# Подсчет количества дубликатов в датафрейме
sum(duplicated(diabetes))

Desc(diabetes)

# Анализ датасета на выбросы --------------------------------------------


# Вычисляем межквартильный размах для показателя Urea
Q1_Urea <- quantile(diabetes$Urea, 0.25)
Q3_Urea <- quantile(diabetes$Urea, 0.75)
IQR_Urea <- Q3_Urea - Q1_Urea

# Определяем границы для выбросов
lower_bound_Urea <- Q1_Urea - 1.5 * IQR_Urea
upper_bound_Urea <- Q3_Urea + 1.5 * IQR_Urea

# Находим выбросы и удаляем строки с ними
outliers_Urea <- diabetes$Urea < lower_bound_Urea | diabetes$Urea > upper_bound_Urea
outliers_Urea_data <- diabetes[outliers_Urea, ]
print(outliers_Urea_data, n = 65)

diabetes <- diabetes %>% 
  filter(Urea != 0.5)


# Вычисляем межквартильный размах для показателя Cr
Q1_Cr <- quantile(diabetes$Cr, 0.25)
Q3_Cr <- quantile(diabetes$Cr, 0.75)
IQR_Cr <- Q3_Cr - Q1_Cr

# Определяем границы для выбросов
lower_bound_Cr <- Q1_Cr - 1.5 * IQR_Cr
upper_bound_Cr <- Q3_Cr + 1.5 * IQR_Cr

# Находим выбросы и удаляем строки с ними
outliers_Cr <- diabetes$Cr < lower_bound_Cr | diabetes$Cr > upper_bound_Cr
outliers_Cr_data <- diabetes[outliers_Cr, ]
print(outliers_Cr_data, n = 52)

diabetes <- diabetes %>%
  filter(Cr != 6)


# Вычисляем межквартильный размах для показателя HbA1c
Q1_HbA1c <- quantile(diabetes$HbA1c, 0.25)
Q3_HbA1c <- quantile(diabetes$HbA1c, 0.75)
IQR_HbA1c <- Q3_HbA1c - Q1_HbA1c

# Определяем границы для выбросов
lower_bound_HbA1c <- Q1_HbA1c - 1.5 * IQR_HbA1c
upper_bound_HbA1c <- Q3_HbA1c + 1.5 * IQR_HbA1c

# Находим выбросы и удаляем строки с ними
outliers_HbA1c <- diabetes$HbA1c < lower_bound_HbA1c | diabetes$HbA1c > upper_bound_HbA1c
outliers_HbA1c_data <- diabetes[outliers_HbA1c, ]
print(outliers_HbA1c_data)

diabetes <- diabetes %>%
  filter(HbA1c != 0.9)


# Вычисляем межквартильный размах для показателя Chol  
Q1_Chol <- quantile(diabetes$Chol, 0.25)
Q3_Chol <- quantile(diabetes$Chol, 0.75)
IQR_Chol <- Q3_Chol - Q1_Chol

# Определяем границы для выбросов
lower_bound_Chol <- Q1_Chol - 1.5 * IQR_Chol
upper_bound_Chol <- Q3_Chol + 1.5 * IQR_Chol

# Находим выбросы и удаляем строки с ними
outliers_Chol <- diabetes$Chol < lower_bound_Chol | diabetes$Chol > upper_bound_Chol
outliers_Chol_data <- diabetes[outliers_Chol, ]
print(outliers_Chol_data, n = 27)

diabetes <- diabetes %>%
  filter(Chol != 0.5 & Chol != 0 & Chol != 0.6 & Chol != 1.2)


# Вычисляем межквартильный размах для показателя TG  
Q1_TG <- quantile(diabetes$TG, 0.25)
Q3_TG <- quantile(diabetes$TG, 0.75)
IQR_TG <- Q3_TG - Q1_TG

# Определяем границы для выбросов
lower_bound_TG <- Q1_TG - 1.5 * IQR_TG
upper_bound_TG <- Q3_TG + 1.5 * IQR_TG

# Находим выбросы
outliers_TG <- diabetes$TG < lower_bound_TG | diabetes$TG > upper_bound_TG
outliers_TG_data <- diabetes[outliers_TG, ]
print(outliers_TG_data, n = 53)


# Вычисляем межквартильный размах для показателя HDL  
Q1_HDL <- quantile(diabetes$HDL, 0.25)
Q3_HDL <- quantile(diabetes$HDL, 0.75)
IQR_HDL <- Q3_HDL - Q1_HDL

# Определяем границы для выбросов
lower_bound_HDL <- Q1_HDL - 1.5 * IQR_HDL
upper_bound_HDL <- Q3_HDL + 1.5 * IQR_HDL

# Находим выбросы
outliers_HDL <- diabetes$HDL < lower_bound_HDL | diabetes$HDL > upper_bound_HDL
outliers_HDL_data <- diabetes[outliers_HDL, ]
print(outliers_HDL_data, n = 50)


# Вычисляем межквартильный размах для показателя LDL  
Q1_LDL <- quantile(diabetes$LDL, 0.25)
Q3_LDL <- quantile(diabetes$LDL, 0.75)
IQR_LDL <- Q3_LDL - Q1_LDL

# Определяем границы для выбросов
lower_bound_LDL <- Q1_LDL - 1.5 * IQR_LDL
upper_bound_LDL <- Q3_LDL + 1.5 * IQR_LDL

# Находим выбросы
outliers_LDL <- diabetes$LDL < lower_bound_LDL | diabetes$LDL > upper_bound_LDL
outliers_LDL_data <- diabetes[outliers_LDL, ]
print(outliers_LDL_data)



# Вычисляем межквартильный размах для показателя VLDL  
Q1_VLDL <- quantile(diabetes$VLDL, 0.25)
Q3_VLDL <- quantile(diabetes$VLDL, 0.75)
IQR_VLDL <- Q3_VLDL - Q1_VLDL

# Определяем границы для выбросов
lower_bound_VLDL <- Q1_VLDL - 1.5 * IQR_VLDL
upper_bound_VLDL <- Q3_VLDL + 1.5 * IQR_VLDL

# Находим выбросы
outliers_VLDL <- diabetes$VLDL < lower_bound_VLDL | diabetes$VLDL > upper_bound_VLDL
outliers_VLDL_data <- diabetes[outliers_VLDL, ]
print(outliers_VLDL_data, n = 74)


# Вычисляем межквартильный размах для показателя BMI  
Q1_BMI <- quantile(diabetes$BMI, 0.25)
Q3_BMI <- quantile(diabetes$BMI, 0.75)
IQR_BMI <- Q3_BMI - Q1_BMI

# Определяем границы для выбросов
lower_bound_BMI <- Q1_BMI - 1.5 * IQR_BMI
upper_bound_BMI <- Q3_BMI + 1.5 * IQR_BMI

# Находим выбросы
outliers_BMI <- diabetes$BMI < lower_bound_BMI | diabetes$BMI > upper_bound_BMI
outliers_BMI_data <- diabetes[outliers_BMI, ]
print(outliers_BMI_data)


# Замена значения в колонке BMI
diabetes <- diabetes %>%
  mutate(BMI = case_when(
    BMI == 3762 ~ 37.62,    
    BMI == 2717 ~ 27.17,    
    BMI == 3265 ~ 32.65,
    BMI == 2836 ~ 28.36,
    BMI == 4775 ~ 47.75,
    BMI == 3765 ~ 37.62,
    BMI == 3516 ~ 35.16,
    BMI == 2733 ~ 27.33,
    BMI == 235 ~ 23.5,
    BMI == 245 ~ 24.5,
    BMI == 225 ~ 22.5,
    BMI == 246 ~ 24.6,
    BMI == 195 ~ 19.5,
    BMI == 372 ~ 37.2,
    BMI == 374 ~ 37.4,
    BMI == 343 ~ 34.3,
    BMI == 283 ~ 28.3,
    BMI == 315 ~ 31.5,
    BMI == 339 ~ 33.9,
    BMI == 314 ~ 31.4,
    BMI == 315 ~ 31.5,
    BMI == 366 ~ 36.6,
    BMI == 301 ~ 30.1,
    BMI == 366 ~ 36.6,
    BMI == 29.36 ~ 34.3,
    BMI == 274 ~ 27.4,
    BMI == 295 ~ 29.5,
    BMI == 346 ~ 34.6,
    BMI == 273 ~ 27.3,
    BMI == 312 ~ 31.2,
    BMI == 405 ~ 40.5,
    BMI == 2117 ~ 21.17,
    BMI == 2936 ~ 29.36,
    BMI == 329 ~ 32.9,
    BMI == 4325 ~ 43.25,
    BMI == 3918 ~ 39.18,
    BMI == 345 ~ 34.5,
    BMI == 336 ~ 33.6,
    BMI == 3405 ~ 34.05,
    BMI == 2775 ~ 27.75,
    BMI == 352 ~ 35.2,
    BMI == 3611 ~ 36.11,
    BMI == 3862 ~ 38.62,
    BMI == 302 ~ 30.2,
    BMI == 2963 ~ 29.63,
    BMI == 256 ~ 25.6,
    TRUE ~ BMI         # Для сохранения остальных значений без изменений
  ))


# Графики -----------------------------------------------------------------

# Группировка данных по полу и классу, и подсчет числа пациентов в каждой группе

patients_count <- diabetes %>%
  group_by(Gender, CLASS) %>%
  summarise(count = n())
patients_count

# Столбчатая диаграмма распределения числа пациентов по полу и классу
ggplot(data = patients_count, aes(x = CLASS, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of the Number of Patients by Gender and Diabetes Class", x = "Class", y = "Number of Patients") +
  scale_fill_manual(values = c("#DB7093", "#4169E1"))  + # Настройка цветов
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray", linetype = "solid"),
        panel.grid.minor = element_blank(), # Оставляем только основную сетку
        panel.grid.major.y = element_line(color = "gray", linetype = "solid")) +
  scale_y_continuous(breaks = seq(0, 500, by = 50))

# Гистограмма распределения возраста пациентов в зависимости от класса
diabetes %>%
  ggplot(aes(x = AGE, fill = CLASS)) + 
  geom_histogram(bins = 10, color = "black", alpha = 0.5) +
  labs(title = "Distribution of Patients' Age by Classes", x = "Age", y = "Number of Patients") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray", linetype = "solid"),
        panel.grid.minor = element_blank(), # Оставляем только основную сетку
        panel.grid.major.y = element_line(color = "gray", linetype = "solid")) +
  scale_y_continuous(breaks = seq(0, 500, by = 50))


min(diabetes$AGE)
max(diabetes$AGE)









