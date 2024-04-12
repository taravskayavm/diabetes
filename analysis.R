# Исследовательский анализ данных -----------------------------------------------

# Проверка данных на наличие пропущенных значений в датафрейме
any(is.na(diabetes))

# Подсчет количества дубликатов в датафрейме
sum(duplicated(diabetes))


# Группировка данных по полу и классу, и подсчет числа пациентов в каждой группе
patients_count <- diabetes %>%
  group_by(Gender, CLASS) %>%
  summarise(count = n())
patients_count

# Столбчатая диаграмма распределения числа пациентов по полу и классу
ggplot(data = patients_count, aes(x = CLASS, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of patients by gender and class", x = "Class", y = "Number of patients") +
  scale_fill_manual(values = c("#4169E1", "#DB7093"))  + # Настройка цветов
  theme(plot.title = element_text(hjust = 0.5))


# Гистограмма распределения возраста пациентов в зависимости от класса
diabetes %>%
  ggplot(aes(x = AGE, fill = CLASS)) + 
  geom_histogram(bins = 10, color = "black", alpha = 0.5) +
  labs(title = "Распределение возраста пациентов по классам", x = "Возраст", y = "Частота") +
  theme_minimal()



# Корреляции между переменными:
correlation_matrix <- diabetes[, c(4:13)] %>% 
  cor()

# Тепловая карта корреляционной матрицы
ggcorrplot::ggcorrplot(correlation_matrix, 
                       hc.order = TRUE, 
                       type = "upper", 
                       lab = TRUE, 
                       tl.col = "black")
























