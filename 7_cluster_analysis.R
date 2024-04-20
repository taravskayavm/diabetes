
# 7. Кластерный анализ -------------------------------------------------------

# Выбираем переменные для кластерного анализа
data_for_clustering <- diabetes[, c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")]

# Проведение кластерного анализа с помощью метода k-средних
k <- 3  # количество кластеров
kmeans_model <- kmeans(scale(data_for_clustering), centers = k)

# Визуализация результатов кластеризации
fviz_cluster(kmeans_model, geom = "point", data = data_for_clustering) +
  ggtitle("Кластерный анализ пациентов с диабетом") +
  theme_minimal()

