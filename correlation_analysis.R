# 3. Корреляции между переменными:
correlation_matrix <- diabetes[, c(4:13)] %>% 
  cor()

# Тепловая карта корреляционной матрицы
ggcorrplot::ggcorrplot(correlation_matrix, 
                       hc.order = TRUE, 
                       type = "upper", 
                       lab = TRUE, 
                       tl.col = "black")


