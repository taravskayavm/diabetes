# 3. Корреляции между переменными:
correlation_matrix <- diabetes[, c(4:13)] %>% 
  cor()

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(correlation_matrix)
head(p.mat)

# Тепловая карта корреляционной матрицы
ggcorrplot::ggcorrplot(correlation_matrix, 
                       hc.order = TRUE, 
                       type = "upper", 
                       lab = TRUE, 
                       p.mat = p.mat,
                       tl.col = "black")


