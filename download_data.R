# Получение данных с гитхаба ----------------------------------------------
url <- "https://raw.githubusercontent.com/taravskayavm/diabetes/main/diabetes.xlsx%20-%20Sheet%201.csv"

# Отправка GET-запроса и сохранение файла локально
response <- GET(url)
destination <- "data.csv"
writeBin(content(response, "raw"), destination)

# Прочтение файла с помощью read.csv
data <- read.csv("data.csv")
