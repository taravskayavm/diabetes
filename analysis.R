#install.packages(c("remotes", "tidyverse", "psych", "plotly", "rmarkdown", "skimr"))
#if (!requireNamespace('BiocManager', quietly = TRUE))
#  install.packages('BiocManager')
#BiocManager::install('PCAtools')
#install.packages("httr")
#install.packages("ggplot2")
#library(ggplot2)
#library(remotes)
#library(tidyverse)
#library(psych)
#library(plotly)
#library(rmarkdown)
#library(skimr)
#library(BiocManager)
#library(httr)

url <- "https://raw.githubusercontent.com/taravskayavm/diabetes/main/diabetes.xlsx%20-%20Sheet%201.csv"

# Отправка GET-запроса и сохранение файла локально
response <- GET(url)
destination <- "data.csv"
writeBin(content(response, "raw"), destination)

# Прочтение файла с помощью read.csv
data <- read.csv("data.csv")

View(data)
