data <- read.csv('test.csv')
#статистика по датасету
summary(data)
# названия колонок
str(data)
#обращение к столбцу
a <- data$score
# количество строк
nrow(data)


#library(help = "datasets") наборы данных
#data(name) название набора данных(подгрузка данных)
#help(name) справка по данным
#x<-name загрузка сета в переменную

my_data <- mtcars
mtcars$even_gear <- (mtcars$gear+1) %% 2
mpg_4 <- my_data$mpg[my_data$cyl==4]

#выборка из значений
mini_mtcars <- my_data[c(3,7,10,12,nrow(my_data)),]
