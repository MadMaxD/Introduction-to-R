data <- read.csv('test.csv')
#���������� �� ��������
summary(data)
# �������� �������
str(data)
#��������� � �������
a <- data$score
# ���������� �����
nrow(data)


#library(help = "datasets") ������ ������
#data(name) �������� ������ ������(��������� ������)
#help(name) ������� �� ������
#x<-name �������� ���� � ����������

my_data <- mtcars
mtcars$even_gear <- (mtcars$gear+1) %% 2
mpg_4 <- my_data$mpg[my_data$cyl==4]

#������� �� ��������
mini_mtcars <- my_data[c(3,7,10,12,nrow(my_data)),]
