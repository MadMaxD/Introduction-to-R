data <- mtcars
#Load dataset

data$vs <- factor(data$vs,labels = c("V","S"))
data$am <- factor(data$vs,labels = c("Auto","Meh"))
#Replace T and F on values from vector

median(data$mpg)
range(data$cyl)
mean(mtcars$qsec[mtcars$cyl!=3 & data$mpg>20])
#Statistic function

mean_hp_vs <- aggregate(x=data$hp,by=list(data$vs),FUN=mean)
colnames(mean_hp_vs) <- c("VS","Mean HP")
#X- vector values, Group by, FUN - Mapping on every group

aggregate(hp~vs,data,mean)
aggregate(hp~vs+am,data,mean)
# Second way 

aggregate(x=data[,-c(8,9)],by=list(data$am),FUN=median)
#Only num values

aggregate(cbind(mpg,disp)~am+vs,data,sd)
# Select few colouns

descriptions_stat <- aggregate(cbind(hp,disp)~am,data,FUN=sd)

library(psych)
describe(x=data)
describeBy(x=data[,-c(8,9)],group=data$vs, mat=T,digits = 1,fast=T)
#Work with library psych mat-view in matrix, digits-count digits in
#result fast-short view values


data1 <- airquality

subset <- data1[data1$Month %in% c(7,8,9),] 
# Select values contains in data sets in rows
result <- aggregate(Ozone~Month,subset,length)


describeBy(airquality,group = airquality$Month)
describe(iris)
#Values from datasets
describeBy(iris,group = iris$Species)
#Values from datasets group by values

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA


x <- mean(my_vector,na.rm = T)
fixed_vector <- c()
fixed_vector <- ifelse(is.na(my_vector),x,my_vector)
#Replace NA on mean(vector)


for(i in 1:length(my_vector)){
  if(is.na(my_vector[i])) fixed_vector[i] <- x
  else fixed_vector[i] <- my_vector[i]
}
#Second way through for



