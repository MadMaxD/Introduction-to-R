data <- read.csv("grants.csv")


str(data)
data$status <- as.factor(data$status)
levels(data$status) <- c("Funded","Not Funded")
data$status <- factor(data$status, labels = c("Funded","not Funded"))
#Изменение бинарных значений из вектора


t1 <- table(data$status)
#1D таблица

t2 <- table(data$status,data$field)
#2D таблица

t2 <- table(status=data$status,field=data$field)

prop.table(t2,1)
prop.table(t2,2)
# Процентное значение по столбцу и по строке

t3 <- table(Years=data$years_in_uni,Field=data$field,Status=data$status)
dim(t3)

data2 <- HairEyeColor

dimnames(data2)
sum(HairEyeColor[,'Green','Female'])


barplot(t2,legend.text = T,args.legend = list(x="topright"))
barplot(t2,legend.text = T,args.legend = list(x="topright"),beside = T)

mosaicplot(t2)


library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data =mydata[mydata$Sex=='Female',] , aes(x =Hair, y = Freq,fill=Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))



binom.test(x=5,n=20,p=0.5)
binom.test(t1)



chisq.test(t1)
chi <- chisq.test(t1)
chi$exp
chi$observed


chisq.test(t2)


fisher.test(t2)


dataFem <- as.data.frame(HairEyeColor['Brown',,'Female'])
chisq.test(dataFem)


td <- table(diamonds$cut,diamonds$color)
main_stat <- chisq.test(td)$statistic


diamonds$factor_price <- ifelse(diamonds$price>=mean(diamonds$price),1,0)
diamonds$factor_carat <- ifelse(diamonds$carat>=mean(diamonds$carat),1,0)
dd <- table(diamonds$factor_price,diamonds$factor_carat)
main_stat <- chisq.test(dd)

fisher_test <- fisher.test(table(mtcars$am,mtcars$vs))$p.value


