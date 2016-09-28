data1 <- read.csv("shops.csv")
data2 <- read.csv("therapy_data.csv")


library(ggplot2)


boxplot(price~origin,data1)

ggplot(data1,aes(origin,price))+
  geom_boxplot()

fit <- aov(price~origin,data1)
summary(fit)


model.tables(fit,"means")


pd=position_dodge(0.1)

ggplot(data1,aes(store,price,color=origin,group=origin))+
  stat_summary(fun.data = mean_cl_boot,geom='errorbar',width=0.2,lwd=0.8,position = pd)+
  stat_summary(fun.data = mean_cl_boot,geom='line',size=1.5,position = pd)+
  stat_summary(fun.data = mean_cl_boot,geom='point',size=5,position = pd,pch=15)+
  theme_bw()


fit3 <- aov(price~origin+store+origin:store,data1) 
summary(fit3)


fit4 <- aov(price~origin*store,data1)
summary(fit4)



dataD <- npk
?npk
f <- aov(yield~P+N+K,dataD)
summary(f)



ggplot(data1,aes(food,price))+geom_boxplot()
fit5 <- aov(price~food,data1)
summary(fit5)


TukeyHSD(fit5)



i <- iris
fit6 <- aov(Sepal.Width~Species,i)
TukeyHSD(fit6)


data2$subject <- as.factor(data2$subject)
fitt <- aov(well_being~therapy,data2)
summary(fitt)
fitt1 <- aov(well_being~therapy+Error(subject/therapy),data2)
summary(fitt1)


fitt2 <- aov(well_being~therapy*price,data2)
summary(fitt2)
ggplot(data2,aes(price,well_being))+
  geom_boxplot()


fitt3 <- aov(well_being~therapy*price+Error(subject/(therapy*price)),data2)
summary(fitt3)

ggplot(data2,aes(price,well_being))+
  geom_boxplot()+
  facet_grid(~subject)


fitt4 <- aov(well_being~therapy*sex*price+Error(subject/(therapy*price)),data2)
summary(fitt4)


P <- read.csv("Pillulkin.csv")
fit <- aov(temperature ~ pill + Error(patient/pill), data = P)
fitP <- aov(temperature ~ pill + Error(patient/pill), data = P)
summary(fit)


DD <- ToothGrowth
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp,group=supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
