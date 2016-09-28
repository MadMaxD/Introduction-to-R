data <- iris


data1 <- subset(data,Species!="setosa")

str(data1)
table(data1$Species)


hist(data1$Sepal.Length)

library(ggplot2)
ggplot(data1,aes(Sepal.Length))+
  geom_histogram(fill="white",col="black",binwidth = 0.4)+
  facet_grid(Species~.)

ggplot(data1,aes(Sepal.Length,fill=Species))+
  geom_density(alpha=0.5)

ggplot(data1,aes(Species,Sepal.Length))+
  geom_boxplot()

shapiro.test(data1$Sepal.Length)
shapiro.test(data1$Sepal.Length[data1$Species=="versicolor"])
shapiro.test(data1$Sepal.Length[data1$Species=="virginica"])


bartlett.test(Sepal.Length~Species,data1)


t.test(Sepal.Length~Species,data1)


t.test(data1$Sepal.Length,mu=6.262)


t.test(data1$Petal.Length,data1$Petal.Width,paired = T)


by(iris$Sepal.Length, INDICES = iris$Species, shapiro.test)

TT <- ToothGrowth
subset1 <- TT$len[TT$dose==0.5 & TT$supp=="OJ"]
subset2 <- TT$len[TT$dose==2 & TT$supp=="VC"]
t_stat <-t.test(subset1,subset2)


a <- read.csv("lekarstva.csv")
t_t <- t.test(a$Pressure_before,a$Pressure_after,paired = T)


install.packages("Hmisc")
ggplot(data1,aes(Species,Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal,geom="errorbar",width=0.1)+
  stat_summary(fun.y = mean,geom="point",size=4)

wilcox.test(data1$Petal.Length,data1$Petal.Width,paired = T)

ggplot(data1,aes(Species,Petal.Length))+
  geom_boxplot()
