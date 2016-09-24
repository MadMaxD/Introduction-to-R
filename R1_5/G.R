data <- mtcars
data$vs <- factor(data$vs,labels = c("V","S"))
data$am <- factor(data$am,labels = c("Auto","Manual"))
# primary determination

hist(data$mpg,breaks = 20, xlab ="MPG")

boxplot(mpg~am,data,ylab="MPG")


plot(data$mpg,data$hp)

library(ggplot2)


ggplot(data,aes(x=mpg))+
    geom_density(fill="red")

ggplot(data,aes(x=mpg,y=hp,fill=am))+
  geom_dotplot()

ggplot(data,aes(x=mpg,fill=am))+
  geom_density(alpha=0.5)

ggplot(data,aes(x=am,y=hp,col=vs))+
  geom_boxplot()

ggplot(data,aes(x=mpg,y=hp,col=vs,size=qsec))+
  geom_point()

my_plot <- ggplot(data,aes(x=mpg,y=hp,col=vs,size=qsec))+
  geom_point()

plot1 <- ggplot(data, aes(x=mpg,y=disp,col=hp))+
  geom_point()

data2 <- iris
ggplot(data2,aes(x=Sepal.Width, y=Sepal.Length, shape = factor(Species),color=Petal.Length)) +
  geom_point()

ggplot(data2,aes(Species,Sepal.Length,fill=Sepal.Width))+
  geom_boxplot()


ggplot(data)+
  geom_abline(slope=1,intercept = 0.1)+
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 0.5)
 
 
ggplot(data2,aes(Species,fill=Species))+
  geom_bar()


ggplot(data, aes(cyl,hp))+
  geom_bin2d(bins=10)
#bins limit size


ggplot(data2,aes(Species,Petal.Length))+
  geom_blank()
#empty blank


ggplot(data2,aes(Sepal.Length,Petal.Length))+
  geom_count()

ggplot(data2, aes(Petal.Length))+
  geom_density(adjust=1/2)

ggplot(data2,aes(y=Sepal.Length,x=Sepal.Width))+
  geom_dotplot(color="blue")


ggplot(data2,aes(y=Sepal.Length,x=Sepal.Width))+
  geom_jitter(aes(colour=Species))


ggplot(data, aes(hp,qsec,colour=factor(cyl),label=rownames(data)))+
  geom_label(size=3,hjust=0,vjust=0)


ggplot(data,aes(cyl,hp,fill=factor(vs)))+
  geom_violin()

