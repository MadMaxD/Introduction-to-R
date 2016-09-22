data <- mtcars


a <- 10

if(a>10){
  print("More")
}else{
  print("Norm")
}
# "Norm"
b <- c(11,12,30,1,5,9,12)
ifelse(b>10,"More","Norm")
# "More" "More" "More" "Norm" "Norm" "Norm" "More"


for (i in 1:10){
  print(i);
}

# [1] 1
# [1] 2
# [1] 3
# [1] 4
# [1] 5
# [1] 6
# [1] 7
# [1] 8
# [1] 9
# [1] 10


data$power <- rep(NA,nrow(data))
# Create new coloun with value = "NA"
data$power<-ifelse(data$cyl>=6,"Power","Normal")

j <- 1
while(j<10){
  print(j*10)
  j<- j+1
}

# [1] 10
# [1] 20
# [1] 30
# [1] 40
# [1] 50
# [1] 60
# [1] 70
# [1] 80
# [1] 90

data$new_var <- rep(NA,nrow(data))
data$new_var <- ifelse((data$cyl>6 | data$carb>=4),1,0)




# window avg
moving_average <- c()

step <- 0
while(step <(length(m)-9)){  
  moving_average[(step+1)] <- cumsum(m[c((1+step):(10+step))])[10]/10
  step <- step+1
}


n <- 10    
d <- AirPassengers    
cx <- c(0, cumsum(d))    
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n
# window avg


