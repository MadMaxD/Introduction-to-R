my_calc <- function(x,y){
  s <- x+y
  return (s)
}

my_calc(x = 10,y=15)


my_calc_2 <- function(x,y,z){
  
  s <- x+y+z
  d <- x-y-z
  return (c(s,d))
}

distr1 <- rnorm(100)
distr1[1:30] <- NA

distr1[is.na(distr1)] <- mean(distr1,na.rm = T)

my_nm_rm <- function(x){
  x[is.na(x)] <- mean(x,na.rm = T)
  return (x)
}


distr1 <- my_nm_rm(distr1)
hist(distr1)


my_nm_rm <- function(x){
  if(is.numeric(x)){
    stat_test <- shapiro.test(x)
    if(stat_test$p.value>0.05){
    x[is.na(x)] <- mean(x,na.rm = T)
    print("NA values were replaced with mean")
    }
    else{
      x[is.na(x)] <- median(x,na.rm = T)
      print("NA values were replaced with median")
    }
    return (x) 
  }
  else{
    print("X is not numeric")
  }
}

d1 <- rnorm(2000)
d2 <- runif(2000)
hist(d1)
hist(d2)

my_vector <- c(1, 2, 3, NA, NA)
NA.position <- function(x){
  return (c(which(is.na(x))))
}
NA.position(my_vector)


NA.counter <- function(x){
  return (length(which(is.na(x))))
}

NA.counter(my_vector)



































dir(pattern = "*.csv")
grant <- data.frame()
for (i in dir(pattern = "*.csv")) {
  temp_df <- read.csv(i)
  grant <- rbind(temp_df,grant)
}



read_data <- function(){
  df <- data.frame()
  number <- 0
  for (i in dir(pattern = "*.csv")) {
    temp_df <- read.csv(i)
    df <- rbind(temp_df,df)
    number <- number+1
  }
  print(paste(as.character(number),"file were combined"))
  return (df)
}
grants2 <- read_data()


filtered.sum(c(1, -2, 3, NA, NA))
filtered.sum <- function(x){
  return(sum(x[x>0 & !is.na(x)]))
}


outliers.rm <- function(x){
  k <- quantile(x,probs = c(0.25,0.75))
  return (x[x>=k[1]-1.5*IQR(x) & x<=1.5*IQR(x)+k[2]])
}
outliers.rm(v_test)
