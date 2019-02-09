#1.

install.packages("gmp")
library(gmp)

my_function<- function(n) {
  a=gmp::factorize(factorial(n))
  print(sum(a==3))
  
}

my_function(9)#4

my_function(100)#1


#2.
a<-read.csv("C:/Users/korea/Desktop/ToyotaCorolla.csv")

head(a)
str(a)


set.seed(1)
sub<-sample(nrow(a),nrow(a)*0.7)

sub

a.train<-a[sub,]
a.test<-a[-sub,]

str(a.train) #1005 obs. of 39 variables 
head(a.train)
str(a.test)  #431 obs. of 39 variables
head(a.test)
#3. 

#min-max scaling 함수 정의
normalize<-function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

normalize(a$KM)

normalize(c(1,2,3,4,5))

#4.

str(a)

a$Year<-as.factor(a$Mfg_Year)


library(plyr)
ddply(a, .(Year), summarize,
      KM = round(mean(KM), 2),)

