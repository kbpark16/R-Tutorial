#import data 

summer<-read.csv("C:/Users/korea/Desktop/summer.csv",head=F)

str(summer)

head(summer)

summer$time<-1:261

sd(summer$search)

colnames(summer)<-c('date','search','time')

# 월별,
summer$month<-as.factor(substr(summer$date,start=6,stop=7))
# 연별
summer$year<-as.factor(substr(summer$date,start=1,stop=4))

library(plyr)
ddply(summer, .(index), summarize,
      sum = round(mean(search), 2))
#2013-10 ~ 2018-10 : sum


str(summer)

# 계절 변수 만들기(봄,여름,가을,겨울)- dummy variable, baseline을 spring으로 두기 
# summer$spring<-ifelse(summer$month %in% c(3,4,5),1,0)
summer$summer_index<-ifelse(summer$month %in% c("06","07","08"),1,0)
summer$autumn_index<-ifelse(summer$month %in% c("09","10","11"),1,0)
summer$winter_index<-ifelse(summer$month %in% c("12","01","02"),1,0)



str(summer)

#4년 1년 summer, test 나누기 - 2017.10.8 4년치 summering 데이터/  2017.10.15~2018.10.7
# 239 까지 
train<-summer[1:209,]

head(train);

test<-summer[210:261,]
head(test);
str(test)


# Note the use of the '.' function to allow
# group and sex to be used without quoting
library(plyr)
ddply(train, .(month), summarize,
      mean = round(mean(search), 2),
      sd = round(sd(search), 2))


table(train$month)

attach(train)

tail(train)

summary(train)


attach(train)

str(train$search)
str(train$time)
summary(search)
summary(time)

train_reg<-lm(train$search~train$time+train$summer_index+train$autumn_index+train$winter_index)
summary(train_reg)


attach(train)
train_reg<-lm(train$search~train$time+train$train_index+train$autumn_index+train$winter_index)
summary(train_reg)


train_reg<-lm(train$search~train$time+train$year+train$month+train$train_index+train$autumn_index+train$winter_index)
summary(train_reg)

5+11+13+1

train_reg$resid
str(train)
plot(scale(train_reg$resid),type='b',xlab="time",ylab="residual");abline(h=0)  # index plot. (Need not to be scaled)

DWtest<- dwtest(train_reg)
DWtest # p-value < 2.2e-16

#MAD(MAE), MSE,RMSE, MAPE 다 해보기


#MAE로 성능평가, 
MAE<-function(actual,predicted) {
  mean(abs(actual-predicted))
}

actual<-test$search
predicted<-exp(predict(dummy.fit,newdata=test))-1

str(actual);str(predicted)

MAE(actual,predicted)

cbind(actual,predicted)


#MSE로 성능평가,
MSE<-function(actual,predicted) {
  (sum(actual-predicted))^2
}

MSE(actual,predicted)

#RMSE로 성능평가,
RMSE<-function(actual,predicted) {
  sqrt((sum(actual-predicted))^2)
}

RMSE(actual,predicted)

#MAPE로 성능평가,-> 217번째값 지우고 계산해야 할 듯 (inf 나옴)
MAPE<-function(actual,predicted) {
  mean(abs((actual-predicted)/actual))
}


MAPE(actual,predicted)


#+----+++ -> sign for positive autocorrelation

###  Durbin- Watson Test


train_reg<-lm(train$search~train$time)
summary(train_reg)
install.packages("lmtest")
library(lmtest)
DWtest<- dwtest(train_reg)
DWtest # p-value < 2.2e-16
#rho>0, first-order autocorrelation is greater than 0 -> 시간 인덱스와 관련된 추가적인 regressor가 필요함
# -> 여기서는 seasonality를 반영하는 regressor가 필요한데
#-> 크게 2가지 방법이 있다. 1) Trigonometric method 2) Dummy(binary) method


Et<-train_reg$resid[-1] #Estimation of rho를 계산할때, resid의 첫 값은 계산이 안 된다.
Etm1<-train_reg$resid[-261]  #lag(Et)는 Et의 마지막 값이 계산에 들어가지 않는다.(샘플로 해보면 이해됨)
rho.top<-sum(Et*Etm1)  #rho 추정값의 분자
rho.botm<- sum(train_reg$resid^2) #rho 추정값의 분모
rho.est<- rho.top/rho.botm
rho.est  # +0.6618005

# 1주일 단위 , 1년 주기로 12개월, 52주

train$costwo<-cos((2*pi*train$time)/52)
train$sintwo<-sin((2*pi*train$time)/52)
train$cosfour<-cos((4*pi*train$time)/52)
train$sinfour<-sin((4*pi*train$time)/52)


train_reg<-lm(train$search~train$time+train$sintwo+train$costwo+train$sinfour+train$cosfour)

summary(train_reg)


train_reg$resid

plot(scale(train_reg$resid),type='b',xlab="time",ylab="residual");abline(h=0)  # index plot. (Need not to be scaled)

#+----+++ -> sign for positive autocorrelation

###  Durbin- Watson Test


train_reg<-lm(train$search~train$time)
summary(train_reg)
install.packages("lmtest")
library(lmtest)
DWtest<- dwtest(train_reg)
DWtest

#(2)Trigonometric 방법 이용


#(3) additive Holt Winters method
# - It should be fixed by month

train

library(plyr)
ddply(train, .(month), summarize,
      mean = round(mean(search), 2),
      sd = round(sd(search), 2))

head(train)

#frequency=52- weekly data

train_ts<-ts(train$search,start=c(2013,10),frequency=52)
train_ts
train$search

train_holt <- HoltWinters(train_ts, seasonal = "additive")

train_holt

train_holt <- HoltWinters(train_ts, alpha=0.1,gamma=0.65,seasonal = "additive")


train_holt

train_holt_m <- HoltWinters(train_ts, alpha=0.2, gamma=0.8, seasonal = "multiplicative")


train_holt_m

### prediction

#52 obs,
str(test)


rm(ls(all))

pre_holt <- predict(train_holt,52,prediction.interval = TRUE)

test$search[1:15]

pre_holt_m <- predict(train_holt_m,52,prediction.interval = TRUE)

plot(train_holt_m,pre_holt_m)
par(new=TRUE)
head(test)
lines(ts(test$search, start = c(2017,10),frequency = 52),col="orange",lwd=2)

plot(train_holt_m,pre_holt_m)
lines(ts(test$search, start = c(2017,10),frequency = 52),col="orange",lwd=2)


#MAE로 성능평가, 
MAE<-function(actual,predicted) {
  mean(abs(actual-predicted))
}

actual<-test$search
predicted<-pre_holt_m

str(actual);str(predicted)

MAE(actual,predicted)

cbind(actual,predicted)


#MSE로 성능평가,
MSE<-function(actual,predicted) {
  (sum(actual-predicted))^2
}

MSE(actual,predicted)

#RMSE로 성능평가,
RMSE<-function(actual,predicted) {
  sqrt((sum(actual-predicted))^2)
}

RMSE(actual,predicted)

#MAPE로 성능평가,-> 217번째값 지우고 계산해야 할 듯 (inf 나옴)
MAPE<-function(actual,predicted) {
  mean(abs((actual-predicted)/actual))
}


MAPE(actual,predicted)



pre_holt
test

plot(train_holt_pred_m,pre_holt_m)
pre_holt
result12_2 <- cbind(train_test,pre_m12_2)
head(result12_2)
result12_2

### result




?HoltWinters
# 0.069 0.091 1



#4년 1년 train, test 나누기 - 2017.10.8 4년치 training 데이터/  2017.10.15~2018.10.7
# 239 까지 
train<-train[1:209,]

tail(train);

test<-train[210:261,]
head(test);
str(test)

ts(train$search, start=c(2013,10,1),frequency=52)

ski_hw_add1 <- HoltWinters(ts(train$search, start=c(2013,10),frequency=52), seasonal = "additive")

ski_hw_add1

pre_add1 <- predict(ski_hw_add1,52,prediction.interval = TRUE)

pre_add1[,1]

plot(train$search,pre_add1[,1])


pre_add1 <- predict(ski_hw_add1,test$search)
pre_add1


ski_hw_add1 <- HoltWinters(train, alpha=0.2,beta=0.2,gamma = 1, seasonal = "additive")
pre_add1 <- predict(ski_hw_add1,52,prediction.interval = TRUE)

pre_add1[,1]

plot(train$search,pre_add1[,1])

str(test)

err.mat[1,3]<-(sum((test$search-pre_add1[,1])^2))/length(test$search)
err.mat[2,3]<-sqrt((sum(((test$search)-pre_add1[,1])^2))/length(test$search))

plot(ski_hw_add1,pre_add1)
lines(ts(ski_data$search[154:166], start = c(2016,10),frequency = 12),col="orange",lwd=2)

pre_add <- predict(ski_hw_mult,12,prediction.interval = TRUE)

plot(ski_hw_mult,pre_mult)
lines(ts(ski_data$search[154:166], start = c(2016,10),frequency = 12),col="orange",lwd=2)


ski_hw_mult
# 0.292 0.0278 0.5790

#4년 1년 train, test 나누기 - 2017.9.24 4년치 training 데이터/  2017.10.1~2018.9.23
# 239 까지 
train<-train[1:209,]

head(train);

test<-train[210:261,]
tail(test);
str(test)

### Trigonometric method - 년마다 반복되는 패턴을 보이므로, 주기를 12로 줬습니다. -> time....월별일땐 12로 그냥 나누면 되지만
# 여기선 1주일 단위로 되어 있기 때문에 일단 월별로 합쳐야 12로 나누는게 가능 
# 1주일 단위라, 4주를 1개월로 친다면 12*4=48주를 L로 줘야함 

# 1주일 단위 , 1년 주기로 12개월, 52주

train$costwo<-cos((2*pi*train$time)/52)
train$sintwo<-sin((2*pi*train$time)/52)
train$cosfour<-cos((4*pi*train$time)/52)
train$sinfour<-sin((4*pi*train$time)/52)

test$costwo<-cos((2*pi*test$time)/52)
test$sintwo<-sin((2*pi*test$time)/52)
test$cosfour<-cos((4*pi*test$time)/52)
test$sinfour<-sin((4*pi*test$time)/52)
train$costwo<-cos((2*pi*train$time)/52)
train$sintwo<-sin((2*pi*train$time)/52)
train$cosfour<-cos((4*pi*train$time)/52)
train$sinfour<-sin((4*pi*train$time)/52)


str(train);str(test);str(train);

#model1.
tri.fit<-lm(log(search+1)~time+costwo+sintwo,data=train)
summary(tri.fit)

###  Durbin- Watson Test
install.packages("lmtest")
library(lmtest)
DWtest<- dwtest(tri.fit)
DWtest # p-value < 2.2e-16


#model2.
tri.fit2<-lm(log(search+1)~time+costwo+sintwo+cosfour+sinfour,data=train)
summary(tri.fit2)

DWtest<- dwtest(tri.fit)
DWtest # p-value < 2.2e-16


#perfomance is poor -> adjusted R squared: 0.4818


#2. binary method


# binary: 계절: 봄(3-5월)여름(6-8월),가을(9-11월),겨울(12월-2월) -> 3개 binary 변수 
# or 월별로 1월,2월,3월...12월 -> 11개 binary 변수: 월별로 계절성 있다기 보다는 계절별로 있기 때문에, 계절 factor를 만들겠음 

# 1. 계절별도 일단 월별이 필요함
# 

str(train$date);

# 월만 짤라내기 -> 월별 변수는 11개 변수를 만들어야 하나, R의 lm function 내에서 factor(month)를 쓰면 자동으로 dummy변수 생성 가능 


# 계절 변수 만들기(봄,여름,가을,겨울)

# 계절 변수 만들기(봄,여름,가을,겨울)- dummy variable, baseline을 spring으로 두기 
# train$spring<-ifelse(train$month %in% c(3,4,5),1,0)
train$train<-ifelse(train$month %in% c("06","07","08"),1,0)
train$autumn<-ifelse(train$month %in% c("09","10","11"),1,0)
train$winter<-ifelse(train$month %in% c("12","01","02"),1,0)

test$train<-ifelse(test$month %in% c("06","07","08"),1,0)
test$autumn<-ifelse(test$month %in% c("09","10","11"),1,0)
test$winter<-ifelse(test$month %in% c("12","01","02"),1,0)

train$first<-ifelse(train$month %in% c("01","02","03"),1,0)
train$second<-ifelse(train$month %in% c("04","05","06"),1,0)
train$third<-ifelse(train$month %in% c("07","08","09"),1,0)

test$first<-ifelse(test$month %in% c("01","02","03"),1,0)
test$second<-ifelse(test$month %in% c("04","05","06"),1,0)
test$third<-ifelse(test$month %in% c("07","08","09"),1,0)


head(train)

table(train$train)
table(train$autumn)
table(train$winter)

head(train)

str(train$month);str(test$month);str(train$month);


#model3.
#binary: season
dummy.fit<-lm(log(search+1)~time+train+autumn+winter, data=train)
summary(dummy.fit) # adjusted R-squared: 0.1686

DWtest<- dwtest(dummy.fit)
DWtest # p-value < 2.2e-16

plot(scale(dummy.fit$resid),type='b',xlab="time",ylab="residual");abline(h=0)  # index plot. (Need not to be scaled)

#model4.
#binary: quarter
dummy.fit<-lm(log(search+1)~time+first+second+third, data=train)
summary(dummy.fit) # adjusted R-squared: 0.3341-better
predict(dummy.fit,newdata=test)

DWtest<- dwtest(dummy.fit)
DWtest # p-value < 2.2e-16


#binary: month
dummy.fit<-lm(search~time+factor(month), data=train)
summary(dummy.fit) # adjusted R-squared: 0.506- farbetter
predict(dummy.fit,newdata=test) #

DWtest<- dwtest(dummy.fit)
DWtest # p-value < 2.2e-16



#binary: month
# 3개 binary 다 쓰니까 애초에 추정이 안 됨-> 아마도 다중공선성 때문
dummy.fit<-lm(search~time+factor(month)+first+second+third+train+autumn+winter, data=train)
summary(dummy.fit) # adjusted R-squared: 0.3941- farbetter
predict(dummy.fit,newdata=test) 



#factor(month);
dummy.fit<-lm(search~time+train+autumn+winter, data=train)
summary(dummy.fit) # adjusted R-squared: 0.4334 
predict(dummy.fit,newdata=test)

#MAD(MAE), MSE,RMSE, MAPE 다 해보기


#MAE로 성능평가, 
MAE<-function(actual,predicted) {
  mean(abs(actual-predicted))
}

actual<-test$search
predicted<-exp(predict(dummy.fit,newdata=test))-1

str(actual);str(predicted)

MAE(actual,predicted)

cbind(actual,predicted)


#MSE로 성능평가,
MSE<-function(actual,predicted) {
  (sum(actual-predicted))^2
}

MSE(actual,predicted)

#RMSE로 성능평가,
RMSE<-function(actual,predicted) {
  sqrt((sum(actual-predicted))^2)
}

RMSE(actual,predicted)

#MAPE로 성능평가,-> 217번째값 지우고 계산해야 할 듯 (inf 나옴)
MAPE<-function(actual,predicted) {
  mean(abs((actual-predicted)/actual))
}


MAPE(actual,predicted)
