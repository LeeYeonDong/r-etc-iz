# boxplot str dim
install.packages("tidyverse")
library(tidyverse)
data(airquality)
str(airquality)

hist(airquality$Ozone)
hist(airquality$Solar.R)

## 도수분포표 예제 2.1
a <- rep("A",1520)
b <- rep("B",770)
c <- rep("C",510)
x <- c(a,b,c)
y <- as.matrix(table(x))
freq <- y[,1] 
relative_freq<-freq/sum(y)
t(rbind(freq,relative_freq))

## 파이차트 예제 2.2
x <- c(1520,770,510)
lab <- c("A","B","C")

y <- round(x/sum(x)*100,digits=1) # y에 해당 백분위 값 주기
z <- paste(lab,"(",y,"%",")") # 도표에 포함될 lable값 주기
pie(x,labels=z,main="예제 2.2 파이차트")


## 파레토 그림 예제 2.3
a <- rep("A",35)
b <- rep("B",9)
c <- rep("C",45)
d <- rep("D",6)
e <- rep("E",5)
x <- factor(c(a,b,c,d,e)) #도수가 큰 범주 순으로 재정렬
sort <- sort(table(x),decreasing=TRUE) # 정렬
barplot(sort,xlab="요소",ylab="count",main="예제 2.3 파레토그림")


## 히스토그램
data <- sample(c(1:100),size = 50, replace = TRUE)
data %>% sort(decreasing = FALSE)

hist(data, breaks = 20)
hist(data, breaks = 10)
hist(data, breaks = 3)

## 점도표
data(cars)
str(cars)
View(cars)
stripchart(cars$dist, method="stack", offset=0.5, pch=1)

## 분산
x <- c(1,3,4,6,6,7,8,8,9,10,15)
var(x)

##표준편차
sd(x)

## 줄기 잎 도표
data(mtcars)
str(mtcars)
mtcars$mpg
stem(mtcars$mpg)


## 상자그림
data(mtcars)
str(mtcars)

boxplot(mtcars, horizontal = TRUE)

outlier <- append(mtcars$disp,c(-1000,-500,500,1000))

boxplot(outlier, horizontal = TRUE)
summary(outlier)

quantile(outlier)
quantile(outlier, probs = seq(0,1, by=0.05)) # percentile
quantile(outlier, probs = seq(0,1, by=0.01)) # percentile

## 이상치
outlier1 <- (quantile(outlier)[4] - quantile(outlier)[2])*1.5 + quantile(outlier)[4]
class(outlier1)
as.numeric(outlier1)

## 극이상치
outlier2 <- (quantile(outlier)[4] - quantile(outlier)[2])*3 + quantile(outlier)[4]
class(outlier2)
as.numeric(outlier2)


## 산점도
str(mtcars)
plot(mtcars$disp, mtcars$hp)

# 상관계수
cor(mtcars$disp, mtcars$hp)
cor.test(mtcars$disp, mtcars$hp)
corrplot::corrplot(cor(mtcars))
corrplot::corrplot(cor(mtcars), method="ellipse")
corrplot::corrplot(cor(mtcars), method="number")
