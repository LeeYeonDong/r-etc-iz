## 도수분포표
a <- rep("A",1520)
b <- rep("B",770)
c <- rep("C",510)
x <- c(a,b,c)
y <- as.matrix(table(x))
freq <- y[,1] 
relative_freq<-freq/sum(y)
t(rbind(freq,relative_freq))

## 파이차트
x <- c(1520,770,510)
lab <- c("A","B","C")

y <- round(x/sum(x)*100,digits=1) # y에 해당 백분위 값 주기
z <- paste(lab,"(",y,"%",")") # 도표에 포함될 lable값 주기
pie(x,labels=z,main="예제 2.2 파이차트")


## 파레토 그림
a <- rep("A",35)
b <- rep("B",9)
c <- rep("C",45)
d <- rep("D",6)
e <- rep("E",5)
x <- factor(c(a,b,c,d,e)) #도수가 큰 범주 순으로 재정렬
sort <- sort(table(x),decreasing=TRUE)
barplot(sort,xlab="요소",ylab="count",main="예제 2.3 파레토그림")

## 히스토그램
data <- sample(c(1:100),size = 50, replace = TRUE)
data %>% sort(decreasing = FALSE)

hist(data, breaks = 20)
hist(data, breaks = 10)
hist(data, breaks = 3)

## 분산
x <- c(1,3,4,6,6,7,8,8,9,10,15)
var(x)

##표준편차
sd(x)

## 상자그림
data(mpg)
str(mpg)
boxplot(mpg$displ)
summary(mpg$displ)

quantile(mpg$displ)
quantile(mpg$displ, probs = seq(0,1, by=0.05)) # percentile

## 산점도
plot(mpg$displ, mpg$hwy)

# 상관계수
cor(mpg$displ, mpg$hwy)
