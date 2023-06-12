## 예제 8.1
alpha <- 0.1
qt(c(1 - alpha, alpha), df = 5)


## 예제 8.2
alpha <- 0.05
qt(1-alpha, df = 9)


## 예제 8.3
n <- 15
xbar <- 39.3
s <- 2.6
alpha <- 0.05

# 임계치(critical value)
t.cv <- qt(1-alpha, df = n-1)

# 신뢰구간
xbar + c(-t.cv * s/sqrt(n), t.cv * s/sqrt(n))

# 정규성 검정
library(tidyverse)
head(airquality)

hist(airquality$Temp)
shapiro.test(airquality$Temp)
qqnorm(airquality$Temp)
qqline(airquality$Temp, col = 2)

# t.test
library(tidyverse)
head(airquality)
t.test(airquality$Temp)
t.test(airquality$Temp, alternative = "greater")


## 8.4
x <- c(175, 190, 215, 198, 184, 207, 210, 193, 196, 180)
n <- 10
xbar <- mean(x)
s <- sd(x)
alpha <- 0.01

# 임계치(critical value)
t.cv <- qt(1-alpha, df = n-1)

# t.test
t.test(x, alternative = "less", mu = 200)


## 예제 8.7
n <- 10
xbar <- 0.7
s <- 0.4
alpha <- 0.05

# chisq.test - 일원 카이제곱 검정
chisq.test(airquality$Temp)
# 귀무가설(영가설): 기대치(기대도수)와 관찰치는 차이가 없다(관련성x). : p >= 알파 (유의수준 인계치 값) 

