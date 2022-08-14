## 표준정규분포
data(iris)
head(iris)

hist(iris$Sepal.Length)

mean(iris$Sepal.Length)
sd(iris$Sepal.Length)

a <- (iris$Sepal.Length - mean(iris$Sepal.Length))/sd(iris$Sepal.Length)
mean(a)
sd(a)

#왼쪽꼬리확률 계산
pnorm(q = 1.96) # default : mean = 0, sd = 1
1 - pnorm(q = 1.96)
pnorm(q = 1.96, lower.tail = FALSE) # lower.tail : 기준으로 왼쪽 면적
pnorm(q = 1.96, lower.tail = TRUE)

pnorm(q = 1.6)-pnorm(q = -0.155)

pnorm(q = -1.9)+pnorm(q = 2.1,lower.tail=FALSE)
                  
#왼쪽넓이를 만족하는 z값 찾기
qnorm(p = 0.975)
qnorm(p = 0.95) # default : mean = 0, sd = 1

# 예제 5.7
1-pnorm(q = 208, mean = 200, sd = 5)
pnorm(q = 208, mean = 200, sd = 5, lower.tail = FALSE)

pnorm(q = 200, mean = 200, sd = 5) - pnorm(q = 190, mean = 200, sd = 5)

# 예제 5.8
1 - pnorm(q = 208, mean = 200, sd = 5)


## 이항분포의 정규근사
bi <- rbinom(n = 1000, size = 100, prob = 0.5) # 이항분포 난수 생성성
hist(bi)
qqnorm(bi)
qqline(bi, col = "red")

# Shapiro-Wilk 검정
# H0 : f(x)는 정규분포이다.
# H1 : f(x)는 정규분포가 아니다
shapiro.test(bi)

# 예제 5.9
pnorm(q = 1.83) - pnorm(q = -1.3)


## 5.7 근사적 정규성을 위한 변환
data(cars)
head(cars)
str(cars)

hist(cars$dist)
hist(sqrt(cars$dist))
hist(sqrt(cars$dist)^1/4)
hist(log(cars$dist))
hist(cars$dist^2)

