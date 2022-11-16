## 예제 7.3
x <- c(2.6,1.9,1.8,1.6,1.4,2.2,1.2,1.6,1.6,1.5,1.4,1.6,2.3,1.5,1.1,
     1.6,2.0,1.5,1.7,1.5,+1.6,2.1,2.8,1.0,1.2,1.2,1.8,1.7,0.8,1.5,2.0,
     2.2,1.5,1.6,2.2,2.1,3.1,1.7,1.7,1.2)

# 표본평균
mean(x)

# 표본표준편차
sd(x)

n <- length(x)
alpha <- 0.05

qt(1-alpha/2, df = n-1)*sd(x)/sqrt(n)
qnorm(1-alpha/2)*sd(x)/sqrt(n)


## 예제 7.4
alpha <- 0.1
sigma <- 4
d <- 0.8
(qnorm(1-alpha/2) * sigma/d)^2


## 예제 7.5
#표준편차
sigma <- 8
n <- 25
#평균
xbar <- 42.7
alpha <- 0.05
#표준오차
se <- sigma/sqrt(n)
#오차한계
d <- qnorm(1-alpha/2, lower.tail = TRUE)*se
#신뢰구간
xbar + c(-d, d)

## t.test
x <- rnorm(100, mean = 10, sd = 5) # 데이터가 주어지면 신뢰구간도 구해준다
t.test(x)

# 그림 7-2
# https://rfriend.tistory.com/113


# 예제 7.10
n <- 250
x <- 70
#모비율
phat <- x/n
alpha <- 0.046
#표준오차
se <- sqrt(phat*(1 - phat)/n)
#95.4% 오차한계
me_95.4 <- qnorm(1 - alpha/2)*se


# 예제 7.11
n <- 500
x <- 41
#모비율
phat <- x/n
alpha <- 0.05
#표준오차
se <- sqrt(phat *(1 - phat)/n)
#오차한계
me_95 <- qnorm(1 - alpha/2)*se
#신뢰구간
phat + c(-me_95,me_95)


# 예제 7.12
alpha <- 0.02
#허용오차
d <- 0.05
# p가 알려져 있지 않은 경우
(qnorm(1-alpha/2)/d)^2*1/4
# p가 0.3으로 알려져 있는 경우
p <- 0.3
z_2 <- qnorm(1-alpha/2) 
p*(1-p)*(z_2/d)^2 


# 예제 7.13
n <- 400
p0 <- 0.2
phat <- 70/400
#Z-검정통계량
z <- (phat-p0)/sqrt(p0*(1-p0)/n)
#유의확률
pval <- 2*pnorm(z)
#모비율 검정
prop.test(70, 400, p = 0.2, correct = FALSE)
