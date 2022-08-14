## 예제 6.3
# 균일분포에서 난수 생성
x <- runif(500,0,9) 
# 생성된 난수를 행렬로 정리
y <- matrix(x, nrow = 100, ncol = 5)
#x의 평균들을 새로운 벡터로 정의
xbar <- apply(y,1,mean)
hist(xbar, probability = T)
mean(xbar)
sd(xbar)

# array(배열)
array(x,dim = c(20,5,5)) # 값은 1~500이고 20개의 행 5개의 열로 구성된 행렬 5개로 이루어진 배열.
array(x,dim = c(4,5,5,5))


## 예제 6.4
m <- 82
sd1 <- 12/sqrt(64)
sd2 <- 12/sqrt(100)

pnorm(83.2, mean = m, sd = sd1) - pnorm(80.8, mean = m, sd = sd1)
pnorm(83.2, mean = m, sd = sd2) - pnorm(80.8, mean = m, sd = sd2)
