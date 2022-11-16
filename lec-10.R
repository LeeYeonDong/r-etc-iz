## 예제 10.4
plot(airquality$Wind, airquality$Temp)

x <- c(3,3,4,5,6,6,7,8,8,9)
y <- c(9,5,12,9,14,16,22,18,24,22)

plot(x,y, type = "p", pch = 19)


# r scatter plot
# Data
set.seed(1201)
x <- runif(300)
y <- 5 * x ^ 2 + rnorm(length(x), sd = 2)
group <- ifelse(x < 0.4, "Group 1",
                ifelse(x > 0.8, "Group 3",
                       "Group 2"))
# Some noise after defining the groups
x <- x + runif(length(x), -0.2, 0.2)

# Scatter plot
plot(x, y,
     pch = 19,
     col = factor(group))

# Legend
legend("topleft",
       legend = levels(factor(group)),
       pch = 19, cex = 0.5,
       col = factor(levels(factor(group))))



## 표 10.3
x <- c(3,3,4,5,6,6,7,8,8,9)
y <- c(9,5,12,9,14,16,22,18,24,22)

length(x)
length(y)

fit <- lm(y ~ x)
summary(fit)

# plot
plot(x,y,type = "p", pch = 19)
abline(fit, col = "red")

# 잔차
fit$residuals

# 회귀계수
fit$coefficients

# yhat(추정량)
fit$fitted.values

# rank
fit$rank

## 예제 10.5
x <- c(3,3,4,5,6,6,7,8,8,9)
y <- c(9,5,12,9,14,16,22,18,24,22)

length(x)
length(y)

fit <- lm(y ~ x)
summary(fit)

fit <- lm(Temp ~ Wind, data = airquality)
summary(fit)

# plot
plot(airquality$Wind,airquality$Temp,type = "p", pch = 19)
abline(fit, col = "red")

## 예제 10.6
airquality$Wind
airquality$Temp

confint(fit, level = 0.95) #b1에 대한 신뢰구간

## 예제 10.7
predict(fit)
fitted(fit)

# 예측과 신뢰구간
predict.lm(fit, interval = "confidence")

# 예측과  예측구간
predict.lm(fit, interval = "prediction")

# 새로운 데이터에 대한 예측과 신뢰구간
fit <- lm(Temp ~ Wind, data = airquality)
summary(fit)

Wind <- seq(from = 9, to = 10, by = 0.1)
length(Wind)
nd <- data.frame(Wind) # b1 이름이랑 똑같아야 한다
predict.lm(fit, newdata = nd, interval = "confidence", level = 0.95)

predict.lm(fit, newdata = nd, interval = "prediction", level = 0.95)


# 선형변환
View(mtcars)
str(mtcars)

plot(mtcars$hp, mtcars$drat, type = "p", pch = 19)
fit <- lm(mtcars$drat ~ mtcars$hp)
summary(fit)

plot(log(mtcars$hp), mtcars$drat, type = "p", pch = 19)
fit <- lm(mtcars$drat ~ log(mtcars$hp))
summary(fit)


# 다중회귀
fit <- lm(mtcars$drat ~ mtcars$hp + mtcars$wt + mtcars$mpg + mtcars$disp)
summary(fit)

# 잔차분석
par(mfrow = c(2,2))
plot(fit)
# 1 : 잔차 ~ 회귀선 -> 1) 잔차 형태가 커브 : 선형성 의심, 2) 등분산성 의심
# 2 : QQplot -> 정규성
# 3: scaled 잔차 ~ 회귀선 -> 등분산성
# 4 : 이상치 확인
