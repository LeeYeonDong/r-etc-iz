## 이항분포
?dbinom
dbinom(x = 0, size = 5, prob = 0.25)
dbinom(x = 1, size = 5, prob = 0.25)
dbinom(x = 3, size = 5, prob = 0.25)

y <- dbinom(x = c(1:100), size = 100, prob = 0.5)
# 확률 : 0.5 / 100번시행(size) / 1번 성공할 확률, 2번, 3번 ... 100번
# sample space : c(1:100)에 성공확률이 0.5인 베르누이 시행을 100번 했을때
plot(c(1:100) , y)
plot(c(1:100) , y, type = "h")

y <- dbinom(x = c(1:100), size = 100, prob = 0.3)
plot(c(1:100) , y)

y <- dbinom(x = c(1:100), size = 100, prob = 0.8)
plot(c(1:100) , y)


## 누적이항분포
pbinom(q = 3, size = 5, prob = 0.25 , lower.tail = TRUE)
# P(X<=3) 확률계산       
pbinom(q = 3, size = 5, prob = 0.25 , lower.tail = FALSE)
# P(X>3) 확률계산       

d0 <- dbinom(x = 0, size = 5, prob = 0.25)
d1 <- dbinom(x = 1, size = 5, prob = 0.25)
d2 <- dbinom(x = 2, size = 5, prob = 0.25)
d3 <- dbinom(x = 3, size = 5, prob = 0.25)
d4 <- dbinom(x = 4, size = 5, prob = 0.25)
d5 <- dbinom(x = 5, size = 5, prob = 0.25)

d0 + d1 + d2 + d3
d4 + d5

y <- pbinom(q = c(1:100), size = 100, prob = 0.5)
# 확률 : 0.5 / 100번시행(size) / 1번 성공할 확률, 2번, 3번 ... 100번
plot(c(1:100) , y)
plot(c(1:100) , y, type = "h")


## 이항분포 난수 생성
rbinom(n = 10, size = 100, prob = 0.5)


# 이항분포에서 prob를 모를때
dbinom(x = 50, size = 100, prob = 0.5) 
# 0.5모를때 0.07이상이 나오는 확률을 찾아라

seq_prob <- seq(from = 0, to = 1, by = 0.01)
binom_value <- c()

for(i in 0:length(seq_prob)){
  binom_value.tmp <- dbinom(x = 50, size = 100, prob = seq_prob[i])
  binom_value <- append(binom_value, binom_value.tmp)
  cat("prob =" , seq_prob[i], "일때 dbinom =",  binom_value.tmp,"\n")
  
  Sys.sleep(time = 0.3)
}

library(tidyverse)
data.frame(seq_prob, binom_value) %>% filter(binom_value >= 0.07)


pbinom(q = 2, size = 18, prob = 0.2 , lower.tail = TRUE)
dbinom(x = 0, size = 18, prob = 0.2) 
dbinom(x = 1, size = 18, prob = 0.2)
