install.packages("qcc")
library(qcc)
library(tidyverse)

# 예제 4-2
# Generate and edit random data 
set.seed(1029)

d <- 5 # 5일간 측정
t <- 4 # 4시간마다
s <- 24/t # 표본번호

pst <- replicate(d, rnorm(d*24/t, mean = 110, sd = 2)) %>% as.data.frame()

names(pst) <- paste0("day",1:ncol(pst)) # 일
pst <- pst %>% 
  gather(key = "일", value = "value")

pst$시각 <- seq(from = 4, to = 24, by = 4) # 시각

pst$표본번호 <- c(1:s) # 표본번호

pst$부분군 <- c(1:(nrow(pst)/d)) # 부분군

# mutate 함수를 사용하면 데이터 프레임 크기를 맞춰줘야함

pst$일 <- str_replace_all(pst$일, pattern = "day", replacement = "") %>% 
  as.integer()

pst <- pst %>% 
  as_tibble() %>% 
  select(일, 시각, 표본번호, 부분군, value)

# xbar chart
pst %>% 
  select(value) %>% 
  as.matrix() %>% 
  matrix(nrow = 30, byrow = FALSE) %>% 
  qcc(type = "xbar", plot = TRUE) %>% 
  summary()
