## 예제 3.24
x <- c(0,1,2,3)
fx <- c(1/8,3/8,3/8,1/8)
xfx <- x*fx
t <- t(rbind(x,fx,xfx))
rbind(t,apply(t,2,sum))
# x fx xfx
# [1,] 0 0.125 0.000
# [2,] 1 0.375 0.375
# [3,] 2 0.375 0.750
# [4,] 3 0.125 0.375
# [5,] 6 1.000 1.500
data.frame(x,fx,xfx)

## 예제 3.25
x <- c(0,1,2,3,4)
fx <- c(0.1,0.2,0.4,0.2,0.1)
xfx <- x*fx
x_mu <- x-sum(xfx)
x_mu2 <- x_mu^2
x_mu2fx <- x_mu2*fx

# matrix
t <- t(rbind(x,fx,xfx,x_mu,x_mu2,x_mu2fx))
rbind(t,apply(t,2,sum))
class(rbind(t,apply(t,2,sum)))

# data frame
df <- data.frame(x,fx,xfx,x_mu,x_mu2,x_mu2fx)
df_sum <- as.data.frame(apply(t,2,sum))
df_sum <- t(df_sum)
rownames(df_sum) <- "summation"
rbind(df, df_sum)


## 연습문제 3.9 (2)
n <- 150
factorial(365-n+1) / 365^n
