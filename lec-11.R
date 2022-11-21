## 예제 11.4
x <- matrix(c(773,231,238,59), nrow = 1, ncol = 4)
chi <- chisq.test(x,p=c(9/16, 3/16, 3/16, 1/16))

chi$observed

chi$expected

chi$residuals
# the Pearson residuals, (observed - expected) / sqrt(expected)


## 예제 11.5 - 독립성 검정
x <- matrix(c(31,17,109,122), ncol = 2)
chi <- chisq.test(x, correct = FALSE)

chi$observed

chi$expected

chi$residuals


## 예제 11.6
x <-matrix(c(5,5,3,7,15,23,17,17), ncol = 4, byrow = TRUE)
chi <- chisq.test(x, correct = FALSE)
