## 로지스틱 회귀분석
install.packages("survival")
library(survival)
head(colon)
str(colon)
colon1 <- na.omit(colon)

logit <- glm(status ~ sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4 + etype, family = binomial(), data=colon1)
summary(logit)

logit <- glm(status ~ adhere + nodes + extent + surg + node4, family = binomial(), data=colon1)
summary(logit) # AIC : 클수록 좋지 않다 / 모형 적합도


