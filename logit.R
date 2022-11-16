# 로지스틱 회귀분석

data(mtcars)
str(mtcars)
View(mtcars)

df <- mtcars

# vs factor
df$vs <- as.factor(df$vs)

# logit
df_logit <- glm(vs ~ mpg + am + gear, data = df, family = binomial())
summary(df_logit)

# 회귀계수
coef(df_logit)

# exp -> 1단위 증가시 odds증가량
exp(coef(df_logit))

# 신뢰구간
confint(df_logit, parm = "mpg")
exp(confint(df_logit, parm = "mpg"))

# 로지스틱 회귀분석 적합결과
fitted(df_logit)

result_df <- data.frame(fitted(df_logit))
result_df$vs_result <- ifelse(result_df$fitted.df_logit. > 0.5, 1, 0)
result_df$vs <- df$vs

table(result_df$vs_result == result_df$vs)

# 잔차분석
par(mfrow = c(1,1))
plot(df_logit)
# 1 : 잔차 ~ 회귀선 -> 1) 잔차 형태가 커브 : 선형성 의심, 2) 등분산성 의심
# 2 : QQplot -> 정규성
# 3: scaled 잔차 ~ 회귀선 -> 등분산성
# 4 : 이상치 확인