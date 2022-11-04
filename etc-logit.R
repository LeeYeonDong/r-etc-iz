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