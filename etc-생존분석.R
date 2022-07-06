install.packages("tidyverse")
library(tidyverse)

##kaplan-Meier Model
# lets read in the AIDSsurv.csv dataset
AIDSsurv <- read.csv("C:/Users/YoungseukCho/Desktop/Lee/AIDSsurv.csv",header = TRUE)
# note : death = 1 = event ouccured(death), = 0 = no event(they're sensored), make sure you know
# what 0 and 1 code for

names(AIDSsurv)
class(AIDSsurv)
AIDSsurv$time %>% class()
AIDSsurv$death %>% class()

AIDSsurv %>% head()

# survival library
install.packages("survival")
library(survival)

# fit the KM(survival) model for the data
km_model <- survfit(
  Surv(time = AIDSsurv$time,
       event = AIDSsurv$death) ~ 1, data = AIDSsurv, type="kaplan-meier") 
# kaplan_meier is the default
# what's the ~1?... since we have No X-variables, we put a 1 there... its just survival, no X-variables to relate it to

# ask for some summaries of the model
km_model
# ask for model summary
km_model %>% summary()
#0.95lcl, ucl <- level of confidence (신뢰수준 : 모수를 포함할 확률)

# ask plot
km_model %>% plot()
plot(km_model, conf.int = TRUE, Xlab = "Time(months)", ylab = "%Alive = S(t)", main = "KM-model", mark.time = TRUE)
abline(h=0.5, col="red")

install.packages("GGally")
library(GGally)
km_model %>% ggsurv()
km_model %>% ggsurv(plot.cens = FALSE) + ggtitle("KM-model") + xlab("Time(month)") + 
ylab("%Alive = S(t)") +
  geom_abline(intercept = 0.5, slope = 0, color = "red")

km_model %>% ggsurv(plot.cens = TRUE) + ggtitle("KM-model") + xlab("Time(month)") + 
  ylab("%Alive = S(t)") +
  geom_abline(intercept = 0.5, slope = 0, color = "red")
# plot에서의 mark.time = plot.cens censored data 표시

install.packages("survminer")
library(survminer)
km_model %>% ggsurvplot(pval = TRUE)


# 난소암(ovarian cancer data)
data(cancer)
cancer %>% str()
dataa <- cancer
dataa$age %>% summary() 
dataa$over50 <- ifelse(cancer$age>50,1,0)
dataa$over50 %>% summary()

km_model2 <- survfit(
  Surv(time = dataa$time,
       event = dataa$status) ~ dataa$over50, data = dataa, type="kaplan-meier") 

km_model2 %>% summary()

km_model2 %>% ggsurvplot(conf.int = TRUE, surv.median.line = "hv", pval = TRUE, pval.method = TRUE)

km_model2 %>% plot(conf.int = TRUE, Xlab = "Time(months)", ylab = "%Alive = S(t)", main = "KM-model", col=c("red","blue"), las = 1, lwd = 1, mark.time = TRUE)

# do the Log-Rank-test
# H0 : survival in two groups is same
# Ha : H0 is not true
survdiff(Surv(dataa$time,dataa$status) ~ dataa$over50)


## Cox Proportional Hazard model
# dataset
library(tidyverse)
library(survival)

stan <- force(stanford2)
stan %>% str()
stan$over40 <- ifelse(stan$age>40,1,0)
stan$mismatchlevel <- cut(stan$t5, breaks=3, labels = c(0, 1, 2))

stan2 <- stan %>% na.omit()

stan2$over40 <- stan2$over40 %>% as.factor()
stan2$mismatchlevel <- stan2$mismatchlevel %>% as.factor()

stan2 %>% summary()
stan2 %>% str()


# let's take a look at K-M model
stan2_KM <- survfit(
  Surv(time = stan2$time,
       event = stan2$status) ~ stan2$mismatchlevel, data = stan2, type="kaplan-meier") 

library(survminer)
stan2_KM %>% ggsurvplot(conf.int = TRUE, surv.median.line = "hv", pval = TRUE, pval.method = TRUE)


# Cox Proportional Hazard model
stan2_cox <- coxph( Surv(time = time, event = status) ~ strata(over40) + mismatchlevel, data = stan2) 

stan2_cox %>% summary()

plot(survfit(stan2_cox), xlab="time", ylab="Survival Rate", conf.int=FALSE, col=1:2)
legend("bottomleft", legend=c(1,2), lty = 1, col = 1:2, text.col = 1:2, title = 'over40') 


stan2 %>% str()
stan2_new <- with(stan2,
data.frame(mismatchlevel = c(0,1,2),
age = rep(mean(stan2$age,na.rm = TRUE),3)))

stan2_cox %>% survfit(newdata = stan2_new) %>% ggsurvplot(conf.int = TRUE, surv.median.line = "hv", pval = TRUE, pval.method = TRUE)


# exp(coef) 값이 1.7249의 의미는 over40인 사람이 under40에 비해 죽을 확률이 72%배 높다는 뜻이다.(under40 is adjusting for mismatch level) 
# exp(-coef) 값이 0.5797의 의미는 over40인 사람이 under40에 비해 죽을 확률이 58%배 낮다는 뜻이다.

# note :  there is no coef for "intercept"... as noted, COX-Ph model does NOT estimate the "baseline hazard" but still estimates coef

# comparing nested models using the LRT (can we drop mismatchlevel)
cox_mod <- coxph( Surv(time = stan2$time,
                       event = stan2$status) ~ stan2$over40 + stan2$mismatchlevel) 
cox_mod %>% summary()

cox_mod2 <- coxph( Surv(time = stan2$time,event = stan2$status) ~ stan2$over40) 
cox_mod2 %>% summary()

# do the LRT
anova(cox_mod, cox_mod2, test = "LRT")

# we could include NUMERIC X's(we only explored the CAT-X so that we could directly compare to the KM-Model)
cox_num <- coxph( Surv(time = stan2$time,event = stan2$status) ~ stan2$age + stan2$t5)

cox_num %>% summary()
 
# exp(coef) = 1.030 1살 많을 수록 사망 확률이 3% 높아진다 


# check linearity (for the model that used num X's) using MARTINGALE residuals 
# add a line ax y=residual = 0
# fit a smoother thru the points
# type: the type of residuals to present on Y axis. Allowed values include one of c(“martingale”, “deviance”, “score”, “schoenfeld”, “dfbeta”, “dfbetas”, “scaledsch”, “partial”).
ggcoxdiagnostics(cox_num, type = "martingale", linear.predictions = TRUE)

ggcoxdiagnostics(cox_num, type = "deviance", linear.predictions = TRUE)
# it turns to be non-linearity... we can address this using the usual solutions...


## Checking Proportional Hazard assumption
# test for prop hazards using Schoenfeld test for PH
# H0 : HAZARDS are prop (hazard ratio is constant over time)
# Ha : HAZARDS are Not prop will return test for each X, and for overall model
cox_num %>% cox.zph() 

# cox.zph : Test the Proportional Hazards Assumption of a Cox Regressions

# test IF coef for variable(x) changes over time...if it changes over time -> non-prop hazard (HR changes over time)

# we can see a plot of these as well..(one plot for each parameter) these are plots of "changes in b over time", if we let "b" vary over time recall,... if "b" varies over time, this means that there is NOT PH! the effect is not constatnt over time... it varies!
# pay less attention to the extremes, as line is sensitive here
plot(cox.zph(cox_num)[1])
abline(h=0, col=2)
plot(cox.zph(cox_num)[2])
abline(h=0, col=2)

# zero on the plot means there is no change all right
# case[2] : y=0(red line)이 confidense interval에 상당히 포함 되어있으므로 Hazard ratio가 변하지 않는다고 볼 수 있다다