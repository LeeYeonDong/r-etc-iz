## 예제 12.2
# 일원배치 분산분석(oneway-test)
library(tidyverse)
data(airquality)

head(airquality)

dd <- data.frame(airquality$Ozone,airquality$Wind,airquality$Temp)

dd <- na.omit(dd)

dd <- round((dd - min(dd))/(max(dd)-min(dd))*100,digits = 0)

dd_stack <- stack(head(dd,4))
oneway.test(values ~ ind, data = dd_stack, var.equal = TRUE)

boxplot(values ~ ind, data = dd_stack)
stripchart(dd)

## 일원배치 분산분석(aov)
class(dd_stack$ind)
summary(aov(dd_stack$value ~ dd_stack$ind))

.libPaths()[1]
