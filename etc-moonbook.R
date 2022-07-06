library(tidyverse)

event <- read_csv(file = "C:/간호대/기록지/이벤트기록지/이벤트기록지DB.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

event %>% str()

event <- apply(event, 2, as.factor)

event <- event %>% as_tibble()

# table
table(event$낙상,event$날짜)

# crosstable
library(gmodels)
CrossTable(event$낙상,event$날짜)
CrossTable(event$낙상,event$날짜, format=c("SPSS"))

# moonbook
library(moonBook)
mytable(날짜~낙상, data=event)


