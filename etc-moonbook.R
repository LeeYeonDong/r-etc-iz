library(tidyverse)

event <- read_csv(file = "C:/��ȣ��/�����/�̺�Ʈ�����/�̺�Ʈ�����DB.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

event %>% str()

event <- apply(event, 2, as.factor)

event <- event %>% as_tibble()

# table
table(event$����,event$��¥)

# crosstable
library(gmodels)
CrossTable(event$����,event$��¥)
CrossTable(event$����,event$��¥, format=c("SPSS"))

# moonbook
library(moonBook)
mytable(��¥~����, data=event)

