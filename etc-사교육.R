# 
library(readxl)
raw <- read_excel("D:/대학원/사교육영향/rawdata_04.xlsx", sheet = 1)

library(tidyverse)
raw <- raw %>% mutate_all(~ str_replace(., "\\.", ","))
raw <- raw %>% apply(2, function(x) str_remove(x, ",.*"))

raw_df <- raw %>% as_tibble()
raw_df[is.na(raw_df)] <- " "

library(openxlsx)
write.xlsx(raw_df, file = "D:/대학원/사교육영향/rawdata_03.xlsx", fileEncoding = "CP949")









# 명사추출
library(readxl)
raw <- read_excel("D:/대학원/사교육영향/rawdata_04.xlsx", sheet = 1)

library(ggplot2)
library(tm)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(tidytext)
library(dplyr)

# a60
reply <- raw$`A60. 부산대학교 입학전형과 관련하여 추가로 제공되었으면 하는 정보가 있으면 작성해주십시오.` %>% na.omit()
reply_df <- reply %>% extractNoun() %>% unlist() %>% table() %>% sort(decreasing = TRUE) %>% as_tibble()
names(reply_df) <- c("word","freq")


# 불용어 제거
reply_df <- reply_df %>% 
  mutate(len = str_length(word)) %>%
  filter(len >= 2) 


삭제 <- c()

chr <- c("없습니","좋겠습니", "같습니", "좋겠", "하게","해서")

for(i in 1:length(chr)){
  
  cat(i, '번째 전처리 단어 제거 중 입니다.\n') 
  
  del.tmp <- grep(chr[i],reply_df$word)
  삭제 <- append(삭제,del.tmp)
}

삭제 <- 삭제 %>% unique()

reply_df <- reply_df[-삭제,]

(reply_df %>% 
  filter(freq >= 2))[1:30,] %>% view()

(reply_df %>% 
    filter(freq >= 2))[1:30,] %>% wordcloud2(minRotation=0, maxRotation=0, color = "#0000CD")


# a61
reply <- raw$`A61. 부산대학교에서 학생 및 학부모에게 입시 정보 제공을 위해 더욱 확대되었으면 하는 프로그램을 작성해주십시오.` %>% na.omit()
reply_df <- reply %>% extractNoun() %>% unlist() %>% table() %>% sort(decreasing = TRUE) %>% as_tibble()
names(reply_df) <- c("word","freq")


# 불용어 제거
reply_df <- reply_df %>% 
  mutate(len = str_length(word)) %>%
  filter(len >= 2) 

삭제 <- c()

reply_df %>% view()

chr <- c("없습니","좋겠습니", "같습니", "좋겠", "하게","해서","프로그","해주")

for(i in 1:length(chr)){
  
  cat(i, '번째 전처리 단어 제거 중 입니다.\n') 
  
  del.tmp <- grep(chr[i],reply_df$word)
  삭제 <- append(삭제,del.tmp)
}

삭제 <- 삭제 %>% unique()

reply_df <- reply_df[-삭제,]

(reply_df %>% 
    filter(freq >= 2))[1:30,] %>% view()

(reply_df %>% 
    filter(freq >= 2)) %>% wordcloud2(minRotation=0, maxRotation=0, color = "#0000CD")
