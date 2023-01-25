library(KoNLP)
library(tidyverse)

library(ggplot2)
library(tm)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(tidytext)
library(dplyr)
library(reshape) 

# 데이터 불러오기 및 전처리(Preprocessing)
raw1 <- read_csv(file = "D:/대학원/textmining/Doit/빅카인즈_국립대학_육성사업.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))
# bigkinds만 encoding = 'utf-8'

# 고유명사 추가
buildDictionary(ext_dic = "NIADic", user_dic = data.frame("새단어","nqq"),replace_usr_dic = TRUE)

str(raw1) # 구조확인
glimpse(raw1) # tidyverse 제공

# 중복기사 제거
raw1 <- raw1[-which(duplicated(raw1$제목)),]
glimpse(raw1)

# 필요한 변수 선택 - 변수(variable)가 무엇이냐?
raw1_df <- select(raw1,키워드,일자) # Syntax of select() select(x, variables_to_select)

raw1_df$id <- c(1:dim(raw1_df)[1])

# Frequency table
raw1_token_df <- unnest_tokens(raw1_df, input = 키워드, output = word, token = "words", drop = FALSE)

raw1_token_df_한글 <- raw1_token_df %>%  
  mutate(단어 = str_match(word,'([가-힣]+)')[,2]) %>% # "한글" variable을 만들고 한글만 저장 / ([가-힣]+)/P') 한글만을 선택하는 정규표현식        
  na.omit() %>% # 
  mutate(글자수 = str_length(단어)) %>%  # "글자수" variable을 만듭니다 
  filter(str_length(단어) >= 2) 

raw1_token_df_영어 <- raw1_token_df %>%  
  mutate(단어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
  na.omit() %>% ## 
  mutate(글자수=str_length(단어)) %>%   ## "글자수" variable을 만듭니다 
  filter(str_length(단어) >= 3) 

raw1_token_df <- bind_rows(raw1_token_df_한글, raw1_token_df_영어)
raw1_token_df <- select(raw1_token_df,-단어)

raw1_token_df$일자 <- as.integer(raw1_token_df$일자)


# 불용어 - 반복 작업 필요
제거 <- c()

chr <- c("블랙스튜디오", "반선섭", "개소식")

for(i in 1:length(chr)){
  
  cat(i, '번째 전처리 제거 단어를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(chr[i], raw1_token_df$word)
  제거 <- append(제거,del.tmp)
}

raw1_token_df <- raw1_token_df[-제거,]


# 최다 빈도 단어 Top30을 뽑습니다
token_count_table <- table(raw1_token_df$word) # 객체별 빈도를 셉니다
token_count <- sort(token_count_table, decreasing = TRUE) # 내림차순 정렬 합니다
class(token_count)
token_count30 <- head(token_count, 30)  ## Top 30까지 추립니다

# frequency table
token_count30_df <- as.data.frame(token_count30)

write.csv(frequency_table, file = "D:/대학원/textmining/Doit/frequency_table.csv", fileEncoding = 'cp949')

read_csv(file = "D:/대학원/textmining/Doit/frequency_table.csv", col_names = TRUE, locale=locale('ko',encoding = 'cp949'))

# word cloud
wordcloud2(token_count30_df, minRotation = 0, maxRotation = 0, color = 'black') 

# bargraph
token_count30_df %>% 
  ggplot(aes(x = Freq, y = reorder(Var1, Freq), fill = Freq)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = Freq),hjust = -0.1, size = 5) 

# TF-IDF
raw1_tf_idf <- raw1_token_df %>% 
  dplyr::count(키워드, word) %>% 
  filter(str_count(word) > 1) %>% 
  bind_tf_idf(term = word, document = 키워드, n = n) %>% 
  arrange(-tf_idf)

write.csv(raw1_tf_idf, file = "D:/대학원/textmining/Doit/raw1_tf_idf.csv", fileEncoding = 'cp949')


raw1_token_count_df <- dplyr::count(raw1_token_df, 키워드, word)
# tally(group_by(raw1_token_df, 키워드, word))
raw1_token_count_df <- dplyr::filter(raw1_token_count_df, str_count(word) > 1)
raw1_token_count_df <- bind_tf_idf(raw1_token_count_df, term = word, document = 키워드, n = n) 
raw1_tf_idf <- dplyr::arrange(raw1_token_count_df, -tf_idf)

dplyr::arrange(bind_tf_idf(dplyr::filter(dplyr::count(raw1_token_df, 키워드, word), str_count(word) > 1), term = word, document = 키워드, n = n), -tf_idf) 
