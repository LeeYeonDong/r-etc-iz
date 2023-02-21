library(KoNLP)
library(tidyverse)

library(ggplot2)
library(tm)
library(qdap)
library(corpus)
library(stringr)
library(rJava)
library(tidytext)
library(dplyr)
library(reshape) 

# 데이터 불러오기 및 전처리(Preprocessing)
raw1 <- read_csv(file = "D:/대학원/textmining/Doit/대학혁신지원사업.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))

raw1 %>% str()

raw2 <- read_csv(file = "D:/대학원/textmining/Doit/대학재정지원사업.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))

raw2 %>% str()
raw2$`뉴스 식별자` <- raw2$`뉴스 식별자` %>% as.character()
  
raw3 <- bind_rows(raw1, raw2)

# bigkinds만 encoding = 'utf-8'

# 고유명사 추가
buildDictionary(ext_dic = "NIADic", user_dic = data.frame("새단어","nqq"),replace_usr_dic = TRUE)

str(raw3) # 구조확인
glimpse(raw3) # tidyverse 제공

# 중복기사 제거
raw3 <- distinct(raw3, 제목, .keep_all = TRUE)
glimpse(raw3)

# 인사이동 기사 제거
인사 <- grep("인사이동", raw3$키워드)
raw3 <- raw3[-인사,]


# 필요한 변수 선택 - 변수(variable)가 무엇이냐?
raw3_df <- select(raw3, 일자, 언론사, 키워드, 제목) # Syntax of select() select(x, variables_to_select)

# id 부여
raw3_df$id <- c(1:dim(raw3_df)[1])

# 키워드 바꾸기
raw3_df$키워드 <- gsub("블랙 스튜디오", "블랙스튜디오", raw3_df$키워드)

grep("블랙스튜디오", raw3_df$키워드)
View(raw3_df)

# 월별추이
raw3_df$일자 <- str_sub(raw3_df$일자,1,6)

# Frequency table
raw3_token_df <- unnest_tokens(raw3_df, input = 키워드, output = word, token = "words")

raw3_token_df_한글 <- raw3_token_df %>%  
  mutate(단어 = str_match(word,"([가-힣]+)")[,2]) %>% # "한글" variable을 만들고 한글만 저장 / ([가-힣]+)/P') 한글만을 선택하는 정규표현식        
  na.omit() %>% # 
  filter(str_length(단어) >= 2) 

raw3_token_df_영어 <- raw3_token_df %>%  
  mutate(단어 = str_match(word,"([a-zA-Z]+)")[,2]) %>% ## "영어" variable을 만들고 영어만 저장         
  na.omit() %>% 
  filter(str_length(단어) >= 3) 

raw3_token_df <- bind_rows(raw3_token_df_한글, raw3_token_df_영어)
raw3_token_df <- select(raw3_token_df, -word)

raw3_token_df$일자 <- as.integer(raw3_token_df$일자)

View(head(raw3_token_df, 1000))


# 불용어 - 반복 작업 필요
stopwords_kor <- read_csv(file = "D:/대학원/textmining/Doit/stopwords_kor.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))

stopwords_kor

chr <- stopwords_kor$stopwords_kor



제거 <- c()

for(i in 1:length(chr)){

  cat(i, '번째 전처리 제거 단어를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(chr[i], raw3_token_df$단어)
  제거 <- append(제거,del.tmp)
}

raw3_token_df <- raw3_token_df[-제거,]
raw3_token_df %>% head(1000) %>% view()
## lec.3

# 최다 빈도 단어 Top30을 뽑습니다
token_count_table <- table(raw3_token_df$단어) # 객체별 빈도를 셉니다
token_count <- sort(token_count_table, decreasing = TRUE) # 내림차순 정렬 합니다
class(token_count)
token_count30 <- head(token_count, 30)  ## Top 30까지 추립니다

# frequency table
token_count30_df <- as.data.frame(token_count30)

write.csv(token_count30_df, file = "D:/대학원/textmining/Doit/frequency_table.csv", fileEncoding = 'cp949')

read_csv(file = "D:/대학원/textmining/Doit/frequency_table.csv", col_names = TRUE, locale=locale('ko',encoding = 'cp949'))

# word cloud
library(devtools)
# devtools::install_github("lchiffon/wordcloud2") # 기존 wordcloud2 패키지 제거
library(wordcloud2)

wordcloud2(token_count30, minRotation = 0, maxRotation = 0, color = "black") 
wordcloud2(token_count30)
wordcloud2(token_count30, minRotation = pi/6, maxRotation = -pi/6) 

# 글자크기로 순위를 정함
wordcloud2(token_count, minSize = 5) 

# 글자색
wordcloud2(token_count30, color = "random-light", minRotation = 0, maxRotation = 0) 
wordcloud2(data = token_count30, color = "random-light", backgroundColor = "black")
wordcloud2(token_count30, color = "random-light", backgroundColor = "black")
wordcloud2(token_count30, color = "random-dark", backgroundColor = "grey")

# letterCloud
letterCloud(data = head(token_count,1000), word = "대학", wordSize = 10)

# use figure file
wordcloud2(head(token_count,1000), figPath = "D:/대학원/example.png", color = "random-dark")
wordcloud2(head(token_count,1000), figPath = "D:/대학원/example1.png", color = "random-dark", size = 5)

# bargraph
token_count30_df <- as.data.frame(token_count30)

token_count30_df %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Freq)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 20)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF") +
  geom_text(aes(label = Freq),hjust = -0.1, size = 5) 

# TF-IDF
raw3_count <- count(raw3_token_df, id, 단어)
raw3_count <- filter(raw3_count, n > 3)
raw3_tf_idf <- bind_tf_idf(raw3_count, term = 단어, document = id, n = n)
raw3_tf_idf <- arrange(raw3_tf_idf, -tf_idf)

raw3_tf_idf %>% filter(단어 == "장학금")

summary(raw3_tf_idf$tf_idf)

raw3_tf <- raw3_token_df %>% 
  count(id, 단어) %>% 
  filter(n > 10) %>% 
  bind_tf_idf(term = 단어, document = id, n = n) %>% 
  arrange(-tf)

raw3_tf100 <- head(raw3_tf,100)

write.csv(raw3_tf_idf100, file = "D:/대학원/textmining/Doit/raw3_tf_idf100.csv", fileEncoding = 'cp949')



# raw3_df을 대학 유형별로 나누기
library(readxl)

# 전문대학교 리스트 만들기
전문대학교 <- read_excel(path = "D:/대학원/textmining/Doit/고등교육기관 주소록.xlsx", sheet = 1)
names(전문대학교) <- 전문대학교[1,] %>% as.character()
전문대학교 <- 전문대학교[-1,]

전문대학교 <- 전문대학교$대학명
전문대학 <- gsub("대학교", "대학", 전문대학교)
전문대 <- gsub("대학교", "대", 전문대학교)

전문대_리스트 <- c(전문대학교,전문대학,전문대)

# 전문대 기사 위치
전문대_기사 <- c()

for(i in 1:length(전문대_리스트)){
  
  cat(i, '번째 전문대가 들어간 기사를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(전문대_리스트[i], raw3_df$키워드)
  전문대_기사 <- append(전문대_기사,del.tmp)
}

# 전문대 기사 제거
전문대_기사 <- unique(전문대_기사)
raw3_df <- raw3_df[-전문대_기사,]


# 국립대학교
국립대학교 <- read_excel(path = "D:/대학원/textmining/Doit/고등교육기관 주소록.xlsx", sheet = 2)
names(국립대학교) <- 국립대학교[1,] %>% as.character()
국립대학교 <- 국립대학교[-1,]

국립대학교 <- 국립대학교$대학명
국립대학 <- gsub("대학교", "대학", 국립대학교)
국립대 <- gsub("대학교", "대", 국립대학교)

국립대_리스트 <- c(국립대학교,국립대학,국립대)

# 국립대 기사 추출
국립대_기사 <- c()

for(i in 1:length(국립대_리스트)){
  
  cat(i, '번째 국립대가 들어간 기사를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(국립대_리스트[i], raw3_df$키워드)
  국립대_기사 <- append(국립대_기사,del.tmp)
}

국립대_기사 <- unique(국립대_기사)
국립대_df <- raw3_df[국립대_기사,]


# 사립대학교
사립대학교 <- read_excel(path = "D:/대학원/textmining/Doit/고등교육기관 주소록.xlsx", sheet = 3)
names(사립대학교) <- 사립대학교[1,] %>% as.character()
사립대학교 <- 사립대학교[-1,]

사립대학교 <- 사립대학교$대학명
사립대학 <- gsub("대학교", "대학", 사립대학교)
사립대 <- gsub("대학교", "대", 사립대학교)

사립대_리스트 <- c(사립대학교,사립대학,사립대)

# 사립대 기사 추출
사립대_기사 <- c()

for(i in 1:length(사립대_리스트)){
  
  cat(i, '번째 사립대가 들어간 기사를 찾는 중 입니다.\n') 
  
  del.tmp <- grep(사립대_리스트[i], raw3_df$키워드)
  사립대_기사 <- append(사립대_기사,del.tmp)
}

사립대_기사 <- unique(사립대_기사)
사립대_df <- raw3_df[사립대_기사,]


# 국립대 사립대 외 기사 
국_사_기사 <- c(국립대_기사, 사립대_기사) %>% unique()
국_사_df <- raw3_df[국_사_기사,]
국_사_외_df <- raw3_df[-국_사_기사,]

