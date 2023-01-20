library(KoNLP) # 가장 먼저 불러오기

v1 <- c("선생님 오늘 말고 내일 회신 주셔도 됩니다. 다름아니라, 빅카이즈 활용하지 않고 간단한 문장을 단어 분리 해 보고 싶어서 실습해 보았습니다. 그런데 아래와 같이 나타났습니다.")
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 
extractNoun(v1)

# 1. https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/scala-library-2.11.8.jar 여기에 접속하셔서 <scala-library-2.11.8> 파일을 다운
# 2. C:\Users\선생님 노트북 이름\AppData\Local\R\win-library\4.2\KoNLP\java에 가셔서 다운받은 scala-library-2.11.8 파일을 붙여넣기한다
# 3. C:\Program Files\R\R-4.2.2\library 로 간다
# 4. 아래 KoNLP 다운받아 C:\Program Files\R\R-4.2.2\library에 붙여넣은 다음 KoNLP폴더를 압축을 푼다
# 5. R studio를 모두 끄고 다시 실행한다
library(tidyverse)

## 산술 연산자(arithmetic operators)
(20+3*2)/3
20^2
10 %% 3 # 나눗셈의 나머지
12 %/% 5 # 나눗셈의 몫

## 할당 연산자
x1 <- 1:10
x2 = 101:110
1001:1100 -> x3

## 논리 연산자 (TRUE, FALSE)
10 < 11
100 < 29
10 <= 11
100 <= 29

10 == 10 # 정확히 같음(Exactly equal to)
10 == 29

10 != 29 # 같지 않음(Not equal to)
10 != 10

## 수열(sequence)의 생성
1:10

seq(from = 1, to = 5, by = 0.5)

rep(x = 10, times = 5)
rep(x = 1:5, times = 2)

## 값(value)을 변수에 할당
x <- 3*4
print(x)
x

## 함수에 대한 도움말
help(summary)
?summary
??summary

summary(1:10)

## 객체 종류 
벡터_숫자 <- c(1,2,3) # 숫자형 벡터
벡터_숫자1 <- c("1","2","3") # 문자형 벡터
벡터_문자 <- c("일", "이", "삼")

행렬 <- matrix(c(1:10), ncol = 2, nrow = 5)
matrix(c(1:10), ncol = 2, byrow = TRUE)
matrix(c(1:10), ncol = 2, byrow = FALSE)

행1 <- c(1:10)
행2 <- c(11:20)

데이터프레임 <- data.frame(행1,행2)

행a <- c(1:10000)
행b <- c(10001:20000)
data.frame(행a,행b)

티블 <- tibble(행a,행b) # tidyverse에서 제공하는 data.frame
리스트 <- list(데이터프레임,티블)

## 객체 확인
class(벡터)
class(행렬)
class(행1)
class(데이터프레임)
class(티블)
class(리스트)

## 객체 변환
벡터_숫자 <- as.character(벡터_숫자)
class(벡터_숫자)

class(as.integer(벡터_숫자))

행렬 %>% class()
행렬 %>% as.data.frame() %>% class()

# 요소(factor) 변환
data(mpg)
mpg
ggplot(mpg, aes(x = cty, y= hwy, colour = cyl)) + 
  geom_point()
# "cyl" 숫자가 높거나 낮은건 단순히 cyl의 차이 
ggplot(mpg, aes(x = cty, y= hwy, colour = factor(cyl))) + 
  geom_point()


## 데이터 프레임 살펴보기
head(mpg,10)
tail(mpg,20)

str(mpg)

dim(mpg)
nrow(mpg)
ncol(mpg)

## 색인(index, 첨자) 종류 
view(mpg)

mpg[1,4]
mpg[20,5]

mpg[1,]
mpg[c(1:10),] # 1:10 가능
mpg[,1]
mpg[,1:3]
mpg[,c("class")]
mpg[,c("cty","hwy")]

# negative index
mpg[-1,]
mpg[-c(1:10),]
mpg[,-1]
mpg[,-c(1:3)]
mpg[,-c("class")]
mpg[,-c("cty","hwy")]

# $(선택 : 벡터)
mpg[,c("manufacturer")] # 콘크리트 바른 벽돌 일부를 통째로 들어내는 격
mpg$manufacturer  # 콘크리트 바른 벽돌을 하나하나 분리


#### textmining
install.packages("corpus")
install.packages("qdap")
install.packages("tm")
install.packages("wordcloud2")
install.packages("topicmodels")
install.packages("ldatuning")
install.packages("dplyr")
install.packages("tidytext")
install.packages("stringr")
install.packages("plyr")
install.packages("rJava")
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

# memory
memory.size(20000)
memory.limit()

# 데이터 불러오기 및 전처리(Preprocessing)
raw1 <- read_csv(file = "D:/대학원/textmining/Doit/빅카인즈_국립대학_육성사업.csv", col_names = TRUE, locale=locale('ko',encoding='utf-8'))
# bigkinds만 encoding='utf-8'

# 고유명사 추가
buildDictionary(ext_dic = "NIADic", user_dic = data.frame("아파트","nqq"),replace_usr_dic = TRUE)

str(raw1) # 구조확인
glimpse(raw1) #tidyverse 제공

raw1 <- raw1[-which(duplicated(raw1$제목)),]

# 필요한 변수 선택 - 변수(variable)가 무엇이냐?
raw1_df <- select(raw1,"키워드","일자") # Syntax of select() select(x, variables_to_select)
raw1_df <- arrange(raw1_df, 일자)
raw1_df$일자

nrow(raw1_df)
ncol(raw1_df)

dim(raw1_df)
dim(raw1_df)[1]
dim(raw1_df)[2]

raw1_df$id <- c(1:dim(raw1_df)[1])

# 월별추이
raw1_df$일자 <- str_sub(raw1_df$일자,1,6)         

raw1_df_월별_table <- as.data.frame(table(raw1_df$일자)) # raw1_df$일자 %>% table() %>% as.data.frame()
names(raw1_df_월별_table) <- c("날짜_월별","Freq")

ggplot(data = raw1_df_월별_table, aes(x = 날짜_월별, y = Freq, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 1, colour="#006600") +
  coord_cartesian(ylim = c(0, 30)) + 
  geom_text(aes(label = Freq),hjust = 0.5, vjust = -1, size = 7) +
  geom_hline(yintercept = mean(raw1_df_월별_table$Freq), color='red',linetype='dashed', size = 1) +
  labs(x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 15))


# Frequency table
raw1_token_df <- unnest_tokens(raw1_df, input = 키워드, output = word, token = "words", drop = FALSE)
  
raw1_token_df_한글 <- raw1_token_df %>%  
    mutate(단어 = str_match(word,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수 = str_length(단어)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(단어) >= 2) 
  
raw1_token_df_영어 <- raw1_token_df %>%  
    mutate(단어 = str_match(word,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수=str_length(단어)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(단어) >= 3) 
  
raw1_token_df <- bind_rows(raw1_token_df_한글, raw1_token_df_영어)
raw1_token_df <- select(raw1_token_df,-c(키워드,단어))

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
token_count30 <- head(token_count, 30)  ## Top 30까지 추립니다

# frequency table
frequency_table = as.data.frame(token_count30)

write.csv(frequency_table, file = "D:/대학원/textmining/Doit/frequency_table.csv", fileEncoding = 'cp949')

read_csv(file = "D:/대학원/textmining/Doit/frequency_table.csv", col_names = TRUE, locale=locale('ko',encoding = 'cp949'))

# word cloud
token_count30_df <- as.data.frame(token_count30)
wordcloud2(token_count30_df, minRotation = 0, maxRotation = 0, color = 'black') 
