library(KoNLP) # 가장 먼저 불러오기

v1 <- c("선생님 오늘 말고 내일 회신 주셔도 됩니다. 다름아니라, 빅카이즈 활용하지 않고 간단한 문장을 단어 분리 해 보고 싶어서 실습해 보았습니다. 그런데 아래와 같이 나타났습니다.")
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 
extractNoun("한국사 길잡이")
v1 %>% SimplePos09()

# KoNLP useNIADic
install.packages("multilinguer")
library(multilinguer)
install_jdk() # rjava 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#
install.packages("remotes")
devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-18.0.1.1')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 

# package
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tm)
library(NLP)
library(qdap)
library(wordcloud2)
library(stringr)
library(rJava)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(tidytext)
library(ggthemes)
library(widyr)
library(ggraph)
library(tidygraph)
library(tidytext)

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
10 %/% 3 # 나눗셈의 몫

## 할당 연산자
x1 <- 1:10
x2 = c("이연동","이연동2")
1001:1100 -> x3 # vector

## 논리 연산자 (TRUE, FALSE)
10 < 11
"이연동" < "이연동"
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
data(mpg)
class(mpg)

head(mpg)

head(mpg, 10)

tail(mpg, 20)

str(mpg)
glimpse(mpg) # tidyverse

dim(mpg)
nrow(mpg)
ncol(mpg)

mpg$manufacturer

## 색인(index, 첨자) 종류 
View(mpg)

mpg[  ,1]
mpg[10, ]

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
숫자1 <- c(1:3)
class(숫자1)
숫자2 <- c("1":"3")
class(숫자2)

# memory
memory.size(20000)
memory.limit()

# 데이터 불러오기 및 전처리(Preprocessing)
raw1 <- read_csv(file = "D:/대학원/textmining/Doit/빅카인즈_국립대학_육성사업.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))
# bigkinds만 encoding = 'utf-8'

# 고유명사 추가
buildDictionary(ext_dic = "NIADic", user_dic = data.frame("새단어","nqq"),replace_usr_dic = TRUE)

str(raw1) # 구조확인
glimpse(raw1) # tidyverse 제공

# 중복기사 제거
raw1 <- distinct(raw1, 제목, .keep_all = TRUE)
glimpse(raw1)
View(raw1)

# 필요한 변수 선택 - 변수(variable)가 무엇이냐?
raw1_df <- select(raw1, 일자, 언론사, 키워드) # Syntax of select() select(x, variables_to_select)
View(raw1_df)
raw1_df

# 일자별 정렬
raw1_df <- arrange(raw1_df, 일자)

nrow(raw1_df)
ncol(raw1_df)

dim(raw1_df)
dim(raw1_df)[1] # index 첨자 
dim(raw1_df)[2] 


raw1_df$id <- 1:dim(raw1_df)[1]
View(raw1_df)

# 월별추이
raw1_df$일자 <- str_sub(raw1_df$일자,1,6) # 20190301 -> 201903      
View(raw1_df)

class(raw1_df)
class(raw1_df$일자)
raw1_table <- table(raw1_df$일자)
class(raw1_table)

raw1_df_월별_table <- as.data.frame(raw1_table) # raw1_df$일자 %>% table() %>% as.data.frame()
names(raw1_df_월별_table)

names(raw1_df_월별_table) <- c("날짜_월별","빈도")
raw1_df_월별_table

ggplot(data = raw1_df_월별_table, aes(x = 날짜_월별, y = 빈도, group = 1)) + 
  geom_line(size = 2, colour="#006600") + 
  geom_point(size = 5, colour="#006600") +
  coord_cartesian(ylim = c(0, 30)) +
  geom_text(aes(label = 빈도),hjust = 0.5, vjust = -1, size = 5) +
  geom_hline(yintercept = mean(raw1_df_월별_table$빈도), color='red',linetype='dashed', size = 1) +
  labs(x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 15))


