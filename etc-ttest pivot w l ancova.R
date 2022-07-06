library(reshape2)
library(dplyr)
library(tidyr)
library(HH)

# p102 병상수
미참여 <- c(98,29,116,72,19)
참여 <- c(0,0,4,2,0)
병상수 <- c("30미만","30인이상-50인미만","50인이상-100인미만","100인이상-200인미만","200인이상")

병상수df <- tibble(미참여,참여,병상수)
병상수df_l <- 병상수df %>% pivot_longer(names_to = "참여",
                                cols = c("미참여","참여"),
                                values_to = "value")
# t test
t.test(병상수df$미참여, 병상수df$참여, var.equal = FALSE)

# paired t test
t.test(병상수df$미참여, 병상수df$참여, var.equal = FALSE, paired = TRUE)

# ancova
병상수df_l$참여 <- gsub("미참여",0,병상수df_l$참여)
병상수df_l$참여 <- gsub("참여",1,병상수df_l$참여)
병상수df_l$참여 <- 병상수df_l$참여 %>% as.factor()

병상수df_l$병상수 <- gsub("30미만",0,병상수df_l$병상수)
병상수df_l$병상수 <- gsub("30인이상-50인미만",1,병상수df_l$병상수)
병상수df_l$병상수 <- gsub("50인이상-100인미만",2,병상수df_l$병상수)
병상수df_l$병상수 <- gsub("100인이상-200인미만",3,병상수df_l$병상수)
병상수df_l$병상수 <- gsub("200인이상",4,병상수df_l$병상수)
병상수df_l$병상수 <- 병상수df_l$병상수 %>% as.integer()

ancova(병상수 ~ 참여 * value, data = 병상수df_l)
# "*" 교호작용확인, "+" 공분산분석


# p102 간호사수
미참여 <- c(8,21,52,28,23,11,184)
참여 <- c(0,1,0,0,1,3,3)
간호사수 <- c("0명","1명","2명","3명","4명","5명","5명이상")

간호사수df <- tibble(미참여,참여,간호사수)
간호사수df_l <- 간호사수df %>% pivot_longer(names_to = "참여",
                                  cols = c("미참여","참여"),
                                  values_to = "value")
# t test
t.test(간호사수df$미참여, 간호사수df$참여, var.equal = FALSE)

# paired t test
t.test(간호사수df$미참여, 간호사수df$참여, var.equal = FALSE, paired = TRUE)

# ancova
간호사수df_l$참여 <- gsub("미참여",0,간호사수df_l$참여)
간호사수df_l$참여 <- gsub("참여",1,간호사수df_l$참여)
간호사수df_l$참여 <- 간호사수df_l$참여 %>% as.factor()

간호사수df_l$간호사수 <- gsub("0명",0,간호사수df_l$간호사수)
간호사수df_l$간호사수 <- gsub("1명",1,간호사수df_l$간호사수)
간호사수df_l$간호사수 <- gsub("2명",2,간호사수df_l$간호사수)
간호사수df_l$간호사수 <- gsub("3명",3,간호사수df_l$간호사수)
간호사수df_l$간호사수 <- gsub("4명",4,간호사수df_l$간호사수)
간호사수df_l$간호사수 <- gsub("5명",5,간호사수df_l$간호사수)
간호사수df_l$간호사수 <- gsub("5이상",6,간호사수df_l$간호사수)
간호사수df_l$간호사수 <- 간호사수df_l$간호사수 %>% as.integer()

ancova(간호사수 ~ 참여 * value, data = 간호사수df_l) 


# p102 간호조무사
미참여 <- c(8,21,52,28,23,11,184)
참여 <- c(0,1,0,0,1,3,3)
간호조무사수 <- c("0명","1명","2명","3명","4명","5명","5명이상")

간호조무사수df <- tibble(미참여,참여,간호조무사수)
간호조무사수df_l <- 간호조무사수df %>% pivot_longer(names_to = "참여",
                                        cols = c("미참여","참여"),
                                        values_to = "value")
# t test
t.test(간호조무사수df$미참여, 간호조무사수df$참여, var.equal = FALSE)

# paired t test
t.test(간호조무사수df$미참여, 간호조무사수df$참여, var.equal = FALSE, paired = TRUE)

# ancova
간호조무사수df_l$참여 <- gsub("미참여",0,간호조무사수df_l$참여)
간호조무사수df_l$참여 <- gsub("참여",1,간호조무사수df_l$참여)
간호조무사수df_l$참여 <- 간호조무사수df_l$참여 %>% as.factor()

간호조무사수df_l$간호조무사수 <- gsub("0명",0,간호조무사수df_l$간호조무사수)
간호조무사수df_l$간호조무사수 <- gsub("1명",1,간호조무사수df_l$간호조무사수)
간호조무사수df_l$간호조무사수 <- gsub("2명",2,간호조무사수df_l$간호조무사수)
간호조무사수df_l$간호조무사수 <- gsub("3명",3,간호조무사수df_l$간호조무사수)
간호조무사수df_l$간호조무사수 <- gsub("4명",4,간호조무사수df_l$간호조무사수)
간호조무사수df_l$간호조무사수 <- gsub("5명",5,간호조무사수df_l$간호조무사수)
간호조무사수df_l$간호조무사수 <- gsub("5명이상",6,간호조무사수df_l$간호조무사수)
간호조무사수df_l$간호조무사수 <- 간호조무사수df_l$간호조무사수 %>% as.integer()

ancova(간호조무사수 ~ 참여 * value, data = 간호조무사수df_l) 


# 103p 성별
미참여 <- c(16,54,16,54)*70/42
참여 <- c(6,36,6,36)
성별 <- rep(c("남성","여성"),length(미참여)/2)

성별df <- tibble(미참여,참여,성별)
성별df_l <- 성별df %>% pivot_longer(names_to = "참여",
                                cols = c("미참여","참여"),
                                values_to = "value")
# t test
t.test(성별df$미참여, 성별df$참여, var.equal = FALSE)

# paired t test
t.test(성별df$미참여, 성별df$참여, var.equal = FALSE, paired = TRUE)

# ancova
성별df_l$참여 <- gsub("미참여",0,성별df_l$참여)
성별df_l$참여 <- gsub("참여",1,성별df_l$참여)
성별df_l$참여 <- 성별df_l$참여 %>% as.factor()

성별df_l$성별 <- gsub("남성",0,성별df_l$성별)
성별df_l$성별 <- gsub("여성",1,성별df_l$성별)
성별df_l$성별 <- 성별df_l$성별 %>% as.integer()

ancova(성별 ~ 참여 * value, data = 성별df_l) 

# 103p 연령
미참여 <- c(1,6,24,39)
참여 <- c(2,2,16,22)
연령 <- c("65세미만","65세이상-75세미만","75세이상-85세미만","85세이상")


연령df <- tibble(미참여,참여,연령)
연령df_l <- 연령df %>% pivot_longer(names_to = "참여",
                                cols = c("미참여","참여"),
                                values_to = "value")
# t test
t.test(연령df$미참여, 연령df$참여, var.equal = FALSE)

# paired t test
t.test(연령df$미참여, 연령df$참여, var.equal = FALSE, paired = TRUE)

# ancova
연령df_l$참여 <- gsub("미참여",0,연령df_l$참여)
연령df_l$참여 <- gsub("참여",1,연령df_l$참여)
연령df_l$참여 <- 연령df_l$참여 %>% as.factor()

연령df_l$연령 <- gsub("65세미만",0,연령df_l$연령)
연령df_l$연령 <- gsub("65세이상-75세미만",1,연령df_l$연령)
연령df_l$연령 <- gsub("75세이상-85세미만",2,연령df_l$연령)
연령df_l$연령 <- gsub("85세이상",3,연령df_l$연령)
연령df_l$연령 <- 연령df_l$연령 %>% as.integer()

ancova(연령 ~ 참여 * value, data = 연령df_l) 


# 103p 등급
미참여 <- c(12,13,23,14)*70/42
참여 <- c(7,9,13,12)
등급 <- c("1등급","2등급","3등급","4등급")


등급df <- tibble(미참여,참여,등급)
등급df_l <- 등급df %>% pivot_longer(names_to = "참여",
                                cols = c("미참여","참여"),
                                values_to = "value")
# t test
t.test(등급df$미참여, 등급df$참여, var.equal = FALSE)

# paired t test
t.test(등급df$미참여, 등급df$참여, var.equal = FALSE, paired = TRUE)

# ancova
등급df_l$참여 <- gsub("미참여",0,등급df_l$참여)
등급df_l$참여 <- gsub("참여",1,등급df_l$참여)
등급df_l$참여 <- 등급df_l$참여 %>% as.factor()

등급df_l$등급 <- gsub("1등급",0,등급df_l$등급)
등급df_l$등급 <- gsub("2등급",1,등급df_l$등급)
등급df_l$등급 <- gsub("3등급",2,등급df_l$등급)
등급df_l$등급 <- gsub("4등급",3,등급df_l$등급)
등급df_l$등급 <- 등급df_l$등급 %>% as.integer()

ancova(등급 ~ 참여 * value, data = 등급df_l) 

# 103p 자격
미참여 <- c(1,16,23,30)*70/42
참여 <- c(0,16,9,17)
자격 <- c("0","1","2","3")

자격df <- tibble(미참여,참여,자격)
자격df_l <- 자격df %>% pivot_longer(names_to = "참여",
                                cols = c("미참여","참여"),
                                values_to = "value")
# t test
t.test(자격df$미참여, 자격df$참여, var.equal = FALSE)

# paired t test
t.test(자격df$미참여, 자격df$참여, var.equal = FALSE, paired = TRUE)

# ancova
자격df_l$참여 <- gsub("미참여",0,자격df_l$참여)
자격df_l$참여 <- gsub("참여",1,자격df_l$참여)
자격df_l$참여 <- 자격df_l$참여 %>% as.factor()

자격df_l$자격 <- gsub("0",0,자격df_l$자격)
자격df_l$자격 <- gsub("1",1,자격df_l$자격)
자격df_l$자격 <- gsub("2",2,자격df_l$자격)
자격df_l$자격 <- gsub("3",3,자격df_l$자격)
자격df_l$자격 <- 자격df_l$자격 %>% as.integer()

ancova(자격 ~ 참여 * value, data = 자격df_l) 


# 103p 간호처치
미참여 <- c(0,1,1,1,5,0,1,0,0)*70/42
참여 <- c(0,0,0,0,2,0,0,0,1)
간호처치 <- c("기관지절개간호","흡인","산소요법","욕창간호","경관영양_튜브급식","암성통증간호","도뇨관리","장루_인공항문간호","당뇨발간호")

간호처치df <- tibble(미참여,참여,간호처치)
간호처치df_l <- 간호처치df %>% pivot_longer(names_to = "참여",
                                    cols = c("미참여","참여"),
                                    values_to = "value")
# t test
t.test(간호처치df$미참여, 간호처치df$참여, var.equal = FALSE)

# paired t test
t.test(간호처치df$미참여, 간호처치df$참여, var.equal = FALSE, paired = TRUE)

# ancova
간호처치df_l$참여 <- gsub("미참여",0,간호처치df_l$참여)
간호처치df_l$참여 <- gsub("참여",1,간호처치df_l$참여)
간호처치df_l$참여 <- 간호처치df_l$참여 %>% as.factor()

간호처치df_l$간호처치 <- gsub("기관지절개간호",0,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("흡인",1,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("산소요법",2,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("욕창간호",3,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("경관영양_튜브급식",4,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("암성통증간호",5,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("도뇨관리",6,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("장루_인공항문간호",7,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- gsub("당뇨발간호",8,간호처치df_l$간호처치)
간호처치df_l$간호처치 <- 간호처치df_l$간호처치 %>% as.integer()

ancova(간호처치 ~ 참여 * value, data = 간호처치df_l) 


# 103p 간호처치수
미참여 <- c(64,5,0,0,1)*70/42
참여 <- c(39,3,0,0,0)
간호처치수 <- c("0개","1개","2개","3개","4개")

간호처치수df <- tibble(미참여,참여,간호처치수)
간호처치수df_l <- 간호처치수df %>% pivot_longer(names_to = "참여",
                                      cols = c("미참여","참여"),
                                      values_to = "value")
# t test
t.test(간호처치수df$미참여, 간호처치수df$참여, var.equal = FALSE)

# paired t test
t.test(간호처치수df$미참여, 간호처치수df$참여, var.equal = FALSE, paired = TRUE)

# ancova
간호처치수df_l$참여 <- gsub("미참여",0,간호처치수df_l$참여)
간호처치수df_l$참여 <- gsub("참여",1,간호처치수df_l$참여)
간호처치수df_l$참여 <- 간호처치수df_l$참여 %>% as.factor()

간호처치수df_l$간호처치수 <- gsub("0개",0,간호처치수df_l$간호처치수)
간호처치수df_l$간호처치수 <- gsub("1개",1,간호처치수df_l$간호처치수)
간호처치수df_l$간호처치수 <- gsub("2개",2,간호처치수df_l$간호처치수)
간호처치수df_l$간호처치수 <- gsub("3개",3,간호처치수df_l$간호처치수)
간호처치수df_l$간호처치수 <- gsub("4개",4,간호처치수df_l$간호처치수)
간호처치수df_l$간호처치수 <- 간호처치수df_l$간호처치수 %>% as.integer()

ancova(간호처치수 ~ 참여 * value, data = 간호처치수df_l) 


# 103p 주상병
미참여 <- c(30,15,12,8,1,4)*70/42
참여 <- c(1,7,6,8,3,17)
주상병 <- c("1점","2점","3점","4점","5점","6점이상")

주상병df <- tibble(미참여,참여,주상병)
주상병df_l <- 주상병df %>% pivot_longer(names_to = "참여",
                                  cols = c("미참여","참여"),
                                  values_to = "value")
# t test
t.test(주상병df$미참여, 주상병df$참여, var.equal = FALSE)

# paired t test
t.test(주상병df$미참여, 주상병df$참여, var.equal = FALSE, paired = TRUE)

# ancova
주상병df_l$참여 <- gsub("미참여",0,주상병df_l$참여)
주상병df_l$참여 <- gsub("참여",1,주상병df_l$참여)
주상병df_l$참여 <- 주상병df_l$참여 %>% as.factor()

주상병df_l$주상병 <- gsub("1점",0,주상병df_l$주상병)
주상병df_l$주상병 <- gsub("2점",1,주상병df_l$주상병)
주상병df_l$주상병 <- gsub("3점",2,주상병df_l$주상병)
주상병df_l$주상병 <- gsub("4점",3,주상병df_l$주상병)
주상병df_l$주상병 <- gsub("5점",4,주상병df_l$주상병)
주상병df_l$주상병 <- gsub("6점이상",5,주상병df_l$주상병)
주상병df_l$주상병 <- 주상병df_l$주상병 %>% as.integer()

ancova(주상병~ 참여 * value, data = 주상병df_l) 


############################
#1인당 의료비용 의료이용 - *70/42
# 주요처치
##########################


# 117p 급여비용_미참
미참여 <- c(0,0,2974930,57680,0,5593640,47850,0,40401410,840330,53890,39785140,0,18807060,2947100,0,340970,0,6830,6380,0,617030,0,8970810)*70/42
참여 <- c(9761310,0,18248290,6565380,166950,0,0,0,0,19280270,59686180,0,0,0,0,0,0,0,0,0,0,28973620,1203710,0)
급여비용_전후 <- rep(c("전","후"),length(미참여)/2)

급여비용_전후df <- tibble(미참여,참여,급여비용_전후)
급여비용_전후df_l <- 급여비용_전후df %>% pivot_longer(names_to = "참미",
                                        cols = c("미참여","참여"),
                                        values_to = "value")
# t test
t.test(급여비용_전후df$미참여, 급여비용_전후df$참여, var.equal = FALSE)

# paired t test
t.test(급여비용_전후df$미참여, 급여비용_전후df$참여, var.equal = FALSE, paired = TRUE)

# ancova
급여비용_전후df_l$참미 <- gsub("미참여",0,급여비용_전후df_l$참미)
급여비용_전후df_l$참미 <- gsub("참여",1,급여비용_전후df_l$참미)
급여비용_전후df_l$참미 <- 급여비용_전후df_l$참미 %>% as.factor()

급여비용_전후df_l$급여비용_전후 <- gsub("전",0,급여비용_전후df_l$급여비용_전후)
급여비용_전후df_l$급여비용_전후 <- gsub("후",1,급여비용_전후df_l$급여비용_전후)
급여비용_전후df_l$급여비용_전후 <- 급여비용_전후df_l$급여비용_전후 %>% as.integer()

ancova(급여비용_전후 ~ 참미 * value, data = 급여비용_전후df_l) 


# 117p 급여비용_전후
전 <- c(9761310,0,18248290,6565380,166950,0,0,0,0,19280270,59686180,0,0,0,2974930,57680,0,5593640,47850,0,40401410,840330,53890,39785140)
후 <- c(0,0,0,0,0,0,0,0,0,28973620,1203710,0,0,18807060,2947100,0,340970,0,6380,6380,0,617030,0,8970810)
급여비용_참미 <- rep(c("참여","미참여"),length(후)/2)

급여비용_참미df <- tibble(전,후,급여비용_참미)
급여비용_참미df_l <- 급여비용_참미df %>% pivot_longer(names_to = "전후",
                                        cols = c("전","후"),
                                        values_to = "value")
# t test
t.test(급여비용_참미df$전, 급여비용_참미df$후, var.equal = FALSE)

# paired t test
t.test(급여비용_참미df$전, 급여비용_참미df$후, var.equal = FALSE, paired = TRUE)

# ancova
급여비용_참미df_l$전후 <- gsub("전",0,급여비용_참미df_l$전후)
급여비용_참미df_l$전후 <- gsub("후",1,급여비용_참미df_l$전후)
급여비용_참미df_l$전후 <- 급여비용_참미df_l$전후 %>% as.factor()

급여비용_참미df_l$급여비용_참미 <- gsub("미참여",0,급여비용_참미df_l$급여비용_참미)
급여비용_참미df_l$급여비용_참미 <- gsub("참여",1,급여비용_참미df_l$급여비용_참미)
급여비용_참미df_l$급여비용_참미 <- 급여비용_참미df_l$급여비용_참미 %>% as.integer()

ancova(급여비용_참미 ~ 전후 * value, data = 급여비용_참미df_l) 


# 117p 보험자부담금_미참
미참여 <- c(0,0,2570250,57680,0,4731200,34350,0,32372490,590730,45890,33565000,0,12305950,2929960,0,170870,0,4830,4580,0,466090,0,8817040)*70/42
참여 <- c(7562700,0,14864060,5013020,149350,0,0,0,0,15104770,47983440,0,0,0,0,0,0,0,0,0,0,23477300,1158890,0)
보험자부담금_전후 <- rep(c("전","후"),length(미참여)/2)

보험자부담금_전후df <- tibble(미참여,참여,보험자부담금_전후)
보험자부담금_전후df_l <- 보험자부담금_전후df %>% pivot_longer(names_to = "참미",
                                              cols = c("미참여","참여"),
                                              values_to = "value")
# t test
t.test(보험자부담금_전후df$미참여, 보험자부담금_전후df$참여, var.equal = FALSE)

# paired t test
t.test(보험자부담금_전후df$미참여, 보험자부담금_전후df$참여, var.equal = FALSE, paired = TRUE)

# ancova
보험자부담금_전후df_l$참미 <- gsub("미참여",0,보험자부담금_전후df_l$참미)
보험자부담금_전후df_l$참미 <- gsub("참여",1,보험자부담금_전후df_l$참미)
보험자부담금_전후df_l$참미 <- 보험자부담금_전후df_l$참미 %>% as.factor()

보험자부담금_전후df_l$보험자부담금_전후 <- gsub("전",0,보험자부담금_전후df_l$보험자부담금_전후)
보험자부담금_전후df_l$보험자부담금_전후 <- gsub("후",1,보험자부담금_전후df_l$보험자부담금_전후)
보험자부담금_전후df_l$보험자부담금_전후 <- 보험자부담금_전후df_l$보험자부담금_전후 %>% as.integer()

ancova(보험자부담금_전후 ~ 참미 * value, data = 보험자부담금_전후df_l) 


# 117p 보험자부담금_전후
전 <- c(7562700,0,14864060,5013020,149350,0,0,0,0,15104770,47983440,0,0,0,2570250,57680,0,4731200,34350,0,32372490,590730,45890,33565000)
후 <- c(0,0,0,0,0,0,0,0,0,23477300,1158890,0,0,12305950,2929960,0,170870,0,4830,4580,0,466090,0,8817040)
보험자부담금_참미 <- rep(c("참여","미참여"),length(후)/2)

보험자부담금_참미df <- tibble(전,후,보험자부담금_참미)
보험자부담금_참미df_l <- 보험자부담금_참미df %>% pivot_longer(names_to = "전후",
                                              cols = c("전","후"),
                                              values_to = "value")
# t test
t.test(보험자부담금_참미df$전, 보험자부담금_참미df$후, var.equal = FALSE)

# paired t test
t.test(보험자부담금_참미df$전, 보험자부담금_참미df$후, var.equal = FALSE, paired = TRUE)

# ancova

보험자부담금_참미df_l$전후 <- gsub("전",0,보험자부담금_참미df_l$전후)
보험자부담금_참미df_l$전후 <- gsub("후",1,보험자부담금_참미df_l$전후)
보험자부담금_참미df_l$전후 <- 보험자부담금_참미df_l$전후 %>% as.factor()

보험자부담금_참미df_l$보험자부담금_참미 <- gsub("미참여",0,보험자부담금_참미df_l$보험자부담금_참미)
보험자부담금_참미df_l$보험자부담금_참미 <- gsub("참여",1,보험자부담금_참미df_l$보험자부담금_참미)
보험자부담금_참미df_l$보험자부담금_참미 <- 보험자부담금_참미df_l$보험자부담금_참미 %>% as.integer()

ancova(보험자부담금_참미 ~ 전후 * value, data = 보험자부담금_참미df_l) 


# 117p 이용횟수_미참
미참여 <- c(0,0,14,14,0,18,15,0,100,32,8,291,0,9,9,0,6,0,2,2,0,4,0,8)*70/42
참여 <- c(8,	0,	75,	27,	12,	0,	0,	0,	0,	28,	282,	0,		0,	0,	0,	0,	0,	0,	0,	0,	0,	14,	4,	0
)
이용횟수_전후 <- rep(c("전","후"),length(미참여)/2)

이용횟수_전후df <- tibble(미참여,참여,이용횟수_전후)
이용횟수_전후df_l <- 이용횟수_전후df %>% pivot_longer(names_to = "참미",
                                          cols = c("미참여","참여"),
                                          values_to = "value")
# t test
t.test(이용횟수_전후df$미참여, 이용횟수_전후df$참여, var.equal = FALSE)

# paired t test
t.test(이용횟수_전후df$미참여, 이용횟수_전후df$참여, var.equal = FALSE, paired = TRUE)

# ancova
이용횟수_전후df_l$참미 <- gsub("미참여",0,이용횟수_전후df_l$참미)
이용횟수_전후df_l$참미 <- gsub("참여",1,이용횟수_전후df_l$참미)
이용횟수_전후df_l$참미 <- 이용횟수_전후df_l$참미 %>% as.factor()

이용횟수_전후df_l$이용횟수_전후 <- gsub("전",0,이용횟수_전후df_l$이용횟수_전후)
이용횟수_전후df_l$이용횟수_전후 <- gsub("후",1,이용횟수_전후df_l$이용횟수_전후)
이용횟수_전후df_l$이용횟수_전후 <- 이용횟수_전후df_l$이용횟수_전후 %>% as.integer()

ancova(이용횟수_전후 ~ 참미 * value, data = 이용횟수_전후df_l) 


# 117p 이용횟수_전후
전 <- c(8,	0,	75,	27,	12,	0,	0,	0,	0,	28,	282,	0,	0,	0,	14,	14,	0,	18,	15,	0,	100,	32,	8,	291
)
후 <- c(0,	0,	0,	0,	0,	0,	0,	0,	0,	14,	4,	0,0,	9,	9,	0,	6,	0,	2,	2,	0,	4,	0,	8
)
이용횟수_참미 <- rep(c("참여","미참여"),length(후)/2)

이용횟수_참미df <- tibble(전,후,이용횟수_참미)
이용횟수_참미df_l <- 이용횟수_참미df %>% pivot_longer(names_to = "전후",
                                          cols = c("전","후"),
                                          values_to = "value")
# t test
t.test(이용횟수_참미df$전, 이용횟수_참미df$후, var.equal = FALSE)

# paired t test
t.test(이용횟수_참미df$전, 이용횟수_참미df$후, var.equal = FALSE, paired = TRUE)

# ancova
이용횟수_참미df_l$전후 <- gsub("전",0,이용횟수_참미df_l$전후)
이용횟수_참미df_l$전후 <- gsub("후",1,이용횟수_참미df_l$전후)
이용횟수_참미df_l$전후 <- 이용횟수_참미df_l$전후 %>% as.factor()

이용횟수_참미df_l$이용횟수_참미 <- gsub("미참여",0,이용횟수_참미df_l$이용횟수_참미)
이용횟수_참미df_l$이용횟수_참미 <- gsub("참여",1,이용횟수_참미df_l$이용횟수_참미)
이용횟수_참미df_l$이용횟수_참미 <- 이용횟수_참미df_l$이용횟수_참미 %>% as.integer()

ancova(이용횟수_참미 ~ 전후 * value, data = 이용횟수_참미df_l) 


# 117p 입원일수_미참
미참여 <- c(0,0,14,14,0,18,15,0,100,32,8,291,0,9,9,0,6,0,2,2,0,4,0,8)*70/42
참여 <- c(876,	0,	153,	43,	12,	0,	0,	0,	0,	53,	517,	0,		0,	0,	0,	0,	0,	0,	0,	0,	0,	137,	4,	0
)
입원일수_전후 <- rep(c("전","후"),length(미참여)/2)

입원일수_전후df <- tibble(미참여,참여,입원일수_전후)
입원일수_전후df_l <- 입원일수_전후df %>% pivot_longer(names_to = "참미",
                                          cols = c("미참여","참여"),
                                          values_to = "value")
# t test
t.test(입원일수_전후df$미참여, 입원일수_전후df$참여, var.equal = FALSE)

# paired t test
t.test(입원일수_전후df$미참여, 입원일수_전후df$참여, var.equal = FALSE, paired = TRUE)

# ancova
입원일수_전후df_l$참미 <- gsub("미참여",0,입원일수_전후df_l$참미)
입원일수_전후df_l$참미 <- gsub("참여",1,입원일수_전후df_l$참미)
입원일수_전후df_l$참미 <- 입원일수_전후df_l$참미 %>% as.factor()

입원일수_전후df_l$입원일수_전후 <- gsub("전",0,입원일수_전후df_l$입원일수_전후)
입원일수_전후df_l$입원일수_전후 <- gsub("후",1,입원일수_전후df_l$입원일수_전후)
입원일수_전후df_l$입원일수_전후 <- 입원일수_전후df_l$입원일수_전후 %>% as.integer()

ancova(입원일수_전후 ~ 참미 * value, data = 입원일수_전후df_l) 


# 117p 입원일수_전후
전 <- c(76,	0,	153,	43,	12,	0,	0,	0,	0,	53,	517,	0,		0,	0,	27,	14,	0,	27,	15,	0,	255,	32,	8,	390
)
후 <- c(0,	0,	0,	0,	0,	0,	0,	0,	0,	137,	4,	0,	0,	71,	22,	0,	6,	0,	2,	2,	0,	9,	0,	62

)
입원일수_참미 <- rep(c("참여","미참여"),length(후)/2)

입원일수_참미df <- tibble(전,후,입원일수_참미)
입원일수_참미df_l <- 입원일수_참미df %>% pivot_longer(names_to = "전후",
                                          cols = c("전","후"),
                                          values_to = "value")
# t test
t.test(입원일수_참미df$전, 입원일수_참미df$후, var.equal = FALSE)

# paired t test
t.test(입원일수_참미df$전, 입원일수_참미df$후, var.equal = FALSE, paired = TRUE)

# ancova
입원일수_참미df_l$전후 <- gsub("전",0,입원일수_참미df_l$전후)
입원일수_참미df_l$전후 <- gsub("후",1,입원일수_참미df_l$전후)
입원일수_참미df_l$전후 <- 입원일수_참미df_l$전후 %>% as.factor()

입원일수_참미df_l$입원일수_참미 <- gsub("미참여",0,입원일수_참미df_l$입원일수_참미)
입원일수_참미df_l$입원일수_참미 <- gsub("참여",1,입원일수_참미df_l$입원일수_참미)
입원일수_참미df_l$입원일수_참미 <- 입원일수_참미df_l$입원일수_참미 %>% as.integer()

ancova(입원일수_참미 ~ 전후 * value, data = 입원일수_참미df_l) 