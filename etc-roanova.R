# install.packages("readxl")
library(readxl)
인구 <- read_excel("D:/대학원/상담/미병치료/rawdata.xlsx", sheet = 2)
미핵 <- read_excel("D:/대학원/상담/미병치료/rawdata.xlsx", sheet = 3)
만족 <- read_excel("D:/대학원/상담/미병치료/rawdata.xlsx", sheet = 4)

library(tidyverse)

# data
미핵$구분 <- 미핵$구분 %>% 
  factor(levels = c("실험군", "대조군")) 
미핵$시점 <- 미핵$시점 %>% 
  factor(levels = c("시행전", "시행후", "4주후")) 

levels(미핵$구분)
levels(미핵$시점)

## 미병
미병 <- 미핵 %>% 
  select(구분:V7_불안감_4)
  
# 실험군 대조군 동질성 
# H0 : 실험군 대조군의 점수가 동일하다
미병 %>% str()

미평_평균 <- 미병 %>% 
  select(-(구분:시점)) %>% 
  rowMeans(na.rm = TRUE)

미병$평균 <- 미평_평균

미병 %>%
  select(구분, 시점, 평균) %>% 
  group_by(구분, 시점) %>% 
  summarise(평균 = mean(평균)) %>% 
    pivot_wider(
    names_from = 시점,
    values_from = 평균
  ) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "구분") %>% 
  chisq.test(correct = FALSE) # H0 기각 실패


# 실험군 대조군 문항별 평균
미병_실험군_시행전 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

# 정규성 검정 H0 : F(x)는 정규분포이다.
미병_실험군_시행전 %>% shapiro.test() # H0 기각 실패
미병_실험군_시행후 %>% shapiro.test() # H0 기각 실패
미병_실험군_4주후 %>% shapiro.test() # H0 기각 실패
미병_대조군_시행전 %>% shapiro.test() # H0 기각 실패
미병_대조군_시행후 %>% shapiro.test() # H0 기각
미병_대조군_4주후 %>% shapiro.test() # H0 기각 실패

# boxplot
# install.packages("ggpubr")
library(ggpubr)

미병 %>% 
  select(구분, 시점, 평균) %>% 
  ggboxplot(x = "시점", y = "평균", color = "구분", add = "dotplot", palette = c("#000000", "#F0A30A"))

미병 %>% 
  select(구분, 시점, 평균) %>% 
  filter(구분 == "실험군") %>% 
  ggboxplot(x = "시점", y = "평균", add = "dotplot")

미병 %>% 
  select(구분, 시점, 평균) %>% 
  filter(구분 == "대조군") %>% 
  ggboxplot(x = "시점", y = "평균", add = "dotplot")

# paired t test
# H0: 시행전과 시행후 차이가 없다
t.test(미병_실험군_시행전, 미병_실험군_시행후, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전, 미병_실험군_4주후, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전, 미병_대조군_시행후, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전, 미병_대조군_4주후, paired = TRUE) # H0 기각




## 핵심감정
핵심_희 <- 미핵 %>% 
  select(구분:시점,V8_핵심감정_1:V8_핵심감정_4)

핵심 <- 미핵 %>% 
  select(구분:시점,V8_핵심감정_5:V8_핵심감정_28) 
  
# 실험군 대조군 동질성 
# H0 : 실험군 대조군의 점수가 동일하다
핵심 %>% str()

핵심_희_평균 <- 핵심_희 %>% 
  select(-(구분:시점)) %>% 
  rowMeans(na.rm = TRUE)

핵심_평균 <- 핵심 %>% 
  select(-(구분:시점)) %>% 
  rowMeans(na.rm = TRUE)

핵심_희$평균 <- 핵심_희_평균
핵심$평균 <- 핵심_평균

핵심_희 %>%
  na.omit() %>% 
  select(구분, 시점, 평균) %>% 
  group_by(구분, 시점) %>% 
  summarise(평균 = mean(평균)) %>% 
  pivot_wider(
    names_from = 시점,
    values_from = 평균
  ) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "구분") %>% 
  chisq.test(correct = FALSE) # H0 기각 실패

핵심 %>%
  select(구분, 시점, 평균) %>% 
  group_by(구분, 시점) %>% 
  summarise(평균 = mean(평균)) %>% 
  pivot_wider(
    names_from = 시점,
    values_from = 평균
  ) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "구분") %>% 
  chisq.test(correct = FALSE) # H0 기각 실패


# 실험군 대조군 문항별 평균
핵심_실험군_시행전 <- 핵심 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_실험군_시행후 <- 핵심 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_실험군_4주후 <- 핵심 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_대조군_시행전 <- 핵심 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_대조군_시행후 <- 핵심 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_대조군_4주후 <- 핵심 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()


핵심_실험군_시행전_희 <- 핵심_희 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V8_핵심감정_1:V8_핵심감정_4) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_실험군_시행후_희 <- 핵심_희 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V8_핵심감정_1:V8_핵심감정_4) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_실험군_4주후_희 <- 핵심_희 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V8_핵심감정_1:V8_핵심감정_4) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_대조군_시행전_희 <- 핵심_희 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V8_핵심감정_1:V8_핵심감정_4) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_대조군_시행후_희 <- 핵심_희 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V8_핵심감정_1:V8_핵심감정_4) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심_대조군_4주후_희 <- 핵심_희 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V8_핵심감정_1:V8_핵심감정_4) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()


# 정규성 검정 H0 : F(x)는 정규분포이다.
핵심_실험군_시행전_희 %>% shapiro.test() # H0 기각 실패
핵심_실험군_시행후_희 %>% shapiro.test() # H0 기각 실패
핵심_실험군_4주후_희 %>% shapiro.test() # H0 기각
핵심_대조군_시행전_희 %>% shapiro.test() # H0 기각 실패
핵심_대조군_시행후_희 %>% shapiro.test() # H0 기각 실패
핵심_대조군_4주후_희 %>% shapiro.test() # H0 기각 실패


핵심_실험군_시행전 %>% shapiro.test() # H0 기각 실패
핵심_실험군_시행후 %>% shapiro.test() # H0 기각
핵심_실험군_4주후 %>% shapiro.test() # H0 기각
핵심_대조군_시행전 %>% shapiro.test() # H0 기각 실패
핵심_대조군_시행후 %>% shapiro.test() # H0 기각 실패
핵심_대조군_4주후 %>% shapiro.test() # H0 기각 실패

# boxplot
핵심_희 %>% 
  select(구분, 시점, 평균) %>% 
  ggboxplot(x = "시점", y = "평균", color = "구분", add = "dotplot", palette = c("#000000", "#F0A30A"))

핵심_희 %>% 
  select(구분, 시점, 평균) %>% 
  filter(구분 == "실험군") %>% 
  ggboxplot(x = "시점", y = "평균", add = "dotplot")

핵심_희 %>% 
  select(구분, 시점, 평균) %>% 
  filter(구분 == "대조군") %>% 
  ggboxplot(x = "시점", y = "평균", add = "dotplot")

핵심 %>% 
  select(구분, 시점, 평균) %>% 
  ggboxplot(x = "시점", y = "평균", color = "구분", add = "dotplot", palette = c("#000000", "#F0A30A"))

핵심 %>% 
  select(구분, 시점, 평균) %>% 
  filter(구분 == "실험군") %>% 
  ggboxplot(x = "시점", y = "평균", add = "dotplot")

핵심 %>% 
  select(구분, 시점, 평균) %>% 
  filter(구분 == "대조군") %>% 
  ggboxplot(x = "시점", y = "평균", add = "dotplot")


# paired t test
# H0: 시행전과 시행후 차이가 없다
t.test(핵심_실험군_시행전, 핵심_실험군_시행후, paired = TRUE) # H0 기각
t.test(핵심_실험군_시행전, 핵심_실험군_4주후, paired = TRUE) # H0 기각

t.test(핵심_대조군_시행전, 핵심_대조군_시행후, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전, 핵심_대조군_4주후, paired = TRUE) # H0 기각실패


t.test(핵심_실험군_시행전_희, 핵심_실험군_시행후_희, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_희, 핵심_실험군_4주후_희, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_희, 핵심_대조군_시행후_희, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_희, 핵심_대조군_4주후_희, paired = TRUE) # H0 기각실패









# wilcox test
wilcox.test(핵심_실험군_시행전, 핵심_실험군_시행후, paired = TRUE) # H0 기각
wilcox.test(핵심_실험군_시행전, 핵심_실험군_4주후, paired = TRUE) # H0 기각

wilcox.test(핵심_대조군_시행전, 핵심_대조군_시행후, paired = TRUE) # H0 기각실패
wilcox.test(핵심_대조군_시행전, 핵심_대조군_4주후, paired = TRUE) # H0 기각실패


wilcox.test(핵심_실험군_시행전_희, 핵심_실험군_시행후_희, paired = TRUE) # H0 기각실패
wilcox.test(핵심_실험군_시행전_희, 핵심_실험군_4주후_희, paired = TRUE) # H0 기각실패

wilcox.test(핵심_대조군_시행전_희, 핵심_대조군_시행후_희, paired = TRUE) # H0 기각실패
wilcox.test(핵심_대조군_시행전_희, 핵심_대조군_4주후_희, paired = TRUE) # H0 기각실패

