# install.packages("readxl")
library(readxl)
미핵 <- read_excel("D:/대학원/상담/미병치료/rawdata02.xlsx", sheet = 1)
만족 <- read_excel("D:/대학원/상담/미병치료/rawdata02.xlsx", sheet = 4)

library(tidyverse)
select <- dplyr::select

# data
미핵$구분 <- 미핵$구분 %>% 
  factor(levels = c("실험군", "대조군")) 
미핵$시점 <- 미핵$시점 %>% 
  factor(levels = c("시행전", "시행후", "4주후")) 

levels(미핵$구분)
levels(미핵$시점)

## 미병
미병 <- 미핵 %>% 
  select(구분:V7_불안감_3)

# 실험군 대조군 동질성 
# H0 : 실험군 대조군의 점수가 동일하다
미병 %>% str()

미평_합계 <- 미병 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

미병$합계 <- 미평_합계

미병 %>%
  select(구분, 시점, 합계) %>% 
  group_by(구분, 시점) %>% 
  summarise(합계 = sum(합계)) %>% 
  pivot_wider(
    names_from = 시점,
    values_from = 합계
  ) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "구분") %>% 
  chisq.test(correct = FALSE) # H0 기각 실패


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


# 실험군 대조군 문항별 합계
미병_실험군_시행전 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colSums(na.rm = TRUE) %>% as.vector()

미병 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V1_피로_1:V1_피로_3) %>% rowSums(na.rm = TRUE) %>% sd() %>% round(2)

미병_실험군_시행후 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  colSums(na.rm = TRUE) %>% as.vector()

# 정규성 검정 H0 : F(x)는 정규분포이다.
미병_실험군_시행전 %>% shapiro.test() # H0 기각 실패
미병_실험군_시행후 %>% shapiro.test() # H0 기각 실패
미병_실험군_4주후 %>% shapiro.test() # H0 기각 실패
미병_대조군_시행전 %>% shapiro.test() # H0 기각 실패
미병_대조군_시행후 %>% shapiro.test() # H0 기각 실패
미병_대조군_4주후 %>% shapiro.test() # H0 기각 실패



# 실험군 대조군 문항별 평균
핵심_실험군_시행전 <- 핵심 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 
  colMeans(na.rm = TRUE) %>% as.vector()

핵심 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V8_핵심감정_5:V8_핵심감정_28) %>% 

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
핵심_대조군_시행전 %>% shapiro.test() # H0 기각
핵심_대조군_시행후 %>% shapiro.test() # H0 기각
핵심_대조군_4주후 %>% shapiro.test() # H0 기각 실패


# boxplot
# install.packages("ggpubr")
library(ggpubr)

미병 %>% 
  select(구분, 시점, 합계) %>% 
  ggboxplot(x = "시점", y = "합계", color = "구분", add = "dotplot", palette = c("#000000", "#F0A30A"))

미병 %>% 
  select(구분, 시점, 합계) %>% 
  filter(구분 == "실험군") %>% 
  ggboxplot(x = "시점", y = "합계", add = "dotplot")

미병 %>% 
  select(구분, 시점, 합계) %>% 
  filter(구분 == "대조군") %>% 
  ggboxplot(x = "시점", y = "합계", add = "dotplot")



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
t.test(미병_실험군_시행전, 미병_실험군_시행후, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전, 미병_실험군_4주후, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전, 미병_대조군_시행후, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전, 미병_대조군_4주후, paired = TRUE) # H0 기각


# paired t test
# H0: 시행전과 시행후 차이가 없다
t.test(핵심_실험군_시행전_희, 핵심_실험군_시행후_희, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_희, 핵심_실험군_4주후_희, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_희, 핵심_대조군_시행후_희, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_희, 핵심_대조군_4주후_희, paired = TRUE) # H0 기각실패


t.test(핵심_실험군_시행전, 핵심_실험군_시행후, paired = TRUE) # H0 기각
t.test(핵심_실험군_시행전, 핵심_실험군_4주후, paired = TRUE) # H0 기각
t.test(핵심_대조군_시행전, 핵심_대조군_시행후, paired = TRUE) # H0 기각
t.test(핵심_대조군_시행전, 핵심_대조군_4주후, paired = TRUE) # H0 기각


# wilcox test - 정규성 만족 X
wilcox.test(핵심_실험군_시행전, 핵심_실험군_시행후, paired = TRUE) # H0 기각
wilcox.test(핵심_실험군_시행전, 핵심_실험군_4주후, paired = TRUE) # H0 기각

wilcox.test(핵심_대조군_시행전, 핵심_대조군_시행후, paired = TRUE) # H0 기각실패
wilcox.test(핵심_대조군_시행전, 핵심_대조군_4주후, paired = TRUE) # H0 기각





## ancova
# install.packages("HH")
library(HH)

### <연령> 공변량
## 미병_실험군
미병_실험군_인구 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(no:시점)

y <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% rowSums(na.rm = TRUE)

미병_실험군_인구 <- tibble(미병_실험군_인구, y)

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 연령, data = 미병_실험군_인구)

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 연령, data = 미병_실험군_인구) %>% summary()


# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 연령, data = 미병_실험군_인구) 


## 미병_대조군
미병_대조군_인구 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(no:시점)

y <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% rowSums(na.rm = TRUE)

미병_대조군_인구 <- tibble(미병_대조군_인구, y)

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 연령, data = 미병_대조군_인구)

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 연령, data = 미병_대조군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 연령, data = 미병_대조군_인구) 



## 핵심_실험군_희
핵심_실험군_인구_희 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(no:시점)

y <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(V8_핵심감정_1:V8_핵심감정_4) %>% rowSums(na.rm = TRUE)

핵심_실험군_인구_희 <- tibble(핵심_실험군_인구_희, y)

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 연령, data = 핵심_실험군_인구_희)

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 연령, data = 핵심_실험군_인구_희) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 연령, data = 핵심_실험군_인구_희)



## 핵심_대조군_희
핵심_대조군_인구_희 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(no:시점)

y <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(V8_핵심감정_1:V8_핵심감정_4) %>% rowSums(na.rm = TRUE)

핵심_대조군_인구_희 <- tibble(핵심_대조군_인구_희, y)

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 연령, data = 핵심_대조군_인구_희)

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 연령, data = 핵심_대조군_인구_희) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 연령, data = 핵심_대조군_인구_희)



## 핵심_실험군
핵심_실험군_인구 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(no:시점)

y <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(V8_핵심감정_5:V8_핵심감정_28) %>% rowSums(na.rm = TRUE)

핵심_실험군_인구 <- tibble(핵심_실험군_인구, y)

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 연령, data = 핵심_실험군_인구)

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 연령, data = 핵심_실험군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 연령, data = 핵심_실험군_인구)



## 핵심_대조군
핵심_대조군_인구 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(no:시점)

y <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(V8_핵심감정_5:V8_핵심감정_28) %>% rowSums(na.rm = TRUE)

핵심_대조군_인구 <- tibble(핵심_대조군_인구, y)

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 연령, data = 핵심_대조군_인구)

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 연령, data = 핵심_대조군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 연령, data = 핵심_대조군_인구) 


### <배우자> 공변량
## 미병_실험군
미병_실험군_인구 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% rowSums(na.rm = TRUE)
미병_실험군_인구 <- tibble(미병_실험군_인구, y)

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 배우자, data = 미병_실험군_인구)

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 배우자, data = 미병_실험군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 배우자, data = 미병_실험군_인구)


## 미병_대조군
미병_대조군_인구 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% rowMeans(na.rm = TRUE)
미병_대조군_인구 <- tibble(미병_대조군_인구, y)
# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 배우자, data = 미병_대조군_인구)
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 배우자, data = 미병_대조군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 배우자, data = 미병_대조군_인구)



## 핵심_실험군
핵심_실험군_인구 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(V8_핵심감정_1:V8_핵심감정_28) %>% rowSums(na.rm = TRUE)
핵심_실험군_인구 <- tibble(핵심_실험군_인구, y)
# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 배우자, data = 핵심_실험군_인구)
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 배우자, data = 핵심_실험군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 배우자, data = 핵심_실험군_인구)


## 핵심_대조군
핵심_대조군_인구 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(V8_핵심감정_1:V8_핵심감정_28) %>% rowSums(na.rm = TRUE)
핵심_대조군_인구 <- tibble(핵심_대조군_인구, y)
# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 배우자, data = 핵심_대조군_인구)
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 배우자, data = 핵심_대조군_인구) %>% summary()
# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 배우자, data = 핵심_대조군_인구) 


### <동거> 공변량
## 미병_실험군
미병_실험군_인구 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% rowSums(na.rm = TRUE)
미병_실험군_인구 <- tibble(미병_실험군_인구, y)
# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 동거, data = 미병_실험군_인구)
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 동거, data = 미병_실험군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 동거, data = 미병_실험군_인구)



## 미병_대조군
미병_대조군_인구 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% rowSums(na.rm = TRUE)
미병_대조군_인구 <- tibble(미병_대조군_인구, y)
# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 동거, data = 미병_대조군_인구)
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 동거, data = 미병_대조군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 동거, data = 미병_대조군_인구) 


## 핵심_실험군
핵심_실험군_인구 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  select(V8_핵심감정_1:V8_핵심감정_28) %>% rowSums(na.rm = TRUE)
핵심_실험군_인구 <- tibble(핵심_실험군_인구, y)
# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 동거, data = 핵심_실험군_인구)
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 동거, data = 핵심_실험군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 동거, data = 핵심_실험군_인구) 


## 핵심_대조군
핵심_대조군_인구 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(no:시점)
y <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  select(V8_핵심감정_1:V8_핵심감정_28) %>% rowSums(na.rm = TRUE)
핵심_대조군_인구 <- tibble(핵심_대조군_인구, y)
# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(y ~ 시점 * 동거, data = 핵심_대조군_인구)
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(y ~ 시점 + 동거, data = 핵심_대조군_인구) %>% summary()

# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(y ~ 시점 + 동거, data = 핵심_대조군_인구) 
