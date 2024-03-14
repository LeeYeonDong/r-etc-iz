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
# 연령 - 평균
matrix(c(3,0,1,1,0,1,1,0,1,0,0,1,0,1,1,4,1,1,2,1,2,2), ncol = 2, byrow = TRUE) %>% 
  chisq.test()
matrix(c(31,17,109,122),nc=2)

미핵 %>% 
  filter(구분=="실험군" & 시점=="시행전") %>% 
  select(연령) %>% as.vector() %>% unlist() %>% mean() %>% round(2)

미핵 %>% 
  filter(구분=="실험군" & 시점=="시행전") %>% 
  select(연령) %>% as.vector() %>% unlist() %>% sd() %>% round(2)

미핵 %>% 
  filter(구분=="대조군" & 시점=="시행전") %>% 
  select(연령) %>% as.vector() %>% unlist() %>% mean() %>% round(2)

미핵 %>% 
  filter(구분=="대조군" & 시점=="시행전") %>% 
  select(연령) %>% as.vector() %>% unlist() %>% sd() %>% round(2)


# 배우자
matrix(c(8,4,4,7), nc = 2, byrow = TRUE) %>% 
  chisq.test()

mat <- matrix(c(8, 4, 4, 7), nrow = 2, byrow = TRUE)
percent_mat <- prop.table(mat, margin = 2) * 100
row_totals <- rowSums(mat)
total_percent <- row_totals / sum(row_totals) * 100
formatted_mat <- matrix(sprintf("%d(%.1f%%)", mat, percent_mat), nc = 2)
cbind(formatted_mat, sprintf("%d(%.1f%%)", row_totals, total_percent))


# 자녀 수
matrix(c(5,4,5,5,1,2), nc = 2, byrow = TRUE) %>% 
  chisq.test()

mat <- matrix(c(5,4,5,5,1,2), nc = 2, byrow = TRUE)
percent_mat <- prop.table(mat, margin = 2) * 100
row_totals <- rowSums(mat)
total_percent <- row_totals / sum(row_totals) * 100
formatted_mat <- matrix(sprintf("%d(%.1f%%)", mat, percent_mat), nc = 2)
cbind(formatted_mat, sprintf("%d(%.1f%%)", row_totals, total_percent))


# 동거형태 
matrix(c(7,3,2,1,3,7), nc = 2, byrow = TRUE) %>% 
  chisq.test()

mat <- matrix(c(7,3,2,1,3,7), nc = 2, byrow = TRUE)
percent_mat <- prop.table(mat, margin = 2) * 100
row_totals <- rowSums(mat)
total_percent <- row_totals / sum(row_totals) * 100
formatted_mat <- matrix(sprintf("%d(%.1f%%)", mat, percent_mat), nc = 2)
cbind(formatted_mat, sprintf("%d(%.1f%%)", row_totals, total_percent))


# 건강 상태 - 기각
matrix(c(0,5,12,6,0,1), nc = 2, byrow = TRUE) %>% 
  chisq.test()

mat <- matrix(c(0,5,12,6,0,1), nc = 2, byrow = TRUE)
percent_mat <- prop.table(mat, margin = 2) * 100
row_totals <- rowSums(mat)
total_percent <- row_totals / sum(row_totals) * 100
formatted_mat <- matrix(sprintf("%d(%.1f%%)", mat, percent_mat), nc = 2)
cbind(formatted_mat, sprintf("%d(%.1f%%)", row_totals, total_percent))


# 입원 경험
matrix(c(10,10,1,2), nc = 2, byrow = TRUE) %>% 
  chisq.test()

mat <- matrix(c(10,10,1,2), nc = 2, byrow = TRUE)
percent_mat <- prop.table(mat, margin = 2) * 100
row_totals <- rowSums(mat)
total_percent <- row_totals / sum(row_totals) * 100
formatted_mat <- matrix(sprintf("%d(%.1f%%)", mat, percent_mat), nc = 2)
cbind(formatted_mat, sprintf("%d(%.1f%%)", row_totals, total_percent))




## 핵심감정
핵심_희 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_1,V8_핵심감정_8,V8_핵심감정_15,V8_핵심감정_22)

핵심 <- 미핵 %>% 
  select(구분:시점, V8_핵심감정_1:V8_핵심감정_22) %>%   select(-c(V8_핵심감정_1,V8_핵심감정_8,V8_핵심감정_15,V8_핵심감정_22))

핵심_노 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_2,V8_핵심감정_9,V8_핵심감정_16,V8_핵심감정_23)

핵심_사 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_3,V8_핵심감정_10,V8_핵심감정_17,V8_핵심감정_24)

핵심_우 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_4,V8_핵심감정_11,V8_핵심감정_18,V8_핵심감정_25)

핵심_비 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_5,V8_핵심감정_12,V8_핵심감정_19,V8_핵심감정_26)

핵심_공 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_6,V8_핵심감정_13,V8_핵심감정_20,V8_핵심감정_27)
  
핵심_경 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_7,V8_핵심감정_14,V8_핵심감정_21,V8_핵심감정_28)

# 실험군 대조군 동질성 
# H0 : 실험군 대조군의 점수가 동일하다
핵심_희_sum <- 핵심_희 %>% 
    select(-(구분:시점)) %>% 
    rowSums(na.rm = TRUE)

핵심_sum <- 핵심 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

핵심_희_std <- (핵심_희_sum - mean(핵심_희_sum)) / sd(핵심_희_sum)

핵심_std <- (핵심_sum - mean(핵심_sum)) / sd(핵심_sum)

핵심_희_std <- 핵심_희_std * 10 + 50 
핵심_std <- 핵심_std * 10 + 50 

핵심_희$std <- 핵심_희_std
핵심$std <- 핵심_std

핵심_희 %>%
  na.omit() %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise(std = mean(std)) %>% 
  pivot_wider(
    names_from = 시점,
    values_from = std
  ) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "구분") %>% 
  chisq.test(correct = FALSE) # H0 기각 실패

핵심 %>%
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise(std = mean(std)) %>% 
  pivot_wider(
    names_from = 시점,
    values_from = std
  ) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "구분") %>% 
  chisq.test(correct = FALSE) # H0 기각 실패


# 핵심감정별 표준화 점수
# 희
핵심_희 %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise_all(mean) %>% 
  pivot_wider(names_from = 구분, values_from = std)

# 노
핵심_노 <- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_2,V8_핵심감정_9,V8_핵심감정_16,V8_핵심감정_23)

핵심_노_sum <- 핵심_노 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

핵심_노_std <- (핵심_노_sum - mean(핵심_노_sum)) / sd(핵심_노_sum)

핵심_노_std <- 핵심_노_std * 10 + 50 
핵심_노$std <- 핵심_노_std

핵심_노 %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise_all(mean) %>% 
  pivot_wider(names_from = 구분, values_from = std)

# 사
핵심_사<- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_3,V8_핵심감정_10,V8_핵심감정_17,V8_핵심감정_24)

핵심_사_sum <- 핵심_사 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

핵심_사_std <- (핵심_사_sum - mean(핵심_사_sum)) / sd(핵심_사_sum)

핵심_사_std <- 핵심_사_std * 10 + 50 
핵심_사$std <- 핵심_사_std

핵심_사 %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise_all(mean) %>% 
  pivot_wider(names_from = 구분, values_from = std)

# 우
핵심_우<- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_4,V8_핵심감정_11,V8_핵심감정_18,V8_핵심감정_25)

핵심_우_sum <- 핵심_우 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

핵심_우_std <- (핵심_우_sum - mean(핵심_우_sum)) / sd(핵심_사_sum)

핵심_우_std <- 핵심_우_std * 10 + 50 
핵심_우$std <- 핵심_우_std

핵심_우 %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise_all(mean) %>% 
  pivot_wider(names_from = 구분, values_from = std)

# 비
핵심_비<- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_5,V8_핵심감정_12,V8_핵심감정_19,V8_핵심감정_26)

핵심_비_sum <- 핵심_비 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

핵심_비_std <- (핵심_비_sum - mean(핵심_비_sum)) / sd(핵심_비_sum)

핵심_비_std <- 핵심_비_std * 10 + 50 
핵심_비$std <- 핵심_비_std

핵심_비 %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise_all(mean) %>% 
  pivot_wider(names_from = 구분, values_from = std)

# 공
핵심_공<- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_6,V8_핵심감정_13,V8_핵심감정_20,V8_핵심감정_27)

핵심_공_sum <- 핵심_공 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

핵심_공_std <- (핵심_공_sum - mean(핵심_공_sum)) / sd(핵심_공_sum)

핵심_공_std <- 핵심_공_std * 10 + 50 
핵심_공$std <- 핵심_공_std

핵심_공 %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise_all(mean) %>% 
  pivot_wider(names_from = 구분, values_from = std)

# 경
핵심_경<- 미핵 %>% 
  select(구분:시점,
         V8_핵심감정_7,V8_핵심감정_14,V8_핵심감정_21,V8_핵심감정_28)

핵심_경_sum <- 핵심_경 %>% 
  select(-(구분:시점)) %>% 
  rowSums(na.rm = TRUE)

핵심_경_std <- (핵심_경_sum - mean(핵심_경_sum)) / sd(핵심_공_sum)

핵심_경_std <- 핵심_경_std * 10 + 50 
핵심_경$std <- 핵심_경_std

핵심_경 %>% 
  select(구분, 시점, std) %>% 
  group_by(구분, 시점) %>% 
  summarise_all(mean) %>% 
  pivot_wider(names_from = 구분, values_from = std)



# 실험군 대조군 문항별 합계
미병_실험군_시행전 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()


미병_실험군_시행전_피로 <- 미병 %>%  
  filter(구분 == "실험군",시점 == "시행전") %>%
  select(V1_피로_1:V1_피로_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector() %>% 

미병_실험군_시행후_피로 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V1_피로_1:V1_피로_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후_피로 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V1_피로_1:V1_피로_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전_피로 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V1_피로_1:V1_피로_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후_피로 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V1_피로_1:V1_피로_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후_피로 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V1_피로_1:V1_피로_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()


미병_실험군_시행전_통증 <- 미병 %>%  
  filter(구분 == "실험군",시점 == "시행전") %>%
  select(V2_통증_1:V2_통증_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후_통증 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V2_통증_1:V2_통증_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후_통증 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V2_통증_1:V2_통증_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전_통증 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V2_통증_1:V2_통증_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후_통증 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V2_통증_1:V2_통증_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후_통증 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V2_통증_1:V2_통증_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()



미병_실험군_시행전_수면 <- 미병 %>%  
  filter(구분 == "실험군",시점 == "시행전") %>%
  select(V3_수면장애_1:V3_수면장애_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후_수면 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V3_수면장애_1:V3_수면장애_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후_수면 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V3_수면장애_1:V3_수면장애_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전_수면 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V3_수면장애_1:V3_수면장애_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후_수면 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V3_수면장애_1:V3_수면장애_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후_수면 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V3_수면장애_1:V3_수면장애_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()



미병_실험군_시행전_소화 <- 미병 %>%  
  filter(구분 == "실험군",시점 == "시행전") %>%
  select(V4_소화불량_1:V4_소화불량_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후_소화 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V4_소화불량_1:V4_소화불량_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후_소화 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V4_소화불량_1:V4_소화불량_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전_소화 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V4_소화불량_1:V4_소화불량_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후_소화 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V4_소화불량_1:V4_소화불량_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후_소화 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V4_소화불량_1:V4_소화불량_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()



미병_실험군_시행전_우울 <- 미병 %>%  
  filter(구분 == "실험군",시점 == "시행전") %>%
  select(V5_우울감_1:V5_우울감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후_우울 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V5_우울감_1:V5_우울감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후_우울 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V5_우울감_1:V5_우울감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전_우울 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V5_우울감_1:V5_우울감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후_우울 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V5_우울감_1:V5_우울감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후_우울 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V5_우울감_1:V5_우울감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()



미병_실험군_시행전_분노 <- 미병 %>%  
  filter(구분 == "실험군",시점 == "시행전") %>%
  select(V6_분노감_1:V6_분노감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후_분노 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V6_분노감_1:V6_분노감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후_분노 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V6_분노감_1:V6_분노감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전_분노 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V6_분노감_1:V6_분노감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후_분노 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V6_분노감_1:V6_분노감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후_분노 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V6_분노감_1:V6_분노감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()



미병_실험군_시행전_불안 <- 미병 %>%  
  filter(구분 == "실험군",시점 == "시행전") %>%
  select(V7_불안감_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_시행후_불안 <- 미병 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% 
  select(V7_불안감_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_실험군_4주후_불안 <- 미병 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% 
  select(V7_불안감_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행전_불안 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% 
  select(V7_불안감_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_시행후_불안 <- 미병 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% 
  select(V7_불안감_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()

미병_대조군_4주후_불안 <- 미병 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% 
  select(V7_불안감_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE) %>% as.vector()



# 정규성 검정 H0 : F(x)는 정규분포이다.
미병_실험군_시행전 %>% shapiro.test() 
미병_실험군_시행후 %>% shapiro.test() 
미병_실험군_4주후 %>% shapiro.test() 
미병_대조군_시행전 %>% shapiro.test() 
미병_대조군_시행후 %>% shapiro.test() 
미병_대조군_4주후 %>% shapiro.test() 

미병_실험군_시행전_피로 %>% shapiro.test()
미병_실험군_시행후_피로 %>% shapiro.test() 
미병_실험군_4주후_피로 %>% shapiro.test() 
미병_대조군_시행전_피로 %>% shapiro.test() 
미병_대조군_시행후_피로 %>% shapiro.test() 
미병_대조군_4주후_피로 %>% shapiro.test() 


미병_실험군_시행전_통증 %>% shapiro.test()
미병_실험군_시행후_통증 %>% shapiro.test() 
미병_실험군_4주후_통증 %>% shapiro.test() 
미병_대조군_시행전_통증 %>% shapiro.test() 
미병_대조군_시행후_통증 %>% shapiro.test() 
미병_대조군_4주후_통증 %>% shapiro.test() 

미병_실험군_시행전_수면 %>% shapiro.test()
미병_실험군_시행후_수면 %>% shapiro.test() 
미병_실험군_4주후_수면 %>% shapiro.test() 
미병_대조군_시행전_수면 %>% shapiro.test() 
미병_대조군_시행후_수면 %>% shapiro.test() 
미병_대조군_4주후_수면 %>% shapiro.test() 

미병_실험군_시행전_소화 %>% shapiro.test()
미병_실험군_시행후_소화 %>% shapiro.test() 
미병_실험군_4주후_소화 %>% shapiro.test() 
미병_대조군_시행전_소화 %>% shapiro.test() 
미병_대조군_시행후_소화 %>% shapiro.test() 
미병_대조군_4주후_소화 %>% shapiro.test() 

미병_실험군_시행전_우울 %>% shapiro.test()
미병_실험군_시행후_우울 %>% shapiro.test() 
미병_실험군_4주후_우울 %>% shapiro.test() 
미병_대조군_시행전_우울 %>% shapiro.test() 
미병_대조군_시행후_우울 %>% shapiro.test() 
미병_대조군_4주후_우울 %>% shapiro.test() 

미병_실험군_시행전_분노 %>% shapiro.test()
미병_실험군_시행후_분노 %>% shapiro.test() 
미병_실험군_4주후_분노 %>% shapiro.test() 
미병_대조군_시행전_분노 %>% shapiro.test() 
미병_대조군_시행후_분노 %>% shapiro.test() 
미병_대조군_4주후_분노 %>% shapiro.test() 

미병_실험군_시행전_불안 %>% shapiro.test()
미병_실험군_시행후_불안 %>% shapiro.test() 
미병_실험군_4주후_불안 %>% shapiro.test() 
미병_대조군_시행전_불안 %>% shapiro.test() 
미병_대조군_시행후_불안 %>% shapiro.test() 
미병_대조군_4주후_불안 %>% shapiro.test()

미병_df <- data.frame(미병_실험군_시행전 ,
           미병_실험군_시행후 ,
           미병_실험군_4주후 ,
           미병_대조군_시행전 ,
           미병_대조군_시행후 ,
           미병_대조군_4주후 ,
           미병_실험군_시행전_피로 ,
           미병_실험군_시행후_피로 ,
           미병_실험군_4주후_피로 ,
           미병_대조군_시행전_피로 ,
           미병_대조군_시행후_피로 ,
           미병_대조군_4주후_피로 ,
           미병_실험군_시행전_통증 ,
           미병_실험군_시행후_통증 ,
           미병_실험군_4주후_통증 ,
           미병_대조군_시행전_통증 ,
           미병_대조군_시행후_통증 ,
           미병_대조군_4주후_통증 ,
           미병_실험군_시행전_수면 ,
           미병_실험군_시행후_수면 ,
           미병_실험군_4주후_수면 ,
           미병_대조군_시행전_수면 ,
           미병_대조군_시행후_수면 ,
           미병_대조군_4주후_수면 ,
           미병_실험군_시행전_소화 ,
           미병_실험군_시행후_소화 ,
           미병_실험군_4주후_소화 ,
           미병_대조군_시행전_소화 ,
           미병_대조군_시행후_소화 ,
           미병_대조군_4주후_소화 ,
           미병_실험군_시행전_우울 ,
           미병_실험군_시행후_우울 ,
           미병_실험군_4주후_우울 ,
           미병_대조군_시행전_우울 ,
           미병_대조군_시행후_우울 ,
           미병_대조군_4주후_우울 ,
           미병_실험군_시행전_분노 ,
           미병_실험군_시행후_분노 ,
           미병_실험군_4주후_분노 ,
           미병_대조군_시행전_분노 ,
           미병_대조군_시행후_분노 ,
           미병_대조군_4주후_분노 ,
           미병_실험군_시행전_불안 ,
           미병_실험군_시행후_불안 ,
           미병_실험군_4주후_불안 ,
           미병_대조군_시행전_불안 ,
           미병_대조군_시행후_불안 ,
           미병_대조군_4주후_불안)

미병_df %>% view()
sapply(미병_df, function(x) {
  mean_val <- mean(x)
  sd_val <- sd(x)
  paste0(round(mean_val, 2), "±", round(sd_val, 2))
}) %>% as.data.frame()

# 3열씩 그룹화하여 평균과 표준편차 계산
result <- sapply(seq(1, ncol(미병_df), by = 3), function(i) {
  group_data <- 미병_df[, i:(i+2)]
  mean_val <- mean(rowMeans(group_data, na.rm = TRUE), na.rm = TRUE)
  sd_val <- sd(rowMeans(group_data, na.rm = TRUE), na.rm = TRUE)
  paste0(round(mean_val, 2), "±", round(sd_val, 2))
})
names(result) <- paste("Group", 1:(length(result)))
print(result)

# 결과 출력
names(result) <- paste("Group", 1:(length(result)))


# 실험군 대조군 문항별 평균
핵심_실험군_시행전_희 <- 핵심_희 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_희 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector() %>% sd()

핵심_실험군_시행후_희 <- 핵심_희 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후_희 <- 핵심_희 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전_희 <- 핵심_희 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후_희 <- 핵심_희 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후_희 <- 핵심_희 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



핵심_실험군_시행전 <- 핵심 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_시행후 <- 핵심 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후 <- 핵심 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전 <- 핵심 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후 <- 핵심 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후 <- 핵심 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



핵심_실험군_시행전_노 <- 핵심_노 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_시행후_노 <- 핵심_노 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후_노 <- 핵심_노 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전_노 <- 핵심_노 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후_노 <- 핵심_노 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후_노 <- 핵심_노 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



핵심_실험군_시행전_사 <- 핵심_사 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_시행후_사 <- 핵심_사 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후_사 <- 핵심_사 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전_사 <- 핵심_사 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후_사 <- 핵심_사 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후_사 <- 핵심_사 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



핵심_실험군_시행전_우 <- 핵심_우 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_시행후_우 <- 핵심_우 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후_우 <- 핵심_우 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전_우 <- 핵심_우 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후_우 <- 핵심_우 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후_우 <- 핵심_우 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



핵심_실험군_시행전_비 <- 핵심_비 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_시행후_비 <- 핵심_비 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후_비 <- 핵심_비 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전_비 <- 핵심_비 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후_비 <- 핵심_비 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후_비 <- 핵심_비 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



핵심_실험군_시행전_공 <- 핵심_공 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_시행후_공 <- 핵심_공 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후_공 <- 핵심_공 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전_공 <- 핵심_공 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후_공 <- 핵심_공 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후_공 <- 핵심_공 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



핵심_실험군_시행전_경 <- 핵심_경 %>%
  filter(구분 == "실험군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_시행후_경 <- 핵심_경 %>%
  filter(구분 == "실험군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_실험군_4주후_경 <- 핵심_경 %>%
  filter(구분 == "실험군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행전_경 <- 핵심_경 %>%
  filter(구분 == "대조군",시점 == "시행전") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_시행후_경 <- 핵심_경 %>%
  filter(구분 == "대조군",시점 == "시행후") %>% select(std) %>% unlist() %>% as.vector()

핵심_대조군_4주후_경 <- 핵심_경 %>%
  filter(구분 == "대조군",시점 == "4주후") %>% select(std) %>% unlist() %>% as.vector()



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


핵심_실험군_시행전_노 %>% shapiro.test() # 
핵심_실험군_시행후_노 %>% shapiro.test() # 
핵심_실험군_4주후_노 %>% shapiro.test() # 
핵심_대조군_시행전_노 %>% shapiro.test() # 
핵심_대조군_시행후_노 %>% shapiro.test() # 
핵심_대조군_4주후_노 %>% shapiro.test() # 


핵심_실험군_시행전_사 %>% shapiro.test() # 
핵심_실험군_시행후_사 %>% shapiro.test() # 
핵심_실험군_4주후_사 %>% shapiro.test() # 
핵심_대조군_시행전_사 %>% shapiro.test() # 
핵심_대조군_시행후_사 %>% shapiro.test() # 
핵심_대조군_4주후_사 %>% shapiro.test() # 


핵심_실험군_시행전_우 %>% shapiro.test() # 
핵심_실험군_시행후_우 %>% shapiro.test() # 
핵심_실험군_4주후_우 %>% shapiro.test() # 
핵심_대조군_시행전_우 %>% shapiro.test() # 
핵심_대조군_시행후_우 %>% shapiro.test() # 
핵심_대조군_4주후_우 %>% shapiro.test() # 


핵심_실험군_시행전_비 %>% shapiro.test() # 
핵심_실험군_시행후_비 %>% shapiro.test() # 
핵심_실험군_4주후_비 %>% shapiro.test() # 
핵심_대조군_시행전_비 %>% shapiro.test() # 
핵심_대조군_시행후_비 %>% shapiro.test() # 
핵심_대조군_4주후_비 %>% shapiro.test() # 


핵심_실험군_시행전_공 %>% shapiro.test() # 
핵심_실험군_시행후_공 %>% shapiro.test() # 
핵심_실험군_4주후_공 %>% shapiro.test() # 
핵심_대조군_시행전_공 %>% shapiro.test() # 
핵심_대조군_시행후_공 %>% shapiro.test() # 
핵심_대조군_4주후_공 %>% shapiro.test() # 


핵심_실험군_시행전_경 %>% shapiro.test() # 
핵심_실험군_시행후_경 %>% shapiro.test() # 
핵심_실험군_4주후_경 %>% shapiro.test() # 
핵심_대조군_시행전_경 %>% shapiro.test() # 
핵심_대조군_시행후_경 %>% shapiro.test() # 
핵심_대조군_4주후_경 %>% shapiro.test() # 

핵심_df <-
  data.frame(핵심_실험군_시행전_희, 
             핵심_실험군_시행후_희, 
             핵심_실험군_4주후_희,
             핵심_대조군_시행전_희, 
             핵심_대조군_시행후_희, 
             핵심_대조군_4주후_희, 
             핵심_실험군_시행전, 
             핵심_실험군_시행후,
             핵심_실험군_4주후,
             핵심_대조군_시행전,
             핵심_대조군_시행후,
             핵심_대조군_4주후,
             핵심_실험군_시행전_노,
             핵심_실험군_시행후_노,
             핵심_실험군_4주후_노,
             핵심_대조군_시행전_노,
             핵심_대조군_시행후_노,
             핵심_대조군_4주후_노,
             핵심_실험군_시행전_사,
             핵심_실험군_시행후_사,
             핵심_실험군_4주후_사,
             핵심_대조군_시행전_사,
             핵심_대조군_시행후_사,
             핵심_대조군_4주후_사,
             핵심_실험군_시행전_우,
             핵심_실험군_시행후_우,
             핵심_실험군_4주후_우,
             핵심_대조군_시행전_우,
             핵심_대조군_시행후_우,
             핵심_대조군_4주후_우,
             핵심_실험군_시행전_비,
             핵심_실험군_시행후_비,
             핵심_실험군_4주후_비,
             핵심_대조군_시행전_비,
             핵심_대조군_시행후_비,
             핵심_대조군_4주후_비,
             핵심_실험군_시행전_공,
             핵심_실험군_시행후_공,
             핵심_실험군_4주후_공,
             핵심_대조군_시행전_공,
             핵심_대조군_시행후_공,
             핵심_대조군_4주후_공,
             핵심_실험군_시행전_경,
             핵심_실험군_시행후_경,
             핵심_실험군_4주후_경,
             핵심_대조군_시행전_경,
             핵심_대조군_시행후_경,
             핵심_대조군_4주후_경
  )


핵심_df
sapply(핵심_df, function(x) {
  mean_val <- mean(x)
  sd_val <- sd(x)
  paste0(round(mean_val, 2), "±", round(sd_val, 2))
}) %>% as.data.frame()

# 3열씩 그룹화하여 평균과 표준편차 계산
result <- sapply(seq(1, ncol(핵심_df), by = 3), function(i) {
  group_data <- 핵심_df[, i:(i+2)]
  mean_val <- mean(rowMeans(group_data, na.rm = TRUE), na.rm = TRUE)
  sd_val <- sd(rowMeans(group_data, na.rm = TRUE), na.rm = TRUE)
  paste0(round(mean_val, 2), "±", round(sd_val, 2))
})
names(result) <- paste("Group", 1:(length(result)))
print(result)

# 결과 출력
names(result) <- paste("Group", 1:(length(result)))


핵심_희제외_df <-
  data.frame(핵심_실험군_시행전_노,
             핵심_실험군_시행후_노,
             핵심_실험군_4주후_노,
             핵심_대조군_시행전_노,
             핵심_대조군_시행후_노,
             핵심_대조군_4주후_노,
             핵심_실험군_시행전_사,
             핵심_실험군_시행후_사,
             핵심_실험군_4주후_사,
             핵심_대조군_시행전_사,
             핵심_대조군_시행후_사,
             핵심_대조군_4주후_사,
             핵심_실험군_시행전_우,
             핵심_실험군_시행후_우,
             핵심_실험군_4주후_우,
             핵심_대조군_시행전_우,
             핵심_대조군_시행후_우,
             핵심_대조군_4주후_우,
             핵심_실험군_시행전_비,
             핵심_실험군_시행후_비,
             핵심_실험군_4주후_비,
             핵심_대조군_시행전_비,
             핵심_대조군_시행후_비,
             핵심_대조군_4주후_비,
             핵심_실험군_시행전_공,
             핵심_실험군_시행후_공,
             핵심_실험군_4주후_공,
             핵심_대조군_시행전_공,
             핵심_대조군_시행후_공,
             핵심_대조군_4주후_공,
             핵심_실험군_시행전_경,
             핵심_실험군_시행후_경,
             핵심_실험군_4주후_경,
             핵심_대조군_시행전_경,
             핵심_대조군_시행후_경,
             핵심_대조군_4주후_경
  )
핵심_희제외_df

# 관심 있는 열 이름 패턴
patterns <- c("실험군_시행전", "실험군_시행후", "실험군_4주후", "대조군_시행전", "대조군_시행후", "대조군_4주후")

# 해당 패턴을 포함하는 열 선택
selected_columns <- 핵심_희제외_df[, grepl(paste(patterns, collapse="|"), names(핵심_희제외_df))]

# 각 그룹별로 행 평균의 평균과 표준편차 계산
sapply(patterns, function(pattern) {
  group_data <- selected_columns[, grepl(pattern, names(selected_columns))]
  row_means <- rowMeans(group_data, na.rm = TRUE)
  c(mean = mean(row_means, na.rm = TRUE), sd = sd(row_means, na.rm = TRUE))
})


patterns <- c("실험군", "대조군")

# 해당 패턴을 포함하는 열 선택 및 계산
sapply(patterns, function(pattern) {
  group_data <- 핵심_희제외_df[, grepl(pattern, names(핵심_희제외_df))]
  row_means <- rowMeans(group_data, na.rm = TRUE)
  c(mean = mean(row_means, na.rm = TRUE), sd = sd(row_means, na.rm = TRUE))
})



write.csv(핵심_희제외_df, file = "D:/대학원/상담/미병치료/핵심_희제외_df.csv", row.names=FALSE, fileEncoding = 'cp949')

write.csv(핵심_df, file = "D:/대학원/상담/미병치료/핵심_df.csv", row.names=FALSE, fileEncoding = 'cp949')
write.csv(미병_df , file = "D:/대학원/상담/미병치료/미병_df.csv", row.names=FALSE, fileEncoding = 'cp949')


# boxplot
# install.packages("ggpubr")
library(ggpubr)

미병 %>% 
  select(구분, 시점, 합계) %>% 
  mutate(구분 = str_replace_all(구분, c("실험군" = "실험집단", "대조군" = "통제집단"))) %>%
  mutate(시점 = str_replace_all(시점, c("시행전" = "시행 전", "시행후" = "시행 후", "4주후" = "종결 4주 후"))) %>%
  mutate(구분 = factor(구분, levels = c("실험집단", "통제집단"))) %>% 
  mutate(시점 = factor(시점, levels = c("시행 전", "시행 후","종결 4주 후"))) %>% 
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
  rename(c(T = "std")) %>% 
  select(구분, 시점, T) %>% 
  mutate(구분 = str_replace_all(구분, c("실험군" = "실험집단", "대조군" = "통제집단"))) %>%
  mutate(시점 = str_replace_all(시점, c("시행전" = "시행 전", "시행후" = "시행 후", "4주후" = "종결 4주 후"))) %>%
  mutate(구분 = factor(구분, levels = c("실험집단", "통제집단"))) %>% 
  mutate(시점 = factor(시점, levels = c("시행 전", "시행 후","종결 4주 후"))) %>% 
  ggboxplot(x = "시점", y = "T", color = "구분", add = "dotplot", palette = c("#000000", "#F0A30A"))


핵심 %>% 
  rename(c(T = "std")) %>% 
  select(구분, 시점, T) %>% 
  mutate(구분 = str_replace_all(구분, c("실험군" = "실험집단", "대조군" = "통제집단"))) %>%
  mutate(시점 = str_replace_all(시점, c("시행전" = "시행 전", "시행후" = "시행 후", "4주후" = "종결 4주 후"))) %>%
  mutate(구분 = factor(구분, levels = c("실험집단", "통제집단"))) %>% 
  mutate(시점 = factor(시점, levels = c("시행 전", "시행 후","종결 4주 후"))) %>%  
  ggboxplot(x = "시점", y = "T", color = "구분", add = "dotplot", palette = c("#000000", "#F0A30A"))


# paired t test
# H0: 시행전과 시행후 차이가 없다
t.test(미병_실험군_시행전, 미병_실험군_시행후, paired = TRUE) # H0 기각

t.test(미병_실험군_시행전, 미병_실험군_4주후, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전, 미병_대조군_시행후, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전, 미병_대조군_4주후, paired = TRUE) # H0 기각


t.test(미병_실험군_시행전_피로, 미병_실험군_시행후_피로, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전_피로, 미병_실험군_4주후_피로, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전_피로, 미병_대조군_시행후_피로, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전_피로, 미병_대조군_4주후_피로, paired = TRUE) # H0 기각


t.test(미병_실험군_시행전_통증, 미병_실험군_시행후_통증, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전_통증, 미병_실험군_4주후_통증, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전_통증, 미병_대조군_시행후_통증, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전_통증, 미병_대조군_4주후_통증, paired = TRUE) # H0 기각


t.test(미병_실험군_시행전_수면, 미병_실험군_시행후_수면, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전_수면, 미병_실험군_4주후_수면, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전_수면, 미병_대조군_시행후_수면, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전_수면, 미병_대조군_4주후_수면, paired = TRUE) # H0 기각


t.test(미병_실험군_시행전_소화, 미병_실험군_시행후_소화, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전_소화, 미병_실험군_4주후_소화, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전_소화, 미병_대조군_시행후_소화, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전_소화, 미병_대조군_4주후_소화, paired = TRUE) # H0 기각


t.test(미병_실험군_시행전_우울, 미병_실험군_시행후_우울, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전_우울, 미병_실험군_4주후_우울, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전_우울, 미병_대조군_시행후_우울, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전_우울, 미병_대조군_4주후_우울, paired = TRUE) # H0 기각



t.test(미병_실험군_시행전_분노, 미병_실험군_시행후_분노, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전_분노, 미병_실험군_4주후_분노, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전_분노, 미병_대조군_시행후_분노, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전_분노, 미병_대조군_4주후_분노, paired = TRUE) # H0 기각



t.test(미병_실험군_시행전_불안, 미병_실험군_시행후_불안, paired = TRUE) # H0 기각
t.test(미병_실험군_시행전_불안, 미병_실험군_4주후_불안, paired = TRUE) # H0 기각

t.test(미병_대조군_시행전_불안, 미병_대조군_시행후_불안, paired = TRUE) # H0 기각실패
t.test(미병_대조군_시행전_불안, 미병_대조군_4주후_불안, paired = TRUE) # H0 기각



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


t.test(핵심_실험군_시행전_노, 핵심_실험군_시행후_노, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_노, 핵심_실험군_4주후_노, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_노, 핵심_대조군_시행후_노, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_노, 핵심_대조군_4주후_노, paired = TRUE) # H0 기각실패


t.test(핵심_실험군_시행전_사, 핵심_실험군_시행후_사, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_사, 핵심_실험군_4주후_사, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_사, 핵심_대조군_시행후_사, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_사, 핵심_대조군_4주후_사, paired = TRUE) # H0 기각실패


t.test(핵심_실험군_시행전_우, 핵심_실험군_시행후_우, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_우, 핵심_실험군_4주후_우, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_우, 핵심_대조군_시행후_우, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_우, 핵심_대조군_4주후_우, paired = TRUE) # H0 기각실패


t.test(핵심_실험군_시행전_비, 핵심_실험군_시행후_비, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_비, 핵심_실험군_4주후_비, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_비, 핵심_대조군_시행후_비, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_비, 핵심_대조군_4주후_비, paired = TRUE) # H0 기각실패


t.test(핵심_실험군_시행전_공, 핵심_실험군_시행후_공, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_공, 핵심_실험군_4주후_공, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_공, 핵심_대조군_시행후_공, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_공, 핵심_대조군_4주후_공, paired = TRUE) # H0 기각실패


t.test(핵심_실험군_시행전_경, 핵심_실험군_시행후_경, paired = TRUE) # H0 기각실패
t.test(핵심_실험군_시행전_경, 핵심_실험군_4주후_경, paired = TRUE) # H0 기각실패

t.test(핵심_대조군_시행전_경, 핵심_대조군_시행후_경, paired = TRUE) # H0 기각실패
t.test(핵심_대조군_시행전_경, 핵심_대조군_4주후_경, paired = TRUE) # H0 기각실패



# wilcox test - 정규성 만족 X
wilcox.test(핵심_실험군_시행전, 핵심_실험군_시행후, paired = TRUE) # H0 기각
wilcox.test(핵심_실험군_시행전, 핵심_실험군_4주후, paired = TRUE) # H0 기각

wilcox.test(핵심_대조군_시행전, 핵심_대조군_시행후, paired = TRUE) # H0 기각실패
wilcox.test(핵심_대조군_시행전, 핵심_대조군_4주후, paired = TRUE) # H0 기각




## ancova
# install.packages("HH")
library(HH)

## 미병_실험군
공변량 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

시행후 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

x4주후 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

id <- c(1:12)
구분 <- "실험군"
미병_실험군_공변량 <- tibble(id, 구분, 공변량, 시행후, x4주후)


## 미병_대조군
공변량 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

시행후 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

x4주후 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

id <- c(1:12)
구분 <- "대조군"
미병_대조군_공변량 <- tibble(id, 구분, 공변량, 시행후, x4주후)

미병_공변량 <- rbind(미병_실험군_공변량,미병_대조군_공변량)

미병_공변량$구분 <- 미병_공변량$구분 %>% as.factor()

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(x4주후 ~ 구분 * 공변량, data = 미병_공변량)
ancova(시행후 ~ 구분 * 공변량, data = 미병_공변량)
# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(x4주후 ~ 구분 * 공변량, data = 미병_공변량) 
ancovaplot(시행후 ~ 구분 * 공변량, data = 미병_공변량) 
# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(x4주후 ~ 구분 + 공변량, data = 미병_공변량) %>% summary()
lm(시행후 ~ 구분 + 공변량, data = 미병_공변량) %>% summary()


## 미병_실험군_배우자
공변량 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

시행후 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

x4주후 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

id <- c(1:12)

배우자 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>% 
  select(배우자) 

미병_실험군_공변량 <- tibble(id, 배우자[1:12,], 공변량, 시행후, x4주후)


## 미병_대조군_배우자
공변량 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

시행후 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

x4주후 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(V1_피로_1:V7_불안감_3) %>% 
  rowSums(na.rm = TRUE)

id <- c(1:12)

배우자 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>% 
  select(배우자) 

미병_대조군_공변량 <- tibble(id, 배우자[13:24,], 공변량, 시행후, x4주후)

미병_공변량 <- rbind(미병_실험군_공변량,미병_대조군_공변량)

미병_공변량$배우자 <- 미병_공변량$배우자 %>% 
  str_replace_all("1","배우자 있음") %>% 
  str_replace_all("2","배우자 없음") %>% as.factor()

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(x4주후 ~ 배우자 * 공변량, data = 미병_공변량)
ancova(시행후 ~ 배우자 * 공변량, data = 미병_공변량)
# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(x4주후 ~ 배우자 * 공변량, data = 미병_공변량) 
ancovaplot(시행후 ~ 배우자 * 공변량, data = 미병_공변량) 

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(x4주후 ~ 배우자 + 공변량, data = 미병_공변량) %>% summary()
lm(시행후 ~ 배우자 + 공변량, data = 미병_공변량) %>% summary()







## 핵심_희_실험군
공변량 <- 핵심_희 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심_희 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심_희 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
구분 <- "실험군"

핵심_희_실험군_공변량 <- tibble(id, 구분, 공변량, 시행후, x4주후)


## 핵심_희_대조군
공변량 <- 핵심_희 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심_희 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심_희 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
구분 <- "대조군"

핵심_희_대조군_공변량 <- tibble(id, 구분, 공변량, 시행후, x4주후)

핵심_희_공변량 <- rbind(핵심_희_실험군_공변량,핵심_희_대조군_공변량)

핵심_희_공변량$구분 <- 핵심_희_공변량$구분 %>% as.factor()


# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(x4주후 ~ 구분 * 공변량, data = 핵심_희_공변량)
ancova(시행후 ~ 구분 * 공변량, data = 핵심_희_공변량)
# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(x4주후 ~ 구분 * 공변량, data = 핵심_희_공변량) 
ancovaplot(시행후 ~ 구분 * 공변량, data = 핵심_희_공변량) 

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(x4주후 ~ 구분 + 공변량, data = 핵심_희_공변량) %>% summary()
lm(시행후 ~ 구분 + 공변량, data = 핵심_희_공변량) %>% summary()



## 핵심_희_실험군_배우자
공변량 <- 핵심_희 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심_희 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심_희 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
배우자 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>% 
  select(배우자) 

핵심_희_실험군_공변량 <- tibble(id, 배우자[1:12,], 공변량, 시행후, x4주후)


## 핵심_희_대조군
공변량 <- 핵심_희 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심_희 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심_희 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
배우자 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>% 
  select(배우자) 

핵심_희_대조군_공변량 <- tibble(id, 배우자[1:12,], 공변량, 시행후, x4주후)

핵심_희_공변량 <- rbind(핵심_희_실험군_공변량,핵심_희_대조군_공변량)

핵심_희_공변량$배우자 <- 핵심_희_공변량$배우자 %>% str_replace_all("1","배우자 있음") %>% 
  str_replace_all("2","배우자 없음") %>% as.factor()


# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(x4주후 ~ 배우자 * 공변량, data = 핵심_희_공변량)
ancova(시행후 ~ 배우자 * 공변량, data = 핵심_희_공변량)
# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(x4주후 ~ 배우자 * 공변량, data = 핵심_희_공변량) 
ancovaplot(시행후 ~ 배우자 * 공변량, data = 핵심_희_공변량) 

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(시행후 ~ 배우자 + 공변량, data = 핵심_희_공변량) %>% summary()
lm(시행후 ~ 배우자 + 공변량, data = 핵심_희_공변량) %>% summary()


## 핵심_실험군
공변량 <- 핵심 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
구분 <- "실험군"

핵심_실험군_공변량 <- tibble(id, 구분, 공변량, 시행후, x4주후)


## 핵심_대조군
공변량 <- 핵심 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
구분 <- "대조군"

핵심_대조군_공변량 <- tibble(id, 구분, 공변량, 시행후, x4주후)

핵심_공변량 <- rbind(핵심_실험군_공변량,핵심_대조군_공변량)

핵심_공변량$구분 <- 핵심_공변량$구분 %>% as.factor()

# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(x4주후 ~ 구분 * 공변량, data = 핵심_공변량)
ancova(시행후 ~ 구분 * 공변량, data = 핵심_공변량)
# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(x4주후 ~ 구분 * 공변량, data = 핵심_공변량) 
ancovaplot(시행후 ~ 구분 * 공변량, data = 핵심_공변량) 

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(x4주후 ~ 구분 + 공변량, data = 핵심_공변량) %>% summary()
lm(시행후 ~ 구분 + 공변량, data = 핵심_공변량) %>% summary()


## 핵심_희_실험군_배우자
공변량 <- 핵심 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심 %>% 
  filter(구분 == "실험군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
배우자 <- 미핵 %>% 
  filter(구분 == "실험군" ) %>% 
  select(배우자) 

핵심_실험군_공변량 <- tibble(id, 배우자[1:12,], 공변량, 시행후, x4주후)


## 핵심_희_대조군
공변량 <- 핵심 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행전" ) %>%
  select(std) %>% unlist() %>% as.vector()

시행후 <- 핵심 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "시행후" ) %>%
  select(std) %>% unlist() %>% as.vector()

x4주후 <- 핵심 %>% 
  filter(구분 == "대조군" ) %>%
  filter(시점 == "4주후" ) %>%
  select(std) %>% unlist() %>% as.vector()

id <- c(1:12)
배우자 <- 미핵 %>% 
  filter(구분 == "대조군" ) %>% 
  select(배우자) 

핵심_대조군_공변량 <- tibble(id, 배우자[1:12,], 공변량, 시행후, x4주후)

핵심_공변량 <- rbind(핵심_실험군_공변량,핵심_대조군_공변량)

핵심_공변량$배우자 <- 핵심_공변량$배우자 %>% 
  str_replace_all("1","배우자 있음") %>% 
  str_replace_all("2","배우자 없음") %>% as.factor()


# interaction 확인
# h0 : interaction이 존재하지 않는다 -> ancova 분석이 가능한 가정 만족
ancova(x4주후 ~ 배우자 * 공변량, data = 핵심_공변량)
ancova(시행후 ~ 배우자 * 공변량, data = 핵심_공변량)
# xyplot
# 기울기 효과 동일 그래프로 확인
ancovaplot(x4주후 ~ 배우자 * 공변량, data = 핵심_공변량) 
ancovaplot(시행후 ~ 배우자 * 공변량, data = 핵심_공변량) 

# ancova
# 귀무가설 H01: β = 0 <- regression model
# 귀무가설 H02 : α1 = α2 = … = αI <- regression coef
# 대립가설 H11: β ≠ 0
# 대립가설 H12 : not H02
# H01, H02 기각 -> 공변량의 효과가 없다
lm(x4주후 ~ 배우자 + 공변량, data = 핵심_공변량) %>% summary()
lm(시행후 ~ 배우자 + 공변량, data = 핵심_공변량) %>% summary()

