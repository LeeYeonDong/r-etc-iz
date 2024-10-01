# 필요한 라이브러리 로드
library(readxl)
library(dplyr)
library(writexl)

# 엑셀 파일에서 데이터 읽기
file_path <- "C:/상담/rawdata.xlsx"
data <- read_excel(file_path)

# 분석을 위한 변수 목록
variables <- c("자기수용", "외적자존감", "심적자존감", "합자존감", "불안", "우울")

# 결과를 저장할 리스트 초기화
results_list <- list()

for (var in variables) {
  # 통제 집단 내 사전-사후 비교
  control_pre <- data %>% filter(집단 == "통제" & 전후 == "사전")
  control_post <- data %>% filter(집단 == "통제" & 전후 == "사후")
  control_wilcox <- wilcox.test(control_pre[[var]], control_post[[var]], paired = TRUE)
  
  # 실험 집단 내 사전-사후 비교
  experiment_pre <- data %>% filter(집단 == "실험" & 전후 == "사전")
  experiment_post <- data %>% filter(집단 == "실험" & 전후 == "사후")
  experiment_wilcox <- wilcox.test(experiment_pre[[var]], experiment_post[[var]], paired = TRUE)
  
  # 통제 집단과 실험 집단의 사전 동질성 비교
  homogeneity_test <- wilcox.test(control_pre[[var]], experiment_pre[[var]])
  
  # 사후 관측치에 대한 통제/실험 집단 비교
  post_test <- wilcox.test(control_post[[var]], experiment_post[[var]])
  
  # 결과를 데이터 프레임으로 결합
  results <- data.frame(
    변수 = var,
    영가설 = c(
      "통제 집단 내에서 사전/사후 차이가 없다.",
      "실험 집단 내에서 사전/사후 차이가 없다.",
      "사전 관측치에 대해 통제와 실험 집단 간 차이가 없다.",
      "사후 관측치에 대해 통제와 실험 집단 간 차이가 없다."
    ),
    검정 = c(
      "Wilcoxon Signed-Rank Test",
      "Wilcoxon Signed-Rank Test",
      "Mann-Whitney U Test",
      "Mann-Whitney U Test"
    ),
    유의확률 = c(
      control_wilcox$p.value,
      experiment_wilcox$p.value,
      homogeneity_test$p.value,
      post_test$p.value
    ),
    의사결정 = c(
      ifelse(control_wilcox$p.value < 0.05, "영가설 기각", "영가설 채택"),
      ifelse(experiment_wilcox$p.value < 0.05, "영가설 기각", "영가설 채택"),
      ifelse(homogeneity_test$p.value < 0.05, "영가설 기각", "영가설 채택"),
      ifelse(post_test$p.value < 0.05, "영가설 기각", "영가설 채택")
    )
  )
  
  # 결과를 리스트에 추가
  results_list[[var]] <- results
}

# 모든 결과를 하나의 데이터 프레임으로 결합
final_results <- do.call(rbind, results_list)

# 결과 출력
print(final_results)

# 엑셀 파일로 저장
write_xlsx(final_results, "C:/상담/결과.xlsx")



# 필요한 라이브러리 로드
library(readxl)
library(dplyr)
library(tidyverse)

# 엑셀 파일에서 데이터 읽기
file_path <- "C:/상담/rawdata.xlsx"
data <- read_excel(file_path)

# 각 변수에 대해 집단별, 시점별 기초통계량 계산
summary_stats <- data %>%
  group_by(집단, 전후) %>%
  summarise(
    N = n(),
    across(c(자기수용, 외적자존감, 심적자존감, 합자존감, 불안, 우울), 
           list(
             최소값 = ~min(.),
             최대값 = ~max(.),
             평균 = ~mean(.),
             표준편차 = ~sd(.)
           ), 
           .names = "{col}_{fn}")
  ) %>%
  pivot_longer(
    cols = -c(집단, 전후, N),
    names_to = c("변수", "통계량"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = "통계량",
    values_from = "value"
  ) %>%
  arrange(집단, 전후, 변수)

# 결과 출력
print(summary_stats)

write_xlsx(summary_stats, "C:/상담/summary.xlsx")
