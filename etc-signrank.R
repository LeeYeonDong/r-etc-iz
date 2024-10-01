
library(readxl)
library(dplyr)
library(tidyverse)

# 데이터 읽기 (엑셀 파일 불러오기)
data <- read_excel("D:/대학원/상담/상담 심리/rawdata.xlsx")

# 사전 데이터와 사후 데이터를 각각 필터링
pre_data <- subset(data, 전후 == "사전")
post_data <- subset(data, 전후 == "사후")

## Wilcoxon Signed Rank Test 
# Wilcoxon Signed Rank Test 수행 결과를 저장할 데이터프레임 생성
wilcoxon_results <- data.frame(변수 = character(),
                               집단 = integer(),
                               W = numeric(),
                               p_value = numeric(),
                               stringsAsFactors = FALSE)

# Wilcoxon Signed Rank Test 수행
variables <- c("자기수용", "외적자존감", "심적자존감", "합자존감", "자존감ses", "불안", "우울")

for (group in c(1, 2)) {
  for (var in variables) {
    pre_scores <- pre_data %>% filter(집단 == group) %>% pull(var)
    post_scores <- post_data %>% filter(집단 == group) %>% pull(var)
    
    test_result <- wilcox.test(pre_scores, post_scores, paired = TRUE, exact = FALSE)
    wilcoxon_results <- rbind(wilcoxon_results, data.frame(변수 = var, 집단 = group, W = test_result$statistic, p_value = test_result$p.value))
  }
}
print(wilcoxon_results)

## Mann-Whitney U Test 
# Mann-Whitney U Test 수행 결과를 저장할 데이터프레임 생성
mann_whitney_results <- data.frame(변수 = character(),
                                   W = numeric(),
                                   p_value = numeric(),
                                   stringsAsFactors = FALSE)

# Mann-Whitney U Test 수행
for (var in variables) {
  group1_post <- post_data %>% filter(집단 == 1) %>% pull(var)
  group2_post <- post_data %>% filter(집단 == 2) %>% pull(var)
  
  test_result <- wilcox.test(group1_post, group2_post, exact = FALSE)
  mann_whitney_results <- rbind(mann_whitney_results, data.frame(변수 = var, W = test_result$statistic, p_value = test_result$p.value))
}
print(mann_whitney_results)

# 결과를 하나의 데이터프레임으로 결합
combined_results <- list(wilcoxon = wilcoxon_results, mann_whitney = mann_whitney_results)
print(combined_results)
