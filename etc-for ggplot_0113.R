library(tidyverse)
library(xlsx)
library(ggrepel)

## 2. 조사 결과 요약
# 1. 학생부분  : 종합 만족도
# [그림 2.1.1] 학생부문 종합 만족도
학생 <- read_csv(file = "D:/대학원/연구과제/2023/2023_거창대_학생_설문지.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

학생 %>% str()

학생_long <- 학생 %>% 
  pivot_longer(cols = c(Q01:Q21_4), names_to = "Q", values_to = "value") 

학생_long %>% 
  filter(Q %in% c("Q01", "Q02", "Q03", "Q04","Q05","Q06","Q07", "Q08", "Q09", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15","Q16", "Q17", "Q18", "Q19", "Q20")) %>% 
  group_by(Q) %>% 
  summarize(mean_value = round(mean(value, na.rm = TRUE), digits = 2)) %>% 
  ggplot(aes(x = Q, y = mean_value)) +
  coord_cartesian(ylim = c(4.3,5)) + # Y축 2부터 시작 10에서 끝
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = mean_value, label = mean_value), vjust = -0.7, size = 6) +
  theme_classic() + 
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.background = element_blank())

# 2.학생부문 : 전공별 종합 만족도
# [그림 2.2.1] 학생부문 전공별 종합 만족도
# 실효성 Q01-Q06
학생_long %>% 
  filter(Q %in% c("Q01", "Q02", "Q03", "Q04","Q05","Q06")) %>% 
  group_by(학과) %>% 
  summarize(mean_value = round(mean(value, na.rm = TRUE), digits = 2)) %>% 
  ggplot(aes(x = 학과, y = mean_value)) +
  coord_cartesian(ylim = c(3.5,5.5)) + # Y축 4부터 시작 5에서 끝
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = mean_value, label = mean_value), vjust = -0.7, size = 20) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 30, face = 'bold',
                                   angle=90, hjust=1)) +
    ylab(NULL) +
  xlab(NULL) +
  theme(panel.background = element_blank())

# 적절성 Q07-Q13
학생_long %>% 
  filter(Q %in% c("Q07", "Q08", "Q09", "Q10", "Q11", "Q12", "Q13")) %>% 
  group_by(학과) %>% 
  summarize(mean_value = round(mean(value, na.rm = TRUE), digits = 2)) %>% 
  ggplot(aes(x = 학과, y = mean_value)) +
  coord_cartesian(ylim = c(3.5,5.5)) + # Y축 4부터 시작 5에서 끝
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = mean_value, label = mean_value), vjust = -0.7, size = 20) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 30, face = 'bold',
                                   angle=90, hjust=1)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.background = element_blank())

# 만족도 Q14-Q20
학생_long %>% 
  filter(Q %in% c("Q14", "Q15","Q16", "Q17", "Q18", "Q19", "Q20")) %>% 
  group_by(학과) %>% 
  summarize(mean_value = round(mean(value, na.rm = TRUE), digits = 2)) %>% 
  ggplot(aes(x = 학과, y = mean_value)) +
  coord_cartesian(ylim = c(3.8,5.5)) + # Y축 4부터 시작 5에서 끝
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = mean_value, label = mean_value), vjust = -0.7, size = 20) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 30, face = 'bold',
                                   angle = 90, hjust=1)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.background = element_blank())


# 참여 기업부문 : 종합 만족도
# [그림 2.3.1] 참여 기업부문 종합 만족도
기업 <- read_csv(file = "D:/대학원/연구과제/2023/2023_거창대_기업_설문지.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(기업) <- c("id", "학과", "P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15",	"P16",	"P17",	"P18_1",	"P18_2",	"P18_3",	"P18_4",	"P19")
기업 %>% str()

기업_long <- 기업 %>% 
  pivot_longer(cols = c(P01:P18_4), names_to = "P", values_to = "value") 

기업_long %>% 
  filter(P %in% c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14")) %>% 
  group_by(P) %>% 
  summarize(mean_value = round(mean(value, na.rm = TRUE), digits = 2)) %>% 
  ggplot(aes(x = P, y = mean_value)) +
  coord_cartesian(ylim = c(4.2,5)) + # Y축 1부터 시작 10에서 끝
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = mean_value, label = mean_value), vjust = -0.7, size = 5) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 10, face = 'bold', angle = 0, hjust = 0.5)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.background = element_blank())

# 2.학생부문 : 전공별 종합 만족도
# 참여 기업부문 : 전공별 만족도
# [그림 2.4.1] 참여 기업부문 전공별 종합 만족도
# 실효성 Q01-Q04
기업_long %>% 
  filter(Q %in% c("Q01", "Q02", "Q03", "Q04")) %>% 
  group_by(학과) %>% 
  summarize(mean_value = round(mean(value, na.rm = TRUE), digits = 2)) %>% 
  ggplot(aes(x = 학과, y = mean_value)) +
  coord_cartesian(ylim = c(3,6)) + # Y축 4부터 시작 5에서 끝
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = mean_value, label = mean_value), vjust = -0.7, size = 20) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 30, face = 'bold',
                                   angle = 90, hjust=1)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.background = element_blank())

# 적절성 Q04-Q14
학생_long %>% 
  filter(Q %in% c("Q05", "Q06", "Q07", "Q08", "Q09", "Q10", "Q11", "Q12", "Q13", "Q14")) %>% 
  group_by(학과) %>% 
  summarize(mean_value = round(mean(value, na.rm = TRUE), digits = 2)) %>% 
  ggplot(aes(x = 학과, y = mean_value)) +
  coord_cartesian(ylim = c(3,5.5)) + # Y축 4부터 시작 5에서 끝
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = mean_value, label = mean_value), vjust = -0.7, size = 20) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 30, face = 'bold',
                                   angle = 90, hjust=1)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.background = element_blank())


## 3. 조사 결과
# 1. 학생부문 (문항별)
# 1] 현장실습 내용의 실효성 : 응답비율
# 1.1) 전공지식과 실무능력에 대한 이해정도
# [표 3.1.1] 전공지식과 실무능력에 대한 이해 정도

filepath <- paste0("D:/대학원/연구과제/2023/학생_학과_",names(학생),".png")

for(i in 1:(length(filepath)-2)){
df_전체.tmp <- 학생 %>% 
  lapply(factor, levels = c(0:5, unique(학생$학과))) %>% 
  as_tibble() %>% 
  select(학과, names(학생)[2+i]) %>%
  table() %>% as_tibble() %>% filter(!학과 %in% c(0:5)) %>% 
  pivot_wider(names_from = names(학생)[2+i], values_from = n) %>% 
  select(1,7,6,5,4,3) %>% 
  rename(c(구분 = 학과, 
           "freq5" = "5", 
           "freq4" = "4",
           "freq3" = "3",
           "freq2" = "2",
           "freq1" = "1")) %>% 
  column_to_rownames(var = "구분") %>% 
  colSums() %>% as.matrix() %>% t() %>% as.data.frame() 
rownames(df_전체.tmp) <- c("전체")
df_전체.tmp <- df_전체.tmp %>% rownames_to_column(var = "구분")

df_전체.tmp %>% bind_rows(
  학생 %>% 
    lapply(factor, levels = c(0:5, unique(학생$학과))) %>% 
    as_tibble() %>% 
    select(학과, names(학생)[2+i]) %>%
    table() %>% as_tibble() %>% filter(!학과 %in% c(0:5)) %>% 
    pivot_wider(names_from = names(학생)[2+i], values_from = n) %>% 
    select(1,7,6,5,4,3) %>% 
    rename(c(구분 = 학과, 
             "freq5" = "5", 
             "freq4" = "4",
             "freq3" = "3",
             "freq2" = "2",
             "freq1" = "1"))) %>% 
  mutate(빈도 = rowSums(across(where(is.numeric)))) %>% 
  mutate(prop5 = round(freq5/빈도*100,1)) %>% 
  mutate(prop4 = round(freq4/빈도*100,1)) %>% 
  mutate(prop3 = round(freq3/빈도*100,1)) %>% 
  mutate(prop2 = round(freq2/빈도*100,1)) %>% 
  mutate(prop1 = round(freq1/빈도*100,1)) %>% 
  mutate("paste1" = paste0(freq5,"(",prop5,"%",")")) %>% 
  mutate("paste2" = paste0(freq4,"(",prop4,"%",")")) %>% 
  mutate("paste3" = paste0(freq3,"(",prop3,"%",")")) %>% 
  mutate("paste4" = paste0(freq2,"(",prop2,"%",")")) %>% 
  mutate("paste5" = paste0(freq1,"(",prop1,"%",")")) %>% 
  select(구분, 빈도, paste1, paste2, paste3, paste4, paste5) %>% 
  write.xlsx(sheetName = names(학생)[2+i], file = "D:/대학원/연구과제/2023/2023_학생_학과_Q_빈도.xlsx", append = TRUE)

cat("Q",i,'cross table complete.\n') 
}

for(i in 1:(length(filepath)-2)){
  df_전체.tmp <- 학생 %>% 
    lapply(factor, levels = c(0:5, unique(학생$학과))) %>% 
    as_tibble() %>% 
    select(학과, names(학생)[2+i]) %>%
    table() %>% as_tibble() %>% filter(!학과 %in% c(0:5)) %>% 
    pivot_wider(names_from = names(학생)[2+i], values_from = n) %>% 
    select(1,7,6,5,4,3) %>% 
    rename(c(구분 = 학과, 
             "freq5" = "5", 
             "freq4" = "4",
             "freq3" = "3",
             "freq2" = "2",
             "freq1" = "1")) %>% 
    column_to_rownames(var = "구분") %>% 
    colSums() %>% as.matrix() %>% t() %>% as.data.frame() 
  rownames(df_전체.tmp) <- c("전체")
  df_전체.tmp <- df_전체.tmp %>% rownames_to_column(var = "구분")

df_전체.tmp %>% 
  mutate(빈도 = rowSums(across(where(is.numeric)))) %>% 
  mutate(긍정 = round((freq5 + freq4)/빈도, 3)) %>% 
  mutate(보통 = round(freq3/빈도, 3)) %>% 
  mutate(부정 = round((freq2 + freq1)/빈도, 3)) %>% 
  select(긍정, 보통, 부정) %>% t() %>% as.data.frame() %>% 
  rownames_to_column(var = "구분") %>% 
   write.xlsx(sheetName = names(학생)[2+i], file = "D:/대학원/연구과제/2023/2023_학생_학과_Q_파이차트d.xlsx", append = TRUE)
  # ggplot(aes(x = "", y = 전체, fill = 구분)) +
  # geom_bar(width = 1, stat = "identity", color = "white") +
  # coord_polar("y") +
  # geom_label_repel(aes(label = paste0(구분,"\n",round(전체*100,2), "%")), 
  #                  size=4, show.legend = F, nudge_x = 1) +
  # scale_fill_manual(values=c("#F6AE2D", "#F26419", "#55DDE0"))+
  # theme_void() +
  # theme(legend.position = "none")
  # 
  # png(filename = filepath[i+4], width = 600, unit = "px", height = 250, bg="transparent")
  # 
cat("Q",i,'piechart data complete.\n') 
}
# paste1 = 매우 그렇다 / paste5 = 전혀 아니다


# 5) 현장실습 내용의 실효성 : 전공별 만족도 점수
# 5.1) 전공지식과 실무능력에 대한 이해정도 
# [표 3.1.18] 전공지식과 실무능력에 대한 이해정도 : 만족도 점수
# Q01
i = 1

for(i in 1:(length(filepath)-2)){
학생_long %>% 
  filter(Q == names(학생)[2+i]) %>% 
  group_by(학과) %>% 
  summarize(빈도 = n(),
            최소값 = min(value),
            최대값 = max(value),
            평균 = round(mean(value, na.rm = TRUE), digits = 2),
            표준편차 = round(sd(value, na.rm = TRUE), digits = 3))%>% 
    write.xlsx(sheetName = names(학생)[2+i], file = "E:/대학원/연구과제/2023/2023_학생_학과_Q_기초통계량.xlsx", append = TRUE)
  cat("Q",i,'bar data complete.\n') 
}

filepath <- paste0("D:/대학원/연구과제/2023/학생_학과_",names(기업),".png")

for(i in 1:(length(filepath)-2)){
  기업_long %>% 
    filter(Q == names(기업)[2+i]) %>% 
    group_by(학과) %>% 
    summarize(빈도 = n(),
              최소값 = min(value),
              최대값 = max(value),
              평균 = round(mean(value, na.rm = TRUE), digits = 2),
              표준편차 = round(sd(value, na.rm = TRUE), digits = 3))%>% 
    write.xlsx(sheetName = names(기업)[2+i], file = "D:/대학원/연구과제/2023/2023_기업_학과_Q_기초통계량.xlsx", append = TRUE)
  cat("Q",i,'bar data complete.\n') 
}
기업_long %>% View()
## 2. 참여 기업부문 (문항별)
# 1] 현장실습 내용의 실효성 : 응답비율
# 1.1) 전공지식과 실무능력에 대한 이해정도
# [표 3.1.1] 전공지식과 실무능력에 대한 이해 정도
filepath <- paste0("D:/대학원/연구과제/2023/기업_학과_",names(기업),".png")

for(i in 1:(length(filepath)-2)){
  df_전체.tmp <- 기업 %>% 
    lapply(factor, levels = c(0:5, unique(기업$학과))) %>% 
    as_tibble() %>% 
    select(학과, names(기업)[2+i]) %>%
    table() %>% as_tibble() %>% filter(!학과 %in% c(0:5)) %>% 
    pivot_wider(names_from = names(기업)[2+i], values_from = n) %>% 
    select(1,7,6,5,4,3) %>% 
    rename(c(구분 = 학과, 
             "freq5" = "5", 
             "freq4" = "4",
             "freq3" = "3",
             "freq2" = "2",
             "freq1" = "1")) %>% 
    column_to_rownames(var = "구분") %>% 
    colSums() %>% as.matrix() %>% t() %>% as.data.frame() 
  rownames(df_전체.tmp) <- c("전체")
  df_전체.tmp <- df_전체.tmp %>% rownames_to_column(var = "구분")
  
  df_전체.tmp %>% bind_rows(
    기업 %>% 
      lapply(factor, levels = c(0:5, unique(기업$학과))) %>% 
      as_tibble() %>% 
      select(학과, names(기업)[2+i]) %>%
      table() %>% as_tibble() %>% filter(!학과 %in% c(0:5)) %>% 
      pivot_wider(names_from = names(기업)[2+i], values_from = n) %>% 
      select(1,7,6,5,4,3) %>% 
      rename(c(구분 = 학과, 
               "freq5" = "5", 
               "freq4" = "4",
               "freq3" = "3",
               "freq2" = "2",
               "freq1" = "1"))) %>% 
    mutate(빈도 = rowSums(across(where(is.numeric)))) %>% 
    mutate(prop5 = round(freq5/빈도*100,1)) %>% 
    mutate(prop4 = round(freq4/빈도*100,1)) %>% 
    mutate(prop3 = round(freq3/빈도*100,1)) %>% 
    mutate(prop2 = round(freq2/빈도*100,1)) %>% 
    mutate(prop1 = round(freq1/빈도*100,1)) %>% 
    mutate("paste1" = paste0(freq5,"(",prop5,"%",")")) %>% 
    mutate("paste2" = paste0(freq4,"(",prop4,"%",")")) %>% 
    mutate("paste3" = paste0(freq3,"(",prop3,"%",")")) %>% 
    mutate("paste4" = paste0(freq2,"(",prop2,"%",")")) %>% 
    mutate("paste5" = paste0(freq1,"(",prop1,"%",")")) %>% 
    select(구분, 빈도, paste1, paste2, paste3, paste4, paste5) %>% 
    write.xlsx(sheetName = names(기업)[2+i], file = "E:/대학원/연구과제/2023/2023_기업_학과_Q_빈도.xlsx", append = TRUE)
  
  cat("Q",i,'cross table complete.\n') 
}

for(i in 1:(length(filepath)-2)){
  df_전체.tmp <- 기업 %>% 
    lapply(factor, levels = c(0:5, unique(기업$학과))) %>% 
    as_tibble() %>% 
    select(학과, names(기업)[2+i]) %>%
    table() %>% as_tibble() %>% filter(!학과 %in% c(0:5)) %>% 
    pivot_wider(names_from = names(기업)[2+i], values_from = n) %>% 
    select(1,7,6,5,4,3) %>% 
    rename(c(구분 = 학과, 
             "freq5" = "5", 
             "freq4" = "4",
             "freq3" = "3",
             "freq2" = "2",
             "freq1" = "1")) %>% 
    column_to_rownames(var = "구분") %>% 
    colSums() %>% as.matrix() %>% t() %>% as.data.frame() 
  rownames(df_전체.tmp) <- c("전체")
  df_전체.tmp <- df_전체.tmp %>% rownames_to_column(var = "구분")
  
  df_전체.tmp %>% 
    mutate(빈도 = rowSums(across(where(is.numeric)))) %>% 
    mutate(긍정 = round((freq5 + freq4)/빈도, 3)) %>% 
    mutate(보통 = round(freq3/빈도, 3)) %>% 
    mutate(부정 = round((freq2 + freq1)/빈도, 3)) %>% 
    select(긍정, 보통, 부정) %>% t() %>% as.data.frame() %>% 
    rownames_to_column(var = "구분") %>% 
    write.xlsx(sheetName = names(기업)[2+i], file = "E:/대학원/연구과제/2023/2023_기업_학과_Q_파이차트.xlsx", append = TRUE)
  # ggplot(aes(x = "", y = 전체, fill = 구분)) +
  # geom_bar(width = 1, stat = "identity", color = "white") +
  # coord_polar("y") +
  # geom_label_repel(aes(label = paste0(구분,"\n",round(전체*100,2), "%")), 
  #                  size=4, show.legend = F, nudge_x = 1) +
  # scale_fill_manual(values=c("#F6AE2D", "#F26419", "#55DDE0"))+
  # theme_void() +
  # theme(legend.position = "none")
  # 
  # png(filename = filepath[i+4], width = 600, unit = "px", height = 250, bg="transparent")
  # 
  cat("Q",i,'piechart data complete.\n') 
}

# ※ 가장 적절한 현장실습 학생 인원 (전공별 분석)  - 기업 Q11

