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

# 데이터 불러오기
교양_df <- read_csv(file = "D:/대학원/textmining/Doit/대학.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))

교양_df %>% head()
교양_df %>% glimpse()

# 교양 과목에서 명사만 추출
extractNoun(교양_df$과목[1])

교양_명사 <- sapply(교양_df$과목, extractNoun) %>% 
              unlist() %>% 
              as_tibble() %>% 
              mutate(단어 = str_match(value,"([가-힣]+)")[,2]) %>% 
              na.omit() %>% 
              mutate(글자수 = str_length(단어)) %>% 
              filter(글자수 > 1) %>% 
              select(단어)


# double bar graph
bar_col <- c("#E69F00", "#56B4E9", "#009E73","#D55E00")


# 교양과목  ~ 부산소재 대학 | 교류대학_지역 
교양_df %>% 
  filter(개설대학 %in% c("부산대", "부경대", "한국해양대", "부산교대")) %>%
  group_by(교류대학_지역,개설대학) %>% 
  distinct(과목, .keep_all = TRUE) %>%  # 교양과목 - 종류
  summarise(n = n()) %>% 
  mutate(교류대학_지역 = if_else(교류대학_지역 != "부산", "부산 외", 교류대학_지역)) %>% 
  group_by(교류대학_지역,개설대학) %>% 
  summarise(n = sum(n)) %>% 
  arrange(factor(개설대학, levels = c("부산대", "부경대", "한국해양대", "부산교대"))) %>% 
  ggplot(aes(x = factor(개설대학, levels = c("부산대", "부경대", "한국해양대", "부산교대")), y = n, fill = 교류대학_지역)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_text(aes(label = n), vjust = -1, position = position_dodge(0.9),size = 5) +
  scale_fill_manual(values = bar_col) +
  coord_cartesian(ylim = c(0, 250))
  

# 교류대학 ~ 부산소재 대학 | 교류대학_지역 
교양_df %>% 
  filter(개설대학 %in% c("부산대", "부경대", "한국해양대", "부산교대")) %>%
  group_by(교류대학_지역,개설대학) %>% 
  distinct(교류대학, .keep_all = TRUE) %>%  # 교류대학
  summarise(n = n()) %>% 
  mutate(교류대학_지역 = if_else(교류대학_지역 != "부산", "부산 외", 교류대학_지역)) %>% 
  group_by(교류대학_지역,개설대학) %>% 
  summarise(n = sum(n)) %>% 
  arrange(factor(개설대학, levels = c("부산대", "부경대", "한국해양대", "부산교대"))) %>% 
  ggplot(aes(x = factor(개설대학, levels = c("부산대", "부경대", "한국해양대", "부산교대")), y = n, fill = 교류대학_지역)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = "") +
  theme_minimal() +
  geom_text(aes(label = n), vjust = -1, position = position_dodge(0.9),size = 5) +
  scale_fill_manual(values = bar_col) +
  coord_cartesian(ylim = c(0, 50))


# 
교양_df %>% 
  filter(개설대학 == "부산대") %>%
  filter(개설대학_지역 == "부산") %>% 
  select(과목) %>% 
  sapply(extractNoun) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(단어 = str_match(value,"([가-힣]+)")[,2]) %>% 
  na.omit() %>% 
  mutate(글자수 = str_length(단어)) %>% 
  filter(글자수 > 1) %>% 
  select(단어)
