library(tidyverse)
library(agricolae)


data3 <- read.csv("D:/유아특수논문/data3.csv", header = TRUE)
data3 %>% str()
data3$보조교사 <- as.integer(data3$보조교사) 
data3$보육경력 <- as.factor(data3$보육경력) 
data3$연령 <- as.factor(data3$연령) 
data3$학력 <- as.factor(data3$학력) 
data3$애착유형 <- as.factor(data3$애착유형) 

# 보육경력에 따른 직무 스트레스
스트레스_list <- paste0("스트레스",1:26)

보육경력1_스트레스 <- data3 %>% 
  filter(보육경력 == 1) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력2_스트레스 <- data3 %>% 
  filter(보육경력 == 2) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력3_스트레스 <- data3 %>% 
  filter(보육경력 == 3) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력4_스트레스 <- data3 %>% 
  filter(보육경력 == 4) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력_스트레스 <- c(보육경력1_스트레스,보육경력2_스트레스,보육경력3_스트레스,보육경력4_스트레스)

group1 <- rep(1, length(보육경력1_스트레스)) 
group2 <- rep(2, length(보육경력2_스트레스)) 
group3 <- rep(3, length(보육경력3_스트레스)) 
group4 <- rep(4, length(보육경력4_스트레스)) 

group <- c(group1,group2,group3,group4)

보육경력_스트레스df <- data.frame(보육경력_스트레스, group)
보육경력_스트레스df$group <- 보육경력_스트레스df$group %>% as.factor()

# 평균 표준편차
보육경력_스트레스df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(보육경력_스트레스),
            group_sd = sd(보육경력_스트레스),
            min = min(보육경력_스트레스),
            max = max(보육경력_스트레스))

(data3 %>% filter(보육경력 == 1) %>% dim())[1]
(data3 %>% filter(보육경력 == 2) %>% dim())[1]
(data3 %>% filter(보육경력 == 3) %>% dim())[1]
(data3 %>% filter(보육경력 == 4) %>% dim())[1]

보육경력_스트레스df %>% 
  summarise(group_mean = mean(보육경력_스트레스),
            group_sd = sd(보육경력_스트레스),
            min = min(보육경력_스트레스),
            max = max(보육경력_스트레스))

# anova
aov(보육경력_스트레스 ~ group, data = 보육경력_스트레스df) %>% summary()

# scheffe
보육경력_스트레스_result <- aov(보육경력_스트레스 ~ group, data = 보육경력_스트레스df)
scheffe.test(보육경력_스트레스_result, "group", alpha = 0.05, console = TRUE)


# 연령에 따른 직무 스트레스
연령1_스트레스 <- data3 %>% 
  filter(연령 == 1) %>% 
  select(스트레스_list) %>% 
  colMeans() %>% as.vector()

연령2_스트레스 <- data3 %>% 
  filter(연령 == 2) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

연령3_스트레스 <- data3 %>% 
  filter(연령 == 3) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

연령4_스트레스 <- data3 %>% 
  filter(연령 == 4) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

연령_스트레스 <- c(연령1_스트레스,연령2_스트레스,연령3_스트레스,연령4_스트레스)

group1 <- rep(1, length(연령1_스트레스)) 
group2 <- rep(2, length(연령2_스트레스)) 
group3 <- rep(3, length(연령3_스트레스)) 
group4 <- rep(4, length(연령4_스트레스)) 

group <- c(group1,group2,group3,group4)

연령_스트레스df <- data.frame(연령_스트레스, group)
연령_스트레스df$group <- 연령_스트레스df$group %>% as.factor()

# 평균 표준편차
연령_스트레스df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(연령_스트레스),
            group_sd = sd(연령_스트레스))

# anova
aov(연령_스트레스 ~ group, data = 연령_스트레스df) %>% summary()

# scheffe
연령_스트레스_result <- aov(연령_스트레스 ~ group, data = 연령_스트레스df)
scheffe.test(연령_스트레스_result, "group", alpha = 0.05, console = TRUE)


# 학력에 따른 직무 스트레스
학력1_스트레스 <- data3 %>% 
  filter(학력 == 1) %>% 
  select(스트레스_list) %>% 
  colMeans() %>% as.vector()

학력2_스트레스 <- data3 %>% 
  filter(학력 == 2) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

학력3_스트레스 <- data3 %>% 
  filter(학력 == 3) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

학력4_스트레스 <- data3 %>% 
  filter(학력 == 4) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

학력_스트레스 <- c(학력1_스트레스,학력2_스트레스,학력3_스트레스,학력4_스트레스)

group1 <- rep(1, length(학력1_스트레스)) 
group2 <- rep(2, length(학력2_스트레스)) 
group3 <- rep(3, length(학력3_스트레스)) 
group4 <- rep(4, length(학력4_스트레스)) 

group <- c(group1,group2,group3,group4)

학력_스트레스df <- data.frame(학력_스트레스, group)
학력_스트레스df$group <- 학력_스트레스df$group %>% as.factor()

# 평균 표준편차
학력_스트레스df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(학력_스트레스),
            group_sd = sd(학력_스트레스))

# anova
aov(학력_스트레스 ~ group, data = 학력_스트레스df) %>% summary()

# scheffe
학력_스트레스_result <- aov(학력_스트레스 ~ group, data = 학력_스트레스df)
scheffe.test(학력_스트레스_result, "group", alpha = 0.05, console = TRUE)



# 보육경력에 따른 대처방안
대처방안_list <- paste0("대처방안",1:16)

보육경력1_대처방안 <- data3 %>% 
  filter(보육경력 == 1) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력2_대처방안 <- data3 %>% 
  filter(보육경력 == 2) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력3_대처방안 <- data3 %>% 
  filter(보육경력 == 3) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력4_대처방안 <- data3 %>% 
  filter(보육경력 == 4) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

보육경력_대처방안 <- c(보육경력1_대처방안,보육경력2_대처방안,보육경력3_대처방안,보육경력4_대처방안)

group1 <- rep(1, length(보육경력1_대처방안)) 
group2 <- rep(2, length(보육경력2_대처방안)) 
group3 <- rep(3, length(보육경력3_대처방안)) 
group4 <- rep(4, length(보육경력4_대처방안)) 

group <- c(group1,group2,group3,group4)

보육경력_대처방안df <- data.frame(보육경력_대처방안, group)
보육경력_대처방안df$group <- 보육경력_대처방안df$group %>% as.factor()

# 평균 표준편차
보육경력_대처방안df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(보육경력_대처방안),
            group_sd = sd(보육경력_대처방안))

# anova
aov(보육경력_대처방안 ~ group, data = 보육경력_대처방안df) %>% summary()

# scheffe
보육경력_대처방안_result <- aov(보육경력_대처방안 ~ group, data = 보육경력_대처방안df)
scheffe.test(보육경력_대처방안_result, "group", alpha = 0.05, console = TRUE)


# 연령에 따른 대처방안
연령1_대처방안 <- data3 %>% 
  filter(연령 == 1) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

연령2_대처방안 <- data3 %>% 
  filter(연령 == 2) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

연령3_대처방안 <- data3 %>% 
  filter(연령 == 3) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

연령4_대처방안 <- data3 %>% 
  filter(연령 == 4) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

연령_대처방안 <- c(연령1_대처방안,연령2_대처방안,연령3_대처방안,연령4_대처방안)

group1 <- rep(1, length(연령1_대처방안)) 
group2 <- rep(2, length(연령2_대처방안)) 
group3 <- rep(3, length(연령3_대처방안)) 
group4 <- rep(4, length(연령4_대처방안)) 

group <- c(group1,group2,group3,group4)

연령_대처방안df <- data.frame(연령_대처방안, group)
연령_대처방안df$group <- 연령_대처방안df$group %>% as.factor()

# 평균 표준편차
연령_대처방안df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(연령_대처방안),
            group_sd = sd(연령_대처방안))

# anova
aov(연령_대처방안 ~ group, data = 연령_대처방안df) %>% summary()

# scheffe
연령_대처방안_result <- aov(연령_대처방안 ~ group, data = 연령_대처방안df)
scheffe.test(연령_대처방안_result, "group", alpha = 0.05, console = TRUE)


# 학력에 따른 대처방안
학력1_대처방안 <- data3 %>% 
  filter(학력 == 1) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

학력2_대처방안 <- data3 %>% 
  filter(학력 == 2) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

학력3_대처방안 <- data3 %>% 
  filter(학력 == 3) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

학력4_대처방안 <- data3 %>% 
  filter(학력 == 4) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

학력_대처방안 <- c(학력1_대처방안,학력2_대처방안,학력3_대처방안,학력4_대처방안)

group1 <- rep(1, length(학력1_대처방안)) 
group2 <- rep(2, length(학력2_대처방안)) 
group3 <- rep(3, length(학력3_대처방안)) 
group4 <- rep(4, length(학력4_대처방안)) 

group <- c(group1,group2,group3,group4)

학력_대처방안df <- data.frame(학력_대처방안, group)
학력_대처방안df$group <- 학력_대처방안df$group %>% as.factor()

# 평균 표준편차
학력_대처방안df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(학력_대처방안),
            group_sd = sd(학력_대처방안))

# anova
aov(학력_대처방안 ~ group, data = 학력_대처방안df) %>% summary()

# scheffe
학력_대처방안_result <- aov(학력_대처방안 ~ group, data = 학력_대처방안df)
scheffe.test(학력_대처방안_result, "group", alpha = 0.05, console = TRUE)


# 애착유형에 따른 직무스트레스 차이
스트레스_list <- paste0("스트레스",1:26)

애착유형1_스트레스 <- data3 %>% 
  filter(애착유형 == 1) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형2_스트레스 <- data3 %>% 
  filter(애착유형 == 2) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형3_스트레스 <- data3 %>% 
  filter(애착유형 == 3) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형4_스트레스 <- data3 %>% 
  filter(애착유형 == 4) %>% 
  select(스트레스_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형_스트레스 <- c(애착유형1_스트레스,애착유형2_스트레스,애착유형3_스트레스,애착유형4_스트레스)

group1 <- rep(1, length(애착유형1_스트레스)) 
group2 <- rep(2, length(애착유형2_스트레스)) 
group3 <- rep(3, length(애착유형3_스트레스)) 
group4 <- rep(4, length(애착유형4_스트레스)) 

group <- c(group1,group2,group3,group4)

애착유형_스트레스df <- data.frame(애착유형_스트레스, group)
애착유형_스트레스df$group <- 애착유형_스트레스df$group %>% as.factor()

# 평균 표준편차
애착유형_스트레스df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(애착유형_스트레스),
            group_sd = sd(애착유형_스트레스))

# anova
aov(애착유형_스트레스 ~ group, data = 애착유형_스트레스df) %>% summary()

# scheffe
애착유형_스트레스_result <- aov(애착유형_스트레스 ~ group, data = 애착유형_스트레스df)
scheffe.test(애착유형_스트레스_result, "group", alpha = 0.05, console = TRUE)


# 애착유형에 따른 대처방안 차이
대처방안_list <- paste0("대처방안",1:16)

애착유형1_대처방안 <- data3 %>% 
  filter(애착유형 == 1) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형2_대처방안 <- data3 %>% 
  filter(애착유형 == 2) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형3_대처방안 <- data3 %>% 
  filter(애착유형 == 3) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형4_대처방안 <- data3 %>% 
  filter(애착유형 == 4) %>% 
  select(대처방안_list) %>% 
  apply(2,mean) %>% as.vector()

애착유형_대처방안 <- c(애착유형1_대처방안,애착유형2_대처방안,애착유형3_대처방안,애착유형4_대처방안)

group1 <- rep(1, length(애착유형1_대처방안)) 
group2 <- rep(2, length(애착유형2_대처방안)) 
group3 <- rep(3, length(애착유형3_대처방안)) 
group4 <- rep(4, length(애착유형4_대처방안)) 

group <- c(group1,group2,group3,group4)

애착유형_대처방안df <- data.frame(애착유형_대처방안, group)
애착유형_대처방안df$group <- 애착유형_대처방안df$group %>% as.factor()

# 평균 표준편차
애착유형_대처방안df %>% 
  group_by(group) %>% 
  summarise(group_mean = mean(애착유형_대처방안),
            group_sd = sd(애착유형_대처방안))

# anova
aov(애착유형_대처방안 ~ group, data = 애착유형_대처방안df) %>% summary()

# scheffe
애착유형_대처방안_result <- aov(애착유형_대처방안 ~ group, data = 애착유형_대처방안df)
scheffe.test(애착유형_대처방안_result, "group", alpha = 0.05, console = TRUE)



# 애착유형에 따른 직무스트레스 하위요인 차이
library(HH)

과부하_list <- paste0("스트레스",c(2,4,5,7,8,15,21,24))
동료_list <- paste0("스트레스",c(3,6,12,20,22))
학부모_list <- paste0("스트레스",c(1,16,23,26))
원장_list <- paste0("스트레스",c(9,10,11,13,14,17,18,19,25))

애착유형_과부하 <- data3 %>% 
  select(애착유형, all_of(과부하_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean2 = mean(스트레스2),
            mean4 = mean(스트레스4),
            mean5 = mean(스트레스5),
            mean7 = mean(스트레스7),
            mean8 = mean(스트레스8),
            mean15 = mean(스트레스15),
            mean21 = mean(스트레스21),
            mean24 = mean(스트레스24)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균)
애착유형_과부하$직무하위 <- c("과부하")

애착유형_동료 <- data3 %>% 
  select(애착유형, all_of(동료_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean3 = mean(스트레스3),
            mean6 = mean(스트레스6),
            mean12 = mean(스트레스12),
            mean20 = mean(스트레스20),
            mean22 = mean(스트레스22)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균)
애착유형_동료$직무하위 <- c("동료")

애착유형_학부모 <- data3 %>% 
  select(애착유형, all_of(학부모_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean1 = mean(스트레스1),
            mean16 = mean(스트레스16),
            mean23 = mean(스트레스23),
            mean26 = mean(스트레스26)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균)
애착유형_학부모$직무하위 <- c("학부모")

애착유형_원장 <- data3 %>% 
  select(애착유형, all_of(원장_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean9 = mean(스트레스9),
            mean10 = mean(스트레스10),
            mean11 = mean(스트레스11),
            mean13 = mean(스트레스13),
            mean14 = mean(스트레스14),
            mean17 = mean(스트레스17),
            mean18 = mean(스트레스18),
            mean19 = mean(스트레스19),
            mean25 = mean(스트레스25)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형_원장$직무하위 <- c("원장")

애착유형_직무하위df <- bind_rows(애착유형_과부하,애착유형_동료,애착유형_학부모,애착유형_원장)
애착유형_직무하위df$직무하위 <- 애착유형_직무하위df$직무하위 %>% as.factor()

# ancova
애착유형_직무하위df$애착유형 <- 애착유형_직무하위df$애착유형 %>% as.integer()
ancova(freq ~ 애착유형 + 직무하위, data = 애착유형_직무하위df)

애착유형_직무하위df$애착유형 <- 애착유형_직무하위df$애착유형 %>% as.factor()
aov(freq ~ 애착유형 + 직무하위, data = 애착유형_직무하위df) %>% summary()

# scheffe
애착유형_직무하위_result <- aov(freq ~ 애착유형 + 직무하위, data = 애착유형_직무하위df)
scheffe.test(애착유형_직무하위_result, "애착유형", alpha = 0.05, console = TRUE)

# 애착유형 각각에 영향을 주는 직무 스트레스 요인
# 애착유형 1
애착유형1_과부하 <- data3 %>% 
  select(애착유형, all_of(과부하_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_과부하$직무하위 <- c("과부하")

애착유형1_동료 <- data3 %>% 
  select(애착유형, all_of(동료_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_동료$직무하위 <- c("동료")

애착유형1_학부모 <- data3 %>% 
  select(애착유형, all_of(학부모_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_학부모$직무하위 <- c("학부모")

애착유형1_원장 <- data3 %>% 
  select(애착유형, all_of(원장_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_원장$직무하위 <- c("원장")

애착유형1_직무하위df <- bind_rows(애착유형1_과부하,애착유형1_동료,애착유형1_학부모,애착유형1_원장)
애착유형1_직무하위df$직무하위 <- 애착유형1_직무하위df$직무하위 %>% as.factor()

# anova
aov(freq ~ 직무하위, data = 애착유형1_직무하위df) %>% summary()

# scheffe
애착유형1_직무하위_result <- aov(freq ~ 직무하위, data = 애착유형1_직무하위df)
scheffe.test(애착유형1_직무하위_result, "직무하위", alpha = 0.05, console = TRUE)


# 애착유형 2
애착유형2_과부하 <- data3 %>% 
  select(애착유형, all_of(과부하_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_과부하$직무하위 <- c("과부하")

애착유형2_동료 <- data3 %>% 
  select(애착유형, all_of(동료_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_동료$직무하위 <- c("동료")

애착유형2_학부모 <- data3 %>% 
  select(애착유형, all_of(학부모_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_학부모$직무하위 <- c("학부모")

애착유형2_원장 <- data3 %>% 
  select(애착유형, all_of(원장_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_원장$직무하위 <- c("원장")

애착유형2_직무하위df <- bind_rows(애착유형2_과부하,애착유형2_동료,애착유형2_학부모,애착유형2_원장)
애착유형2_직무하위df$직무하위 <- 애착유형2_직무하위df$직무하위 %>% as.factor()

# anova
aov(freq ~ 직무하위, data = 애착유형2_직무하위df) %>% summary()

# scheffe
애착유형2_직무하위_result <- aov(freq ~ 직무하위, data = 애착유형2_직무하위df)
scheffe.test(애착유형2_직무하위_result, "직무하위", alpha = 0.05, console = TRUE)



# 애착유형 3
애착유형3_과부하 <- data3 %>% 
  select(애착유형, all_of(과부하_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_과부하$직무하위 <- c("과부하")

애착유형3_동료 <- data3 %>% 
  select(애착유형, all_of(동료_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_동료$직무하위 <- c("동료")

애착유형3_학부모 <- data3 %>% 
  select(애착유형, all_of(학부모_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_학부모$직무하위 <- c("학부모")

애착유형3_원장 <- data3 %>% 
  select(애착유형, all_of(원장_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_원장$직무하위 <- c("원장")

애착유형3_직무하위df <- bind_rows(애착유형3_과부하,애착유형3_동료,애착유형3_학부모,애착유형3_원장)
애착유형3_직무하위df$직무하위 <- 애착유형3_직무하위df$직무하위 %>% as.factor()

# anova
aov(freq ~ 직무하위, data = 애착유형3_직무하위df) %>% summary()

# scheffe
애착유형3_직무하위_result <- aov(freq ~ 직무하위, data = 애착유형3_직무하위df)
scheffe.test(애착유형3_직무하위_result, "직무하위", alpha = 0.05, console = TRUE)


# 애착유형 4
애착유형4_과부하 <- data3 %>% 
  select(애착유형, all_of(과부하_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_과부하$직무하위 <- c("과부하")

애착유형4_동료 <- data3 %>% 
  select(애착유형, all_of(동료_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_동료$직무하위 <- c("동료")

애착유형4_학부모 <- data3 %>% 
  select(애착유형, all_of(학부모_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_학부모$직무하위 <- c("학부모")

애착유형4_원장 <- data3 %>% 
  select(애착유형, all_of(원장_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_원장$직무하위 <- c("원장")

애착유형4_직무하위df <- bind_rows(애착유형4_과부하,애착유형4_동료,애착유형4_학부모,애착유형4_원장)
애착유형4_직무하위df$직무하위 <- 애착유형4_직무하위df$직무하위 %>% as.factor()

# anova
aov(freq ~ 직무하위, data = 애착유형4_직무하위df) %>% summary()

# scheffe
애착유형4_직무하위_result <- aov(freq ~ 직무하위, data = 애착유형4_직무하위df)
scheffe.test(애착유형4_직무하위_result, "직무하위", alpha = 0.05, console = TRUE)



# 애착유형에 따른 대처방안 하위요인 차이
library(HH)

문제_list <- paste0("대처방안",c(1,2,3,4))
심리_list <- paste0("대처방안",c(5,6,7,8))
사회_list <- paste0("대처방안",c(9,10,11,12))
소망_list <- paste0("대처방안",c(13,14,15,16))

애착유형_문제 <- data3 %>% 
  select(애착유형, all_of(문제_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean1 = mean(대처방안1),
            mean2 = mean(대처방안2),
            mean3 = mean(대처방안3),
            mean4 = mean(대처방안4)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균)
애착유형_문제$대처하위 <- c("문제")

애착유형_심리 <- data3 %>% 
  select(애착유형, all_of(심리_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean5 = mean(대처방안5),
            mean6 = mean(대처방안6),
            mean7 = mean(대처방안7),
            mean8 = mean(대처방안8)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균)
애착유형_심리$대처하위 <- c("심리")

애착유형_사회 <- data3 %>% 
  select(애착유형, all_of(사회_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean9 = mean(대처방안9),
            mean10 = mean(대처방안10),
            mean11 = mean(대처방안11),
            mean12 = mean(대처방안12)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균)
애착유형_사회$대처하위 <- c("사회")

애착유형_소망 <- data3 %>% 
  select(애착유형, all_of(소망_list)) %>% 
  group_by(애착유형) %>% 
  summarise(mean13 = mean(대처방안13),
            mean14 = mean(대처방안14),
            mean15 = mean(대처방안15),
            mean16 = mean(대처방안16)) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형_소망$대처하위 <- c("소망")

애착유형_대처하위df <- bind_rows(애착유형_문제,애착유형_심리,애착유형_사회,애착유형_소망)
애착유형_대처하위df$대처하위 <- 애착유형_대처하위df$대처하위 %>% as.factor()

# ancova
애착유형_대처하위df$애착유형 <- 애착유형_대처하위df$애착유형 %>% as.integer()
ancova(freq ~ 애착유형 + 대처하위, data = 애착유형_대처하위df)

애착유형_대처하위df$애착유형 <- 애착유형_대처하위df$애착유형 %>% as.factor()
aov(freq ~ 애착유형 + 대처하위, data = 애착유형_대처하위df) %>% summary()

# scheffe
애착유형_대처하위_result <- aov(freq ~ 애착유형 + 대처하위, data = 애착유형_대처하위df)
scheffe.test(애착유형_대처하위_result, "애착유형", alpha = 0.05, console = TRUE)



# 애착유형 각각에 영향을 주는 직무 스트레스 요인
# 애착유형 1
애착유형1_문제 <- data3 %>% 
  select(애착유형, all_of(문제_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_문제$대처하위 <- c("문제")

애착유형1_심리 <- data3 %>% 
  select(애착유형, all_of(심리_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_심리$대처하위 <- c("심리")

애착유형1_사회 <- data3 %>% 
  select(애착유형, all_of(사회_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_사회$대처하위 <- c("사회")

애착유형1_소망 <- data3 %>% 
  select(애착유형, all_of(소망_list)) %>% 
  filter(애착유형 == 1) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형1_소망$대처하위 <- c("소망")

애착유형1_대처하위df <- bind_rows(애착유형1_문제,애착유형1_심리,애착유형1_사회,애착유형1_소망)
애착유형1_대처하위df$대처하위 <- 애착유형1_대처하위df$대처하위 %>% as.factor()

# anova
aov(freq ~ 대처하위, data = 애착유형1_대처하위df) %>% summary()

# scheffe
애착유형1_대처하위_result <- aov(freq ~ 대처하위, data = 애착유형1_대처하위df)
scheffe.test(애착유형1_대처하위_result, "대처하위", alpha = 0.05, console = TRUE)


# 애착유형 2
애착유형2_문제 <- data3 %>% 
  select(애착유형, all_of(문제_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_문제$대처하위 <- c("문제")

애착유형2_심리 <- data3 %>% 
  select(애착유형, all_of(심리_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_심리$대처하위 <- c("심리")

애착유형2_사회 <- data3 %>% 
  select(애착유형, all_of(사회_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_사회$대처하위 <- c("사회")

애착유형2_소망 <- data3 %>% 
  select(애착유형, all_of(소망_list)) %>% 
  filter(애착유형 == 2) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형2_소망$대처하위 <- c("소망")

애착유형2_대처하위df <- bind_rows(애착유형2_문제,애착유형2_심리,애착유형2_사회,애착유형2_소망)
애착유형2_대처하위df$대처하위 <- 애착유형2_대처하위df$대처하위 %>% as.factor()

# anova
aov(freq ~ 대처하위, data = 애착유형2_대처하위df) %>% summary()

# scheffe
애착유형2_대처하위_result <- aov(freq ~ 대처하위, data = 애착유형2_대처하위df)
scheffe.test(애착유형2_대처하위_result, "대처하위", alpha = 0.05, console = TRUE)



# 애착유형 3
애착유형3_문제 <- data3 %>% 
  select(애착유형, all_of(문제_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_문제$대처하위 <- c("문제")

애착유형3_심리 <- data3 %>% 
  select(애착유형, all_of(심리_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_심리$대처하위 <- c("심리")

애착유형3_사회 <- data3 %>% 
  select(애착유형, all_of(사회_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_사회$대처하위 <- c("사회")

애착유형3_소망 <- data3 %>% 
  select(애착유형, all_of(소망_list)) %>% 
  filter(애착유형 == 3) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형3_소망$대처하위 <- c("소망")

애착유형3_대처하위df <- bind_rows(애착유형3_문제,애착유형3_심리,애착유형3_사회,애착유형3_소망)
애착유형3_대처하위df$대처하위 <- 애착유형3_대처하위df$대처하위 %>% as.factor()

# anova
aov(freq ~ 대처하위, data = 애착유형3_대처하위df) %>% summary()

# scheffe
애착유형3_대처하위_result <- aov(freq ~ 대처하위, data = 애착유형3_대처하위df)
scheffe.test(애착유형3_대처하위_result, "대처하위", alpha = 0.05, console = TRUE)


# 애착유형 4
애착유형4_문제 <- data3 %>% 
  select(애착유형, all_of(문제_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_문제$대처하위 <- c("문제")

애착유형4_심리 <- data3 %>% 
  select(애착유형, all_of(심리_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_심리$대처하위 <- c("심리")

애착유형4_사회 <- data3 %>% 
  select(애착유형, all_of(사회_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_사회$대처하위 <- c("사회")

애착유형4_소망 <- data3 %>% 
  select(애착유형, all_of(소망_list)) %>% 
  filter(애착유형 == 4) %>% 
  pivot_longer(cols = -애착유형, names_to = "문항평균", values_to = "freq") %>% 
  select(-문항평균) 
애착유형4_소망$대처하위 <- c("소망")

애착유형4_대처하위df <- bind_rows(애착유형4_문제,애착유형4_심리,애착유형4_사회,애착유형4_소망)
애착유형4_대처하위df$대처하위 <- 애착유형4_대처하위df$대처하위 %>% as.factor()

# anova
aov(freq ~ 대처하위, data = 애착유형4_대처하위df) %>% summary()

# scheffe
애착유형4_대처하위_result <- aov(freq ~ 대처하위, data = 애착유형4_대처하위df)
scheffe.test(애착유형4_대처하위_result, "대처하위", alpha = 0.05, console = TRUE)
