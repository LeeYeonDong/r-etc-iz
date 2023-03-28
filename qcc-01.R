# R 버전에 맞는 Rtools 설치
# 삭제된 패키지 아카이브 확인
# https://cran.r-project.org/web/packages/qualityTools/index.html
# https://cran.r-project.org/src/contrib/Archive/qualityTools/

# 오프라인 패키지 설치
install.packages("qualityTools_1.55.tar.gz", repos = NULL, type = "source")

# 깃헙 패키지 설치
devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

remotes::install_url("https://github.com/microsoft/LightGBM/releases/download/v3.0.0/lightgbm-3.0.0-r-cran.tar.gz")

install.packages("qcc")
library(qcc)
library(tidyverse)

# Generate random data
set.seed(1029)
data <- data.frame(replicate(3, rnorm(100, mean = 10, sd = 2)))
names(data) <- paste0("operator",1:3)
data$part <- paste0("M",1:10)

data %>% head()

data <- data %>% 
  pivot_longer(cols = -part, names_to = "operator", values_to = "Measurement")

# Measurement by part
ggplot(data, aes(x = part, y = Measurement, fill = part)) +
  geom_boxplot() +
  labs(title = "Measurement by part",
       x = "part",
       y = "Measurement") +
  theme_bw()

# Measurement by operator
ggplot(data, aes(x = operator, y = Measurement, fill = operator)) +
  geom_boxplot() +
  labs(title = "Measurement by operator",
       x = "operator",
       y = "Measurement") +
  theme_bw()

# xbar Chart by operator and part
data_mat <- data %>%
  group_by(operator) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = operator, values_from = Measurement) %>%
  select(-c(id,part))

qcc.groups(data$Measurement, data$operator) %>% 
  qcc(type = "xbar",plot = TRUE) %>% 
  summary()

qcc.groups(data$Measurement, data$part) %>% 
  qcc(type = "xbar", plot = TRUE)

# R Chart
data_mat %>% 
  qcc(type = "R", plot = TRUE) %>% 
  summary()
