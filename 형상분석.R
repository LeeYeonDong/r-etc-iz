library(tidyverse)

data <- read_csv(file = "D:/대학원/형상분석/국민건강영양조사-가중치.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

data <- data[,-c(1:2)]
data <- data %>% na.omit()
data %>% summary()

names(data) <- c("지역","가구조사","건강설문_검진조사","구강검사","폐기능검사_만40세이상",
                 "영양조사","건강설문_검진조사_영양조사","구강검사 & 영양조사 가중치","폐기능검사_영양조사_만40세이상")


library(simplevis)
gg_boxplot(data, 지역, 가구조사)

library(reshape)
data_longer <- pivot_longer(data = data, cols = -지역, names_to = "var", values_to = "freq")

data_longer %>% 
  filter(지역 == "부산") %>%
ggplot(aes(x = var, y = freq)) + 
geom_boxplot(alpha = 0.7, show.legend = FALSE, aes(fill = var)) +
  geom_jitter(width = 0.1, alpha = 0.9) +
  labs(x="", y="") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()



rm(list = ls())

library(tidyverse)

# -------------------------------------------------------------------------

data_raw <- read_csv("D:/대학원/형상분석/국민건강영양조사-가중치.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr')) %>% na.omit
data_raw %>% summary

data_raw %>% select(4:11) %>% as.matrix %>% cor %>% round(3)

region <- unique(data_raw$지역)
g <- length(region)
k <- 8
m <- 2

# -------------------------------------------------------------------------

data <- list()
for (i in 1:g){
  # i = 1
  data[[i]] <- data_raw %>% filter(지역 == region[i]) %>% select(-c(1:3))
  names(data)[i] <- region[i]
}

k2 <- k / 2
data_temp <- list()
for (i in 1:g){
  # i = 1
  data_temp[[i]] <- list()
  names(data_temp)[i] <- names(data)[i]
  for (j in 1:nrow(data[[i]])){
    # j = 1
    data_temp[[i]][[j]] <- data[[i]][j, ] %>% as.numeric %>% {matrix(., k2, m)}
  }
}



data_array_강원 <- array(dim=c(4,2,86))
for (i in 1:86){
  data_array_강원[,,i] <- array(data_temp$강원[[i]], dim = c(4,2))
}
plotshapes(data_array_강원, joinline=1:4)

data_array_경기 <- array(dim=c(4,2,586))
for (i in 1:586){
  data_array_경기[,,i] <- array(data_temp$경기[[i]], dim = c(4,2))
}
plotshapes(data_array_경기, joinline=1:4)

data_array_경남 <- array(dim=c(4,2,213))
for (i in 1:213){
  data_array_경남[,,i] <- array(data_temp$경남[[i]], dim = c(4,2))
}
plotshapes(data_array_경남, joinline=1:4)

data_array_경북 <- array(dim=c(4,2,157))
for (i in 1:157){
  data_array_경북[,,i] <- array(data_temp$경북[[i]], dim = c(4,2))
}
plotshapes(data_array_경북, joinline=1:4)

data_array_광주 <- array(dim=c(4,2,83))
for (i in 1:83){
  data_array_광주[,,i] <- array(data_temp$광주[[i]], dim = c(4,2))
}
plotshapes(data_array_광주, joinline=1:4)

data_array_대구 <- array(dim=c(4,2,174))
for (i in 1:174){
  data_array_대구[,,i] <- array(data_temp$대구[[i]], dim = c(4,2))
}
plotshapes(data_array_대구, joinline=1:4)

data_array_대전 <- array(dim=c(4,2,78))
for (i in 1:78){
  data_array_대전[,,i] <- array(data_temp$대전[[i]], dim = c(4,2))
}
plotshapes(data_array_대전, joinline=1:4)

data_array_대전 <- array(dim=c(4,2,194))
for (i in 1:194){
  data_array_대전[,,i] <- array(data_temp$부산[[i]], dim = c(4,2))
}
plotshapes(data_array_대전, joinline=1:4)

data_array_서울 <- array(dim=c(4,2,405))
for (i in 1:405){
  data_array_서울[,,i] <- array(data_temp$서울[[i]], dim = c(4,2))
}
plotshapes(data_array_서울, joinline=1:4)

data_array_세종 <- array(dim=c(4,2,38))
for (i in 1:38){
  data_array_세종[,,i] <- array(data_temp$세종[[i]], dim = c(4,2))
}
plotshapes(data_array_세종, joinline=1:4)

data_array_울산 <- array(dim=c(4,2,59))
for (i in 1:59){
  data_array_울산[,,i] <- array(data_temp$울산[[i]], dim = c(4,2))
}
plotshapes(data_array_울산, joinline=1:4)

data_array_인천 <- array(dim=c(4,2,119))
for (i in 1:119){
  data_array_인천[,,i] <- array(data_temp$인천[[i]], dim = c(4,2))
}
plotshapes(data_array_인천, joinline=1:4)

data_array_전남 <- array(dim=c(4,2,103))
for (i in 1:103){
  data_array_전남[,,i] <- array(data_temp$전남[[i]], dim = c(4,2))
}
plotshapes(data_array_전남, joinline=1:4)

data_array_전북 <- array(dim=c(4,2,96))
for (i in 1:96){
  data_array_전북[,,i] <- array(data_temp$전북[[i]], dim = c(4,2))
}
plotshapes(data_array_전북, joinline=1:4)

data_array_제주 <- array(dim=c(4,2,68))
for (i in 1:68){
  data_array_제주[,,i] <- array(data_temp$제주[[i]], dim = c(4,2))
}
plotshapes(data_array_제주, joinline=1:4)

data_array_충남 <- array(dim=c(4,2,107))
for (i in 1:107){
  data_array_충남[,,i] <- array(data_temp$충남[[i]], dim = c(4,2))
}
plotshapes(data_array_충남, joinline=1:4)

data_array_충북 <- array(dim=c(4,2,84))
for (i in 1:84){
  data_array_충북[,,i] <- array(data_temp$충북[[i]], dim = c(4,2))
}
plotshapes(data_array_충북, joinline=1:4)

data_array_전국 <- append(data_temp$서울, data_temp$부산)
data_array_전국 <- append(data_array_전국,data_temp$대구)
data_array_전국 <- append(data_array_전국,data_temp$인천)
data_array_전국 <- append(data_array_전국,data_temp$광주)
data_array_전국 <- append(data_array_전국,data_temp$대전)
data_array_전국 <- append(data_array_전국,data_temp$울산)
data_array_전국 <- append(data_array_전국,data_temp$경기)
data_array_전국 <- append(data_array_전국,data_temp$강원)
data_array_전국 <- append(data_array_전국,data_temp$충북)
data_array_전국 <- append(data_array_전국,data_temp$충남)
data_array_전국 <- append(data_array_전국,data_temp$전북)
data_array_전국 <- append(data_array_전국,data_temp$전남)
data_array_전국 <- append(data_array_전국,data_temp$경북)
data_array_전국 <- append(data_array_전국,data_temp$경남)
data_array_전국 <- append(data_array_전국,data_temp$제주)
data_array_전국 <- append(data_array_전국,data_temp$세종)

data_array <- array(dim=c(4,2,2650))
for (i in 1:2650){
  data_array[,,i] <- array(data_array_전국[[i]], dim = c(4,2))
}
plotshapes(data_array, joinline=1:4)

# 사전 중심화 평균형상 -------------------------------------------------------------------------

Centering_mat <- k2 %>% {diag(.) - matrix(1 / ., ., .)}
data_Centred_Pre_shape <- list()
for (i in 1:g){
  # i = 1
  data_Centred_Pre_shape[[i]] <- list()
  names(data_Centred_Pre_shape)[i] <- names(data_temp)[i]
  for (j in 1:length(data_temp[[i]])){
    # j = 1
    temp_data <- Centering_mat %*% data_temp[[i]][[j]]
    temp_size <- temp_data %>% {t(.) %*% .} %>% diag %>% sum %>% sqrt
    data_Centred_Pre_shape[[i]][[j]] <- temp_data / temp_size
  }
}

data_Centred_Pre_shape_mean <- list()
for (i in 1:g){
  # i = 1
  data_Centred_Pre_shape_mean[[i]] <- data_Centred_Pre_shape[[i]] %>% {Reduce('+', .)/ length(.)}
  names(data_Centred_Pre_shape_mean)[i] <- names(data_Centred_Pre_shape)[i]
}
data_Centred_Pre_shape_mean$전국 <- data_Centred_Pre_shape_mean %>% {Reduce('+', .)/ length(.)}

data_Centred_Pre_shape_mean_df <- tribble(~V1, ~V2, ~region)
for (i in 1:length(data_Centred_Pre_shape_mean)){
  # i = 1
  temp_data <- data_Centred_Pre_shape_mean[[i]] %>% as_tibble %>% mutate(region = names(data_Centred_Pre_shape_mean)[i])
  data_Centred_Pre_shape_mean_df <- data_Centred_Pre_shape_mean_df %>% add_row(temp_data)
}

data_Centred_Pre_shape_mean_df %>%
  mutate(region = region %>% fct_relevel("전국")) %>%
  filter(region == "전국")  %>% 
  ggplot(aes(x = V1, y = V2, group = region)) +
  geom_polygon() +
  geom_point(size = 2) +
  labs(x = "", y = "") +
  theme_bw()

# 북스테인 평균형상 -------------------------------------------------------------------------

library(shapes)



data_array <- list()
for (i in 1:g){
  # i = 1
  data_array[[i]] <- data_temp[[i]] %>% unlist %>% as.numeric %>% array(dim = c(4, 2, length(data_temp[[i]]))) 
  names(data_array)[i] <- names(data_temp)[i]
}

data_bookstein_mean_df <- tribble(~V1, ~V2, ~region)
temp_mat <- matrix(0, 4, 2)
for (i in 1:g){
  # i = 1
  temp_data <- data_array[[i]] %>% bookstein2d %>% .$mshape %>% as_tibble %>% mutate(region = names(data_temp)[i])
  temp_mat <- temp_mat + as.matrix(temp_data[, -3])
  data_bookstein_mean_df <- data_bookstein_mean_df %>% add_row(temp_data)
}
temp_mat <- temp_mat / g

data_bookstein_mean_df %>% add_row(temp_mat %>% as_tibble %>% mutate(region = "전국")) %>% 
  mutate(region = region %>% fct_relevel("전국")) %>%
  filter(region == "전국")  %>% 
  ggplot(aes(x = V1, y = V2, group = region)) +
  geom_polygon() +
  geom_point(size = 2) +
  labs(x = "", y = "") +
  theme_bw()

# GPA 전체형상과 평균형상 -------------------------------------------------------------------------

data_GPA_df <- data_GPA_mean_df <- tribble(~V1, ~V2, ~region)
temp_mat <- matrix(0, 4, 2)
for (i in 1:g){
  # i = 1
  temp_gpa <- data_array[[i]] %>% procGPA(distances = FALSE, pcaoutput = FALSE)
  temp_mat <- temp_mat + temp_gpa$mshape
  temp_data <- c()
  for (j in 1:dim(data_array[[i]])[3]){
    # j = 1
    temp_data <- temp_data %>% rbind(temp_gpa$rotated[, , j])
  }
  data_GPA_df <- data_GPA_df %>% add_row(temp_data %>% as_tibble %>% mutate(region = names(data_temp)[i]))
  data_GPA_mean_df <- data_GPA_mean_df %>% add_row(temp_gpa$mshape %>% as_tibble %>% mutate(region = names(data_temp)[i]))
}
temp_mat <- temp_mat / g
data_GPA_df <- data_GPA_df %>% add_row(data_GPA_df %>% mutate(region = "전국")) %>% mutate(region = region %>% fct_relevel("전국"))
data_GPA_mean_df <- data_GPA_mean_df %>% add_row(temp_mat %>% as_tibble %>% mutate(region = "전국")) %>% mutate(region = region %>% fct_relevel("전국"))

data_GPA_df %>% 
  ggplot(aes(x = V1, y = V2, group = region)) +
  # geom_point(size = 2) +
  geom_polygon(data = data_GPA_mean_df %>% filter(region == "충북")) +
  # facet_wrap(~ region, ncol = 2) +
  labs(x = "", y = "") +
  theme_bw()

# shapiro / t / riemann / proc -------------------------------------------------------------------------

temp_gpa <- list()
for (i in 1:g){
  # i = 1
  temp_gpa[[i]] <- data_array[[i]] %>% procGPA
  names(temp_gpa)[i] <- names(data_temp)[i]
}

result <- matrix(NA, ncol = 5, nrow = 18) %>% as.data.frame
colnames(result) <- c("지역", "shapiro", "t", "riemann", "proc")

# shapiro
temp_size <- c()
for (i in 1:g){
  # i = 1
  result[i, 1] <- names(data_array)[i]
  result[i, 2] <- temp_gpa[[i]] %>% .$size %>% shapiro.test %>% .$p.value %>% round(4)
  temp_size <- temp_size %>% c(., (temp_gpa[[i]] %>% .$size))
}
result[18, 1] <- "전국"
result[18, 2] <- temp_size %>% shapiro.test %>% .$p.value %>% round(4)

# t
base <- temp_gpa$부산$size
for (i in 1:g){
  # i = 1
  result[i, 3] <- temp_gpa[[i]] %>% .$size %>% t.test(., base) %>% .$p.value %>% round(4)
}
result[18, 3] <- temp_size %>% t.test(., base) %>% .$p.value %>% round(4)

# riemann
base <- temp_gpa$부산$mshape
temp_mat <- matrix(0, 4, 2)
for (i in 1:g){
  # i = 1
  result[i, 4] <- temp_gpa[[i]] %>% .$mshape %>% riemdist(., base)
  temp_mat <- temp_mat + (temp_gpa[[i]] %>% .$mshape)
}
temp_mat <- temp_mat / g
result[18, 4] <- temp_mat %>% riemdist(., base)

# proc
for (i in 1:g){
  # i = 1
  result[i, 5] <- temp_gpa[[i]] %>% .$GSS
}
result[18, 5] <- data_temp %>% unlist %>% as.numeric %>% array(dim = c(4, 2, nrow(data_raw))) %>% procGPA %>% .$GSS

result %>% as_tibble %>% arrange(지역) %>% slice(13, 1:12, 14:18) %>% write.table("clipboard", sep = "\t")

# mvn test -------------------------------------------------------------------------

library(MVN)

rotate_mat <- matrix(c(cos(90), -sin(90), sin(90), cos(90)), 2)
whole_array <- data_temp %>% unlist %>% as.numeric %>% array(dim = c(4, 2, nrow(data_raw))) #
for (i in 1:dim(whole_array)[3]){
  # i = 1
  whole_array[, , i] <- whole_array[, , i] %*% rotate_mat
}
pusan_array <- data_array$부산 #
for (i in 1:dim(pusan_array)[3]){
  # i = 1
  pusan_array[, , i] <- pusan_array[, , i] %*% rotate_mat
}
bookwhole <- whole_array %>% bookstein2d
bookpusan <- pusan_array %>% bookstein2d

whole <- bookwhole$bshpv[3:4, , ] %>% apply(3, c) %>% t 
pusan <- bookpusan$bshpv[3:4, , ] %>% apply(3, c) %>% t 

whole %>% mvn %>% .$multivariateNormality
pusan %>% mvn %>% .$multivariateNormality

# cov homo -------------------------------------------------------------------------

library(biotools)

group <- as.factor(c(rep("whole", dim(whole_array)[3]), rep("pusan", dim(pusan_array)[3])))
uw <- whole %>% rbind(pusan) %>% cbind(group)

boxM(uw[,-5], group) 

# T square -------------------------------------------------------------------------

u <- whole
w <- pusan
n1=dim(whole_array)[3]; n2=dim(pusan_array)[3]
q=2*(k2-2)
ub <- matrix(1,n1,1)%*%matrix(1/n1,1,n1)%*%u
wb <- matrix(1,n2,1)%*%matrix(1/n2,1,n2)%*%w
db=ub[1,]-wb[1,]
s1 <- t(u-ub)%*%(u-ub)/(n1-1)
s2 <- t(w-wb)%*%(w-wb)/(n2-1)
sp <- ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
Dsq <- t(db)%*%solve(sp)%*%db
Fa <- qf(1-0.05,q,n1+n2-q-1)
F0 <- (n1*n2)*(n1+n2-q-1)/((n1+n2)*(n1+n2-2)*q)*Dsq
pv <- 1-pf(F0,q,n1+n2-q-1)
c(round(F0,7), round(Fa,7), pv)

# mean shape -------------------------------------------------------------------------

data_GPA_mean_df %>% filter(region != "전국") %>% 
  ggplot(aes(x = V1, y = V2, group = region, color = region)) +
  geom_polygon(size = 2, fill = NA) +
  labs(x = "", y = "") +
  theme_bw()

# weight -------------------------------------------------------------------------

weight <- list()
temp_mean <- c()
temp_n <- numeric(g)
SST <- TSS <- 0
for (i in 1:g){
  # i = 1
  temp_weight <- data_array[[i]] %>% bookstein2d %>% .$bshpv %>% .[3:4, , ] %>% apply(3, c)
  temp_n[i] <- temp_weight %>% ncol
  temp_mean <- temp_mean %>% cbind(temp_weight)
  weight[[i]] <- temp_weight %>% apply(1, mean)
  names(weight)[i] <- region[i]
}
weight[[18]] <- temp_mean %>% apply(1, mean)
names(weight)[18] <- "전국"

temp_df <- weight %>% do.call(rbind, .)
temp_df %>% as_tibble %>% mutate(지역 = temp_df %>% rownames) %>% arrange(지역) %>% slice(13, 1:12, 14:18) %>% write.table("clipboard", sep = "\t")

SST <- TSS <- matrix(0, 4, 4)
for (i in 1:g){
  # i = 1
  SST <- SST + (t(t(temp_n[i] * (weight[[i]] - weight[[18]]))) %*% (weight[[i]] - weight[[18]]))
  temp_weight <- data_array[[i]] %>% bookstein2d %>% .$bshpv %>% .[3:4, , ] %>% apply(3, c)
  TSS <- TSS + ((temp_weight - weight[[18]]) %*% t(temp_weight - weight[[18]]))
}
SSE = TSS - SST
U0 <- det(SSE)/det(SSE + SST)

p = 2
m = g-1
alpha=0.05
n = temp_n %>% sum
f = n - g

chi0 <- -(f-((p-1)-m+1)/2)*log(U0)
chia <- qchisq(1-alpha,(p-1)*m)
pv <- 1-pchisq(chi0,(p-1)*m) 
test <- function(U0,chi0,chia,pv,alpha){
  values<-c(U0,chi0,chia,pv); names(values)<-c("U0","chi0","chia","p-value")
  if(chi0 > chia) {result<-c("chi0 > chia : reject H0:  at alpha=0.05")}
  if(chi0 < chia) {result<-c("chi0 < chia : accept H0:  at alpha=0.05")}
  list(values=values, result=result)
}
test(U0,chi0,chia,pv,alpha)
