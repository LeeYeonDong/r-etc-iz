library(tidyverse)
library(readxl)
library(stringr)

# df 불러오기
df_smp <- read_excel(path = "D:/대학원/간호대/기록지/한글파일 코딩 샘플.xlsx", sheet = 1)

col139 <- df_smp %>% colnames()
col138 <- col139[2:length(col139)]


# data 불러오기
smp1 <- read_excel(path = "D:/대학원/간호대/기록지/sample.xlsx", sheet = 1)
smp2 <- read_excel(path = "D:/대학원/간호대/기록지/sample.xlsx", sheet = 2)

col_name <- paste0("v",1:10) 
colnames(smp1) <- col_name

smp1 %>% view()

# 벡터에 저장
#### 1페이지
장기요양인정번호 <- smp1$v7[2]

생년월일 <- ((smp1$v7[1] %>% 
           str_squish() %>% 
           strsplit(split="/"))[[1]][1]) %>% 
           str_remove_all(" ")

성별 <- (smp1$v7[1] %>% 
         str_remove_all(" ") %>% 
         strsplit(split="/"))[[1]][2]

장기요양등급 <- smp1$v4[2]

욕창위험도평가점수_1분기 <- ((smp1$v4[3] %>% 
  str_squish() %>% 
  strsplit(split="점"))[[1]][1] %>% 
  strsplit(split=":"))[[1]][2] %>% 
  str_squish()

욕창위험도평가점수_2분기 <- ((smp1$v4[3] %>% 
  str_squish() %>% 
  strsplit(split="점"))[[1]][2] %>% 
  strsplit(split=":"))[[1]][2] %>%
  str_squish()

욕창위험도평가점수_3분기 <- ((smp1$v4[3] %>% 
  str_squish() %>% 
  strsplit(split="점"))[[1]][3] %>% 
  strsplit(split=":"))[[1]][2] %>%
  str_squish()

욕창위험도평가점수_4분기 <- ((smp1$v4[3] %>% 
  str_squish() %>% 
  strsplit(split="점"))[[1]][4] %>% 
  strsplit(split=":"))[[1]][2] %>%
  str_squish()

욕창발생_고위험군_유_12점이하 <- ((smp1$v7[3] %>%
  str_squish() %>% 
  strsplit(split="\\)"))[[1]][1] %>% 
  str_replace_all("■","Yes") %>% 
  str_replace_all("□","NO") %>% 
  strsplit(split=" "))[[1]][1]


욕창발생_고위험군_무 <- ((smp1$v7[3] %>%
  str_squish() %>% 
  strsplit(split="\\)"))[[1]][2] %>% 
  str_replace_all("■","Yes") %>% 
  str_replace_all("□","NO") %>% 
  strsplit(split=" "))[[1]][2]
  

기관명 <- "기관명"

날짜 <- smp1$v3[5] %>% 
  str_squish() %>% 
  str_replace_all(" 월", "월") %>% 
  str_replace_all(" 일", "일")
  
의식 <- str_sub(smp1$v3[6] %>% 
                str_squish(),
              
  ((((smp1$v3[6] %>% 
  str_squish() %>%
  str_locate_all("■"))[[1]]) %>% 
  as.vector())[1])+2,
  
  ((((smp1$v3[6] %>% 
     str_squish() %>%
     str_locate_all("■"))[[1]]) %>% 
     as.vector())[1])+3)

활력징후_정규_BP <- ((smp1$v3[7] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][1]) %>% 
  str_remove_all("BP:") %>% 
  str_remove_all("mmHg") %>%
  str_remove_all(" ")

활력징후_정규_PR <- ((smp1$v3[7] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

활력징후_정규_BT <- ((smp1$v3[7] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][3]) %>% 
  str_remove_all("BT:") %>% 
  str_remove_all("℃") %>%
  str_remove_all(" ")

활력징후_정규_RR <- ((smp1$v3[7] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][4]) %>% 
  str_remove_all("RR:") %>% 
  str_match("\\d+") %>% as.vector()

활력징후_추가_BP <- ((smp1$v3[8] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][1]) %>% 
  str_remove_all("BP:") %>% 
  str_remove_all("mmHg") %>%
  str_remove_all(" ")

활력징후_추가_PR <- ((smp1$v3[8] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

활력징후_추가_BT <- ((smp1$v3[8] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][3]) %>% 
  str_match("\\d+") %>% as.vector()

활력징후_추가_RR <- ((smp1$v3[8] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][4]) %>% 
  str_match("\\d+") %>% as.vector()

혈당_식전 <- ((smp1$v3[9] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][1]) %>% 
  str_match("\\d+") %>% as.vector()

혈당_식후2시간 <- ((smp1$v3[9] %>% 
  str_squish() %>% 
  strsplit(split="\\,"))[[1]][2]) %>% 
  str_remove_all("식후 2시간 : ") %>% 
  str_match("\\d+") %>% as.vector()

통증_일반통증 <- (((smp1$v3[10] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x"))) %>% 
  strsplit(split="x"))[[1]][2] %>% 
  str_remove_all(" ")

통증_암성통증 <- (((smp1$v3[10] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x"))) %>% 
  strsplit(split="x"))[[1]][4] %>% 
  str_remove_all(" ")

통증_통증양상 <- paste0((((smp1$v3[10] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x"))) %>% 
  strsplit(split="x"))[[1]][6],
  ":",
  (((smp1$v3[10] %>% 
     str_squish() %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x"))) %>% 
     strsplit(split="x"))[[1]][7] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ") 

통증_점수 <- paste0((((smp1$v3[10] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x"))) %>% 
  strsplit(split="x"))[[1]][8],
  ":",
  (((smp1$v3[10] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x"))) %>% 
  strsplit(split="x"))[[1]][9] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ") 

통증_중재약물 <- paste0((((smp1$v3[10] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x"))) %>% 
  strsplit(split="x"))[[1]][10],
  ":",
  (((smp1$v3[10] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x"))) %>% 
  strsplit(split = "x"))[[1]][11] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_경관영양_cc1 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][1] %>% 
  strsplit(split = "[-]"))[[1]][1]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_경관영양_cc2 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][1] %>% 
  strsplit(split = "[-]"))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_경관영양_cc3 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][1] %>% 
  strsplit(split = "[-]"))[[1]][3]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_경관영양_cc4 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][1] %>% 
  strsplit(split = "[-]"))[[1]][4]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_위루관영양_cc1 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][2] %>% 
  strsplit(split = "\\s"))[[1]][5]) %>% 
  str_match("\\d+") %>% as.vector()
  
영양관리_위루관영양_cc2 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][2] %>% 
  strsplit(split = "\\s"))[[1]][6]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_위루관영양_cc3 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][2] %>% 
  strsplit(split = "\\s"))[[1]][7]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_위루관영양_cc4 <- (((smp1$v3[11] %>% 
  str_squish() %>% 
  str_remove_all("□") %>% 
  str_remove_all("■") %>% 
  strsplit(split = "cc"))[[1]][2] %>% 
  strsplit(split = "\\s"))[[1]][8]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_중심정액영양 <- paste0(
((smp1$v6[11] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  strsplit(split = "x"))[[1]][2],
  ":",
((smp1$v6[11] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_구강치료식 <- paste0(
  ((smp1$v6[11] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "\r\n"))[[1]][2] %>% 
  strsplit(split = "x"))[[1]][2],
  ":",
  ((smp1$v6[11] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_L_tube_사이즈<- paste0(
  ((smp1$v3[12] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = ","))[[1]][1] %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  str_remove_all("x") %>% 
  str_remove_all("L-tube"),
  ":",
  ((smp1$v3[12] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = ","))[[1]][1] %>% 
  strsplit(split = "\r\n"))[[1]][2] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_L_tube_고정위치 <- ((smp1$v3[12] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = ","))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_L_tube_드레싱 <- smp1$v6[12] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  str_remove_all("x") %>% 
  str_remove_all("드레싱") %>% 
  str_squish()

영양관리_L_tube_교환<- smp1$v6[13] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  str_remove_all("x") %>% 
  str_remove_all("교환") %>% 
  str_squish()

영양관리_Gastrostomy_tube_사이즈 <- paste0( 
((smp1$v3[14] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2],
":",
(((smp1$v3[14] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ":"))[[1]][2]) %>% 
  str_remove_all(" ")

영양관리_Gastrostomy_tube_고정위치 <-
 ((smp1$v3[14] %>% 
  strsplit(split = ","))[[1]][2] %>% 
  strsplit(split = "\r\n"))[[1]][2] %>% 
  str_remove_all("\\)")

영양관리_Gastrostomy_tube_드레싱 <-
(smp1$v6[14] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

영양관리_Gastrostomy_tube_교환 <-
(smp1$v6[15] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

호흡관리_산소장치_종류 <- paste0(
(smp1$v3[16] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2],
":",
((smp1$v3[16] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ":"))[[1]][2] %>% 
  str_remove_all("\r\n") %>% 
  str_remove_all("\\)")) %>% 
  str_remove_all(" ")

호흡관리_산소용량 <- paste0(
(smp1$v3[16] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][4],
":",
((smp1$v3[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][5]) %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_인공호흡기_Mode <- paste0(
  (smp1$v3[16] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][6],
":",
(((smp1$v3[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][7] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  strsplit(split = "\\("))[[1]][2] %>% 
  str_remove_all("Mode")) %>% 
  str_remove_all(" ")

호흡관리_인공호흡기_FiO2 <- paste0(
(smp1$v3[16] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][6],
":",
((smp1$v3[16] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][7] %>% 
    strsplit(split = ","))[[1]][2] %>% 
   str_remove_all("Fi02") %>% 
   str_match("\\d+") %>% as.vector()) %>%   str_remove_all(" ")

호흡관리_인공호흡기_O2 <- paste0(
(smp1$v3[16] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][6],
":",
((smp1$v3[16] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][7] %>% 
  strsplit(split = ","))[[1]][3] %>%
  str_remove_all("O2") %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_흡인 <- paste0(
(smp1$v6[16] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2],
":",
(smp1$v6[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  str_remove_all("\r\n")) %>% 
  str_remove_all(" ")

호흡관리_흡인_oral_nasal <- paste0(
(smp1$v6[16] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][4],
":",
(smp1$v6[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][5] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_흡인_intra_tracheal <- paste0(
  (smp1$v6[16] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][6],
":",
(smp1$v6[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][7] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_Nebulizer_종류 <- paste0(
(smp1$v6[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][8],
":",
(((smp1$v6[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][9] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  strsplit(split = "\\:"))[[1]][2]) %>% 
  str_remove_all(" ")

호흡관리_Nebulizer_회 <- paste0(
(smp1$v6[16] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][8],
":",
((smp1$v6[16] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][9] %>% 
    strsplit(split = ","))[[1]][2] %>% 
   str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_기관지절개관_사이즈 <- smp1$v3[17] %>% 
  str_match("\\d+") %>% as.vector()

호흡관리_기관지절개관_드레싱 <- (smp1$v6[17]%>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

호흡관리_기관지절개관_교환 <- (smp1$v6[18]%>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_회음부간호 <- (smp1$v3[19] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_방광세척 <- (smp1$v3[20] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_단순도뇨_회 <- paste0(
(smp1$v3[21] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2],
":",
((smp1$v3[21] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][1] %>%
  str_match("\\d+") %>% as.vector()) %>% 
str_remove_all(" ")

배뇨_배설관리_단순도뇨_총배설량cc <- paste0(
(smp1$v3[21] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][2],
":",
((smp1$v3[21] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    strsplit(split = "x"))[[1]][3] %>% 
   strsplit(split = ","))[[1]][2] %>%
  str_match("\\d+") %>% as.vector())%>% 
  str_remove_all(" ")

배뇨_배설관리_방광훈련 <- (smp1$v6[21] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_배뇨_회 <- paste0(
  (smp1$v3[22] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2],
":",
((smp1$v3[22] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

배뇨_배설관리_배뇨_양상 <- paste0(
(smp1$v3[22] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2],
":",
((smp1$v3[22] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][2] %>% 
  str_remove_all("양상") %>% 
  str_remove_all("\\)")) %>% 
  str_squish() %>% 
  str_remove_all(" ")

배뇨_배설관리_배변_회 <- paste0(
(smp1$v6[22] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2],
":",
((smp1$v6[22] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

배뇨_배설관리_배변_양상 <- paste0(
(smp1$v6[22] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2],
":",
((smp1$v6[22] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][2] %>% 
  str_remove_all("양상") %>% 
  str_remove_all("\\)")) %>% 
  str_squish() %>% 
  str_remove_all(" ")

배뇨_배설관리_유치도뇨관_사이즈 <- paste0(
 (smp1$v3[23] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2],
":",
((smp1$v3[23] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ":"))[[1]][2] %>% 
  str_remove_all("\\)")) %>% 
  str_squish() %>% 
  str_remove_all(" ")

배뇨_배설관리_유치도뇨관_교환 <- (smp1$v6[23] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_요루관리 <-str_sub(
smp1$v3[24],
((smp1$v3[24] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x")) %>% 
  str_locate_all("Yes"))[[1]][1,1],
  
((smp1$v3[24] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x")) %>% 
  str_locate_all("Yes"))[[1]][1,2])


(smp1$v3[24] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))


배뇨_배설관리_장루관리 <- (smp1$v3[25] %>%     
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_장루관리_Plate <- (smp1$v3[25] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][4] %>% 
  str_remove_all(" ")

배뇨_배설관리_장루관리_Pouch <- (smp1$v3[25] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][6] %>% 
  str_remove_all(" ")

배뇨_배설관리_장루관리_교환 <- (smp1$v6[25] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

상처관리_외과적상처 <- (smp1$v3[26] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

상처관리_외과적상처_부위 <- paste0(
  (smp1$v3[26] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
":",
(((smp1$v3[26] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  strsplit(split = ":"))[[1]][3]) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_외과적상처_삼출물 <- paste0(
  (smp1$v3[26] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  (((smp1$v3[26] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ",")))[[1]][2] %>% 
  str_remove_all("삼출물:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_외과적상처_크기 <- paste0(
(smp1$v3[26] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
":",
((smp1$v3[26] %>% 
    str_replace_all("□","z No z") %>% 
    str_replace_all("■","z Yes z") %>% 
    strsplit(split = "z"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][3] %>% 
  str_remove_all("크기:") %>% 
  str_remove_all("cm")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_외과적상처_주위피부 <- paste0(
(smp1$v3[26] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
":",
(((smp1$v3[26] %>% 
    str_replace_all("□","z No z") %>% 
    str_replace_all("■","z Yes z") %>% 
    strsplit(split = "z"))[[1]][3] %>% 
   strsplit(split = ","))[[1]][4] %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  str_remove_all("주위피부:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_외과적상처_감염징후 <-paste0(
str_sub(
  (smp1$v3[26] %>% 
     str_squish() %>% 
     strsplit(split = "감염징후"))[[1]][2] %>% 
    str_remove_all("\\("),
  
  ((((smp1$v3[26] %>% 
       str_squish() %>% 
       strsplit(split = "감염징후"))[[1]][2] %>% 
      str_remove_all("\\(") %>%
       str_locate_all("■"))[[1]] %>% 
      as.vector())[1]+2),
  
((((smp1$v3[26] %>% 
     str_squish() %>% 
     strsplit(split = "감염징후"))[[1]][2] %>% 
    str_remove_all("\\(") %>%
    str_locate_all("■"))[[1]] %>% 
   as.vector())[1]+3)), 

((smp1$v3[26] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\(") %>% 
  strsplit(split = "기타"))[[1]][2]
)

상처관리_외과적상처_드레싱종류 <- smp1$v3[27] %>% 
  str_remove_all("드레싱 종류:") %>% 
  str_squish() %>% 
  str_remove_all(" ")

상처관리_단순드레싱 <- (smp1$v3[28] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

상처관리_단순드레싱_부위 <- paste0(
  (smp1$v3[28] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  (((smp1$v3[28] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][1] %>% 
  strsplit(split = ":"))[[1]][3]) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_삼출물 <- paste0(
  (smp1$v3[28] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  (((smp1$v3[28] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ",")))[[1]][2] %>% 
  str_remove_all("삼출물:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_크기 <- paste0(
  (smp1$v3[28] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  ((smp1$v3[28] %>% 
  str_replace_all("□","z No z") %>% 
  str_replace_all("■","z Yes z") %>% 
  strsplit(split = "z"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][3] %>% 
  str_remove_all("크기:") %>% 
  str_remove_all("cm")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_주위피부 <- paste0(
  (smp1$v3[28] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  (((smp1$v3[28] %>% 
  str_replace_all("□","z No z") %>% 
  str_replace_all("■","z Yes z") %>% 
  strsplit(split = "z"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][4] %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  str_remove_all("주위피부:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_감염징후 <-paste0(
  str_sub(
  (smp1$v3[28] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\("),
                           
  ((((smp1$v3[28] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\(") %>%
  str_locate_all("■"))[[1]] %>% 
  as.vector())[1]+2),
                           
  ((((smp1$v3[28] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\(") %>%
  str_locate_all("■"))[[1]] %>% 
  as.vector())[1]+3)), 
                         
  ((smp1$v3[28] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\(") %>% 
  strsplit(split = "기타"))[[1]][2]
)

상처관리_단순드레싱_드레싱종류 <- smp1$v3[29] %>% 
  str_remove_all("드레싱 종류:") %>% 
  str_squish() %>% 
  str_remove_all(" ")

상처관리_복합드레싱 <- (smp1$v3[30] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

상처관리_복합드레싱_부위 <- paste0(
  (smp1$v3[30] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
   (((smp1$v3[30] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
   strsplit(split = ","))[[1]][1] %>% 
   strsplit(split = ":"))[[1]][3]) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_복합드레싱_삼출물 <- paste0(
   (smp1$v3[30] %>% 
str_replace_all("□","x No x") %>% 
str_replace_all("■","x Yes x") %>% 
 strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  (((smp1$v3[30] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ",")))[[1]][2] %>% 
  str_remove_all("삼출물:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_복합드레싱_크기 <- paste0(
  (smp1$v3[30] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  ((smp1$v3[30] %>% 
  str_replace_all("□","z No z") %>% 
  str_replace_all("■","z Yes z") %>% 
  strsplit(split = "z"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][3] %>% 
  str_remove_all("크기:") %>% 
  str_remove_all("cm")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_복합드레싱_주위피부 <- paste0(
  (smp1$v3[30] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" "),
  ":",
  (((smp1$v3[30] %>% 
  str_replace_all("□","z No z") %>% 
  str_replace_all("■","z Yes z") %>% 
  strsplit(split = "z"))[[1]][3] %>% 
  strsplit(split = ","))[[1]][4] %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  str_remove_all("주위피부:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_복합드레싱_감염징후 <-paste0(
  str_sub(
  (smp1$v3[30] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\("),
                           
  ((((smp1$v3[30] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\(") %>%
  str_locate_all("■"))[[1]] %>% 
  as.vector())[1]+2),
                           
  ((((smp1$v3[30] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\(") %>%
  str_locate_all("■"))[[1]] %>% 
  as.vector())[1]+3)), 
                         
  ((smp1$v3[30] %>% 
  str_squish() %>% 
  strsplit(split = "감염징후"))[[1]][2] %>% 
  str_remove_all("\\(") %>% 
  strsplit(split = "기타"))[[1]][2]
)

상처관리_복합드레싱_드레싱종류 <- smp1$v3[31] %>% 
  str_remove_all("드레싱 종류:") %>% 
  str_squish() %>% 
  str_remove_all(" ")

상처관리_봉합사제거 <- (smp1$v3[32] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")


### 2페이지
col_name <- paste0("v",1:12) 
colnames(smp2) <- col_name

욕창_예방_예방도구_에어메트리스 <- (smp2$v3[2] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2]%>% 
  str_remove_all(" ") 
  
욕창_예방_예방도구_쿠션 <- (smp2$v3[2] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][4] %>% 
  str_remove_all(" ") 

욕창_예방_예방도구_방석 <- (smp2$v3[2] %>% 
  str_squish() %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][6] %>% 
  str_remove_all(" ") 

욕창_예방_체위변경시간 <- smp2$v3[3] %>% 
  str_remove_all("체위변경 시간 :") %>% 
  str_squish()

욕창_예방_욕창호발부위_피부상태_양호 <- 
  (smp2$v3[4] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

욕창_예방_욕창호발부위_피부상태_불량 <- paste0(
  (smp2$v3[4] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][4],
":",
((smp2$v3[4] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][5] %>% 
  strsplit(split = ":"))[[1]][2] %>% 
  str_remove_all("\\)")) %>% 
  str_remove_all(" ")

욕창_관리_부위 <- paste0(
  (smp2$v3[5] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2],
":",
((smp2$v3[5] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = ":"))[[1]][2]) %>% 
  str_squish() %>% 
  str_remove_all(" ")
  
욕창_관리_삼출물 <- paste0(
  (smp2$v3[5] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][4],
  ":",
  (((smp2$v3[5] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][5] %>% 
  strsplit(split = "\r\n"))[[1]][1] %>% 
  strsplit(split = ":"))[[1]][2]) %>% 
  str_squish() %>% 
  str_remove_all(" ")

욕창_관리_단계 <- str_sub(
  (smp2$v3[5] %>% 
    str_squish()),
                    
    (((smp2$v3[5] %>% 
    str_squish() %>%
    str_locate_all("●"))[[1]] %>% 
    as.vector())[1])+1,
                    
   (((smp2$v3[5] %>% 
    str_squish() %>%
    str_locate_all("●"))[[1]] %>% 
    as.vector())[1])+2) %>% 
  str_replace_all("D","DTI") %>% 
  str_replace_all("U","Unstageable") %>% 
  str_remove_all(" ") 

욕창_관리_크기 <- (smp2$v3[6] %>% 
  strsplit(split = "\\("))[[1]][1] %>% 
  str_remove_all("크기:") %>% 
  str_remove_all(" ") %>% 
  str_remove_all("cm") %>% 
  str_replace_all("x"," x ") %>% 
  str_remove_all(" ") 

욕창_관리_깊이 <- (smp2$v3[6] %>% 
  strsplit(split = "\\("))[[1]][2] %>% 
  str_match("\\d+") %>% as.vector()

욕창_관리_드레싱제제 <- paste0(
(smp2$v8[6] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2],
":",
((smp2$v8[6] %>% 
   str_replace_all("□","x No x") %>% 
   str_replace_all("■","x Yes x") %>% 
   strsplit(split = "x"))[[1]][3] %>% 
  strsplit(split = "\r\n"))[[1]][2] %>% 
  str_remove_all("\\)")) %>% 
  str_remove_all(" ") 
  
당뇨발관리_위치 <- (smp2$v3[7] %>% 
  strsplit(split = "\r\n"))[[1]][2] 
  
당뇨발관리_크기 <- smp2$v6[7] %>% 
  str_remove_all("크기:") 

당뇨발관리_드레싱제제 <- paste0(
  (smp2$v8[7] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][2],
  ":",
  ((smp2$v8[7] %>% 
      str_replace_all("□","x No x") %>% 
      str_replace_all("■","x Yes x") %>% 
      strsplit(split = "x"))[[1]][3] %>% 
     strsplit(split = "\r\n"))[[1]][2] %>% 
    str_remove_all("\\)")) %>% 
  str_remove_all(" ") 

억제대_종류 <- paste0(
str_sub(
  (smp2$v3[8] %>% 
     str_squish()),
  
  (((smp2$v3[8] %>% 
       str_squish() %>%
       str_locate_all("■"))[[1]] %>% 
      as.vector())[1])+2,
  
  (((smp2$v3[8] %>% 
       str_squish() %>%
       str_locate_all("■"))[[1]] %>% 
      as.vector())[1])+3) %>% 
  str_replace_all("손장","손장갑") %>% 
  str_remove_all(" "),

(smp2$v3[8] %>% 
  str_squish() %>% 
  strsplit(split = "기"))[[1]][2] %>% 
  str_remove_all("타"))

억제대_적용시간 <- paste0(
  str_sub(
    (smp2$v3[9] %>% 
       str_squish()),
    
    (((smp2$v3[9] %>% 
         str_squish() %>%
         str_locate_all("■"))[[1]] %>% 
        as.vector())[1])+2,
    
    (((smp2$v3[9] %>% 
         str_squish() %>%
         str_locate_all("■"))[[1]] %>% 
        as.vector())[1])+6) %>% 
    str_replace_all("손장","손장갑") %>% 
    str_remove_all(" "),
  
  (smp2$v3[9] %>% 
     str_squish() %>% 
     strsplit(split = "기"))[[1]][2] %>% 
    str_remove_all("타") %>% 
    str_remove_all(" "))

적용시간적용부위_피부상태_확인_시간작성 <- (smp2$v3[10] %>% 
  strsplit(split = ":"))[[1]][2] %>%
  str_squish() %>% 
  str_remove_all(" ")

의료서비스이용_가정간호서비스이용 <- 
  (smp2$v3[11] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

의료서비스이용_가정간호서비스이용_사유 <- paste0(
  str_sub(
    smp2$v5[11],
    
    (((smp2$v5[11] %>% 
         str_squish() %>%
         str_locate_all("■"))[[1]] %>% 
        as.vector())[1])+2,
    
    ((smp2$v5[11] %>% 
         str_squish() %>%
         str_locate_all("■"))[[1]] %>% 
        as.vector())[1]+3),
  
  (smp2$v5[11] %>% 
     strsplit(split = "기"))[[1]][2] %>% 
    str_remove_all("타") %>% 
    str_remove_all("\\(") %>% 
    str_remove_all("\\)") %>% 
    str_remove_all(" "))

의료서비스이용_외래방문 <- 
  (smp2$v3[12] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

의료서비스이용_외래방문_진료과 <-
paste0(
str_sub(
  (smp2$v5[12] %>% 
  str_remove_all("\r\n") %>% 
  strsplit(split = "\\)"))[[1]][1],
  
  (((smp2$v5[12] %>% 
     str_remove_all("\r\n") %>% 
     strsplit(split = "\\)"))[[1]][1] %>%
     str_locate_all("■"))[[1]] %>% 
     as.vector())[1]+2,
  
  ((((smp2$v5[12] %>% 
       str_remove_all("\r\n") %>% 
       strsplit(split = "\\)"))[[1]][1] %>%
      str_locate_all("■"))[[1]] %>% 
     as.vector())[1]+3)) %>% 
  str_replace_all("가정","가정의학과") %>% 
  str_replace_all("비뇨","비뇨기과") %>% 
  str_replace_all("피부","피부과") %>% 
  str_replace_all("한의","한의원"),

((smp2$v5[12] %>% 
   str_remove_all("\r\n") %>% 
   strsplit(split = "\\)"))[[1]][1] %>% 
  strsplit(split = "\\("))[[1]][2] %>% 
  str_remove_all(" "))

의료서비스이용_외래방문_사유 <-
  paste0(
    str_sub(
      (smp2$v5[12] %>% 
         str_remove_all("\r\n") %>% 
         strsplit(split = "\\)"))[[1]][2],
      
      (((smp2$v5[12] %>% 
           str_remove_all("\r\n") %>% 
           strsplit(split = "\\)"))[[1]][2] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      (((smp2$v5[12] %>% 
            str_remove_all("\r\n") %>% 
            strsplit(split = "\\)"))[[1]][2] %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+3) %>% 
        str_replace_all("약처","약처방") %>% 
        str_replace_all("정규","정규진료"),
    
    ((smp2$v5[12] %>% 
        str_remove_all("\r\n") %>% 
        strsplit(split = "\\)"))[[1]][2] %>% 
       strsplit(split = "\\("))[[1]][2] %>% 
      str_remove_all(" "))

의료서비스이용_외래방문_이동수단 <-
  paste0(
    str_sub(
      smp2$v5[13],
      
      ((smp2$v5[13] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      ((smp2$v5[13] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+4) %>% 
        str_replace_all("시설차","시설차량") %>% 
        str_replace_all("개인차","개인차량"),
    
    (smp2$v5[13] %>% 
      strsplit(split = "기"))[[1]][2] %>% 
      str_remove_all("타") %>% 
      str_remove_all("\\(") %>% 
      str_remove_all("\\)") %>% 
      str_remove_all(" "))

의료서비스이용_외래방문_동반_주보호자 <-
  paste0(
    str_sub(
      smp2$v5[13],
      
      ((smp2$v5[13] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      ((smp2$v5[13] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+4) %>% 
      str_replace_all("시설차","시설차량") %>% 
      str_replace_all("개인차","개인차량"),
    
    (smp2$v5[13] %>% 
       strsplit(split = "기"))[[1]][2] %>% 
      str_remove_all("타") %>% 
      str_remove_all("\\(") %>% 
      str_remove_all("\\)") %>% 
      str_remove_all(" "))

특이사항_시간1 <- smp2$v3[16]
특이사항_시간2 <- smp2$v3[17]
특이사항_시간3 <- smp2$v3[18]
특이사항_시간4 <- smp2$v3[19]
특이사항_시간5 <- smp2$v3[20]

특이사항_기록1 <- smp2$v4[16]
특이사항_기록2 <- smp2$v4[17]
특이사항_기록3 <- smp2$v4[18]
특이사항_기록4 <- smp2$v4[19]
특이사항_기록5 <- smp2$v4[20]

담당자_N <- smp2$v3[21] %>% 
  str_remove_all("N") %>% 
  str_remove_all(" ")

담당자_D <- smp2$v7[21] %>% 
  str_remove_all("D") %>% 
  str_remove_all(" ")

담당자_E <- smp2$v9[21] %>% 
  str_remove_all("E") %>% 
  str_remove_all(" ")


# vec -> df
df.tmp <- data.frame(
  장기요양인정번호,
  생년월일,
  성별,
  장기요양등급,
  욕창위험도평가점수_1분기,
  욕창위험도평가점수_2분기,
  욕창위험도평가점수_3분기,
  욕창위험도평가점수_4분기,
  욕창발생_고위험군_유_12점이하,
  욕창발생_고위험군_무,
  기관명,
  날짜,
  의식,
  활력징후_정규_BP,
  활력징후_정규_PR,
  활력징후_정규_BT,
  활력징후_정규_RR,
  활력징후_추가_BP,
  활력징후_추가_PR,
  활력징후_추가_BT,
  활력징후_추가_RR,
  혈당_식전,
  혈당_식후2시간,
  통증_일반통증,
  통증_암성통증,
  통증_통증양상,
  통증_점수,
  통증_중재약물,
  영양관리_경관영양_cc1,
  영양관리_경관영양_cc2,
  영양관리_경관영양_cc3,
  영양관리_경관영양_cc4,
  영양관리_위루관영양_cc1,
  영양관리_위루관영양_cc2,
  영양관리_위루관영양_cc3,
  영양관리_위루관영양_cc4,
  영양관리_중심정액영양,
  영양관리_구강치료식,
  영양관리_L_tube_사이즈,
  영양관리_L_tube_고정위치,
  영양관리_L_tube_드레싱,
  영양관리_L_tube_교환,
  영양관리_Gastrostomy_tube_사이즈,
  영양관리_Gastrostomy_tube_고정위치,
  영양관리_Gastrostomy_tube_드레싱,
  영양관리_Gastrostomy_tube_교환,
  호흡관리_산소장치_종류,
  호흡관리_산소용량,
  호흡관리_인공호흡기_Mode,
  호흡관리_인공호흡기_FiO2,
  호흡관리_인공호흡기_O2,
  호흡관리_흡인,
  호흡관리_흡인_oral_nasal,
  호흡관리_흡인_intra_tracheal,
  호흡관리_Nebulizer_종류,
  호흡관리_Nebulizer_회,
  호흡관리_기관지절개관_사이즈,
  호흡관리_기관지절개관_드레싱,
  호흡관리_기관지절개관_교환,
  배뇨_배설관리_회음부간호,
  배뇨_배설관리_방광세척,
  배뇨_배설관리_단순도뇨_회,
  배뇨_배설관리_단순도뇨_총배설량cc,
  배뇨_배설관리_방광훈련,
  배뇨_배설관리_배뇨_회,
  배뇨_배설관리_배뇨_양상,
  배뇨_배설관리_배변_회,
  배뇨_배설관리_배변_양상,
  배뇨_배설관리_유치도뇨관_사이즈,
  배뇨_배설관리_유치도뇨관_교환,
  배뇨_배설관리_요루관리,
  배뇨_배설관리_요루관리_cystostomy,
  배뇨_배설관리_요루관리_인공방광,
  배뇨_배설관리_요루관리_기타,
  배뇨_배설관리_장루관리,
  배뇨_배설관리_장루관리_Plate,
  배뇨_배설관리_장루관리_Pouch,
  배뇨_배설관리_장루관리_교환,
  상처관리_외과적상처,
  상처관리_외과적상처_부위,
  상처관리_외과적상처_삼출물,
  상처관리_외과적상처_크기,
  상처관리_외과적상처_주위피부,
  상처관리_외과적상처_감염징후,
  상처관리_외과적상처_드레싱종류,
  상처관리_단순드레싱,
  상처관리_단순드레싱_부위,
  상처관리_단순드레싱_삼출물,
  상처관리_단순드레싱_크기,
  상처관리_단순드레싱_주위피부,
  상처관리_단순드레싱_감염징후,
  상처관리_단순드레싱_드레싱종류,
  상처관리_복합드레싱,
  상처관리_복합드레싱_부위,
  상처관리_복합드레싱_삼출물,
  상처관리_복합드레싱_크기,
  상처관리_복합드레싱_주위피부,
  상처관리_복합드레싱_감염징후,
  상처관리_복합드레싱_드레싱종류
)

#
