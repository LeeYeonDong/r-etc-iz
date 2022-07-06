library(tidyverse)
library(readxl)
library(stringr)

# 간호기록지 환자수
n <- 1000

# 기관명
기관명 <- "너싱홈그린힐"

# data 불러오기
{너싱홈_03_1 <- read_excel(path = "D:/대학원/간호대/기록지/너싱홈그린힐/너싱홈_03월.xlsx", sheet = (2*1-1))
  너싱홈_03_2 <- read_excel(path = "D:/대학원/간호대/기록지/너싱홈그린힐/너싱홈_03월.xlsx", sheet = 2*1)
  
  colnames(너싱홈_03_1) <- paste0("v",1:10)
  colnames(너싱홈_03_2) <- paste0("v",1:12)
  
## 벡터에 저장
#### 1페이지
기관명 <- 기관명
  
장기요양인정번호 <- 너싱홈_03_1$v7[2]
  
성명 <- 너싱홈_03_1$v4[1]
  
생년월일 <- ((너싱홈_03_1$v7[1] %>% 
                  str_squish() %>% 
                  strsplit(split="/"))[[1]][1]) %>% 
    str_remove_all(" ") %>% 
    str_remove_all("\\.") %>% 
    str_sub(3,8)
  
  성별 <- (너싱홈_03_1$v7[1] %>% 
               str_remove_all(" ") %>% 
               strsplit(split="/"))[[1]][2]
  
  날짜 <- 너싱홈_03_1$v3[5] %>% 
    str_squish() %>% 
    str_replace_all(" 월", "월") %>% 
    str_replace_all(" 일", "일")
  
  장기요양등급 <- 너싱홈_03_1$v4[2]
  
  욕창위험도평가점수_1분기 <- ((너싱홈_03_1$v4[3] %>% 
                           str_squish() %>% 
                           strsplit(split="점"))[[1]][1] %>% 
                      strsplit(split=" "))[[1]][2] %>% 
    str_squish()
  
  욕창위험도평가점수_2분기 <- ((너싱홈_03_1$v4[3] %>% 
                           str_squish() %>% 
                           strsplit(split="점"))[[1]][2] %>% 
                      strsplit(split=":"))[[1]][2] %>%
    str_squish()
  
  욕창위험도평가점수_3분기 <- ((너싱홈_03_1$v4[3] %>% 
                           str_squish() %>% 
                           strsplit(split="점"))[[1]][3] %>% 
                      strsplit(split=":"))[[1]][2] %>%
    str_squish()
  
  욕창위험도평가점수_4분기 <- ((너싱홈_03_1$v4[3] %>% 
                           str_squish() %>% 
                           strsplit(split="점"))[[1]][4] %>% 
                      strsplit(split=":"))[[1]][2] %>%
    str_squish()
  
  욕창발생_고위험군_유_12점이하 <- ((너싱홈_03_1$v7[3] %>%
                               str_squish() %>% 
                               strsplit(split="\\)"))[[1]][1] %>% 
                          str_replace_all("■","Yes") %>% 
                          str_replace_all("□","No") %>% 
                          strsplit(split=" "))[[1]][1]
  
  욕창발생_고위험군_무 <- ((너싱홈_03_1$v7[3] %>%
                         str_squish() %>% 
                         strsplit(split="\\)"))[[1]][2] %>% 
                    str_replace_all("■","Yes") %>% 
                    str_replace_all("□","No") %>% 
                    strsplit(split=" "))[[1]][2]
  
  의식 <- str_sub(너싱홈_03_1$v3[6] %>% 
                      str_squish(),
                    
                    ((((너싱홈_03_1$v3[6] %>% 
                              str_squish() %>%
                              str_locate_all("■"))[[1]]) %>% 
                        as.vector())[1])+2,
                    
                    ((((너싱홈_03_1$v3[6] %>% 
                              str_squish() %>%
                              str_locate_all("■"))[[1]]) %>% 
                        as.vector())[1])+3)
  
  활력징후_정규_BP <- ((너싱홈_03_1$v3[7] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][1]) %>% 
    str_remove_all("BP:") %>% 
    str_remove_all("mmHg") %>%
    str_remove_all(" ")
  
  활력징후_정규_PR <- ((너싱홈_03_1$v3[7] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][2]) %>% 
    str_match("\\d+") %>% as.vector()
  
  활력징후_정규_BT <- ((너싱홈_03_1$v3[7] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][3]) %>% 
    str_remove_all("BT:") %>% 
    str_remove_all("℃") %>%
    str_remove_all(" ")
  
  활력징후_정규_RR <- ((너싱홈_03_1$v3[7] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][4]) %>% 
    str_remove_all("RR:") %>% 
    str_match("\\d+") %>% as.vector()
  
  활력징후_추가_BP <- ((너싱홈_03_1$v3[8] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][1]) %>% 
    str_remove_all("BP:") %>% 
    str_remove_all("mmHg") %>%
    str_remove_all("/") %>%
    str_remove_all(" ")
  
  활력징후_추가_PR <- ((너싱홈_03_1$v3[8] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][2]) %>% 
    str_match("\\d+") %>% as.vector()
  
  활력징후_추가_BT <- ((너싱홈_03_1$v3[8] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][3]) %>% 
    str_match("\\d+") %>% as.vector()
  
  활력징후_추가_RR <- ((너싱홈_03_1$v3[8] %>% 
                        str_squish() %>% 
                        strsplit(split="\\,"))[[1]][4]) %>% 
    str_match("\\d+") %>% as.vector()
  
  혈당_식전 <- ((너싱홈_03_1$v3[9] %>% 
                   str_squish() %>% 
                   strsplit(split="\\,"))[[1]][1]) %>% 
    str_match("\\d+") %>% as.vector()
  
  혈당_식후2시간 <- ((너싱홈_03_1$v3[9] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][2]) %>% 
    str_remove_all("식후 2시간 : ") %>% 
    str_match("\\d+") %>% as.vector()
  
  통증_일반통증 <- (((너싱홈_03_1$v3[10] %>% 
                      str_squish() %>% 
                      str_replace_all("□","x No x") %>% 
                      str_replace_all("■","x Yes x"))) %>% 
                strsplit(split="x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  통증_암성통증 <- (((너싱홈_03_1$v3[10] %>% 
                      str_squish() %>% 
                      str_replace_all("□","x No x") %>% 
                      str_replace_all("■","x Yes x"))) %>% 
                strsplit(split="x"))[[1]][4] %>% 
    str_remove_all(" ")
  
  통증_통증양상 <- paste0((((너싱홈_03_1$v3[10] %>% 
                             str_squish() %>% 
                             str_replace_all("□","x No x") %>% 
                             str_replace_all("■","x Yes x"))) %>% 
                       strsplit(split="x"))[[1]][6],
                    " ",
                    (((너싱홈_03_1$v3[10] %>% 
                             str_squish() %>% 
                             str_replace_all("□","x No x") %>% 
                             str_replace_all("■","x Yes x"))) %>% 
                       strsplit(split="x"))[[1]][7] %>% 
                      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ") 
  
  통증_점수 <- paste0((((너싱홈_03_1$v3[10] %>% 
                           str_squish() %>% 
                           str_replace_all("□","x No x") %>% 
                           str_replace_all("■","x Yes x"))) %>% 
                     strsplit(split="x"))[[1]][8],
                  " ",
                  (((너싱홈_03_1$v3[10] %>% 
                           str_squish() %>% 
                           str_replace_all("□","x No x") %>% 
                           str_replace_all("■","x Yes x"))) %>% 
                     strsplit(split="x"))[[1]][9] %>% 
                    str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ") 
  
  통증_중재약물 <- paste0((((너싱홈_03_1$v3[10] %>% 
                             str_squish() %>% 
                             str_replace_all("□","x No x") %>% 
                             str_replace_all("■","x Yes x"))) %>% 
                       strsplit(split="x"))[[1]][10],
                    " ",
                    (((너싱홈_03_1$v3[10] %>% 
                             str_squish() %>% 
                             str_replace_all("□","x No x") %>% 
                             str_replace_all("■","x Yes x"))) %>% 
                       strsplit(split = "x"))[[1]][11] %>% 
                      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  영양관리_경관영양_cc1 <- (((너싱홈_03_1$v3[11] %>% 
                            str_squish() %>% 
                            str_remove_all("□") %>% 
                            str_remove_all("■") %>% 
                            strsplit(split = "cc"))[[1]][1] %>% 
                       strsplit(split = "[-]"))[[1]][1]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_경관영양_cc2 <- (((너싱홈_03_1$v3[11] %>% 
                            str_squish() %>% 
                            str_remove_all("□") %>% 
                            str_remove_all("■") %>% 
                            strsplit(split = "cc"))[[1]][1] %>% 
                       strsplit(split = "[-]"))[[1]][2]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_경관영양_cc3 <- (((너싱홈_03_1$v3[11] %>% 
                            str_squish() %>% 
                            str_remove_all("□") %>% 
                            str_remove_all("■") %>% 
                            strsplit(split = "cc"))[[1]][1] %>% 
                       strsplit(split = "[-]"))[[1]][3]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_경관영양_cc4 <- (((너싱홈_03_1$v3[11] %>% 
                            str_squish() %>% 
                            str_remove_all("□") %>% 
                            str_remove_all("■") %>% 
                            strsplit(split = "cc"))[[1]][1] %>% 
                       strsplit(split = "[-]"))[[1]][4]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_위루관영양_cc1 <- (((너싱홈_03_1$v3[11] %>% 
                             str_squish() %>% 
                             str_remove_all("□") %>% 
                             str_remove_all("■") %>% 
                             strsplit(split = "cc"))[[1]][2] %>% 
                        strsplit(split = "\\s"))[[1]][5]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_위루관영양_cc2 <- (((너싱홈_03_1$v3[11] %>% 
                             str_squish() %>% 
                             str_remove_all("□") %>% 
                             str_remove_all("■") %>% 
                             strsplit(split = "cc"))[[1]][2] %>% 
                        strsplit(split = "\\s"))[[1]][6]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_위루관영양_cc3 <- (((너싱홈_03_1$v3[11] %>% 
                             str_squish() %>% 
                             str_remove_all("□") %>% 
                             str_remove_all("■") %>% 
                             strsplit(split = "cc"))[[1]][2] %>% 
                        strsplit(split = "\\s"))[[1]][7]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_위루관영양_cc4 <- (((너싱홈_03_1$v3[11] %>% 
                             str_squish() %>% 
                             str_remove_all("□") %>% 
                             str_remove_all("■") %>% 
                             strsplit(split = "cc"))[[1]][2] %>% 
                        strsplit(split = "\\s"))[[1]][8]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_중심정액영양 <- paste0(
    ((너싱홈_03_1$v6[11] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "\r\n"))[[1]][1] %>% 
       strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v6[11] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "\r\n"))[[1]][1] %>% 
       strsplit(split = "x"))[[1]][3] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  영양관리_구강치료식 <- paste0(
    ((너싱홈_03_1$v6[11] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "\r\n"))[[1]][2] %>% 
       strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v6[11] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "\r\n"))[[1]][1] %>% 
       strsplit(split = "x"))[[1]][3] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  영양관리_L_tube_사이즈<- paste0(
    ((너싱홈_03_1$v3[12] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = ","))[[1]][1] %>% 
       strsplit(split = "\r\n"))[[1]][1] %>% 
      str_remove_all("x") %>% 
      str_remove_all("L-tube"),
    " : ",
    ((너싱홈_03_1$v3[12] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = ","))[[1]][1] %>% 
       strsplit(split = "\r\n"))[[1]][2] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  영양관리_L_tube_고정위치 <- ((너싱홈_03_1$v3[12] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = ","))[[1]][2]) %>% 
    str_match("\\d+") %>% as.vector()
  
  영양관리_L_tube_드레싱 <- 너싱홈_03_1$v6[12] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    str_remove_all("x") %>% 
    str_remove_all("드레싱") %>% 
    str_squish()
  
  영양관리_L_tube_교환<- 너싱홈_03_1$v6[13] %>% 
    str_replace_all("□","x No x") %>% 
    str_replace_all("■","x Yes x") %>% 
    str_remove_all("x") %>% 
    str_remove_all("교환") %>% 
    str_squish()
  
  영양관리_Gastrostomy_tube_사이즈 <- paste0( 
    ((너싱홈_03_1$v3[14] %>% 
            strsplit(split = ","))[[1]][1] %>% 
       str_replace_all("□","x No x") %>% 
       str_replace_all("■","x Yes x") %>% 
       strsplit(split = "x"))[[1]][2],
    " : ",
    (((너싱홈_03_1$v3[14] %>% 
             strsplit(split = ","))[[1]][1] %>% 
        str_replace_all("□","x No x") %>% 
        str_replace_all("■","x Yes x") %>% 
        strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = " "))[[1]][2]) %>% 
    str_remove_all(" ")
  
  영양관리_Gastrostomy_tube_고정위치 <-
    ((너싱홈_03_1$v3[14] %>% 
            strsplit(split = ","))[[1]][2] %>% 
       strsplit(split = "\r\n"))[[1]][2] %>% 
    str_remove_all("\\)")
  
  영양관리_Gastrostomy_tube_드레싱 <-
    (너싱홈_03_1$v6[14] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  영양관리_Gastrostomy_tube_교환 <-
    (너싱홈_03_1$v6[15] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  호흡관리_산소장치_종류 <- paste0(
    (너싱홈_03_1$v3[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v3[16] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = " "))[[1]][2] %>% 
      str_remove_all("\r\n") %>% 
      str_remove_all("\\)")) %>% 
    str_remove_all(" ")
  
  호흡관리_산소용량 <- paste0(
    (너싱홈_03_1$v3[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][4],
    " : ",
    ((너싱홈_03_1$v3[16] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][5]) %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  호흡관리_인공호흡기_Mode <- paste0(
    (너싱홈_03_1$v3[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][6],
    " : ",
    (((너싱홈_03_1$v3[16] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][7] %>% 
        strsplit(split = ","))[[1]][1] %>% 
       strsplit(split = "\\("))[[1]][2] %>% 
      str_remove_all("Mode")) %>% 
    str_remove_all(" ")
  
  호흡관리_인공호흡기_FiO2 <- paste0(
    (너싱홈_03_1$v3[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][6],
    " : ",
    ((너싱홈_03_1$v3[16] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][7] %>% 
       strsplit(split = ","))[[1]][2] %>% 
      str_remove_all("Fi02") %>% 
      str_match("\\d+") %>% as.vector()) %>%   
    str_remove_all(" ")
  
  호흡관리_인공호흡기_O2 <- paste0(
    (너싱홈_03_1$v3[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][6],
    " : ",
    ((너싱홈_03_1$v3[16] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][7] %>% 
       strsplit(split = ","))[[1]][3] %>%
      str_remove_all("O2") %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  호흡관리_흡인 <- paste0(
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][3] %>% 
      str_remove_all("\r\n")) %>% 
    str_remove_all(" ")
  
  호흡관리_흡인_oral_nasal <- paste0(
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][4],
    " : ",
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][5] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  호흡관리_흡인_intra_tracheal <- paste0(
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][6],
    " : ",
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][7] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  호흡관리_Nebulizer_종류 <- paste0(
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][8],
    " : ",
    (((너싱홈_03_1$v6[16] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][9] %>% 
        strsplit(split = ","))[[1]][1] %>% 
       strsplit(split = "\\:"))[[1]][2]) %>% 
    str_remove_all(" ")
  
  호흡관리_Nebulizer_회 <- paste0(
    (너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][8],
    " : ",
    ((너싱홈_03_1$v6[16] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][9] %>% 
       strsplit(split = ","))[[1]][2] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  호흡관리_기관지절개관_사이즈 <- 너싱홈_03_1$v3[17] %>% 
    str_match("\\d+") %>% as.vector()
  
  호흡관리_기관지절개관_드레싱 <- (너싱홈_03_1$v6[17]%>% 
                            str_replace_all("□","x No x") %>% 
                            str_replace_all("■","x Yes x") %>% 
                            strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  호흡관리_기관지절개관_교환 <- (너싱홈_03_1$v6[18]%>% 
                           str_replace_all("□","x No x") %>% 
                           str_replace_all("■","x Yes x") %>% 
                           strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_회음부간호 <- (너싱홈_03_1$v3[19] %>% 
                          str_replace_all("□","x No x") %>% 
                          str_replace_all("■","x Yes x") %>% 
                          strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_방광세척 <- (너싱홈_03_1$v3[20] %>% 
                         str_replace_all("□","x No x") %>% 
                         str_replace_all("■","x Yes x") %>% 
                         strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_단순도뇨_회 <- paste0(
    (너싱홈_03_1$v3[21] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v3[21] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][1] %>%
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_단순도뇨_총배설량cc <- paste0(
    (너싱홈_03_1$v3[21] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v3[21] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][2] %>%
      str_match("\\d+") %>% as.vector())%>% 
    str_remove_all(" ")
  
  배뇨_배설관리_방광훈련 <- (너싱홈_03_1$v6[21] %>% 
                         str_replace_all("□","x No x") %>% 
                         str_replace_all("■","x Yes x") %>% 
                         strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_배뇨_회 <- paste0(
    (너싱홈_03_1$v3[22] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v3[22] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][1] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_배뇨_양상 <- paste0(
    (너싱홈_03_1$v3[22] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v3[22] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][2] %>% 
      str_remove_all("양상") %>% 
      str_remove_all("\\)")) %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_배변_회 <- paste0(
    (너싱홈_03_1$v6[22] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v6[22] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][1] %>% 
      str_match("\\d+") %>% as.vector()) %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_배변_양상 <- paste0(
    (너싱홈_03_1$v6[22] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v6[22] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][2] %>% 
      str_remove_all("양상") %>% 
      str_remove_all("\\)")) %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_유치도뇨관_사이즈 <- paste0(
    (너싱홈_03_1$v3[23] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_1$v3[23] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = " "))[[1]][2] %>% 
      str_remove_all("\\)")) %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_유치도뇨관_교환 <- (너싱홈_03_1$v6[23] %>% 
                             str_replace_all("□","x No x") %>% 
                             str_replace_all("■","x Yes x") %>% 
                             strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  배뇨_배설관리_요루관리 <- paste0(
    ((너싱홈_03_1$v3[24] %>% 
            strsplit(split = "\\("))[[1]][1] %>% 
       str_replace_all("□","x No x") %>% 
       str_replace_all("■","x Yes x") %>% 
       strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    str_sub(
      (너싱홈_03_1$v3[24] %>% 
             strsplit(split = "\\("))[[1]][2],
      
      (((너싱홈_03_1$v3[24] %>% 
               strsplit(split = "\\("))[[1]][2] %>% 
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      (((너싱홈_03_1$v3[24] %>% 
               strsplit(split = "\\("))[[1]][2] %>% 
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+5) %>% na.omit()
  )
  
  
  배뇨_배설관리_장루관리 <- paste0(((너싱홈_03_1$v3[25] %>%  
                                 strsplit(split = "\\("))[[1]][1] %>% 
                            str_replace_all("□","x No x") %>% 
                            str_replace_all("■","x Yes x") %>% 
                            strsplit(split = "x"))[[1]][2] %>% 
                           str_remove_all(" "),
                         " : ",
                         str_sub(
                           (너싱홈_03_1$v3[25] %>% 
                                  strsplit(split = "\\("))[[1]][2],
                           
                           (((너싱홈_03_1$v3[25] %>% 
                                    strsplit(split = "\\("))[[1]][2] %>% 
                               str_locate_all("■"))[[1]] %>% 
                              as.vector())[1]+2,
                           
                           (((너싱홈_03_1$v3[25] %>% 
                                    strsplit(split = "\\("))[[1]][2] %>% 
                               str_locate_all("■"))[[1]] %>% 
                              as.vector())[1]+5) %>% na.omit()
  )
  
  배뇨_배설관리_장루관리_교환 <- (너싱홈_03_1$v6[25] %>% 
                            str_replace_all("□","x No x") %>% 
                            str_replace_all("■","x Yes x") %>% 
                            strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  상처관리_외과적상처_부위 <- paste0(
    (너싱홈_03_1$v3[26] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[26] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][3] %>% 
        strsplit(split = ","))[[1]][1] %>% 
       strsplit(split = " "))[[1]][3]) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_외과적상처_삼출물 <- paste0(
    (너싱홈_03_1$v3[26] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[26] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][3] %>% 
        strsplit(split = ",")))[[1]][2] %>% 
      str_remove_all("삼출물:")) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_외과적상처_크기 <- paste0(
    (너싱홈_03_1$v3[26] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    ((너싱홈_03_1$v3[26] %>% 
            str_replace_all("□","z No z") %>% 
            str_replace_all("■","z Yes z") %>% 
            strsplit(split = "z"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][3] %>% 
      str_remove_all("크기:") %>% 
      str_remove_all("cm")) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_외과적상처_주위피부 <- paste0(
    (너싱홈_03_1$v3[26] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[26] %>% 
             str_replace_all("□","z No z") %>% 
             str_replace_all("■","z Yes z") %>% 
             strsplit(split = "z"))[[1]][3] %>% 
        strsplit(split = ","))[[1]][4] %>% 
       strsplit(split = "\r\n"))[[1]][1] %>% 
      str_remove_all("감염징후") %>% 
      str_remove_all("\\:")) %>% 
    str_squish() %>% 
    str_remove_all(" ") %>% 
    str_remove_all("\\(")
  
  상처관리_외과적상처_감염징후 <- paste0(
    str_sub(
      (너싱홈_03_1$v3[26] %>% 
             str_squish() %>% 
             strsplit(split = "감염징후"))[[1]][2] %>% 
        str_remove_all("\\("),
      
      ((((너싱홈_03_1$v3[26] %>% 
                str_squish() %>% 
                strsplit(split = "감염징후"))[[1]][2] %>% 
           str_remove_all("\\(") %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+2),
      
      ((((너싱홈_03_1$v3[26] %>% 
                str_squish() %>% 
                strsplit(split = "감염징후"))[[1]][2] %>% 
           str_remove_all("\\(") %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+3)) %>% na.omit(), 
    
    ((너싱홈_03_1$v3[26] %>% 
            str_squish() %>% 
            strsplit(split = "감염징후"))[[1]][2] %>% 
       str_remove_all("\\(") %>% 
       strsplit(split = "기타"))[[1]][2]
  )
  
  상처관리_외과적상처_드레싱종류 <- 너싱홈_03_1$v3[27] %>% 
    str_remove_all("드레싱 종류:") %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  상처관리_단순드레싱_부위 <- paste0(
    (너싱홈_03_1$v3[28] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[28] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][3] %>% 
        strsplit(split = ","))[[1]][1] %>% 
       strsplit(split = " "))[[1]][3]) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_단순드레싱_삼출물 <- paste0(
    (너싱홈_03_1$v3[28] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[28] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][3] %>% 
        strsplit(split = ",")))[[1]][2] %>% 
      str_remove_all("삼출물:")) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_단순드레싱_크기 <- paste0(
    (너싱홈_03_1$v3[28] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    ((너싱홈_03_1$v3[28] %>% 
            str_replace_all("□","z No z") %>% 
            str_replace_all("■","z Yes z") %>% 
            strsplit(split = "z"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][3] %>% 
      str_remove_all("크기:") %>% 
      str_remove_all("cm")) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_단순드레싱_주위피부 <- paste0(
    (너싱홈_03_1$v3[28] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[28] %>% 
             str_replace_all("□","z No z") %>% 
             str_replace_all("■","z Yes z") %>% 
             strsplit(split = "z"))[[1]][3] %>% 
        strsplit(split = ","))[[1]][4] %>% 
       strsplit(split = "\r\n"))[[1]][1] %>% 
      str_remove_all("주위피부:")) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_단순드레싱_감염징후 <- paste0(
    str_sub(
      (너싱홈_03_1$v3[28] %>% 
             str_squish() %>% 
             strsplit(split = "감염징후"))[[1]][2] %>% 
        str_remove_all("\\("),
      
      ((((너싱홈_03_1$v3[28] %>% 
                str_squish() %>% 
                strsplit(split = "감염징후"))[[1]][2] %>% 
           str_remove_all("\\(") %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+2) %>% na.omit(),
      
      ((((너싱홈_03_1$v3[28] %>% 
                str_squish() %>% 
                strsplit(split = "감염징후"))[[1]][2] %>% 
           str_remove_all("\\(") %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+3) %>% na.omit()) , 
    
    ((너싱홈_03_1$v3[28] %>% 
            str_squish() %>% 
            strsplit(split = "감염징후"))[[1]][2] %>% 
       str_remove_all("\\(") %>% 
       strsplit(split = "기타"))[[1]][2] %>% na.omit()
  )
  
  상처관리_단순드레싱_드레싱종류 <- 너싱홈_03_1$v3[29] %>% 
    str_remove_all("드레싱 종류:") %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  상처관리_복합드레싱_부위 <- paste0(
    (너싱홈_03_1$v3[30] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[30] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][3] %>% 
        strsplit(split = ","))[[1]][1] %>% 
       strsplit(split = ":"))[[1]][3] %>% 
      str_remove_all(" ") %>% 
      str_squish()) 
  
  상처관리_복합드레싱_삼출물 <- paste0(
    (너싱홈_03_1$v3[30] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[30] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][3] %>% 
        strsplit(split = ",")))[[1]][2] %>% 
      str_remove_all("삼출물:") %>% 
    str_squish() %>% 
    str_remove_all(" ") )
  
  상처관리_복합드레싱_크기 <- paste0(
    (너싱홈_03_1$v3[30] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    ((너싱홈_03_1$v3[30] %>% 
            str_replace_all("□","z No z") %>% 
            str_replace_all("■","z Yes z") %>% 
            strsplit(split = "z"))[[1]][3] %>% 
       strsplit(split = ","))[[1]][3] %>% 
      str_remove_all("크기:") %>% 
      str_remove_all("cm")) %>% 
    str_squish() %>% 
    str_remove_all(" ") 
  
  상처관리_복합드레싱_주위피부 <- paste0(
    (너싱홈_03_1$v3[30] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2] %>% 
      str_remove_all(" "),
    " : ",
    (((너싱홈_03_1$v3[30] %>% 
             str_replace_all("□","z No z") %>% 
             str_replace_all("■","z Yes z") %>% 
             strsplit(split = "z"))[[1]][3] %>% 
        strsplit(split = ","))[[1]][4] %>% 
       strsplit(split = "\r\n"))[[1]][1] %>% 
      str_remove_all("주위피부:") %>% 
      str_remove_all("감염징후")) %>%
    str_squish() %>% 
    str_remove_all(" ") %>% 
    str_remove_all("\\(")
  
  상처관리_복합드레싱_감염징후 <-paste0(
    str_sub(
      (너싱홈_03_1$v3[30] %>% 
             str_squish() %>% 
             strsplit(split = "감염징후"))[[1]][2] %>% 
        str_remove_all("\\("),
      
      ((((너싱홈_03_1$v3[30] %>% 
                str_squish() %>% 
                strsplit(split = "감염징후"))[[1]][2] %>% 
           str_remove_all("\\(") %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+2),
      
      ((((너싱홈_03_1$v3[30] %>% 
                str_squish() %>% 
                strsplit(split = "감염징후"))[[1]][2] %>% 
           str_remove_all("\\(") %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+3)), 
    
    ((너싱홈_03_1$v3[30] %>% 
            str_squish() %>% 
            strsplit(split = "감염징후"))[[1]][2] %>% 
       str_remove_all("\\(") %>% 
       strsplit(split = "기타"))[[1]][2]
  )
  
  상처관리_복합드레싱_드레싱종류 <- 너싱홈_03_1$v3[31] %>% 
    str_remove_all("드레싱 종류:") %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  상처관리_봉합사제거 <- (너싱홈_03_1$v3[32] %>% 
                       str_replace_all("□","x No x") %>% 
                       str_replace_all("■","x Yes x") %>% 
                       strsplit(split = "x"))[[1]][2] %>% 
    str_remove_all(" ")
  
  
  ### 2페이지
  욕창_예방_예방도구 <- (너싱홈_03_2$v3[2] %>% 
                       str_squish() %>% 
                       str_replace_all("□","x No x") %>% 
                       str_replace_all("■","x Yes x") %>% 
                       strsplit(split = "x"))[[1]][3] %>% 
    str_remove_all(" ") 
  
  욕창_예방_체위변경시간 <- 너싱홈_03_2$v3[3] %>% 
    str_remove_all("체위변경 시간 :") %>% 
    str_squish()
  
  
  욕창_예방_욕창호발부위_피부상태 <- paste0(
    str_sub(
      (너싱홈_03_2$v3[4] %>% 
             str_squish() %>% 
             strsplit(split = "피부상태"))[[1]][2],
      
      (((너싱홈_03_2$v3[4] %>% 
               str_squish() %>% 
               strsplit(split = "피부상태"))[[1]][2] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      (((너싱홈_03_2$v3[4] %>% 
               str_squish() %>% 
               strsplit(split = "피부상태"))[[1]][2] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+3), 
    
    ((너싱홈_03_2$v3[4] %>% 
            str_squish() %>% 
            strsplit(split = "피부상태"))[[1]][2] %>% 
       strsplit(split = "부위:"))[[1]][2] %>% 
      str_remove_all("\\)") %>% 
      str_remove_all(" ")
  )
  
  욕창_관리_부위 <- paste0(
    (너싱홈_03_2$v3[5] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_2$v3[5] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = " "))[[1]][2]) %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  욕창_관리_삼출물 <- paste0(
    (너싱홈_03_2$v3[5] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][4],
    " : ",
    (((너싱홈_03_2$v3[5] %>% 
             str_replace_all("□","x No x") %>% 
             str_replace_all("■","x Yes x") %>% 
             strsplit(split = "x"))[[1]][5] %>% 
        strsplit(split = "\r\n"))[[1]][1] %>% 
       strsplit(split = " "))[[1]][2]) %>% 
    str_squish() %>% 
    str_remove_all(" ")
  
  욕창_관리_단계 <- str_sub(
    (너싱홈_03_2$v3[5] %>% 
           str_squish()),
    
    (((너싱홈_03_2$v3[5] %>% 
             str_squish() %>%
             str_locate_all("●"))[[1]] %>% 
        as.vector())[1])+1,
    
    (((너싱홈_03_2$v3[5] %>% 
             str_squish() %>%
             str_locate_all("●"))[[1]] %>% 
        as.vector())[1])+2) %>% 
    str_replace_all("D","DTI") %>% 
    str_replace_all("U","Unstageable") %>% 
    str_remove_all(" ") 
  
  욕창_관리_크기 <- (너싱홈_03_2$v3[6] %>% 
                     strsplit(split = "\\("))[[1]][1] %>% 
    str_remove_all("크기:") %>% 
    str_remove_all(" ") %>% 
    str_remove_all("cm") %>% 
    str_replace_all("x"," x ") %>% 
    str_remove_all(" ") 
  
  욕창_관리_깊이 <- (너싱홈_03_2$v3[6] %>% 
                     strsplit(split = "\\("))[[1]][2] %>% 
    str_match("\\d+") %>% as.vector()
  
  욕창_관리_드레싱제제 <- paste0(
    (너싱홈_03_2$v8[6] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_2$v8[6] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = "\r\n"))[[1]][2] %>% 
      str_remove_all("\\)")) %>% 
    str_remove_all(" ") 
  
  당뇨발관리_위치 <- (너싱홈_03_2$v3[7] %>% 
                     strsplit(split = "\r\n"))[[1]][2] 
  
  당뇨발관리_크기 <- 너싱홈_03_2$v6[7] %>% 
    str_remove_all("크기:") 
  
  당뇨발관리_드레싱제제 <- paste0(
    (너싱홈_03_2$v8[7] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][2],
    " : ",
    ((너싱홈_03_2$v8[7] %>% 
            str_replace_all("□","x No x") %>% 
            str_replace_all("■","x Yes x") %>% 
            strsplit(split = "x"))[[1]][3] %>% 
       strsplit(split = "\r\n"))[[1]][2] %>% 
      str_remove_all("\\)")) %>% 
    str_remove_all(" ") 
  
  억제대_종류 <- paste0(
    str_sub(
      (너싱홈_03_2$v3[8] %>% 
             str_squish()),
      
      (((너싱홈_03_2$v3[8] %>% 
               str_squish() %>%
               str_locate_all("■"))[[1]] %>% 
          as.vector())[1])+2,
      
      (((너싱홈_03_2$v3[8] %>% 
               str_squish() %>%
               str_locate_all("■"))[[1]] %>% 
          as.vector())[1])+3) %>% 
      str_replace_all("손장","손장갑") %>% 
      str_remove_all(" "),
    
    (너싱홈_03_2$v3[8] %>% 
           str_squish() %>% 
           strsplit(split = "기"))[[1]][2] %>% 
      str_remove_all("타")) %>% 
    str_remove_all(" ")
  
  억제대_적용시간 <- paste0(
    str_sub(
      (너싱홈_03_2$v3[9] %>% 
             str_squish()),
      
      (((너싱홈_03_2$v3[9] %>% 
               str_squish() %>%
               str_locate_all("■"))[[1]] %>% 
          as.vector())[1])+2,
      
      (((너싱홈_03_2$v3[9] %>% 
               str_squish() %>%
               str_locate_all("■"))[[1]] %>% 
          as.vector())[1])+6) %>% 
      str_replace_all("손장","손장갑") %>% 
      str_remove_all(" "),
    
    (너싱홈_03_2$v3[9] %>% 
           str_squish() %>% 
           strsplit(split = "기"))[[1]][2] %>% 
      str_remove_all("타") %>% 
      str_remove_all(" "))
  
  억제대_적용시간적용부위_피부상태_확인_시간작성 <- (너싱홈_03_2$v3[10] %>% 
                                      strsplit(split = " "))[[1]][2] %>%
    str_squish() %>% 
    str_remove_all(" ")
  
  
  의료서비스이용_가정간호서비스이용_사유 <- paste0(
    str_sub(
      너싱홈_03_2$v5[11],
      
      (((너싱홈_03_2$v5[11] %>% 
               str_squish() %>%
               str_locate_all("■"))[[1]] %>% 
          as.vector())[1])+2,
      
      ((너싱홈_03_2$v5[11] %>% 
              str_squish() %>%
              str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+3),
    
    (너싱홈_03_2$v5[11] %>% 
           strsplit(split = "기"))[[1]][2] %>% 
      str_remove_all("타") %>% 
      str_remove_all("\\(") %>% 
      str_remove_all("\\)") %>% 
      str_remove_all(" "))
  
  의료서비스이용_외래방문_진료과 <-
    paste0(
      str_sub(
        (너싱홈_03_2$v5[12] %>% 
               str_remove_all("\r\n") %>% 
               strsplit(split = "\\)"))[[1]][1],
        
        (((너싱홈_03_2$v5[12] %>% 
                 str_remove_all("\r\n") %>% 
                 strsplit(split = "\\)"))[[1]][1] %>%
            str_locate_all("■"))[[1]] %>% 
           as.vector())[1]+2,
        
        ((((너싱홈_03_2$v5[12] %>% 
                  str_remove_all("\r\n") %>% 
                  strsplit(split = "\\)"))[[1]][1] %>%
             str_locate_all("■"))[[1]] %>% 
            as.vector())[1]+3)) %>% 
        str_replace_all("가정","가정의학과") %>% 
        str_replace_all("비뇨","비뇨기과") %>% 
        str_replace_all("피부","피부과") %>% 
        str_replace_all("한의","한의원"),
      
      ((너싱홈_03_2$v5[12] %>% 
              str_remove_all("\r\n") %>% 
              strsplit(split = "\\)"))[[1]][1] %>% 
         strsplit(split = "\\("))[[1]][2] %>% 
        str_remove_all(" "))
  
  의료서비스이용_외래방문_사유 <-
    paste0(
      str_sub(
        (너싱홈_03_2$v5[12] %>% 
               str_remove_all("\r\n") %>% 
               strsplit(split = "\\)"))[[1]][2],
        
        (((너싱홈_03_2$v5[12] %>% 
                 str_remove_all("\r\n") %>% 
                 strsplit(split = "\\)"))[[1]][2] %>%
            str_locate_all("■"))[[1]] %>% 
           as.vector())[1]+2,
        
        (((너싱홈_03_2$v5[12] %>% 
                 str_remove_all("\r\n") %>% 
                 strsplit(split = "\\)"))[[1]][2] %>%
            str_locate_all("■"))[[1]] %>% 
           as.vector())[1]+3) %>% 
        str_replace_all("약처","약처방") %>% 
        str_replace_all("정규","정규진료"),
      
      ((너싱홈_03_2$v5[12] %>% 
              str_remove_all("\r\n") %>% 
              strsplit(split = "\\)"))[[1]][2] %>% 
         strsplit(split = "\\("))[[1]][2] %>% 
        str_remove_all(" "))
  
  의료서비스이용_외래방문_이동수단 <-
    paste0(
      str_sub(
        너싱홈_03_2$v5[13],
        
        ((너싱홈_03_2$v5[13] %>%
                str_locate_all("■"))[[1]] %>% 
           as.vector())[1]+2,
        
        ((너싱홈_03_2$v5[13] %>%
                str_locate_all("■"))[[1]] %>% 
           as.vector())[1]+4) %>% 
        str_replace_all("시설차","시설차량") %>% 
        str_replace_all("개인차","개인차량"),
      
      (너싱홈_03_2$v5[13] %>% 
             strsplit(split = "기"))[[1]][2] %>% 
        str_remove_all("타") %>% 
        str_remove_all("\\(") %>% 
        str_remove_all("\\)") %>% 
        str_remove_all(" "))
  
  의료서비스이용_외래방문_동반_주보호자 <-
    paste0(
      str_sub(
        너싱홈_03_2$v5[13],
        
        ((너싱홈_03_2$v5[13] %>%
                str_locate_all("■"))[[1]] %>% 
           as.vector())[1]+2,
        
        ((너싱홈_03_2$v5[13] %>%
                str_locate_all("■"))[[1]] %>% 
           as.vector())[1]+4) %>% 
        str_replace_all("시설차","시설차량") %>% 
        str_replace_all("개인차","개인차량"),
      
      (너싱홈_03_2$v5[13] %>% 
             strsplit(split = "기"))[[1]][2] %>% 
        str_remove_all("타") %>% 
        str_remove_all("\\(") %>% 
        str_remove_all("\\)") %>% 
        str_remove_all(" "))
}


너싱홈_03 <- {tibble(
  기관명,
  장기요양인정번호,
  성명,
  생년월일,
  성별,
  날짜,
  장기요양등급,
  욕창위험도평가점수_1분기,
  욕창위험도평가점수_2분기,
  욕창위험도평가점수_3분기,
  욕창위험도평가점수_4분기,
  욕창발생_고위험군_유_12점이하,
  욕창발생_고위험군_무,
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
  배뇨_배설관리_장루관리_교환,
  상처관리_외과적상처_부위,
  상처관리_외과적상처_삼출물,
  상처관리_외과적상처_크기,
  상처관리_외과적상처_주위피부,
  상처관리_외과적상처_감염징후,
  상처관리_외과적상처_드레싱종류,
  상처관리_단순드레싱_부위,
  상처관리_단순드레싱_삼출물,
  상처관리_단순드레싱_크기,
  상처관리_단순드레싱_주위피부,
  상처관리_단순드레싱_감염징후,
  상처관리_단순드레싱_드레싱종류,
  상처관리_복합드레싱_부위,
  상처관리_복합드레싱_삼출물,
  상처관리_복합드레싱_크기,
  상처관리_복합드레싱_주위피부,
  상처관리_복합드레싱_감염징후,
  상처관리_복합드레싱_드레싱종류,
  상처관리_봉합사제거,
  욕창_예방_예방도구,
  욕창_예방_체위변경시간,
  욕창_예방_욕창호발부위_피부상태,
  욕창_관리_부위,
  욕창_관리_삼출물,
  욕창_관리_단계,
  욕창_관리_크기,
  욕창_관리_깊이,
  욕창_관리_드레싱제제,
  당뇨발관리_위치,
  당뇨발관리_크기,
  당뇨발관리_드레싱제제,
  억제대_종류,
  억제대_적용시간,
  억제대_적용시간적용부위_피부상태_확인_시간작성,
  의료서비스이용_가정간호서비스이용_사유,
  의료서비스이용_외래방문_진료과,
  의료서비스이용_외래방문_사유,
  의료서비스이용_외래방문_이동수단,
  의료서비스이용_외래방문_동반_주보호자
)}

for (i in 2:n){
  
  cat(i, '번째 간호기록지 불러오는 중.\n') 

너싱홈_03_1 <- read_excel(path = "D:/대학원/간호대/기록지/너싱홈그린힐/너싱홈_03월.xlsx", sheet = (2*i-1))
너싱홈_03_2 <- read_excel(path = "D:/대학원/간호대/기록지/너싱홈그린힐/너싱홈_03월.xlsx", sheet = 2*i)

colnames(너싱홈_03_1) <- paste0("v",1:10)
colnames(너싱홈_03_2) <- paste0("v",1:12)

## 벡터에 저장
#### 1페이지
기관명 <- 기관명

장기요양인정번호 <- 너싱홈_03_1$v7[2]

성명 <- 너싱홈_03_1$v4[1]

생년월일 <- ((너싱홈_03_1$v7[1] %>% 
                str_squish() %>% 
                strsplit(split="/"))[[1]][1]) %>% 
  str_remove_all(" ") %>% 
  str_remove_all("\\.") %>% 
  str_sub(3,8)

성별 <- (너싱홈_03_1$v7[1] %>% 
             str_remove_all(" ") %>% 
             strsplit(split="/"))[[1]][2]

날짜 <- 너싱홈_03_1$v3[5] %>% 
  str_squish() %>% 
  str_replace_all(" 월", "월") %>% 
  str_replace_all(" 일", "일")

장기요양등급 <- 너싱홈_03_1$v4[2]

욕창위험도평가점수_1분기 <- ((너싱홈_03_1$v4[3] %>% 
                     str_squish() %>% 
                     strsplit(split="점"))[[1]][1] %>% 
                    strsplit(split=" "))[[1]][2] %>% 
  str_squish()

욕창위험도평가점수_2분기 <- ((너싱홈_03_1$v4[3] %>% 
                     str_squish() %>% 
                     strsplit(split="점"))[[1]][2] %>% 
                    strsplit(split=":"))[[1]][2] %>%
  str_squish()

욕창위험도평가점수_3분기 <- ((너싱홈_03_1$v4[3] %>% 
                     str_squish() %>% 
                     strsplit(split="점"))[[1]][3] %>% 
                    strsplit(split=":"))[[1]][2] %>%
  str_squish()

욕창위험도평가점수_4분기 <- ((너싱홈_03_1$v4[3] %>% 
                     str_squish() %>% 
                     strsplit(split="점"))[[1]][4] %>% 
                    strsplit(split=":"))[[1]][2] %>%
  str_squish()

욕창발생_고위험군_유_12점이하 <- ((너싱홈_03_1$v7[3] %>%
                         str_squish() %>% 
                         strsplit(split="\\)"))[[1]][1] %>% 
                        str_replace_all("■","Yes") %>% 
                        str_replace_all("□","No") %>% 
                        strsplit(split=" "))[[1]][1]

욕창발생_고위험군_무 <- ((너싱홈_03_1$v7[3] %>%
                   str_squish() %>% 
                   strsplit(split="\\)"))[[1]][2] %>% 
                  str_replace_all("■","Yes") %>% 
                  str_replace_all("□","No") %>% 
                  strsplit(split=" "))[[1]][2]

의식 <- str_sub(너싱홈_03_1$v3[6] %>% 
                    str_squish(),
                  
                  ((((너싱홈_03_1$v3[6] %>% 
                            str_squish() %>%
                            str_locate_all("■"))[[1]]) %>% 
                      as.vector())[1])+2,
                  
                  ((((너싱홈_03_1$v3[6] %>% 
                            str_squish() %>%
                            str_locate_all("■"))[[1]]) %>% 
                      as.vector())[1])+3)

활력징후_정규_BP <- ((너싱홈_03_1$v3[7] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][1]) %>% 
  str_remove_all("BP:") %>% 
  str_remove_all("mmHg") %>%
  str_remove_all(" ")

활력징후_정규_PR <- ((너싱홈_03_1$v3[7] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

활력징후_정규_BT <- ((너싱홈_03_1$v3[7] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][3]) %>% 
  str_remove_all("BT:") %>% 
  str_remove_all("℃") %>%
  str_remove_all(" ")

활력징후_정규_RR <- ((너싱홈_03_1$v3[7] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][4]) %>% 
  str_remove_all("RR:") %>% 
  str_match("\\d+") %>% as.vector()

활력징후_추가_BP <- ((너싱홈_03_1$v3[8] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][1]) %>% 
  str_remove_all("BP:") %>% 
  str_remove_all("mmHg") %>%
  str_remove_all("/") %>%
  str_remove_all(" ")

활력징후_추가_PR <- ((너싱홈_03_1$v3[8] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

활력징후_추가_BT <- ((너싱홈_03_1$v3[8] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][3]) %>% 
  str_match("\\d+") %>% as.vector()

활력징후_추가_RR <- ((너싱홈_03_1$v3[8] %>% 
                      str_squish() %>% 
                      strsplit(split="\\,"))[[1]][4]) %>% 
  str_match("\\d+") %>% as.vector()

혈당_식전 <- ((너싱홈_03_1$v3[9] %>% 
                 str_squish() %>% 
                 strsplit(split="\\,"))[[1]][1]) %>% 
  str_match("\\d+") %>% as.vector()

혈당_식후2시간 <- ((너싱홈_03_1$v3[9] %>% 
                    str_squish() %>% 
                    strsplit(split="\\,"))[[1]][2]) %>% 
  str_remove_all("식후 2시간 : ") %>% 
  str_match("\\d+") %>% as.vector()

통증_일반통증 <- (((너싱홈_03_1$v3[10] %>% 
                str_squish() %>% 
                str_replace_all("□","x No x") %>% 
                str_replace_all("■","x Yes x"))) %>% 
              strsplit(split="x"))[[1]][2] %>% 
  str_remove_all(" ")

통증_암성통증 <- (((너싱홈_03_1$v3[10] %>% 
                str_squish() %>% 
                str_replace_all("□","x No x") %>% 
                str_replace_all("■","x Yes x"))) %>% 
              strsplit(split="x"))[[1]][4] %>% 
  str_remove_all(" ")

통증_통증양상 <- paste0((((너싱홈_03_1$v3[10] %>% 
                           str_squish() %>% 
                           str_replace_all("□","x No x") %>% 
                           str_replace_all("■","x Yes x"))) %>% 
                     strsplit(split="x"))[[1]][6],
                  " ",
                  (((너싱홈_03_1$v3[10] %>% 
                           str_squish() %>% 
                           str_replace_all("□","x No x") %>% 
                           str_replace_all("■","x Yes x"))) %>% 
                     strsplit(split="x"))[[1]][7] %>% 
                    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ") 

통증_점수 <- paste0((((너싱홈_03_1$v3[10] %>% 
                         str_squish() %>% 
                         str_replace_all("□","x No x") %>% 
                         str_replace_all("■","x Yes x"))) %>% 
                   strsplit(split="x"))[[1]][8],
                " ",
                (((너싱홈_03_1$v3[10] %>% 
                         str_squish() %>% 
                         str_replace_all("□","x No x") %>% 
                         str_replace_all("■","x Yes x"))) %>% 
                   strsplit(split="x"))[[1]][9] %>% 
                  str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ") 

통증_중재약물 <- paste0((((너싱홈_03_1$v3[10] %>% 
                           str_squish() %>% 
                           str_replace_all("□","x No x") %>% 
                           str_replace_all("■","x Yes x"))) %>% 
                     strsplit(split="x"))[[1]][10],
                  " ",
                  (((너싱홈_03_1$v3[10] %>% 
                           str_squish() %>% 
                           str_replace_all("□","x No x") %>% 
                           str_replace_all("■","x Yes x"))) %>% 
                     strsplit(split = "x"))[[1]][11] %>% 
                    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_경관영양_cc1 <- (((너싱홈_03_1$v3[11] %>% 
                          str_squish() %>% 
                          str_remove_all("□") %>% 
                          str_remove_all("■") %>% 
                          strsplit(split = "cc"))[[1]][1] %>% 
                     strsplit(split = "[-]"))[[1]][1]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_경관영양_cc2 <- (((너싱홈_03_1$v3[11] %>% 
                          str_squish() %>% 
                          str_remove_all("□") %>% 
                          str_remove_all("■") %>% 
                          strsplit(split = "cc"))[[1]][1] %>% 
                     strsplit(split = "[-]"))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_경관영양_cc3 <- (((너싱홈_03_1$v3[11] %>% 
                          str_squish() %>% 
                          str_remove_all("□") %>% 
                          str_remove_all("■") %>% 
                          strsplit(split = "cc"))[[1]][1] %>% 
                     strsplit(split = "[-]"))[[1]][3]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_경관영양_cc4 <- (((너싱홈_03_1$v3[11] %>% 
                          str_squish() %>% 
                          str_remove_all("□") %>% 
                          str_remove_all("■") %>% 
                          strsplit(split = "cc"))[[1]][1] %>% 
                     strsplit(split = "[-]"))[[1]][4]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_위루관영양_cc1 <- (((너싱홈_03_1$v3[11] %>% 
                       str_squish() %>% 
                       str_remove_all("□") %>% 
                       str_remove_all("■") %>% 
                       strsplit(split = "cc"))[[1]][2] %>% 
                      strsplit(split = "\\s"))[[1]][5]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_위루관영양_cc2 <- (((너싱홈_03_1$v3[11] %>% 
                       str_squish() %>% 
                       str_remove_all("□") %>% 
                       str_remove_all("■") %>% 
                       strsplit(split = "cc"))[[1]][2] %>% 
                      strsplit(split = "\\s"))[[1]][6]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_위루관영양_cc3 <- (((너싱홈_03_1$v3[11] %>% 
                       str_squish() %>% 
                       str_remove_all("□") %>% 
                       str_remove_all("■") %>% 
                       strsplit(split = "cc"))[[1]][2] %>% 
                      strsplit(split = "\\s"))[[1]][7]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_위루관영양_cc4 <- (((너싱홈_03_1$v3[11] %>% 
                       str_squish() %>% 
                       str_remove_all("□") %>% 
                       str_remove_all("■") %>% 
                       strsplit(split = "cc"))[[1]][2] %>% 
                      strsplit(split = "\\s"))[[1]][8]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_중심정액영양 <- paste0(
                      ((너싱홈_03_1$v6[11] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = "\r\n"))[[1]][1] %>% 
                         strsplit(split = "x"))[[1]][2],
                      " : ",
                      ((너싱홈_03_1$v6[11] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = "\r\n"))[[1]][1] %>% 
                         strsplit(split = "x"))[[1]][3] %>% 
                        str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_구강치료식 <- paste0(
  ((너싱홈_03_1$v6[11] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = "\r\n"))[[1]][2] %>% 
     strsplit(split = "x"))[[1]][2],
  " : ",
  ((너싱홈_03_1$v6[11] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = "\r\n"))[[1]][1] %>% 
     strsplit(split = "x"))[[1]][3] %>% 
    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_L_tube_사이즈<- paste0(
  ((너싱홈_03_1$v3[12] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = ","))[[1]][1] %>% 
     strsplit(split = "\r\n"))[[1]][1] %>% 
    str_remove_all("x") %>% 
    str_remove_all("L-tube"),
  " : ",
  ((너싱홈_03_1$v3[12] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = ","))[[1]][1] %>% 
     strsplit(split = "\r\n"))[[1]][2] %>% 
    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

영양관리_L_tube_고정위치 <- ((너싱홈_03_1$v3[12] %>% 
                            str_replace_all("□","x No x") %>% 
                            str_replace_all("■","x Yes x") %>% 
                            strsplit(split = ","))[[1]][2]) %>% 
  str_match("\\d+") %>% as.vector()

영양관리_L_tube_드레싱 <- 너싱홈_03_1$v6[12] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  str_remove_all("x") %>% 
  str_remove_all("드레싱") %>% 
  str_squish()

영양관리_L_tube_교환<- 너싱홈_03_1$v6[13] %>% 
  str_replace_all("□","x No x") %>% 
  str_replace_all("■","x Yes x") %>% 
  str_remove_all("x") %>% 
  str_remove_all("교환") %>% 
  str_squish()

영양관리_Gastrostomy_tube_사이즈 <- paste0( 
  ((너싱홈_03_1$v3[14] %>% 
          strsplit(split = ","))[[1]][1] %>% 
     str_replace_all("□","x No x") %>% 
     str_replace_all("■","x Yes x") %>% 
     strsplit(split = "x"))[[1]][2],
  " : ",
  (((너싱홈_03_1$v3[14] %>% 
           strsplit(split = ","))[[1]][1] %>% 
      str_replace_all("□","x No x") %>% 
      str_replace_all("■","x Yes x") %>% 
      strsplit(split = "x"))[[1]][3] %>% 
     strsplit(split = " "))[[1]][2]) %>% 
  str_remove_all(" ")

영양관리_Gastrostomy_tube_고정위치 <-
  ((너싱홈_03_1$v3[14] %>% 
          strsplit(split = ","))[[1]][2] %>% 
     strsplit(split = "\r\n"))[[1]][2] %>% 
  str_remove_all("\\)")

영양관리_Gastrostomy_tube_드레싱 <-
  (너싱홈_03_1$v6[14] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

영양관리_Gastrostomy_tube_교환 <-
  (너싱홈_03_1$v6[15] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

호흡관리_산소장치_종류 <- paste0(
                       (너싱홈_03_1$v3[16] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = "x"))[[1]][2],
                       " : ",
                       ((너싱홈_03_1$v3[16] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][3] %>% 
                          strsplit(split = " "))[[1]][2] %>% 
                         str_remove_all("\r\n") %>% 
                         str_remove_all("\\)")) %>% 
  str_remove_all(" ")

호흡관리_산소용량 <- paste0(
  (너싱홈_03_1$v3[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][4],
  " : ",
  ((너싱홈_03_1$v3[16] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = "x"))[[1]][5]) %>% 
    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_인공호흡기_Mode <- paste0(
  (너싱홈_03_1$v3[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][6],
  " : ",
  (((너싱홈_03_1$v3[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][7] %>% 
      strsplit(split = ","))[[1]][1] %>% 
     strsplit(split = "\\("))[[1]][2] %>% 
    str_remove_all("Mode")) %>% 
  str_remove_all(" ")

호흡관리_인공호흡기_FiO2 <- paste0(
  (너싱홈_03_1$v3[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][6],
  " : ",
  ((너싱홈_03_1$v3[16] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = "x"))[[1]][7] %>% 
     strsplit(split = ","))[[1]][2] %>% 
    str_remove_all("Fi02") %>% 
    str_match("\\d+") %>% as.vector()) %>%   
  str_remove_all(" ")

호흡관리_인공호흡기_O2 <- paste0(
  (너싱홈_03_1$v3[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][6],
  " : ",
  ((너싱홈_03_1$v3[16] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = "x"))[[1]][7] %>% 
     strsplit(split = ","))[[1]][3] %>%
    str_remove_all("O2") %>% 
    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_흡인 <- paste0(
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][2],
  " : ",
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][3] %>% 
    str_remove_all("\r\n")) %>% 
  str_remove_all(" ")

호흡관리_흡인_oral_nasal <- paste0(
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][4],
  " : ",
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][5] %>% 
    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_흡인_intra_tracheal <- paste0(
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][6],
  " : ",
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][7] %>% 
    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_Nebulizer_종류 <- paste0(
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][8],
  " : ",
  (((너싱홈_03_1$v6[16] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][9] %>% 
      strsplit(split = ","))[[1]][1] %>% 
     strsplit(split = "\\:"))[[1]][2]) %>% 
  str_remove_all(" ")

호흡관리_Nebulizer_회 <- paste0(
  (너싱홈_03_1$v6[16] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][8],
  " : ",
  ((너싱홈_03_1$v6[16] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = "x"))[[1]][9] %>% 
     strsplit(split = ","))[[1]][2] %>% 
    str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

호흡관리_기관지절개관_사이즈 <- 너싱홈_03_1$v3[17] %>% 
  str_match("\\d+") %>% as.vector()

호흡관리_기관지절개관_드레싱 <- (너싱홈_03_1$v6[17]%>% 
                      str_replace_all("□","x No x") %>% 
                      str_replace_all("■","x Yes x") %>% 
                      strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

호흡관리_기관지절개관_교환 <- (너싱홈_03_1$v6[18]%>% 
                     str_replace_all("□","x No x") %>% 
                     str_replace_all("■","x Yes x") %>% 
                     strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_회음부간호 <- (너싱홈_03_1$v3[19] %>% 
                    str_replace_all("□","x No x") %>% 
                    str_replace_all("■","x Yes x") %>% 
                    strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_방광세척 <- (너싱홈_03_1$v3[20] %>% 
                   str_replace_all("□","x No x") %>% 
                   str_replace_all("■","x Yes x") %>% 
                   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_단순도뇨_회 <- paste0(
                         (너싱홈_03_1$v3[21] %>% 
                                str_replace_all("□","x No x") %>% 
                                str_replace_all("■","x Yes x") %>% 
                                strsplit(split = "x"))[[1]][2],
                         " : ",
                         ((너싱홈_03_1$v3[21] %>% 
                                 str_replace_all("□","x No x") %>% 
                                 str_replace_all("■","x Yes x") %>% 
                                 strsplit(split = "x"))[[1]][3] %>% 
                            strsplit(split = ","))[[1]][1] %>%
                           str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

배뇨_배설관리_단순도뇨_총배설량cc <- paste0(
                              (너싱홈_03_1$v3[21] %>% 
                                     str_replace_all("□","x No x") %>% 
                                     str_replace_all("■","x Yes x") %>% 
                                     strsplit(split = "x"))[[1]][2],
                              " : ",
                              ((너싱홈_03_1$v3[21] %>% 
                                      str_replace_all("□","x No x") %>% 
                                      str_replace_all("■","x Yes x") %>% 
                                      strsplit(split = "x"))[[1]][3] %>% 
                                 strsplit(split = ","))[[1]][2] %>%
                                str_match("\\d+") %>% as.vector())%>% 
  str_remove_all(" ")

배뇨_배설관리_방광훈련 <- (너싱홈_03_1$v6[21] %>% 
                   str_replace_all("□","x No x") %>% 
                   str_replace_all("■","x Yes x") %>% 
                   strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_배뇨_회 <- paste0(
                       (너싱홈_03_1$v3[22] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = "x"))[[1]][2],
                       " : ",
                       ((너싱홈_03_1$v3[22] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][3] %>% 
                          strsplit(split = ","))[[1]][1] %>% 
                         str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

배뇨_배설관리_배뇨_양상 <- paste0(
                        (너싱홈_03_1$v3[22] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2],
                        " : ",
                        ((너싱홈_03_1$v3[22] %>% 
                                str_replace_all("□","x No x") %>% 
                                str_replace_all("■","x Yes x") %>% 
                                strsplit(split = "x"))[[1]][3] %>% 
                           strsplit(split = ","))[[1]][2] %>% 
                          str_remove_all("양상") %>% 
                          str_remove_all("\\)")) %>% 
  str_squish() %>% 
  str_remove_all(" ")

배뇨_배설관리_배변_회 <- paste0(
                       (너싱홈_03_1$v6[22] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = "x"))[[1]][2],
                       " : ",
                       ((너싱홈_03_1$v6[22] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][3] %>% 
                          strsplit(split = ","))[[1]][1] %>% 
                         str_match("\\d+") %>% as.vector()) %>% 
  str_remove_all(" ")

배뇨_배설관리_배변_양상 <- paste0(
                        (너싱홈_03_1$v6[22] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2],
                        " : ",
                        ((너싱홈_03_1$v6[22] %>% 
                                str_replace_all("□","x No x") %>% 
                                str_replace_all("■","x Yes x") %>% 
                                strsplit(split = "x"))[[1]][3] %>% 
                           strsplit(split = ","))[[1]][2] %>% 
                          str_remove_all("양상") %>% 
                          str_remove_all("\\)")) %>% 
  str_squish() %>% 
  str_remove_all(" ")

배뇨_배설관리_유치도뇨관_사이즈 <- paste0(
                            (너싱홈_03_1$v3[23] %>% 
                                   str_replace_all("□","x No x") %>% 
                                   str_replace_all("■","x Yes x") %>% 
                                   strsplit(split = "x"))[[1]][2],
                            " : ",
                            ((너싱홈_03_1$v3[23] %>% 
                                    str_replace_all("□","x No x") %>% 
                                    str_replace_all("■","x Yes x") %>% 
                                    strsplit(split = "x"))[[1]][3] %>% 
                               strsplit(split = " "))[[1]][2] %>% 
                              str_remove_all("\\)")) %>% 
  str_squish() %>% 
  str_remove_all(" ")

배뇨_배설관리_유치도뇨관_교환 <- (너싱홈_03_1$v6[23] %>% 
                       str_replace_all("□","x No x") %>% 
                       str_replace_all("■","x Yes x") %>% 
                       strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

배뇨_배설관리_요루관리 <- paste0(
                       ((너싱홈_03_1$v3[24] %>% 
                               strsplit(split = "\\("))[[1]][1] %>% 
                          str_replace_all("□","x No x") %>% 
                          str_replace_all("■","x Yes x") %>% 
                          strsplit(split = "x"))[[1]][2] %>% 
                         str_remove_all(" "),
                       " : ",
                       str_sub(
                         (너싱홈_03_1$v3[24] %>% 
                                strsplit(split = "\\("))[[1]][2],
                         
                         (((너싱홈_03_1$v3[24] %>% 
                                  strsplit(split = "\\("))[[1]][2] %>% 
                             str_locate_all("■"))[[1]] %>% 
                            as.vector())[1]+2,
                         
                         (((너싱홈_03_1$v3[24] %>% 
                                  strsplit(split = "\\("))[[1]][2] %>% 
                             str_locate_all("■"))[[1]] %>% 
                            as.vector())[1]+5) %>% na.omit()
)


배뇨_배설관리_장루관리 <- paste0(((너싱홈_03_1$v3[25] %>%  
                               strsplit(split = "\\("))[[1]][1] %>% 
                          str_replace_all("□","x No x") %>% 
                          str_replace_all("■","x Yes x") %>% 
                          strsplit(split = "x"))[[1]][2] %>% 
                         str_remove_all(" "),
                       " : ",
                       str_sub(
                         (너싱홈_03_1$v3[25] %>% 
                                strsplit(split = "\\("))[[1]][2],
                         
                         (((너싱홈_03_1$v3[25] %>% 
                                  strsplit(split = "\\("))[[1]][2] %>% 
                             str_locate_all("■"))[[1]] %>% 
                            as.vector())[1]+2,
                         
                         (((너싱홈_03_1$v3[25] %>% 
                                  strsplit(split = "\\("))[[1]][2] %>% 
                             str_locate_all("■"))[[1]] %>% 
                            as.vector())[1]+5) %>% na.omit()
)

배뇨_배설관리_장루관리_교환 <- (너싱홈_03_1$v6[25] %>% 
                      str_replace_all("□","x No x") %>% 
                      str_replace_all("■","x Yes x") %>% 
                      strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")

상처관리_외과적상처_부위 <- paste0(
                        (너싱홈_03_1$v3[26] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2] %>% 
                          str_remove_all(" "),
                        " : ",
                        (((너싱홈_03_1$v3[26] %>% 
                                 str_replace_all("□","x No x") %>% 
                                 str_replace_all("■","x Yes x") %>% 
                                 strsplit(split = "x"))[[1]][3] %>% 
                            strsplit(split = ","))[[1]][1] %>% 
                           strsplit(split = " "))[[1]][3]) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_외과적상처_삼출물 <- paste0(
                         (너싱홈_03_1$v3[26] %>% 
                                str_replace_all("□","x No x") %>% 
                                str_replace_all("■","x Yes x") %>% 
                                strsplit(split = "x"))[[1]][2] %>% 
                           str_remove_all(" "),
                         " : ",
                         (((너싱홈_03_1$v3[26] %>% 
                                  str_replace_all("□","x No x") %>% 
                                  str_replace_all("■","x Yes x") %>% 
                                  strsplit(split = "x"))[[1]][3] %>% 
                             strsplit(split = ",")))[[1]][2] %>% 
                           str_remove_all("삼출물:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_외과적상처_크기 <- paste0(
                        (너싱홈_03_1$v3[26] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2] %>% 
                          str_remove_all(" "),
                        " : ",
                        ((너싱홈_03_1$v3[26] %>% 
                                str_replace_all("□","z No z") %>% 
                                str_replace_all("■","z Yes z") %>% 
                                strsplit(split = "z"))[[1]][3] %>% 
                           strsplit(split = ","))[[1]][3] %>% 
                          str_remove_all("크기:") %>% 
                          str_remove_all("cm")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_외과적상처_주위피부 <- paste0(
                          (너싱홈_03_1$v3[26] %>% 
                                 str_replace_all("□","x No x") %>% 
                                 str_replace_all("■","x Yes x") %>% 
                                 strsplit(split = "x"))[[1]][2] %>% 
                            str_remove_all(" "),
                          " : ",
                          (((너싱홈_03_1$v3[26] %>% 
                                   str_replace_all("□","z No z") %>% 
                                   str_replace_all("■","z Yes z") %>% 
                                   strsplit(split = "z"))[[1]][3] %>% 
                              strsplit(split = ","))[[1]][4] %>% 
                             strsplit(split = "\r\n"))[[1]][1] %>% 
                            str_remove_all("감염징후") %>% 
                            str_remove_all("\\:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") %>% 
  str_remove_all("\\(")

상처관리_외과적상처_감염징후 <- paste0(
                          str_sub(
                            (너싱홈_03_1$v3[26] %>% 
                                   str_squish() %>% 
                                   strsplit(split = "감염징후"))[[1]][2] %>% 
                              str_remove_all("\\("),
                            
                            ((((너싱홈_03_1$v3[26] %>% 
                                      str_squish() %>% 
                                      strsplit(split = "감염징후"))[[1]][2] %>% 
                                 str_remove_all("\\(") %>%
                                 str_locate_all("■"))[[1]] %>% 
                                as.vector())[1]+2),
                            
                            ((((너싱홈_03_1$v3[26] %>% 
                                      str_squish() %>% 
                                      strsplit(split = "감염징후"))[[1]][2] %>% 
                                 str_remove_all("\\(") %>%
                                 str_locate_all("■"))[[1]] %>% 
                                as.vector())[1]+3)) %>% na.omit(), 
                          
                          ((너싱홈_03_1$v3[26] %>% 
                                  str_squish() %>% 
                                  strsplit(split = "감염징후"))[[1]][2] %>% 
                             str_remove_all("\\(") %>% 
                             strsplit(split = "기타"))[[1]][2]
)

상처관리_외과적상처_드레싱종류 <- 너싱홈_03_1$v3[27] %>% 
  str_remove_all("드레싱 종류:") %>% 
  str_squish() %>% 
  str_remove_all(" ")

상처관리_단순드레싱_부위 <- paste0(
                        (너싱홈_03_1$v3[28] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2] %>% 
                          str_remove_all(" "),
                        " : ",
                        (((너싱홈_03_1$v3[28] %>% 
                                 str_replace_all("□","x No x") %>% 
                                 str_replace_all("■","x Yes x") %>% 
                                 strsplit(split = "x"))[[1]][3] %>% 
                            strsplit(split = ","))[[1]][1] %>% 
                           strsplit(split = " "))[[1]][3]) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_삼출물 <- paste0(
                         (너싱홈_03_1$v3[28] %>% 
                                str_replace_all("□","x No x") %>% 
                                str_replace_all("■","x Yes x") %>% 
                                strsplit(split = "x"))[[1]][2] %>% 
                           str_remove_all(" "),
                         " : ",
                         (((너싱홈_03_1$v3[28] %>% 
                                  str_replace_all("□","x No x") %>% 
                                  str_replace_all("■","x Yes x") %>% 
                                  strsplit(split = "x"))[[1]][3] %>% 
                             strsplit(split = ",")))[[1]][2] %>% 
                           str_remove_all("삼출물:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_크기 <- paste0(
                        (너싱홈_03_1$v3[28] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2] %>% 
                          str_remove_all(" "),
                        " : ",
                        ((너싱홈_03_1$v3[28] %>% 
                                str_replace_all("□","z No z") %>% 
                                str_replace_all("■","z Yes z") %>% 
                                strsplit(split = "z"))[[1]][3] %>% 
                           strsplit(split = ","))[[1]][3] %>% 
                          str_remove_all("크기:") %>% 
                          str_remove_all("cm")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_주위피부 <- paste0(
                          (너싱홈_03_1$v3[28] %>% 
                                 str_replace_all("□","x No x") %>% 
                                 str_replace_all("■","x Yes x") %>% 
                                 strsplit(split = "x"))[[1]][2] %>% 
                            str_remove_all(" "),
                          " : ",
                          (((너싱홈_03_1$v3[28] %>% 
                                   str_replace_all("□","z No z") %>% 
                                   str_replace_all("■","z Yes z") %>% 
                                   strsplit(split = "z"))[[1]][3] %>% 
                              strsplit(split = ","))[[1]][4] %>% 
                             strsplit(split = "\r\n"))[[1]][1] %>% 
                            str_remove_all("주위피부:")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_단순드레싱_감염징후 <- paste0(
                          str_sub(
                            (너싱홈_03_1$v3[28] %>% 
                                   str_squish() %>% 
                                   strsplit(split = "감염징후"))[[1]][2] %>% 
                              str_remove_all("\\("),
                            
                            ((((너싱홈_03_1$v3[28] %>% 
                                      str_squish() %>% 
                                      strsplit(split = "감염징후"))[[1]][2] %>% 
                                 str_remove_all("\\(") %>%
                                 str_locate_all("■"))[[1]] %>% 
                                as.vector())[1]+2) %>% na.omit(),
                            
                            ((((너싱홈_03_1$v3[28] %>% 
                                      str_squish() %>% 
                                      strsplit(split = "감염징후"))[[1]][2] %>% 
                                 str_remove_all("\\(") %>%
                                 str_locate_all("■"))[[1]] %>% 
                                as.vector())[1]+3) %>% na.omit()) , 
                          
                          ((너싱홈_03_1$v3[28] %>% 
                                  str_squish() %>% 
                                  strsplit(split = "감염징후"))[[1]][2] %>% 
                             str_remove_all("\\(") %>% 
                             strsplit(split = "기타"))[[1]][2] %>% na.omit()
)

상처관리_단순드레싱_드레싱종류 <- 너싱홈_03_1$v3[29] %>% 
  str_remove_all("드레싱 종류:") %>% 
  str_squish() %>% 
  str_remove_all(" ")

상처관리_복합드레싱_부위 <- paste0(
                        (너싱홈_03_1$v3[30] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2] %>% 
                          str_remove_all(" "),
                        " : ",
                        (((너싱홈_03_1$v3[30] %>% 
                                 str_replace_all("□","x No x") %>% 
                                 str_replace_all("■","x Yes x") %>% 
                                 strsplit(split = "x"))[[1]][3] %>% 
                            strsplit(split = ","))[[1]][1] %>% 
                           strsplit(split = ":"))[[1]][3] %>% 
                          str_remove_all(" ") %>% 
                          str_squish()) 

상처관리_복합드레싱_삼출물 <- paste0(
                         (너싱홈_03_1$v3[30] %>% 
                                str_replace_all("□","x No x") %>% 
                                str_replace_all("■","x Yes x") %>% 
                                strsplit(split = "x"))[[1]][2] %>% 
                           str_remove_all(" "),
                         " : ",
                         (((너싱홈_03_1$v3[30] %>% 
                                  str_replace_all("□","x No x") %>% 
                                  str_replace_all("■","x Yes x") %>% 
                                  strsplit(split = "x"))[[1]][3] %>% 
                             strsplit(split = ",")))[[1]][2] %>% 
                           str_remove_all("삼출물:") %>% 
                           str_squish() %>% 
                           str_remove_all(" ") )

상처관리_복합드레싱_크기 <- paste0(
                        (너싱홈_03_1$v3[30] %>% 
                               str_replace_all("□","x No x") %>% 
                               str_replace_all("■","x Yes x") %>% 
                               strsplit(split = "x"))[[1]][2] %>% 
                          str_remove_all(" "),
                        " : ",
                        ((너싱홈_03_1$v3[30] %>% 
                                str_replace_all("□","z No z") %>% 
                                str_replace_all("■","z Yes z") %>% 
                                strsplit(split = "z"))[[1]][3] %>% 
                           strsplit(split = ","))[[1]][3] %>% 
                          str_remove_all("크기:") %>% 
                          str_remove_all("cm")) %>% 
  str_squish() %>% 
  str_remove_all(" ") 

상처관리_복합드레싱_주위피부 <- paste0(
                          (너싱홈_03_1$v3[30] %>% 
                                 str_replace_all("□","x No x") %>% 
                                 str_replace_all("■","x Yes x") %>% 
                                 strsplit(split = "x"))[[1]][2] %>% 
                            str_remove_all(" "),
                          " : ",
                          (((너싱홈_03_1$v3[30] %>% 
                                   str_replace_all("□","z No z") %>% 
                                   str_replace_all("■","z Yes z") %>% 
                                   strsplit(split = "z"))[[1]][3] %>% 
                              strsplit(split = ","))[[1]][4] %>% 
                             strsplit(split = "\r\n"))[[1]][1] %>% 
                            str_remove_all("주위피부:") %>% 
                            str_remove_all("감염징후")) %>%
  str_squish() %>% 
  str_remove_all(" ") %>% 
  str_remove_all("\\(")

상처관리_복합드레싱_감염징후 <-paste0(
                         str_sub(
                           (너싱홈_03_1$v3[30] %>% 
                                  str_squish() %>% 
                                  strsplit(split = "감염징후"))[[1]][2] %>% 
                             str_remove_all("\\("),
                           
                           ((((너싱홈_03_1$v3[30] %>% 
                                     str_squish() %>% 
                                     strsplit(split = "감염징후"))[[1]][2] %>% 
                                str_remove_all("\\(") %>%
                                str_locate_all("■"))[[1]] %>% 
                               as.vector())[1]+2),
                           
                           ((((너싱홈_03_1$v3[30] %>% 
                                     str_squish() %>% 
                                     strsplit(split = "감염징후"))[[1]][2] %>% 
                                str_remove_all("\\(") %>%
                                str_locate_all("■"))[[1]] %>% 
                               as.vector())[1]+3)), 
                         
                         ((너싱홈_03_1$v3[30] %>% 
                                 str_squish() %>% 
                                 strsplit(split = "감염징후"))[[1]][2] %>% 
                            str_remove_all("\\(") %>% 
                            strsplit(split = "기타"))[[1]][2]
)

상처관리_복합드레싱_드레싱종류 <- 너싱홈_03_1$v3[31] %>% 
  str_remove_all("드레싱 종류:") %>% 
  str_squish() %>% 
  str_remove_all(" ")

상처관리_봉합사제거 <- (너싱홈_03_1$v3[32] %>% 
                 str_replace_all("□","x No x") %>% 
                 str_replace_all("■","x Yes x") %>% 
                 strsplit(split = "x"))[[1]][2] %>% 
  str_remove_all(" ")


### 2페이지
욕창_예방_예방도구 <- (너싱홈_03_2$v3[2] %>% 
                 str_squish() %>% 
                 str_replace_all("□","x No x") %>% 
                 str_replace_all("■","x Yes x") %>% 
                 strsplit(split = "x"))[[1]][3] %>% 
  str_remove_all(" ") 

욕창_예방_체위변경시간 <- 너싱홈_03_2$v3[3] %>% 
  str_remove_all("체위변경 시간 :") %>% 
  str_squish()


욕창_예방_욕창호발부위_피부상태 <- paste0(
                            str_sub(
                              (너싱홈_03_2$v3[4] %>% 
                                     str_squish() %>% 
                                     strsplit(split = "피부상태"))[[1]][2],
                              
                              (((너싱홈_03_2$v3[4] %>% 
                                       str_squish() %>% 
                                       strsplit(split = "피부상태"))[[1]][2] %>%
                                  str_locate_all("■"))[[1]] %>% 
                                 as.vector())[1]+2,
                              
                              (((너싱홈_03_2$v3[4] %>% 
                                       str_squish() %>% 
                                       strsplit(split = "피부상태"))[[1]][2] %>%
                                  str_locate_all("■"))[[1]] %>% 
                                 as.vector())[1]+3), 
                            
                            ((너싱홈_03_2$v3[4] %>% 
                                    str_squish() %>% 
                                    strsplit(split = "피부상태"))[[1]][2] %>% 
                               strsplit(split = "부위:"))[[1]][2] %>% 
                              str_remove_all("\\)") %>% 
                              str_remove_all(" ")
)

욕창_관리_부위 <- paste0(
  (너싱홈_03_2$v3[5] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][2],
  " : ",
  ((너싱홈_03_2$v3[5] %>% 
          str_replace_all("□","x No x") %>% 
          str_replace_all("■","x Yes x") %>% 
          strsplit(split = "x"))[[1]][3] %>% 
     strsplit(split = " "))[[1]][2]) %>% 
  str_squish() %>% 
  str_remove_all(" ")

욕창_관리_삼출물 <- paste0(
  (너싱홈_03_2$v3[5] %>% 
         str_replace_all("□","x No x") %>% 
         str_replace_all("■","x Yes x") %>% 
         strsplit(split = "x"))[[1]][4],
  " : ",
  (((너싱홈_03_2$v3[5] %>% 
           str_replace_all("□","x No x") %>% 
           str_replace_all("■","x Yes x") %>% 
           strsplit(split = "x"))[[1]][5] %>% 
      strsplit(split = "\r\n"))[[1]][1] %>% 
     strsplit(split = " "))[[1]][2]) %>% 
  str_squish() %>% 
  str_remove_all(" ")

욕창_관리_단계 <- str_sub(
  (너싱홈_03_2$v3[5] %>% 
         str_squish()),
  
  (((너싱홈_03_2$v3[5] %>% 
           str_squish() %>%
           str_locate_all("●"))[[1]] %>% 
      as.vector())[1])+1,
  
  (((너싱홈_03_2$v3[5] %>% 
           str_squish() %>%
           str_locate_all("●"))[[1]] %>% 
      as.vector())[1])+2) %>% 
  str_replace_all("D","DTI") %>% 
  str_replace_all("U","Unstageable") %>% 
  str_remove_all(" ") 

욕창_관리_크기 <- (너싱홈_03_2$v3[6] %>% 
               strsplit(split = "\\("))[[1]][1] %>% 
  str_remove_all("크기:") %>% 
  str_remove_all(" ") %>% 
  str_remove_all("cm") %>% 
  str_replace_all("x"," x ") %>% 
  str_remove_all(" ") 

욕창_관리_깊이 <- (너싱홈_03_2$v3[6] %>% 
               strsplit(split = "\\("))[[1]][2] %>% 
  str_match("\\d+") %>% as.vector()

욕창_관리_드레싱제제 <- paste0(
                      (너싱홈_03_2$v8[6] %>% 
                             str_replace_all("□","x No x") %>% 
                             str_replace_all("■","x Yes x") %>% 
                             strsplit(split = "x"))[[1]][2],
                      " : ",
                      ((너싱홈_03_2$v8[6] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = "x"))[[1]][3] %>% 
                         strsplit(split = "\r\n"))[[1]][2] %>% 
                        str_remove_all("\\)")) %>% 
  str_remove_all(" ") 

당뇨발관리_위치 <- (너싱홈_03_2$v3[7] %>% 
               strsplit(split = "\r\n"))[[1]][2] 

당뇨발관리_크기 <- 너싱홈_03_2$v6[7] %>% 
  str_remove_all("크기:") 

당뇨발관리_드레싱제제 <- paste0(
                      (너싱홈_03_2$v8[7] %>% 
                             str_replace_all("□","x No x") %>% 
                             str_replace_all("■","x Yes x") %>% 
                             strsplit(split = "x"))[[1]][2],
                      " : ",
                      ((너싱홈_03_2$v8[7] %>% 
                              str_replace_all("□","x No x") %>% 
                              str_replace_all("■","x Yes x") %>% 
                              strsplit(split = "x"))[[1]][3] %>% 
                         strsplit(split = "\r\n"))[[1]][2] %>% 
                        str_remove_all("\\)")) %>% 
  str_remove_all(" ") 

억제대_종류 <- paste0(
  str_sub(
    (너싱홈_03_2$v3[8] %>% 
           str_squish()),
    
    (((너싱홈_03_2$v3[8] %>% 
             str_squish() %>%
             str_locate_all("■"))[[1]] %>% 
        as.vector())[1])+2,
    
    (((너싱홈_03_2$v3[8] %>% 
             str_squish() %>%
             str_locate_all("■"))[[1]] %>% 
        as.vector())[1])+3) %>% 
    str_replace_all("손장","손장갑") %>% 
    str_remove_all(" "),
  
  (너싱홈_03_2$v3[8] %>% 
         str_squish() %>% 
         strsplit(split = "기"))[[1]][2] %>% 
    str_remove_all("타")) %>% 
  str_remove_all(" ")

억제대_적용시간 <- paste0(
  str_sub(
    (너싱홈_03_2$v3[9] %>% 
           str_squish()),
    
    (((너싱홈_03_2$v3[9] %>% 
             str_squish() %>%
             str_locate_all("■"))[[1]] %>% 
        as.vector())[1])+2,
    
    (((너싱홈_03_2$v3[9] %>% 
             str_squish() %>%
             str_locate_all("■"))[[1]] %>% 
        as.vector())[1])+6) %>% 
    str_replace_all("손장","손장갑") %>% 
    str_remove_all(" "),
  
  (너싱홈_03_2$v3[9] %>% 
         str_squish() %>% 
         strsplit(split = "기"))[[1]][2] %>% 
    str_remove_all("타") %>% 
    str_remove_all(" "))

억제대_적용시간적용부위_피부상태_확인_시간작성 <- (너싱홈_03_2$v3[10] %>% 
                                strsplit(split = " "))[[1]][2] %>%
  str_squish() %>% 
  str_remove_all(" ")


의료서비스이용_가정간호서비스이용_사유 <- paste0(
                               str_sub(
                                 너싱홈_03_2$v5[11],
                                  
                                  (((너싱홈_03_2$v5[11] %>% 
                                           str_squish() %>%
                                           str_locate_all("■"))[[1]] %>% 
                                      as.vector())[1])+2,
                                  
                                  ((너싱홈_03_2$v5[11] %>% 
                                          str_squish() %>%
                                          str_locate_all("■"))[[1]] %>% 
                                     as.vector())[1]+3),
                               
                               (너싱홈_03_2$v5[11] %>% 
                                      strsplit(split = "기"))[[1]][2] %>% 
                                 str_remove_all("타") %>% 
                                 str_remove_all("\\(") %>% 
                                 str_remove_all("\\)") %>% 
                                 str_remove_all(" "))

의료서비스이용_외래방문_진료과 <-
  paste0(
    str_sub(
      (너싱홈_03_2$v5[12] %>% 
             str_remove_all("\r\n") %>% 
             strsplit(split = "\\)"))[[1]][1],
      
      (((너싱홈_03_2$v5[12] %>% 
               str_remove_all("\r\n") %>% 
               strsplit(split = "\\)"))[[1]][1] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      ((((너싱홈_03_2$v5[12] %>% 
                str_remove_all("\r\n") %>% 
                strsplit(split = "\\)"))[[1]][1] %>%
           str_locate_all("■"))[[1]] %>% 
          as.vector())[1]+3)) %>% 
      str_replace_all("가정","가정의학과") %>% 
      str_replace_all("비뇨","비뇨기과") %>% 
      str_replace_all("피부","피부과") %>% 
      str_replace_all("한의","한의원"),
    
    ((너싱홈_03_2$v5[12] %>% 
            str_remove_all("\r\n") %>% 
            strsplit(split = "\\)"))[[1]][1] %>% 
       strsplit(split = "\\("))[[1]][2] %>% 
      str_remove_all(" "))

의료서비스이용_외래방문_사유 <-
  paste0(
    str_sub(
      (너싱홈_03_2$v5[12] %>% 
             str_remove_all("\r\n") %>% 
             strsplit(split = "\\)"))[[1]][2],
      
      (((너싱홈_03_2$v5[12] %>% 
               str_remove_all("\r\n") %>% 
               strsplit(split = "\\)"))[[1]][2] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      (((너싱홈_03_2$v5[12] %>% 
               str_remove_all("\r\n") %>% 
               strsplit(split = "\\)"))[[1]][2] %>%
          str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+3) %>% 
      str_replace_all("약처","약처방") %>% 
      str_replace_all("정규","정규진료"),
    
    ((너싱홈_03_2$v5[12] %>% 
            str_remove_all("\r\n") %>% 
            strsplit(split = "\\)"))[[1]][2] %>% 
       strsplit(split = "\\("))[[1]][2] %>% 
      str_remove_all(" "))

의료서비스이용_외래방문_이동수단 <-
  paste0(
    str_sub(
      너싱홈_03_2$v5[13],
      
      ((너싱홈_03_2$v5[13] %>%
              str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      ((너싱홈_03_2$v5[13] %>%
              str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+4) %>% 
      str_replace_all("시설차","시설차량") %>% 
      str_replace_all("개인차","개인차량"),
    
    (너싱홈_03_2$v5[13] %>% 
           strsplit(split = "기"))[[1]][2] %>% 
      str_remove_all("타") %>% 
      str_remove_all("\\(") %>% 
      str_remove_all("\\)") %>% 
      str_remove_all(" "))

의료서비스이용_외래방문_동반_주보호자 <-
  paste0(
    str_sub(
      너싱홈_03_2$v5[13],
      
      ((너싱홈_03_2$v5[13] %>%
              str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+2,
      
      ((너싱홈_03_2$v5[13] %>%
              str_locate_all("■"))[[1]] %>% 
         as.vector())[1]+4) %>% 
      str_replace_all("시설차","시설차량") %>% 
      str_replace_all("개인차","개인차량"),
    
    (너싱홈_03_2$v5[13] %>% 
           strsplit(split = "기"))[[1]][2] %>% 
      str_remove_all("타") %>% 
      str_remove_all("\\(") %>% 
      str_remove_all("\\)") %>% 
      str_remove_all(" "))

너싱홈_03.tmp <- {tibble(
  기관명,
  장기요양인정번호,
  성명,
  생년월일,
  성별,
  날짜,
  장기요양등급,
  욕창위험도평가점수_1분기,
  욕창위험도평가점수_2분기,
  욕창위험도평가점수_3분기,
  욕창위험도평가점수_4분기,
  욕창발생_고위험군_유_12점이하,
  욕창발생_고위험군_무,
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
  배뇨_배설관리_장루관리_교환,
  상처관리_외과적상처_부위,
  상처관리_외과적상처_삼출물,
  상처관리_외과적상처_크기,
  상처관리_외과적상처_주위피부,
  상처관리_외과적상처_감염징후,
  상처관리_외과적상처_드레싱종류,
  상처관리_단순드레싱_부위,
  상처관리_단순드레싱_삼출물,
  상처관리_단순드레싱_크기,
  상처관리_단순드레싱_주위피부,
  상처관리_단순드레싱_감염징후,
  상처관리_단순드레싱_드레싱종류,
  상처관리_복합드레싱_부위,
  상처관리_복합드레싱_삼출물,
  상처관리_복합드레싱_크기,
  상처관리_복합드레싱_주위피부,
  상처관리_복합드레싱_감염징후,
  상처관리_복합드레싱_드레싱종류,
  상처관리_봉합사제거,
  욕창_예방_예방도구,
  욕창_예방_체위변경시간,
  욕창_예방_욕창호발부위_피부상태,
  욕창_관리_부위,
  욕창_관리_삼출물,
  욕창_관리_단계,
  욕창_관리_크기,
  욕창_관리_깊이,
  욕창_관리_드레싱제제,
  당뇨발관리_위치,
  당뇨발관리_크기,
  당뇨발관리_드레싱제제,
  억제대_종류,
  억제대_적용시간,
  억제대_적용시간적용부위_피부상태_확인_시간작성,
  의료서비스이용_가정간호서비스이용_사유,
  의료서비스이용_외래방문_진료과,
  의료서비스이용_외래방문_사유,
  의료서비스이용_외래방문_이동수단,
  의료서비스이용_외래방문_동반_주보호자
)}

너싱홈_03 <- bind_rows(너싱홈_03, 너싱홈_03.tmp)

} %>% tryCatch()

exam_df <- 너싱홈_03

# 저장
write.csv(너싱홈_03, file = "D:/대학원/간호대/기록지/너싱홈그린힐/너싱홈_03월(코딩).csv", row.names=FALSE)

# 추가전처리
rvna <- function(x){
  rv <- gsub("No : ","No",x)
  rv <- gsub("No : NA","No",x)
  rv <- gsub("NANA","No",x)
  return(rv)
}
yn <- function(x){
  rp <- gsub("No","0",x)
  rp <- gsub("Yes","1",x)
  return(rp)
}

너싱홈_03 <- apply(너싱홈_03, MARGIN = 2, FUN = rvna) %>% as_tibble()
너싱홈_03 <- apply(너싱홈_03, MARGIN = 2, FUN = yn) %>% as_tibble()


# 최빈값
get_modemax <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

get_modemin <- function(x) {
  uniqv <- unique(x)
  uniqv[which.min(tabulate(match(x, uniqv)))]
}

# group_by
너싱홈_03_revise <- {너싱홈_03 %>%
  group_by(장기요양인정번호) %>% 
  summarise(기관명_mode = get_modemax(기관명),
장기요양인정번호_mode = get_modemax(장기요양인정번호),
# 성명_mode=getmode(성명),
# 생년월일_mode=getmode(생년월일),
성별_mode = get_modemax(성별),
# 날짜_mode=getmode(날짜),
장기요양등급_mode = get_modemax(장기요양등급),
욕창위험도평가점수_1분기_mean = mean(욕창위험도평가점수_1분기) %>% round(),
욕창위험도평가점수_2분기_mean = mean(욕창위험도평가점수_2분기) %>% round(),
욕창위험도평가점수_3분기_mean = mean(욕창위험도평가점수_3분기) %>% round(),
욕창위험도평가점수_4분기_mean = mean(욕창위험도평가점수_4분기) %>% round(),
욕창발생_고위험군_유_12점이하_mode = getmode_max(욕창발생_고위험군_유_12점이하),
욕창발생_고위험군_무_mode = getmode_max(욕창발생_고위험군_무),
의식_mode = getmode_max(의식),
활력징후_정규_BP_mode = getmode_max(활력징후_정규_BP),
활력징후_정규_PR_mean = mean(활력징후_정규_PR) %>% round(),
활력징후_정규_BT_mean = mean(활력징후_정규_BT) %>% round(),
활력징후_정규_RR_mean = mean(활력징후_정규_RR) %>% round(),
활력징후_추가_BP_mode = getmode_max(활력징후_추가_BP),
활력징후_추가_PR_mean = mean(활력징후_추가_PR) %>% round(),
활력징후_추가_BT_mean = mean(활력징후_추가_BT) %>% round(),
활력징후_추가_RR_mean = mean(활력징후_추가_RR) %>% round(),
                혈당_식전_mode=getmode(혈당_식전),
                혈당_식후2시간_mode=getmode(혈당_식후2시간),
                통증_일반통증_mode=getmode(통증_일반통증),
                통증_암성통증_mode=getmode(통증_암성통증),
                통증_통증양상_mode=getmode(통증_통증양상),
                통증_점수_mode=getmode(통증_점수),
                통증_중재약물_mode=getmode(통증_중재약물),
                영양관리_경관영양_cc1_mode=getmode(영양관리_경관영양_cc1),
                영양관리_경관영양_cc2_mode=getmode(영양관리_경관영양_cc2),
                영양관리_경관영양_cc3_mode=getmode(영양관리_경관영양_cc3),
                영양관리_경관영양_cc4_mode=getmode(영양관리_경관영양_cc4),
                영양관리_위루관영양_cc1_mode=getmode(영양관리_위루관영양_cc1),
                영양관리_위루관영양_cc2_mode=getmode(영양관리_위루관영양_cc2),
                영양관리_위루관영양_cc3_mode=getmode(영양관리_위루관영양_cc3),
                영양관리_위루관영양_cc4_mode=getmode(영양관리_위루관영양_cc4),
                영양관리_중심정액영양_mode=getmode(영양관리_중심정액영양),
                영양관리_구강치료식_mode=getmode(영양관리_구강치료식),
                영양관리_L_tube_사이즈_mode=getmode(영양관리_L_tube_사이즈),
                영양관리_L_tube_고정위치_mode=getmode(영양관리_L_tube_고정위치),
                영양관리_L_tube_드레싱_mode=getmode(영양관리_L_tube_드레싱),
                영양관리_L_tube_교환_mode=getmode(영양관리_L_tube_교환),
                영양관리_Gastrostomy_tube_사이즈_mode=getmode(영양관리_Gastrostomy_tube_사이즈),
                영양관리_Gastrostomy_tube_고정위치_mode=getmode(영양관리_Gastrostomy_tube_고정위치),
                영양관리_Gastrostomy_tube_드레싱_mode=getmode(영양관리_Gastrostomy_tube_드레싱),
                영양관리_Gastrostomy_tube_교환_mode=getmode(영양관리_Gastrostomy_tube_교환),
                호흡관리_산소장치_종류_mode=getmode(호흡관리_산소장치_종류),
                호흡관리_산소용량_mode=getmode(호흡관리_산소용량),
                호흡관리_인공호흡기_Mode_mode=getmode(호흡관리_인공호흡기_Mode),
                호흡관리_인공호흡기_FiO2_mode=getmode(호흡관리_인공호흡기_FiO2),
                호흡관리_인공호흡기_O2_mode=getmode(호흡관리_인공호흡기_O2),
                호흡관리_흡인_mode=getmode(호흡관리_흡인),
                호흡관리_흡인_oral_nasal_mode=getmode(호흡관리_흡인_oral_nasal),
                호흡관리_흡인_intra_tracheal_mode=getmode(호흡관리_흡인_intra_tracheal),
                호흡관리_Nebulizer_종류_mode=getmode(호흡관리_Nebulizer_종류),
                호흡관리_Nebulizer_회_mode=getmode(호흡관리_Nebulizer_회),
                호흡관리_기관지절개관_사이즈_mode=getmode(호흡관리_기관지절개관_사이즈),
                호흡관리_기관지절개관_드레싱_mode=getmode(호흡관리_기관지절개관_드레싱),
                호흡관리_기관지절개관_교환_mode=getmode(호흡관리_기관지절개관_교환),
                배뇨_배설관리_회음부간호_mode=getmode(배뇨_배설관리_회음부간호),
                배뇨_배설관리_방광세척_mode=getmode(배뇨_배설관리_방광세척),
                배뇨_배설관리_단순도뇨_회_mode=getmode(배뇨_배설관리_단순도뇨_회),
                배뇨_배설관리_단순도뇨_총배설량cc_mode=getmode(배뇨_배설관리_단순도뇨_총배설량cc),
                배뇨_배설관리_방광훈련_mode=getmode(배뇨_배설관리_방광훈련),
                배뇨_배설관리_배뇨_회_mode=getmode(배뇨_배설관리_배뇨_회),
                배뇨_배설관리_배뇨_양상_mode=getmode(배뇨_배설관리_배뇨_양상),
                배뇨_배설관리_배변_회_mode=getmode(배뇨_배설관리_배변_회),
                배뇨_배설관리_배변_양상_mode=getmode(배뇨_배설관리_배변_양상),
                배뇨_배설관리_유치도뇨관_사이즈_mode=getmode(배뇨_배설관리_유치도뇨관_사이즈),
                배뇨_배설관리_유치도뇨관_교환_mode=getmode(배뇨_배설관리_유치도뇨관_교환),
                배뇨_배설관리_요루관리_mode=getmode(배뇨_배설관리_요루관리),
                배뇨_배설관리_장루관리_교환_mode=getmode(배뇨_배설관리_장루관리_교환),
                상처관리_외과적상처_부위_mode=getmode(상처관리_외과적상처_부위),
                상처관리_외과적상처_삼출물_mode=getmode(상처관리_외과적상처_삼출물),
                상처관리_외과적상처_크기_mode=getmode(상처관리_외과적상처_크기),
                상처관리_외과적상처_주위피부_mode=getmode(상처관리_외과적상처_주위피부),
                상처관리_외과적상처_감염징후_mode=getmode(상처관리_외과적상처_감염징후),
                상처관리_외과적상처_드레싱종류_mode=getmode(상처관리_외과적상처_드레싱종류),
                상처관리_단순드레싱_부위_mode=getmode(상처관리_단순드레싱_부위),
                상처관리_단순드레싱_삼출물_mode=getmode(상처관리_단순드레싱_삼출물),
                상처관리_단순드레싱_크기_mode=getmode(상처관리_단순드레싱_크기),
                상처관리_단순드레싱_주위피부_mode=getmode(상처관리_단순드레싱_주위피부),
                상처관리_단순드레싱_감염징후_mode=getmode(상처관리_단순드레싱_감염징후),
                상처관리_단순드레싱_드레싱종류_mode=getmode(상처관리_단순드레싱_드레싱종류),
                상처관리_복합드레싱_부위_mode=getmode(상처관리_복합드레싱_부위),
                상처관리_복합드레싱_삼출물_mode=getmode(상처관리_복합드레싱_삼출물),
                상처관리_복합드레싱_크기_mode=getmode(상처관리_복합드레싱_크기),
                상처관리_복합드레싱_주위피부_mode=getmode(상처관리_복합드레싱_주위피부),
                상처관리_복합드레싱_감염징후_mode=getmode(상처관리_복합드레싱_감염징후),
                상처관리_복합드레싱_드레싱종류_mode=getmode(상처관리_복합드레싱_드레싱종류),
                상처관리_봉합사제거_mode=getmode(상처관리_봉합사제거),
                욕창_예방_예방도구_mode=getmode(욕창_예방_예방도구),
                욕창_예방_체위변경시간_mode=getmode(욕창_예방_체위변경시간),
                욕창_예방_욕창호발부위_피부상태_mode=getmode(욕창_예방_욕창호발부위_피부상태),
                욕창_관리_부위_mode=getmode(욕창_관리_부위),
                욕창_관리_삼출물_mode=getmode(욕창_관리_삼출물),
                욕창_관리_단계_mode=getmode(욕창_관리_단계),
                욕창_관리_크기_mode=getmode(욕창_관리_크기),
                욕창_관리_깊이_mode=getmode(욕창_관리_깊이),
                욕창_관리_드레싱제제_mode=getmode(욕창_관리_드레싱제제),
                당뇨발관리_위치_mode=getmode(당뇨발관리_위치),
                당뇨발관리_크기_mode=getmode(당뇨발관리_크기),
                당뇨발관리_드레싱제제_mode=getmode(당뇨발관리_드레싱제제),
                억제대_종류_mode=getmode(억제대_종류),
                억제대_적용시간_mode=getmode(억제대_적용시간),
                억제대_적용시간적용부위_피부상태_확인_시간작성_mode=getmode(억제대_적용시간적용부위_피부상태_확인_시간작성),
                의료서비스이용_가정간호서비스이용_사유_mode=getmode(의료서비스이용_가정간호서비스이용_사유),
                의료서비스이용_외래방문_진료과_mode=getmode(의료서비스이용_외래방문_진료과),
                의료서비스이용_외래방문_사유_mode=getmode(의료서비스이용_외래방문_사유),
                의료서비스이용_외래방문_이동수단_mode=getmode(의료서비스이용_외래방문_이동수단),
                의료서비스이용_외래방문_동반_주보호자_mode=getmode(의료서비스이용_외래방문_동반_주보호자))}

너싱홈_03_mode %>% view()
