
###########


동행지수_비농가_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-3_경기동행지수(비농가취업자수)/raw data/num075-3_경기동행지수(비농가취업자수).xlsx",sheet="Data_비농가취업자수",col_names = FALSE)


동행지수_비농가_raw <- 동행지수_비농가_raw[-c(1:3),c(1,2)]
names(동행지수_비농가_raw) <- c("Date", "Original_Series")

동행지수_비농가 <- 동행지수_비농가_raw
동행지수_비농가 <- na.omit(동행지수_비농가)
동행지수_비농가 <- 동행지수_비농가 %>% as.data.frame()

head(동행지수_비농가)


동행지수_비농가$Date <- 동행지수_비농가$Date %>% as.integer()
동행지수_비농가$Original_Series <- 동행지수_비농가$Original_Series %>% as.integer()



동행지수_비농가 <- 동행지수_비농가 %>% 
  mutate(Year = str_sub(동행지수_비농가$Date,-6,-3))


동행지수_비농가 <- 동행지수_비농가 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_비농가, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-3_경기동행지수(비농가취업자수)/raw data/num075-3_경기동행지수(비농가취업자수)_mean.xlsx", colNames=TRUE)



##########

동행지수_광공업_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-4_경기동행지수(광공업생산지수)/raw data/num075-4_경기동행지수(광공업생산지수).xlsx",sheet="Data_산업생산지수",col_names = FALSE)


동행지수_광공업_raw <- 동행지수_광공업_raw[-c(1:3),c(1,2)]
names(동행지수_광공업_raw) <- c("Date", "Original_Series")

동행지수_광공업 <- 동행지수_광공업_raw
동행지수_광공업 <- na.omit(동행지수_광공업)
동행지수_광공업 <- 동행지수_광공업 %>% as.data.frame()

head(동행지수_광공업)


동행지수_광공업$Date <- 동행지수_광공업$Date %>% as.integer()
동행지수_광공업$Original_Series <- 동행지수_광공업$Original_Series %>% as.integer()



동행지수_광공업 <- 동행지수_광공업 %>% 
  mutate(Year = str_sub(동행지수_광공업$Date,-6,-3))


동행지수_광공업 <- 동행지수_광공업 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_광공업, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-4_경기동행지수(광공업생산지수)/raw data/num075-4_경기동행지수(광공업생산지수)_mean.xlsx", colNames=TRUE)



###############

동행지수_광공업출하_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-5_경기동행지수(광공업출하지수)/raw data/num075-5_경기동행지수(광공업출하지수).xlsx",sheet="Data_생산자출하지수",col_names = FALSE)


동행지수_광공업출하_raw <- 동행지수_광공업출하_raw[-c(1:3),c(1,2)]
names(동행지수_광공업출하_raw) <- c("Date", "Original_Series")

동행지수_광공업출하 <- 동행지수_광공업출하_raw
동행지수_광공업출하 <- na.omit(동행지수_광공업출하)
동행지수_광공업출하 <- 동행지수_광공업출하 %>% as.data.frame()

head(동행지수_광공업출하)


동행지수_광공업출하$Date <- 동행지수_광공업출하$Date %>% as.integer()
동행지수_광공업출하$Original_Series <- 동행지수_광공업출하$Original_Series %>% as.integer()



동행지수_광공업출하 <- 동행지수_광공업출하 %>% 
  mutate(Year = str_sub(동행지수_광공업출하$Date,-6,-3))


동행지수_광공업출하 <- 동행지수_광공업출하 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_광공업출하, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-5_경기동행지수(광공업출하지수)/raw data/num075-5_경기동행지수(광공업출하지수)_mean.xlsx", colNames=TRUE)


###############

동행지수_전력_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-6_경기동행지수(전력판매량)/raw data/num075-6_경기동행지수(전력판매량).xlsx",sheet="Data_전략판매량",col_names = FALSE)


동행지수_전력_raw <- 동행지수_전력_raw[-c(1:3),c(1,2)]
names(동행지수_전력_raw) <- c("Date", "Original_Series")

동행지수_전력 <- 동행지수_전력_raw
동행지수_전력 <- na.omit(동행지수_전력)
동행지수_전력 <- 동행지수_전력 %>% as.data.frame()

head(동행지수_전력)


동행지수_전력$Date <- 동행지수_전력$Date %>% as.integer()
동행지수_전력$Original_Series <- 동행지수_전력$Original_Series %>% as.integer()



동행지수_전력 <- 동행지수_전력 %>% 
  mutate(Year = str_sub(동행지수_전력$Date,-6,-3))


동행지수_전력 <- 동행지수_전력 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_전력, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-6_경기동행지수(전력판매량)/raw data/num075-6_경기동행지수(전력판매량)_mean.xlsx", colNames=TRUE)



###############

동행지수_내수용_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-7_경기동행지수(내수용소비재출하지수-전국)/raw data/num075-7_경기동행지수(내수용소비재출하지수-전국).xlsx",sheet="Data_내수용소비재출하지수전국",col_names = FALSE)


동행지수_내수용_raw <- 동행지수_내수용_raw[-c(1:3),c(1,2)]
names(동행지수_내수용_raw) <- c("Date", "Original_Series")

동행지수_내수용 <- 동행지수_내수용_raw
동행지수_내수용 <- na.omit(동행지수_내수용)
동행지수_내수용 <- 동행지수_내수용 %>% as.data.frame()

head(동행지수_내수용)


동행지수_내수용$Date <- 동행지수_내수용$Date %>% as.integer()
동행지수_내수용$Original_Series <- 동행지수_내수용$Original_Series %>% as.integer()



동행지수_내수용 <- 동행지수_내수용 %>% 
  mutate(Year = str_sub(동행지수_내수용$Date,-6,-3))


동행지수_내수용 <- 동행지수_내수용 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_내수용, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-7_경기동행지수(내수용소비재출하지수-전국)/raw data/num075-7_경기동행지수(내수용소비재출하지수-전국)_mean.xlsx", colNames=TRUE)


###############

동행지수_건축_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-8_경기동행지수(건축착공면적)/raw data/num075-8_경기동행지수(건축착공면적).xlsx",sheet="Data_건축착공면적",col_names = FALSE)


동행지수_건축_raw <- 동행지수_건축_raw[-c(1:3),c(1,2)]
names(동행지수_건축_raw) <- c("Date", "Original_Series")

동행지수_건축 <- 동행지수_건축_raw
동행지수_건축 <- na.omit(동행지수_건축)
동행지수_건축 <- 동행지수_건축 %>% as.data.frame()

head(동행지수_건축)


동행지수_건축$Date <- 동행지수_건축$Date %>% as.integer()
동행지수_건축$Original_Series <- 동행지수_건축$Original_Series %>% as.integer()



동행지수_건축 <- 동행지수_건축 %>% 
  mutate(Year = str_sub(동행지수_건축$Date,-6,-3))


동행지수_건축 <- 동행지수_건축 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_건축, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-8_경기동행지수(건축착공면적)/raw data/num075-8_경기동행지수(건축착공면적)_mean.xlsx", colNames=TRUE)

###############

동행지수_수입액_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-9_경기동행지수(수입액-실질)/raw data/num075-9_경기동행지수(수입액-실질).xlsx",sheet="Data_실질수입액",col_names = FALSE)


동행지수_수입액_raw <- 동행지수_수입액_raw[-c(1:3),c(1,2)]
names(동행지수_수입액_raw) <- c("Date", "Original_Series")

동행지수_수입액 <- 동행지수_수입액_raw
동행지수_수입액 <- na.omit(동행지수_수입액)
동행지수_수입액 <- 동행지수_수입액 %>% as.data.frame()

head(동행지수_수입액)


동행지수_수입액$Date <- 동행지수_수입액$Date %>% as.integer()
동행지수_수입액$Original_Series <- 동행지수_수입액$Original_Series %>% as.integer()



동행지수_수입액 <- 동행지수_수입액 %>% 
  mutate(Year = str_sub(동행지수_수입액$Date,-6,-3))


동행지수_수입액 <- 동행지수_수입액 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_수입액, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-9_경기동행지수(수입액-실질)/raw data/num075-9_경기동행지수(수입액-실질)_mean.xlsx", colNames=TRUE)


###############

동행지수_요구불_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-99_경기동행지수(요구불예금회전율)/raw data/num075-99_경기동행지수(요구불예금회전율).xlsx",sheet="Data_요구불예금회전율",col_names = FALSE)


동행지수_요구불_raw <- 동행지수_요구불_raw[-c(1:3),c(1,2)]
names(동행지수_요구불_raw) <- c("Date", "Original_Series")

동행지수_요구불 <- 동행지수_요구불_raw
동행지수_요구불 <- na.omit(동행지수_요구불)
동행지수_요구불 <- 동행지수_요구불 %>% as.data.frame()

head(동행지수_요구불)


동행지수_요구불$Date <- 동행지수_요구불$Date %>% as.integer()
동행지수_요구불$Original_Series <- 동행지수_요구불$Original_Series %>% as.integer()



동행지수_요구불 <- 동행지수_요구불 %>% 
  mutate(Year = str_sub(동행지수_요구불$Date,-6,-3))


동행지수_요구불 <- 동행지수_요구불 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(동행지수_요구불, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-99_경기동행지수(요구불예금회전율)/raw data/num075-99_경기동행지수(요구불예금회전율)_mean.xlsx", colNames=TRUE)


########


선행지수_종합_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-1_경기선행지수(종합)/raw data/num076-1_경기선행지수(종합).xlsx",sheet="Data_CIofTC",col_names = FALSE)


선행지수_종합_raw <- 선행지수_종합_raw[-c(1:3),c(1,13,17)]
names(선행지수_종합_raw) <- c("Date", "Final_Composite_Index", "Cycle_of_Final_Composite_Index")

선행지수_종합 <- 선행지수_종합_raw
선행지수_종합 <- 선행지수_종합 %>% as.data.frame()



선행지수_종합$Date <- 선행지수_종합$Date %>% as.integer()
선행지수_종합$Final_Composite_Index <- 선행지수_종합$Final_Composite_Index %>% as.integer()
선행지수_종합$Cycle_of_Final_Composite_Index <- 선행지수_종합$Cycle_of_Final_Composite_Index %>% as.integer()


선행지수_종합 <- 선행지수_종합 %>% 
  mutate(Year = str_sub(선행지수_종합$Date,-6,-3))


선행지수_종합 <- 선행지수_종합 %>% 
  group_by(Year) %>% 
  summarise(Final_Composite_Index_mean = mean(Final_Composite_Index, na.rm = TRUE),
            Cycle_of_Final_Composite_Index_mean = mean(Cycle_of_Final_Composite_Index, na.rm = TRUE))



write.xlsx(선행지수_종합, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-1_경기선행지수(종합)/raw data/num076-1_경기선행지수(종합)_mean.xlsx", colNames=TRUE)
write.xlsx(선행지수_종합, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-2_경기선행지수(순환변동치)/raw data/num076-2_경기선행지수(순환변동치)_mean.xlsx", colNames=TRUE)


###############

선행지수_구인배_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-3_경기선행지수(구인배율)/raw data/num076-3_경기선행지수(구인배율).xlsx",sheet="Data_구인배율",col_names = FALSE)


선행지수_구인배_raw <- 선행지수_구인배_raw[-c(1:3),c(1,2)]
names(선행지수_구인배_raw) <- c("Date", "Original_Series")

선행지수_구인배 <- 선행지수_구인배_raw
선행지수_구인배 <- na.omit(선행지수_구인배)
선행지수_구인배 <- 선행지수_구인배 %>% as.data.frame()

head(선행지수_구인배)


선행지수_구인배$Date <- 선행지수_구인배$Date %>% as.integer()
선행지수_구인배$Original_Series <- 선행지수_구인배$Original_Series %>% as.integer()



선행지수_구인배 <- 선행지수_구인배 %>% 
  mutate(Year = str_sub(선행지수_구인배$Date,-6,-3))


선행지수_구인배 <- 선행지수_구인배 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(선행지수_구인배, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-3_경기선행지수(구인배율)/raw data/num076-3_경기선행지수(구인배율)_mean.xlsx", colNames=TRUE)


###############

선행지수_건축_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-4_경기선행지수(건축허가면적)/raw data/num076-4_경기선행지수(건축허가면적).xlsx",sheet="Data_건축허가면적",col_names = FALSE)


선행지수_건축_raw <- 선행지수_건축_raw[-c(1:3),c(1,2)]
names(선행지수_건축_raw) <- c("Date", "Original_Series")

선행지수_건축 <- 선행지수_건축_raw
선행지수_건축 <- na.omit(선행지수_건축)
선행지수_건축 <- 선행지수_건축 %>% as.data.frame()

head(선행지수_건축)


선행지수_건축$Date <- 선행지수_건축$Date %>% as.integer()
선행지수_건축$Original_Series <- 선행지수_건축$Original_Series %>% as.integer()



선행지수_건축 <- 선행지수_건축 %>% 
  mutate(Year = str_sub(선행지수_건축$Date,-6,-3))


선행지수_건축 <- 선행지수_건축 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(선행지수_건축, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-4_경기선행지수(건축허가면적)/raw data/num076-4_경기선행지수(건축허가면적)_mean.xlsx", colNames=TRUE)


###############

선행지수_중간재_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-5_경기선행지수(중간재출하지수)/raw data/num076-5_경기선행지수(중간재출하지수).xlsx",sheet="Data_중간재출하지수",col_names = FALSE)


선행지수_중간재_raw <- 선행지수_중간재_raw[-c(1:3),c(1,2)]
names(선행지수_중간재_raw) <- c("Date", "Original_Series")

선행지수_중간재 <- 선행지수_중간재_raw
선행지수_중간재 <- na.omit(선행지수_중간재)
선행지수_중간재 <- 선행지수_중간재 %>% as.data.frame()

head(선행지수_중간재)


선행지수_중간재$Date <- 선행지수_중간재$Date %>% as.integer()
선행지수_중간재$Original_Series <- 선행지수_중간재$Original_Series %>% as.integer()



선행지수_중간재 <- 선행지수_중간재 %>% 
  mutate(Year = str_sub(선행지수_중간재$Date,-6,-3))


선행지수_중간재 <- 선행지수_중간재 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(선행지수_중간재, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-5_경기선행지수(중간재출하지수)/raw data/num076-5_경기선행지수(중간재출하지수)_mean.xlsx", colNames=TRUE)


###############

선행지수_예금_raw <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-6_경기선행지수(예금은행재출금)/raw data/num076-6_경기선행지수(예금은행재출금).xlsx",sheet="Data_예금은행대출금",col_names = FALSE)


선행지수_예금_raw <- 선행지수_예금_raw[-c(1:3),c(1,2)]
names(선행지수_예금_raw) <- c("Date", "Original_Series")

선행지수_예금 <- 선행지수_예금_raw
선행지수_예금 <- na.omit(선행지수_예금)
선행지수_예금 <- 선행지수_예금 %>% as.data.frame()

head(선행지수_예금)


선행지수_예금$Date <- 선행지수_예금$Date %>% as.integer()
선행지수_예금$Original_Series <- 선행지수_예금$Original_Series %>% as.integer()



선행지수_예금 <- 선행지수_예금 %>% 
  mutate(Year = str_sub(선행지수_예금$Date,-6,-3))


선행지수_예금 <- 선행지수_예금 %>% 
  group_by(Year) %>% 
  summarise(Original_Series = mean(Original_Series, na.rm = TRUE))



write.xlsx(선행지수_예금, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num076-6_경기선행지수(예금은행재출금)/raw data/num076-6_경기선행지수(예금은행재출금)_mean.xlsx", colNames=TRUE)

#####

경기종합_전국_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전국.xlsx",sheet="데이터",col_names = FALSE) 


경기종합_전국_raw <- 경기종합_전국_raw[-1,-c(2,3)]


경기종합_전국_names <- 경기종합_전국_raw[1,]
경기종합_전국_names1 <- 경기종합_전국_names %>% as.data.frame() %>% as.character()
names(경기종합_전국_raw) <- 경기종합_전국_names1
경기종합_전국_raw <- 경기종합_전국_raw[-1,]


경기종합_전국_melt <- melt(경기종합_전국_raw,id=c("지수별")) 
names(경기종합_전국_melt) <- c("구성지표", "variable","value")


경기종합_전국_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_전국_melt$구성지표) 
경기종합_전국_melt$구성지표 <- gsub(" ","",경기종합_전국_melt$구성지표) 


경기종합_전국_melt$구성지표 <- gsub("동행지수순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_전국_melt$구성지표) 
경기종합_전국_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_전국_melt$구성지표) 
경기종합_전국_melt$구성지표 <- gsub("광공업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_전국_melt$구성지표) 
경기종합_전국_melt$구성지표 <- gsub("내수출하지수","동행_내수용소비재출하지수전국(전월비)(%)",경기종합_전국_melt$구성지표) 
경기종합_전국_melt$구성지표 <- gsub("수입액","동행_실질수입액(전월비)(%)",경기종합_전국_melt$구성지표) 
경기종합_전국_melt$구성지표 <- gsub("비농림어업취업자수","동행_비농가취업자수",경기종합_전국_melt$구성지표) 

경기종합_전국_melt$구성지표 <- gsub("선행지수순환변동치","선행_Cycle_of_Final_Composite_Index",경기종합_전국_melt$구성지표) 
경기종합_전국_melt$구성지표 <- gsub("선행종합지수","선행_Final_Composite_Index",경기종합_전국_melt$구성지표) 



경기종합_전국_melt <- 경기종합_전국_melt %>% 
  mutate(Year = str_sub(경기종합_전국_melt$variable,1,4))


경기종합_전국_melt$value <- 경기종합_전국_melt$value %>% as.integer()
경기종합_전국_melt$Year <- 경기종합_전국_melt$Year %>% as.integer()


경기종합_전국_mean <- 경기종합_전국_melt %>% 
  group_by(Year,구성지표) %>% 
  summarise(value = mean(value,na.rm = FALSE))


경기종합_전국_mean <- 경기종합_전국_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_전국_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전국_mean.xlsx", colNames=TRUE)

경기종합_전국_mean_0818 <- 경기종합_전국_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_전국_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전국(0818)_mean.xlsx", colNames=TRUE)



#########

경기종합_부산_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_부산.xlsx",sheet="데이터",col_names = TRUE) 

경기종합_부산_melt <- melt(경기종합_부산_raw,id=c("항목")) 
names(경기종합_부산_melt) <- c("구성지표", "variable","value")

경기종합_부산_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_부산_melt$구성지표) 
경기종합_부산_melt$구성지표 <- gsub("선행종합지수","선행_Final_Composite_Index",경기종합_부산_melt$구성지표) 

경기종합_부산_melt <- 경기종합_부산_melt %>% 
  mutate(Year = str_sub(경기종합_부산_melt$variable,1,4))


경기종합_부산_melt$value <- 경기종합_부산_melt$value %>% as.integer()
경기종합_부산_melt$Year <- 경기종합_부산_melt$Year %>% as.integer()


경기종합_부산_mean <- 경기종합_부산_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))


write.xlsx(경기종합_부산_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_부산_mean.xlsx", colNames=TRUE)

경기종합_부산_mean_0818 <- 경기종합_부산_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_부산_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_부산(0818)_mean.xlsx", colNames=TRUE)



#########

경기종합_대구_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_대구.xlsx",sheet="데이터",col_names = TRUE) 


경기종합_대구_raw <- 경기종합_대구_raw[,-2]

경기종합_대구_melt <- melt(경기종합_대구_raw,id=c("지수별(1)")) 
names(경기종합_대구_melt) <- c("구성지표", "variable","value")


경기종합_대구_melt$구성지표 <- gsub(" ","",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_대구_melt$구성지표) 


경기종합_대구_melt$구성지표 <- gsub("동행종합지수순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("비농가취업자수","동행_비농가취업자수(전월비)(%)",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("산업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("생산자출하지수","동행_광공업출하지수(전월비)(%)",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("실질수입액","동행_실질수입액(전월비)(%)",경기종합_대구_melt$구성지표) 

경기종합_대구_melt$구성지표 <- gsub("선행종합지수순환변동치","선행_Cycle_of_Final_Composite_Index",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("선행종합지수","선행_Final_Composite_Index",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("건축허가면적","선행_건축허가면적(전월비)(%)",경기종합_대구_melt$구성지표) 
경기종합_대구_melt$구성지표 <- gsub("중간재출하지수","선행_중간재출하지수(전월비)(%)",경기종합_대구_melt$구성지표) 


경기종합_대구_melt <- 경기종합_대구_melt %>% 
  mutate(Year = str_sub(경기종합_대구_melt$variable,1,4))


경기종합_대구_melt$value <- 경기종합_대구_melt$value %>% as.integer()
경기종합_대구_melt$Year <- 경기종합_대구_melt$Year %>% as.integer()


경기종합_대구_mean <- 경기종합_대구_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_대구_mean <- 경기종합_대구_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_대구_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_대구_mean.xlsx", colNames=TRUE)

경기종합_대구_mean_0818 <- 경기종합_대구_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_대구_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_대구(0818)_mean.xlsx", colNames=TRUE)



#########

경기종합_인천_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_인천.xlsx",sheet="데이터",col_names = TRUE) 


경기종합_인천_melt <- melt(경기종합_인천_raw,id=c("항목")) 
names(경기종합_인천_melt) <- c("구성지표", "variable","value")


경기종합_인천_melt$구성지표 <- gsub(" ","",경기종합_인천_melt$구성지표) 
경기종합_인천_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_인천_melt$구성지표) 

경기종합_인천_melt <- 경기종합_인천_melt %>% filter(구성지표 %in% c(
  "동행종합지수순환변동치",
  "동행종합지수",
  "선행종합지수순환변동치",
  "선행종합지수"
))

경기종합_인천_melt$구성지표 <- gsub("동행종합지수순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_인천_melt$구성지표) 
경기종합_인천_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_인천_melt$구성지표) 

경기종합_인천_melt$구성지표 <- gsub("선행종합지수순환변동치","선행_Cycle_of_Final_Composite_Index",경기종합_인천_melt$구성지표) 
경기종합_인천_melt$구성지표 <- gsub("선행종합지수","선행_Final_Composite_Index",경기종합_인천_melt$구성지표) 



경기종합_인천_melt <- 경기종합_인천_melt %>% 
  mutate(Year = str_sub(경기종합_인천_melt$variable,1,4))


경기종합_인천_melt$value <- 경기종합_인천_melt$value %>% as.integer()
경기종합_인천_melt$Year <- 경기종합_인천_melt$Year %>% as.integer()


경기종합_인천_mean <- 경기종합_인천_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_인천_mean <- 경기종합_인천_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_인천_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_인천_mean.xlsx", colNames=TRUE)

경기종합_인천_mean_0818 <- 경기종합_인천_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_인천_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_인천(0818)_mean.xlsx", colNames=TRUE)


#######

경기종합_광주1_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_광주1.xlsx",sheet="데이터",col_names = TRUE)
경기종합_광주2_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_광주2.xlsx",sheet="데이터",col_names = TRUE)
경기종합_광주3_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_광주3.xlsx",sheet="데이터",col_names = TRUE)
경기종합_광주4_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_광주4.xlsx",sheet="데이터",col_names = TRUE)

경기종합_광주1_melt <- melt(경기종합_광주1_raw,id=c("지수현황별")) 
경기종합_광주2_melt <- melt(경기종합_광주2_raw,id=c("지수현황별"))
경기종합_광주3_melt <- melt(경기종합_광주3_raw,id=c("구성지표별")) 
경기종합_광주4_melt <- melt(경기종합_광주4_raw,id=c("구성지표별")) 

names(경기종합_광주1_melt) <- c("구성지표", "variable","value")
names(경기종합_광주2_melt) <- c("구성지표", "variable","value")
names(경기종합_광주3_melt) <- c("구성지표", "variable","value")
names(경기종합_광주4_melt) <- c("구성지표", "variable","value")

경기종합_광주_melt <- rbind(경기종합_광주1_melt,경기종합_광주2_melt,경기종합_광주3_melt,경기종합_광주4_melt)


경기종합_광주_melt$구성지표 <- gsub(" ","",경기종합_광주_melt$구성지표) 
경기종합_광주_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_광주_melt$구성지표) 


경기종합_광주_melt$구성지표 <- gsub("순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_광주_melt$구성지표) 
경기종합_광주_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_광주_melt$구성지표) 

경기종합_광주_melt$구성지표 <- gsub("광공업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_광주_melt$구성지표) 
경기종합_광주_melt$구성지표 <- gsub("중간재출하지수","선행_중간재출하지수(전월비)(%)",경기종합_광주_melt$구성지표) 
경기종합_광주_melt$구성지표 <- gsub("비농림어업취업자수","동행_비농가취업자수(전월비)(%)",경기종합_광주_melt$구성지표) 



경기종합_광주_melt <- 경기종합_광주_melt %>% 
  mutate(Year = str_sub(경기종합_광주_melt$variable,1,4))


경기종합_광주_melt$value <- 경기종합_광주_melt$value %>% as.integer()
경기종합_광주_melt$Year <- 경기종합_광주_melt$Year %>% as.integer()


경기종합_광주_mean <- 경기종합_광주_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_광주_mean <- 경기종합_광주_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_광주_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_광주_mean.xlsx", colNames=TRUE)

경기종합_광주_mean_0818 <- 경기종합_광주_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_광주_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_광주(0818)_mean.xlsx", colNames=TRUE)


##########

경기종합_대전1_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_대전1.xlsx",sheet="데이터",col_names = TRUE)
경기종합_대전2_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_대전2.xlsx",sheet="데이터",col_names = TRUE)


경기종합_대전1_melt <- melt(경기종합_대전1_raw,id=c("경기종합지수별")) 
경기종합_대전2_melt <- melt(경기종합_대전2_raw,id=c("경기종합지수별")) 

names(경기종합_대전1_melt) <- c("구성지표","variable","value")
names(경기종합_대전2_melt) <- c("구성지표","variable","value")

경기종합_대전_melt <- rbind(경기종합_대전1_melt,경기종합_대전2_melt)

경기종합_대전_melt$구성지표 <- gsub(" ","",경기종합_대전_melt$구성지표) 
경기종합_대전_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_대전_melt$구성지표) 


경기종합_대전_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_대전_melt$구성지표) 
경기종합_대전_melt$구성지표 <- gsub("비농가취업자수","동행_비농가취업자수(전월비)(%)",경기종합_대전_melt$구성지표) 
경기종합_대전_melt$구성지표 <- gsub("산업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_대전_melt$구성지표) 
경기종합_대전_melt$구성지표 <- gsub("생산자출하지수","동행_광공업출하지수(전월비)(%)",경기종합_대전_melt$구성지표) 
경기종합_대전_melt$구성지표 <- gsub("전력사용량","동행_전력판매량(전월비)(%)",경기종합_대전_melt$구성지표) 
경기종합_대전_melt$구성지표 <- gsub("수입액","동행_실질수입액(전월비)(%)",경기종합_대전_melt$구성지표) 

경기종합_대전_melt$구성지표 <- gsub("선행종합지수","선행_Final_Composite_Index",경기종합_대전_melt$구성지표) 


경기종합_대전_melt <- 경기종합_대전_melt %>% 
  mutate(Year = str_sub(경기종합_대전_melt$variable,1,4))


경기종합_대전_melt$value <- 경기종합_대전_melt$value %>% as.integer()
경기종합_대전_melt$Year <- 경기종합_대전_melt$Year %>% as.integer()


경기종합_대전_mean <- 경기종합_대전_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_대전_mean <- 경기종합_대전_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_대전_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_대전_mean.xlsx", colNames=TRUE)

경기종합_대전_mean_0818 <- 경기종합_대전_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_대전_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_대전(0818)_mean.xlsx", colNames=TRUE)


#####

경기종합_울산_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_울산.xlsx",sheet="데이터",col_names = TRUE) 


경기종합_울산_melt <- melt(경기종합_울산_raw,id=c("항목")) 
names(경기종합_울산_melt) <- c("구성지표", "variable","value")

경기종합_울산_melt$구성지표 <- gsub(" ","",경기종합_울산_melt$구성지표) 
경기종합_울산_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_울산_melt$구성지표) 


경기종합_울산_melt$구성지표 <- gsub("순환변동치","선행_Cycle_of_Final_Composite_Index",경기종합_울산_melt$구성지표) 
경기종합_울산_melt$구성지표 <- gsub("경기동행지수","동행_Final_Composite_Index",경기종합_울산_melt$구성지표) 


경기종합_울산_melt <- 경기종합_울산_melt %>% 
  mutate(Year = str_sub(경기종합_울산_melt$variable,1,4))

경기종합_울산_melt$value <- 경기종합_울산_melt$value %>% as.integer()
경기종합_울산_melt$Year <- 경기종합_울산_melt$Year %>% as.integer()

경기종합_울산_mean <- 경기종합_울산_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_울산_mean <- 경기종합_울산_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_울산_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_울산_mean.xlsx", colNames=TRUE)

경기종합_울산_mean_0818 <- 경기종합_울산_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_울산_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_울산(0818)_mean.xlsx", colNames=TRUE)


#######

경기종합_강원_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_강원.xlsx",sheet="데이터",col_names = TRUE)

경기종합_강원_raw <- 경기종합_강원_raw[,-1]

경기종합_강원_melt <- melt(경기종합_강원_raw,id=c("지수및구성지표별(2)")) 
names(경기종합_강원_melt) <- c("구성지표", "variable","value")

경기종합_강원_melt$구성지표 <- gsub(" ","",경기종합_강원_melt$구성지표) 
경기종합_강원_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_강원_melt$구성지표) 


경기종합_강원_melt$구성지표 <- gsub("동행지수순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_강원_melt$구성지표) 
경기종합_강원_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_강원_melt$구성지표) 
경기종합_강원_melt$구성지표 <- gsub("비농림어업취업자수","동행_비농가취업자수(전월비)(%)",경기종합_강원_melt$구성지표) 
경기종합_강원_melt$구성지표 <- gsub("광공업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_강원_melt$구성지표) 
경기종합_강원_melt$구성지표 <- gsub("생산자제품출하지수","동행_광공업출하지수(전월비)(%)",경기종합_강원_melt$구성지표) 
경기종합_강원_melt$구성지표 <- gsub("전력판매량","동행_전력판매량(전월비)(%)",경기종합_강원_melt$구성지표) 


경기종합_강원_melt <- 경기종합_강원_melt %>% 
  mutate(Year = str_sub(경기종합_강원_melt$variable,1,4))


경기종합_강원_melt$value <- 경기종합_강원_melt$value %>% as.integer()
경기종합_강원_melt$Year <- 경기종합_강원_melt$Year %>% as.integer()


경기종합_강원_mean <- 경기종합_강원_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_강원_mean <- 경기종합_강원_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_강원_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_강원_mean.xlsx", colNames=TRUE)

경기종합_강원_mean_0818 <- 경기종합_강원_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_강원_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_강원(0818)_mean.xlsx", colNames=TRUE)



#######

경기종합_충북1_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충북1.xlsx",sheet="데이터",col_names = TRUE)
경기종합_충북2_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충북2.xlsx",sheet="데이터",col_names = TRUE)

경기종합_충북1_raw <- 경기종합_충북1_raw[,-1]
경기종합_충북2_raw <- 경기종합_충북2_raw[,-1]

경기종합_충북1_melt <- melt(경기종합_충북1_raw,id=c("구성지표별(2)")) 
경기종합_충북2_melt <- melt(경기종합_충북2_raw,id=c("지수별(2)")) 

names(경기종합_충북1_melt) <- c("구성지표", "variable","value")
names(경기종합_충북2_melt) <- c("구성지표", "variable","value")

경기종합_충북_melt <- rbind(경기종합_충북1_melt,경기종합_충북2_melt)

경기종합_충북_melt$구성지표 <- gsub(" ","",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_충북_melt$구성지표) 


경기종합_충북_melt$구성지표 <- gsub("선행종합지수순환변동치","선행_Cycle_of_Final_Composite_Index",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("동행종합지수순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_충북_melt$구성지표) 

경기종합_충북_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("비농가취업자수","동행_비농가취업자수(전월비)(%)",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("광공업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("전력판매량","동행_전력판매량(전월비)(%)",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("수입액","동행_실질수입액(전월비)(%)",경기종합_충북_melt$구성지표) 

경기종합_충북_melt$구성지표 <- gsub("선행종합지수","선행_Final_Composite_Index",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("건축허가면적","선행_건축허가면적(전월비)(%)",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("구인구직비율","선행_구인배율(전월비)(%)",경기종합_충북_melt$구성지표) 
경기종합_충북_melt$구성지표 <- gsub("예금은행대출금","선행_예금은행대출금(전월비)(%)",경기종합_충북_melt$구성지표) 


경기종합_충북_melt <- 경기종합_충북_melt %>% 
  mutate(Year = str_sub(경기종합_충북_melt$variable,1,4))


경기종합_충북_melt$value <- 경기종합_충북_melt$value %>% as.integer()
경기종합_충북_melt$Year <- 경기종합_충북_melt$Year %>% as.integer()


경기종합_충북_mean <- 경기종합_충북_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_충북_mean <- 경기종합_충북_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_충북_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충북_mean.xlsx", colNames=TRUE)

경기종합_충북_mean_0818 <- 경기종합_충북_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_충북_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충북(0818)_mean.xlsx", colNames=TRUE)


######
경기종합_충남1_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충남1.xlsx",sheet="데이터",col_names = TRUE)
경기종합_충남2_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충남2.xlsx",sheet="데이터",col_names = TRUE)

경기종합_충남1_raw <- 경기종합_충남1_raw[,-c(1:2)]
경기종합_충남2_raw <- 경기종합_충남2_raw[,-c(1:2)]

경기종합_충남1_melt <- melt(경기종합_충남1_raw,id=c("지수별")) 
경기종합_충남2_melt <- melt(경기종합_충남2_raw,id=c("지수별"))

names(경기종합_충남1_melt) <- c("구성지표", "variable","value")
names(경기종합_충남2_melt) <- c("구성지표", "variable","value")


경기종합_충남_melt <- rbind(경기종합_충남1_melt,경기종합_충남2_melt)


경기종합_충남_melt$구성지표 <- gsub(" ","",경기종합_충남_melt$구성지표) 
경기종합_충남_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_충남_melt$구성지표) 


경기종합_충남_melt$구성지표 <- gsub("동행종합지수소계","동행_Final_Composite_Index",경기종합_충남_melt$구성지표) 
경기종합_충남_melt$구성지표 <- gsub("동행지수순환변동치소계","동행_Cycle_of_Final_Composite_Index",경기종합_충남_melt$구성지표) 
경기종합_충남_melt$구성지표 <- gsub("선행종합지수소계","선행_Final_Composite_Index",경기종합_충남_melt$구성지표) 
경기종합_충남_melt$구성지표 <- gsub("선행지수순환변동치소계","선행_Cycle_of_Final_Composite_Index",경기종합_충남_melt$구성지표) 

경기종합_충남_melt$구성지표 <- gsub("동행종합지수비농가취업자수","동행_비농가취업자수(전월비)(%)",경기종합_충남_melt$구성지표) 
경기종합_충남_melt$구성지표 <- gsub("동행종합지수산업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_충남_melt$구성지표) 
경기종합_충남_melt$구성지표 <- gsub("동행종합지수전력판매량","동행_전력판매량(전월비)(%)",경기종합_충남_melt$구성지표) 
경기종합_충남_melt$구성지표 <- gsub("동행종합지수수입액","동행_실질수입액(전월비)(%)",경기종합_충남_melt$구성지표) 



경기종합_충남_melt <- 경기종합_충남_melt %>% 
  mutate(Year = str_sub(경기종합_충남_melt$variable,1,4))


경기종합_충남_melt$value <- 경기종합_충남_melt$value %>% as.integer()
경기종합_충남_melt$Year <- 경기종합_충남_melt$Year %>% as.integer()


경기종합_충남_mean <- 경기종합_충남_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_충남_mean <- 경기종합_충남_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_충남_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충남_mean.xlsx", colNames=TRUE)

경기종합_충남_mean_0818 <- 경기종합_충남_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_충남_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_충남(0818)_mean.xlsx", colNames=TRUE)


######

경기종합_전북_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전북.xlsx",sheet="데이터",col_names = TRUE)

경기종합_전북_melt <- melt(경기종합_전북_raw,id=c("항목")) 
names(경기종합_전북_melt) <- c("구성지표", "variable","value")

경기종합_전북_melt$구성지표 <- gsub(" ","",경기종합_전북_melt$구성지표) 
경기종합_전북_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_전북_melt$구성지표) 

경기종합_전북_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_전북_melt$구성지표) 
경기종합_전북_melt$구성지표 <- gsub("동행지수순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_전북_melt$구성지표) 
경기종합_전북_melt$구성지표 <- gsub("전북비농림어업취업자수","동행_비농가취업자수(전월비)(%)",경기종합_전북_melt$구성지표)
경기종합_전북_melt$구성지표 <- gsub("전북광공업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_전북_melt$구성지표) 
경기종합_전북_melt$구성지표 <- gsub("전북지역건축착공면적","동행_건축착공면적(전월비)(%)",경기종합_전북_melt$구성지표) 
경기종합_전북_melt$구성지표 <- gsub("전북수입액","동행_실질수입액(전월비)(%)",경기종합_전북_melt$구성지표) 

경기종합_전북_melt <- 경기종합_전북_melt %>% 
  mutate(Year = str_sub(경기종합_전북_melt$variable,1,4))


경기종합_전북_melt$value <- 경기종합_전북_melt$value %>% as.integer()
경기종합_전북_melt$Year <- 경기종합_전북_melt$Year %>% as.integer()


경기종합_전북_mean <- 경기종합_전북_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_전북_mean <- 경기종합_전북_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_전북_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전북_mean.xlsx", colNames=TRUE)

경기종합_전북_mean_0818 <- 경기종합_전북_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_전북_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전북(0818)_mean.xlsx", colNames=TRUE)


######

경기종합_전남1_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전남1.xlsx",sheet="데이터",col_names = TRUE)
경기종합_전남2_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전남2.xlsx",sheet="데이터",col_names = TRUE)

경기종합_전남1_raw <- 경기종합_전남1_raw[,-c(1:2)]

경기종합_전남1_melt <- melt(경기종합_전남1_raw,id=c("항목")) 
경기종합_전남2_melt <- melt(경기종합_전남2_raw,id=c("시점")) 
names(경기종합_전남1_melt) <- c("구성지표", "variable","value")
names(경기종합_전남2_melt) <- c("구성지표", "variable","value")

경기종합_전남_melt <- rbind(경기종합_전남1_melt,경기종합_전남2_melt)


경기종합_전남_melt$구성지표 <- gsub(" ","",경기종합_전남_melt$구성지표) 
경기종합_전남_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_전남_melt$구성지표) 

경기종합_전남_melt$구성지표 <- gsub("동행종합지수소계","동행_Final_Composite_Index",경기종합_전남_melt$구성지표) 
경기종합_전남_melt$구성지표 <- gsub("순환변동치소계","동행_Cycle_of_Final_Composite_Index",경기종합_전남_melt$구성지표) 
경기종합_전남_melt$구성지표 <- gsub("전력사용량","동행_광공업생산지수(전월비)(%)",경기종합_전남_melt$구성지표)
경기종합_전남_melt$구성지표 <- gsub("비농림어업취업자수","동행_비농가취업자수(전월비)(%)",경기종합_전남_melt$구성지표) 
경기종합_전남_melt$구성지표 <- gsub("실질수입","동행_실질수입액(전월비)(%)",경기종합_전남_melt$구성지표) 

경기종합_전남_melt <- 경기종합_전남_melt %>% 
  mutate(Year = str_sub(경기종합_전남_melt$variable,1,4))


경기종합_전남_melt$value <- 경기종합_전남_melt$value %>% as.integer()
경기종합_전남_melt$Year <- 경기종합_전남_melt$Year %>% as.integer()


경기종합_전남_mean <- 경기종합_전남_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_전남_mean <- 경기종합_전남_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_전남_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전남_mean.xlsx", colNames=TRUE)

경기종합_전남_mean_0818 <- 경기종합_전남_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_전남_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_전남(0818)_mean.xlsx", colNames=TRUE)


######

경기종합_경북_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_경북.xlsx",sheet="데이터",col_names = TRUE)

경기종합_경북_raw <- 경기종합_경북_raw[,-1]

경기종합_경북_melt <- melt(경기종합_경북_raw,id=c("항목")) 
names(경기종합_경북_melt) <- c("구성지표", "variable","value")

경기종합_경북_melt$구성지표 <- gsub(" ","",경기종합_경북_melt$구성지표) 
경기종합_경북_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_경북_melt$구성지표) 

경기종합_경북_melt$구성지표 <- gsub("동행종합지수","동행_Final_Composite_Index",경기종합_경북_melt$구성지표) 
경기종합_경북_melt$구성지표 <- gsub("순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_경북_melt$구성지표) 

경기종합_경북_melt <- 경기종합_경북_melt %>% 
  mutate(Year = str_sub(경기종합_경북_melt$variable,1,4))


경기종합_경북_melt$value <- 경기종합_경북_melt$value %>% as.integer()
경기종합_경북_melt$Year <- 경기종합_경북_melt$Year %>% as.integer()


경기종합_경북_mean <- 경기종합_경북_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_경북_mean <- 경기종합_경북_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_경북_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_경북_mean.xlsx", colNames=TRUE)

경기종합_경북_mean_0818 <- 경기종합_경북_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_경북_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_경북(0818)_mean.xlsx", colNames=TRUE)


######

경기종합_제주1_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_제주1.xlsx",sheet="데이터",col_names = TRUE)
경기종합_제주2_raw <-read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_제주2.xlsx",sheet="데이터",col_names = FALSE)

경기종합_제주1_raw <- 경기종합_제주1_raw[,-c(1,2)]
경기종합_제주2_raw <- 경기종합_제주2_raw[-1,-c(2,4)]
경기종합_제주2_raw_a <- 경기종합_제주2_raw[1,] %>% as.character()

names(경기종합_제주2_raw) <- 경기종합_제주2_raw_a
경기종합_제주2_raw <- 경기종합_제주2_raw[-1,]


경기종합_제주1_melt <- melt(경기종합_제주1_raw,id=c("항목"))
경기종합_제주2_melt <- melt(경기종합_제주2_raw,id=c("지수별")) 
names(경기종합_제주1_melt) <- c("구성지표", "variable","value")
names(경기종합_제주2_melt) <- c("구성지표", "variable","value")

경기종합_제주_melt <- rbind(경기종합_제주1_melt,경기종합_제주2_melt)

경기종합_제주_melt$구성지표 <- gsub(" ","",경기종합_제주_melt$구성지표) 
경기종합_제주_melt$구성지표 <- gsub("\\s*\\([^\\)]+\\)","",경기종합_제주_melt$구성지표) 

경기종합_제주_melt$구성지표 <- gsub("선행종합지수소계","선행_Final_Composite_Index",경기종합_제주_melt$구성지표) 
경기종합_제주_melt$구성지표 <- gsub("동행종합지수소계","동행_Final_Composite_Index",경기종합_제주_melt$구성지표) 
경기종합_제주_melt$구성지표 <- gsub("동행종합지수광공업생산지수","동행_광공업생산지수(전월비)(%)",경기종합_제주_melt$구성지표) 
경기종합_제주_melt$구성지표 <- gsub("선행지수순환변동치","선행_Cycle_of_Final_Composite_Index",경기종합_제주_melt$구성지표) 
경기종합_제주_melt$구성지표 <- gsub("비농가취업자수","동행_비농가취업자수(전월비)(%)",경기종합_제주_melt$구성지표) 
경기종합_제주_melt$구성지표 <- gsub("전력판매량","동행_전력판매량(전월비)(%)",경기종합_제주_melt$구성지표) 
경기종합_제주_melt$구성지표 <- gsub("동행지수순환변동치","동행_Cycle_of_Final_Composite_Index",경기종합_제주_melt$구성지표) 

경기종합_제주_melt <- 경기종합_제주_melt %>% 
  mutate(Year = str_sub(경기종합_제주_melt$variable,1,4))


경기종합_제주_melt$value <- 경기종합_제주_melt$value %>% as.integer()
경기종합_제주_melt$Year <- 경기종합_제주_melt$Year %>% as.integer()


경기종합_제주_mean <- 경기종합_제주_melt %>% 
  group_by(구성지표,Year) %>% 
  summarise(value = mean(value,na.rm = FALSE))

경기종합_제주_mean <- 경기종합_제주_mean %>% filter(구성지표 %in% 
                                              c("동행_Final_Composite_Index",
                                                "동행_Cycle_of_Final_Composite_Index",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "동행_광공업생산지수(전월비)(%)",
                                                "동행_광공업출하지수(전월비)(%)",
                                                "동행_전력판매량(전월비)(%)",
                                                "동행_내수용소비재출하지수전국(전월비)(%)",
                                                "동행_건축착공면적(전월비)(%)",
                                                "동행_실질수입액(전월비)(%)",
                                                "동행_요구불예금회전율(전월비)(%)",
                                                "동행_비농가취업자수(전월비)(%)",
                                                "선행_Final_Composite_Index",
                                                "선행_Cycle_of_Final_Composite_Index",
                                                "선행_구인배율(전월비)(%)",
                                                "선행_건축허가면적(전월비)(%)",
                                                "선행_중간재출하지수(전월비)(%)",
                                                "선행_예금은행대출금(전월비)(%)"
                                              ))

write.xlsx(경기종합_제주_mean, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_제주_mean.xlsx", colNames=TRUE)

경기종합_제주_mean_0818 <- 경기종합_제주_mean %>% filter(Year %in% c(2008:2018))

write.xlsx(경기종합_제주_mean_0818, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/class009_경제산업/num075-1_경기동행지수(종합)/raw data/경기종합지수_제주(0818)_mean.xlsx", colNames=TRUE)

