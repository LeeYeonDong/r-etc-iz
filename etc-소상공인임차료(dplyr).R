
소상공인 <- read_excel("D:/소상공인/소상공인_임차료.xlsx", sheet = 2,col_names = TRUE)




제조업 <- freq(소상공인$업종, plot=TRUE) 
비제조업 <- freq(소상공인$sq4_2, plot=TRUE)%>% as.data.frame()
사업장임차형태 <- freq(소상공인$sq6, plot=TRUE)%>% as.data.frame()
전세_년 <- freq(소상공인$sq6_1_1, plot=TRUE)%>% as.data.frame()
전세_만원 <- freq(소상공인$sq6_1_2, plot=TRUE)%>% as.data.frame()
보증부월세_계약기간 <- freq(소상공인$sq6_2_1, plot=TRUE)%>% as.data.frame()
보증부월세_보증금 <- freq(소상공인$sq6_2_2, plot=TRUE)%>% as.data.frame()
보증부월세_월만원 <- freq(소상공인$sq6_2_3, plot=TRUE)%>% as.data.frame()
월세_계약기간 <- freq(소상공인$sq6_3_1, plot=TRUE)%>% as.data.frame()
월세_만원 <- freq(소상공인$sq6_3_2, plot=TRUE)%>% as.data.frame()
기타 <- freq(소상공인$sq6_4, plot=TRUE)%>% as.data.frame()


소상공인list <- list(제조업,비제조업,사업장임차형태,전세_년,전세_만원,
                    보증부월세_계약기간,보증부월세_보증금,보증부월세_월만원,
                    월세_계약기간,월세_만원,기타)



for(i in 1:length(소상공인list)){    
  write.xlsx(소상공인list, file = "D:/소상공인/소상공인46빈도d.xlsx", sheetName=i,colNames=FALSE)
}


비제조업 <- count(소상공인,"업종")
names(제조업) <- c("비제조업","빈도수")

제조업 <- count(소상공인,"sq4_2")
names(제조업) <- c("제조업","빈도수")

사업장임차형태 <- count(소상공인,"sq6")
names(사업장임차형태) <- c("사업장임차형태업","빈도수")

전세_년 <- count(소상공인,"sq6_1_1")
names(전세_년) <- c("전세_년","빈도수")

전세_만원 <- count(소상공인,"sq6_1_2")
names(전세_만원) <- c("전세_만원","빈도수")

보증부월세_계약기간 <- count(소상공인,"sq6_2_1")
names(보증부월세_계약기간) <- c("보증부월세_계약기간","빈도수")

보증부월세_보증금 <- count(소상공인,"sq6_2_2")
names(보증부월세_보증금) <- c("보증부월세_보증금","빈도수")

보증부월세_월만원 <- count(소상공인,"sq6_2_3")
names(보증부월세_월만원) <- c("보증부월세_월만원","빈도수")

월세_계약기간 <- count(소상공인,"sq6_3_1")
names(월세_계약기간) <- c("월세_계약기간","빈도수")

월세_만원 <- count(소상공인,"sq6_3_2")
names(월세_만원) <- c("월세_만원","빈도수")

소상공인list <- list(제조업,비제조업,사업장임차형태,전세_년,전세_만원,
                    보증부월세_계약기간,보증부월세_보증금,보증부월세_월만원,
                    월세_계약기간,월세_만원,기타)

for(i in 1:length(소상공인list)){    
  write.xlsx(소상공인list, file = "D:/소상공인/소상공인46빈도d.xlsx", sheetName=i,colNames=FALSE)
}


########

비제조업data <- 소상공인$업종
제조업data <- 소상공인$sq4_2
사업장임차형태data <- 소상공인$sq6
전세_년data <- 소상공인$sq6_1_1
전세_만원data <- 소상공인$sq6_1_2

보증부월세_계약기간data <- 소상공인$sq6_2_1
보증부월세_보증금data <- 소상공인$sq6_2_2
보증부월세_월만원data <- 소상공인$sq6_2_3
월세_계약기간data <- 소상공인$sq6_3_1
월세_만원data <- 소상공인$sq6_3_2






##

비제조업_사업장임차형태 <- data.frame(비제조업 = 비제조업data, 사업장임차형태 = 사업장임차형태data)
비제조업_사업장임차형태 <- 비제조업_사업장임차형태 %>% table()

비제조업_사업장임차형태z <- ztable(비제조업_사업장임차형태)
print(비제조업_사업장임차형태z, caption="table" )

비제조업_사업장임차형태z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1. Y = nonmanufact / X = lease")


##

비제조업_전세_년<- data.frame(비제조업 = 비제조업data, 전세_년 = 전세_년data)
비제조업_전세_년 <- 비제조업_전세_년 %>% table()

비제조업_전세_년z <- ztable(비제조업_전세_년)
print(비제조업_전세_년z, caption="table" )

비제조업_전세_년z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1. Y = nonmanufact / X = Jeonse_year")




##

비제조업_전세_만원<- data.frame(비제조업 = 비제조업data, 전세_만원 = 전세_만원data)
비제조업_전세_만원 <- 비제조업_전세_만원 %>% table()

비제조업_전세_만원z <- ztable(비제조업_전세_만원)
print(비제조업_전세_만원z, caption="table" )

비제조업_전세_만원z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1.  Y = nonmanufact / X = Jeonse_sum")




##

비제조업_보증부월세_계약기간<- data.frame(비제조업 = 비제조업data, 보증부월세_계약기간 = 보증부월세_계약기간data)
비제조업_보증부월세_계약기간 <- 비제조업_보증부월세_계약기간 %>% table()

비제조업_보증부월세_계약기간z <- ztable(비제조업_보증부월세_계약기간)
print(비제조업_보증부월세_계약기간z, caption="table" )

비제조업_보증부월세_계약기간z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1. Y = nonmanufact / X = monthly rent1_year ")



##

비제조업_보증부월세_보증금 <- data.frame(비제조업 = 비제조업data, 보증부월세_보증금 = 보증부월세_보증금data)
비제조업_보증부월세_보증금 <- 비제조업_보증부월세_보증금 %>% table()

비제조업_보증부월세_보증금z <- ztable(비제조업_보증부월세_보증금)
print(비제조업_보증부월세_보증금z, caption="table" )

비제조업_보증부월세_보증금z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1. Y = nonmanufact / X = monthly rent1_deposit")



##

비제조업_보증부월세_월만원 <- data.frame(비제조업 = 비제조업data, 보증부월세_월만원 = 보증부월세_월만원data)
비제조업_보증부월세_월만원 <- 비제조업_보증부월세_월만원 %>% table()

비제조업_보증부월세_월만원z <- ztable(비제조업_보증부월세_월만원)
print(비제조업_보증부월세_월만원z, caption="table" )

비제조업_보증부월세_월만원z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1. Y = nonmanufact / X = monthly rent1_sum")


##

비제조업_월세_계약기간 <- data.frame(비제조업 = 비제조업data, 월세_계약기간 = 월세_계약기간data)
비제조업_월세_계약기간 <- 비제조업_월세_계약기간 %>% table()

비제조업_월세_계약기간z <- ztable(비제조업_월세_계약기간)
print(비제조업_월세_계약기간z, caption="table" )

비제조업_월세_계약기간z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1.  Y = nonmanufact / X = monthly rent2_year")



##

비제조업_월세_금액 <- data.frame(비제조업 = 비제조업data, 월세_계약기간 = 월세_만원data)
비제조업_월세_만원 <- 비제조업_월세_만원 %>% table()

비제조업_월세_만원z <- ztable(비제조업_월세_만원)
print(비제조업_월세_만원z, caption="table" )

비제조업_월세_만원z %>% 
  makeHeatmap(palette = "Blues") %>% 
  print(caption = "Table 1. Y = nonmanufact / X = monthly rent2_sum")