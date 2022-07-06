asd <- read_lines("D:/대학원/간호대/기록지/간호기록지(2021.02) 서울요양원/asd.txt")
asd <- gsub('\\s','',asd)
asd <- gsub('■',' ZTrues',asd)
asd <- gsub('□',' ZFalse',asd)

asd_sp <- str_split(asd,"Z")
asd_sp %>% str()
asd_vec <- c()

for (i in 1:length(asd)){
  
  cat(i, '번째 리스트 벡터화', '중 입니다.\n') 
  
  for (j in 1:10){
  asd_vec.tmp <- asd_sp[[i]][j]
  asd_vec <- append(asd_vec,asd_vec.tmp)
  }
}

asd_df <- asd_vec %>% as_tibble()
asd_df %>% view()

대상자_성명 <- asd_df[81,]
생년월일/성별 <- asd_df[101,]
장기요양_등급 <- asd_df[121,]
장기요양인정번호 <- asd_df[141,]
