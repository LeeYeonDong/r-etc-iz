install.packages("circlize")
library(circlize)

rawdata <- read.csv("C:/Users/Administrator/Desktop/18년도 종단분석 연구/raw data/2014~2018 지원,합격,등록현황 수과GPA구간(일반고).csv",head=T)
rawdata <- table(rawdata)
df <- as.data.frame(rawdata)


row_sum = sum(rowSums(abs(rawdata)))
col_sum = sum(colSums(abs(rawdata)))

small_gap = 1
big_gap = 30

nr = nrow(rawdata)
nc = ncol(rawdata)
n_sector = nr + nc
row_sector_degree = (360 - small_gap*(n_sector - 2) - big_gap*2) * (row_sum/(row_sum + col_sum)) + 
  small_gap*(nr-1)
start_degree = 0 - (180 - row_sector_degree)/2



circos.par(gap.after = c(rep(1, length(unique(df[[1]]))-1), 30, 
                         rep(1, length(unique(df[[2]]))-1), 30),
           start.degree=start_degree)
chordDiagram(df)
circos.clear()





rawdata <- read.csv("C:/Users/Administrator/Desktop/18년도 종단분석 연구/raw data/2014~2018 지원,합격,등록현황 수과GPA구간(과학고).csv",head=T)
rawdata <- table(rawdata)
df <- as.data.frame(rawdata)


row_sum = sum(rowSums(abs(rawdata)))
col_sum = sum(colSums(abs(rawdata)))

small_gap = 1
big_gap = 30

nr = nrow(rawdata)
nc = ncol(rawdata)
n_sector = nr + nc
row_sector_degree = (360 - small_gap*(n_sector - 2) - big_gap*2) * (row_sum/(row_sum + col_sum)) + 
  small_gap*(nr-1)
start_degree = 0 - (180 - row_sector_degree)/2



circos.par(gap.after = c(rep(1, length(unique(df[[1]]))-1), 30, 
                         rep(1, length(unique(df[[2]]))-1), 30),
           start.degree=start_degree)
chordDiagram(df)
circos.clear()



rawdata <- read.csv("C:/Users/Administrator/Desktop/18년도 종단분석 연구/raw data/2014~2018 지원,합격,등록현황 수과GPA구간(영재학교).csv",head=T)
rawdata <- table(rawdata)
df <- as.data.frame(rawdata)


row_sum = sum(rowSums(abs(rawdata)))
col_sum = sum(colSums(abs(rawdata)))

small_gap = 1
big_gap = 30

nr = nrow(rawdata)
nc = ncol(rawdata)
n_sector = nr + nc
row_sector_degree = (360 - small_gap*(n_sector - 2) - big_gap*2) * (row_sum/(row_sum + col_sum)) + 
  small_gap*(nr-1)
start_degree = 0 - (180 - row_sector_degree)/2



circos.par(gap.after = c(rep(1, length(unique(df[[1]]))-1), 30, 
                         rep(1, length(unique(df[[2]]))-1), 30),
           start.degree=start_degree)
chordDiagram(df)
circos.clear()
