
########rawdata 변경으로 그래프 변수명 변경

######class001 데이터 불러오기########

ls.class001 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class001_인구_codingdata",pattern="xlsx",full.names = TRUE)

class001 <- lapply(ls.class001, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class001 데이터 가공하기########

i <- 1:length(ls.class001)
class001.name <- paste("class001","idx",i,sep="")


class001.list <- list()
class001.list <- list(c(class001.name))


for(i in 1:length(ls.class001)){
  class001.list[[i]] <- class001[[i]]
  class001.list[[i]] <- class001.list[[i]][-c(1:8),-13]
}


names(class001.list) <- c(class001.name)
class001.file <- class001.list


for(i in 1:length(ls.class001)){    
  write.xlsx(class001.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class001.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class001)){
  class001[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class001.xlsx",i)
  class001[[i]] %>% select(-구분)
  class001[[i]] <- class001[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class001[[i]] <- melt(data=class001[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class001[[i]]$value) <- "numeric"
  class001[[i]]$year <- substr(class001[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class001)
nk <- paste("class001","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class001)
prv <- paste("class001","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class001)
kyn <- paste("class001","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class001$구분 <- factor(class001$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class001)){
  nk.list[[j]] <- subset(class001[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class001)){
  prv.list[[j]] <- subset(class001[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class001)){   
  kyn.list[[j]] <- subset(class001[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class001_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class001_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class001_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2018) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2018) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class002 데이터 불러오기########

ls.class002 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class002_보건의료_codingdata",pattern="xlsx",full.names = TRUE)

class002 <- lapply(ls.class002, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class002 데이터 가공하기########

i <- 1:length(ls.class002)
class002.name <- paste("class002","idx",i,sep="")


class002.list <- list()
class002.list <- list(c(class002.name))


for(i in 1:length(ls.class002)){
  class002.list[[i]] <- class002[[i]]
  class002.list[[i]] <- class002.list[[i]][-c(1:8),-13]
}


names(class002.list) <- c(class002.name)
class002.file <- class002.list


for(i in 1:length(ls.class002)){    
  write.xlsx(class002.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class002.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class002)){
  class002[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class002.xlsx",i)
  class002[[i]] %>% select(-구분)
  class002[[i]] <- class002[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class002[[i]] <- melt(data=class002[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class002[[i]]$value) <- "numeric"
  class002[[i]]$year <- substr(class002[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class002)
nk <- paste("class002","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class002)
prv <- paste("class002","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class002)
kyn <- paste("class002","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class002$구분 <- factor(class002$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class002)){
  nk.list[[j]] <- subset(class002[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class002)){
  prv.list[[j]] <- subset(class002[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class002)){   
  kyn.list[[j]] <- subset(class002[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class002_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class002_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class002_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2017) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2017년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2017) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2017년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class003 데이터 불러오기########

ls.class003 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class003_가구가족_codingdata",pattern="xlsx",full.names = TRUE)

class003 <- lapply(ls.class003, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class003 데이터 가공하기########

i <- 1:length(ls.class003)
class003.name <- paste("class003","idx",i,sep="")


class003.list <- list()
class003.list <- list(c(class003.name))


for(i in 1:length(ls.class003)){
  class003.list[[i]] <- class003[[i]]
  class003.list[[i]] <- class003.list[[i]][-c(1:8),-13]
}


names(class003.list) <- c(class003.name)
class003.file <- class003.list


for(i in 1:length(ls.class003)){    
  write.xlsx(class003.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class003.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class003)){
  class003[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class003.xlsx",i)
  class003[[i]] %>% select(-구분)
  class003[[i]] <- class003[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class003[[i]] <- melt(data=class003[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class003[[i]]$value) <- "numeric"
  class003[[i]]$year <- substr(class003[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class003)
nk <- paste("class003","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class003)
prv <- paste("class003","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class003)
kyn <- paste("class003","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class003$구분 <- factor(class003$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class003)){
  nk.list[[j]] <- subset(class003[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class003)){
  prv.list[[j]] <- subset(class003[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class003)){   
  kyn.list[[j]] <- subset(class003[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class003_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class003_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class003_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=15)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2018) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",color="black",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2018) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}



########rawdata 변경으로 그래프 변수명 변경

######class004 데이터 불러오기########

ls.class004 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class004_교육_codingdata",pattern="xlsx",full.names = TRUE)

class004 <- lapply(ls.class004, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class004 데이터 가공하기########

i <- 1:length(ls.class004)
class004.name <- paste("class004","idx",i,sep="")


class004.list <- list()
class004.list <- list(c(class004.name))


for(i in 1:length(ls.class004)){
  class004.list[[i]] <- class004[[i]]
  class004.list[[i]] <- class004.list[[i]][-c(1:8),-13]
}


names(class004.list) <- c(class004.name)
class004.file <- class004.list


for(i in 1:length(ls.class004)){    
  write.xlsx(class004.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class004.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class004)){
  class004[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class004.xlsx",i)
  class004[[i]] %>% select(-구분)
  class004[[i]] <- class004[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class004[[i]] <- melt(data=class004[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class004[[i]]$value) <- "numeric"
  class004[[i]]$year <- substr(class004[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class004)
nk <- paste("class004","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class004)
prv <- paste("class004","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class004)
kyn <- paste("class004","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class004$구분 <- factor(class004$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class004)){
  nk.list[[j]] <- subset(class004[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class004)){
  prv.list[[j]] <- subset(class004[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class004)){   
  kyn.list[[j]] <- subset(class004[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class004_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class004_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class004_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2018) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2018) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}




########rawdata 변경으로 그래프 변수명 변경

######class005 데이터 불러오기########

ls.class005 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class005_노동_codingdata",pattern="xlsx",full.names = TRUE)

class005 <- lapply(ls.class005, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class005 데이터 가공하기########

i <- 1:length(ls.class005)
class005.name <- paste("class005","idx",i,sep="")


class005.list <- list()
class005.list <- list(c(class005.name))


for(i in 1:length(ls.class005)){
  class005.list[[i]] <- class005[[i]]
  class005.list[[i]] <- class005.list[[i]][-c(1:8),-13]
}


names(class005.list) <- c(class005.name)
class005.file <- class005.list


for(i in 1:length(ls.class005)){    
  write.xlsx(class005.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class005.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class005)){
  class005[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class005.xlsx",i)
  class005[[i]] %>% select(-구분)
  class005[[i]] <- class005[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class005[[i]] <- melt(data=class005[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class005[[i]]$value) <- "numeric"
  class005[[i]]$year <- substr(class005[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class005)
nk <- paste("class005","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class005)
prv <- paste("class005","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class005)
kyn <- paste("class005","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class005$구분 <- factor(class005$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class005)){
  nk.list[[j]] <- subset(class005[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class005)){
  prv.list[[j]] <- subset(class005[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class005)){   
  kyn.list[[j]] <- subset(class005[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class005_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class005_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class005_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2017) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2017년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2017) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2017년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class006 데이터 불러오기########

ls.class006 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class006_소득소비_codingdata",pattern="xlsx",full.names = TRUE)

class006 <- lapply(ls.class006, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class006 데이터 가공하기########

i <- 1:length(ls.class006)
class006.name <- paste("class006","idx",i,sep="")


class006.list <- list()
class006.list <- list(c(class006.name))


for(i in 1:length(ls.class006)){
  class006.list[[i]] <- class006[[i]]
  class006.list[[i]] <- class006.list[[i]][-c(1:8),-13]
}


names(class006.list) <- c(class006.name)
class006.file <- class006.list


for(i in 1:length(ls.class006)){    
  write.xlsx(class006.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class006.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class006)){
  class006[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class006.xlsx",i)
  class006[[i]] %>% select(-구분)
  class006[[i]] <- class006[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class006[[i]] <- melt(data=class006[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class006[[i]]$value) <- "numeric"
  class006[[i]]$year <- substr(class006[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class006)
nk <- paste("class006","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class006)
prv <- paste("class006","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class006)
kyn <- paste("class006","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class006$구분 <- factor(class006$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class006)){
  nk.list[[j]] <- subset(class006[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class006)){
  prv.list[[j]] <- subset(class006[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class006)){   
  kyn.list[[j]] <- subset(class006[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class006_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class006_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class006_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2017) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2017년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2017) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}



########rawdata 변경으로 그래프 변수명 변경

######class007 데이터 불러오기########

ls.class007 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class007_주거건설교통_codingdata",pattern="xlsx",full.names = TRUE)

class007 <- lapply(ls.class007, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class007 데이터 가공하기########

i <- 1:length(ls.class007)
class007.name <- paste("class007","idx",i,sep="")


class007.list <- list()
class007.list <- list(c(class007.name))


for(i in 1:length(ls.class007)){
  class007.list[[i]] <- class007[[i]]
  class007.list[[i]] <- class007.list[[i]][-c(1:8),-13]
}


names(class007.list) <- c(class007.name)
class007.file <- class007.list


for(i in 1:length(ls.class007)){    
  write.xlsx(class007.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class007.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class007)){
  class007[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class007.xlsx",i)
  class007[[i]] %>% select(-구분)
  class007[[i]] <- class007[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class007[[i]] <- melt(data=class007[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class007[[i]]$value) <- "numeric"
  class007[[i]]$year <- substr(class007[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class007)
nk <- paste("class007","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class007)
prv <- paste("class007","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class007)
kyn <- paste("class007","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class007$구분 <- factor(class007$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class007)){
  nk.list[[j]] <- subset(class007[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class007)){
  prv.list[[j]] <- subset(class007[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class007)){   
  kyn.list[[j]] <- subset(class007[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class007_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class007_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class007_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2018) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2018) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class008 데이터 불러오기########

ls.class008 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class008_환경에너지_codingdata",pattern="xlsx",full.names = TRUE)

class008 <- lapply(ls.class008, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class008 데이터 가공하기########

i <- 1:length(ls.class008)
class008.name <- paste("class008","idx",i,sep="")


class008.list <- list()
class008.list <- list(c(class008.name))


for(i in 1:length(ls.class008)){
  class008.list[[i]] <- class008[[i]]
  class008.list[[i]] <- class008.list[[i]][-c(1:8),-13]
}


names(class008.list) <- c(class008.name)
class008.file <- class008.list


for(i in 1:length(ls.class008)){    
  write.xlsx(class008.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class008.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class008)){
  class008[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class008.xlsx",i)
  class008[[i]] %>% select(-구분)
  class008[[i]] <- class008[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class008[[i]] <- melt(data=class008[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class008[[i]]$value) <- "numeric"
  class008[[i]]$year <- substr(class008[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class008)
nk <- paste("class008","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class008)
prv <- paste("class008","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class008)
kyn <- paste("class008","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class008$구분 <- factor(class008$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class008)){
  nk.list[[j]] <- subset(class008[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class008)){
  prv.list[[j]] <- subset(class008[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class008)){   
  kyn.list[[j]] <- subset(class008[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class008_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class008_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class008_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  k <-6
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=15)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2016) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2016년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",color="black",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2016) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2016년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}



########rawdata 변경으로 그래프 변수명 변경

######class009 데이터 불러오기########

ls.class009 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class009_경제산업_codingdata",pattern="xlsx",full.names = TRUE)

class009 <- lapply(ls.class009, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class009 데이터 가공하기########

i <- 1:length(ls.class009)
class009.name <- paste("class009","idx",i,sep="")


class009.list <- list()
class009.list <- list(c(class009.name))


for(i in 1:length(ls.class009)){
  class009.list[[i]] <- class009[[i]]
  class009.list[[i]] <- class009.list[[i]][-c(1:8),-13]
}


names(class009.list) <- c(class009.name)
class009.file <- class009.list


for(i in 1:length(ls.class009)){    
  write.xlsx(class009.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class009.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class009)){
  class009[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class009.xlsx",i)
  class009[[i]] %>% select(-구분)
  class009[[i]] <- class009[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class009[[i]] <- melt(data=class009[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class009[[i]]$value) <- "numeric"
  class009[[i]]$year <- substr(class009[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class009)
nk <- paste("class009","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class009)
prv <- paste("class009","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class009)
kyn <- paste("class009","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class009$구분 <- factor(class009$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class009)){
  nk.list[[j]] <- subset(class009[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class009)){
  prv.list[[j]] <- subset(class009[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class009)){   
  kyn.list[[j]] <- subset(class009[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class009_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class009_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class009_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=15)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2018) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",color="black",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2018) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class010 데이터 불러오기########

ls.class010 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class010_사회통합_codingdata",pattern="xlsx",full.names = TRUE)

class010 <- lapply(ls.class010, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class010 데이터 가공하기########

i <- 1:length(ls.class010)
class010.name <- paste("class010","idx",i,sep="")


class010.list <- list()
class010.list <- list(c(class010.name))


for(i in 1:length(ls.class010)){
  class010.list[[i]] <- class010[[i]]
  class010.list[[i]] <- class010.list[[i]][-c(1:8),-13]
}


names(class010.list) <- c(class010.name)
class010.file <- class010.list


for(i in 1:length(ls.class010)){    
  write.xlsx(class010.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class010.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class010)){
  class010[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class010.xlsx",i)
  class010[[i]] %>% select(-구분)
  class010[[i]] <- class010[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class010[[i]] <- melt(data=class010[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class010[[i]]$value) <- "numeric"
  class010[[i]]$year <- substr(class010[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class010)
nk <- paste("class010","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class010)
prv <- paste("class010","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class010)
kyn <- paste("class010","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class010$구분 <- factor(class010$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class010)){
  nk.list[[j]] <- subset(class010[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class010)){
  prv.list[[j]] <- subset(class010[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class010)){   
  kyn.list[[j]] <- subset(class010[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class010_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class010_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class010_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    
    theme(legend.text= element_text(face='bold',size=15)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2015) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2015년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",color="black",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2015) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2015년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}



########rawdata 변경으로 그래프 변수명 변경

######class011 데이터 불러오기########

ls.class011 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class011_문화관광_codingdata",pattern="xlsx",full.names = TRUE)

class011 <- lapply(ls.class011, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class011 데이터 가공하기########

i <- 1:length(ls.class011)
class011.name <- paste("class011","idx",i,sep="")


class011.list <- list()
class011.list <- list(c(class011.name))


for(i in 1:length(ls.class011)){
  class011.list[[i]] <- class011[[i]]
  class011.list[[i]] <- class011.list[[i]][-c(1:8),-13]
}


names(class011.list) <- c(class011.name)
class011.file <- class011.list


for(i in 1:length(ls.class011)){    
  write.xlsx(class011.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class011.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class011)){
  class011[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class011.xlsx",i)
  class011[[i]] %>% select(-구분)
  class011[[i]] <- class011[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class011[[i]] <- melt(data=class011[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class011[[i]]$value) <- "numeric"
  class011[[i]]$year <- substr(class011[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class011)
nk <- paste("class011","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class011)
prv <- paste("class011","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class011)
kyn <- paste("class011","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class011$구분 <- factor(class011$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class011)){
  nk.list[[j]] <- subset(class011[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class011)){
  prv.list[[j]] <- subset(class011[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class011)){   
  kyn.list[[j]] <- subset(class011[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class011_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class011_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class011_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2018) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2018) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class012 데이터 불러오기########

ls.class012 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class012_안전_codingdata",pattern="xlsx",full.names = TRUE)

class012 <- lapply(ls.class012, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class012 데이터 가공하기########

i <- 1:length(ls.class012)
class012.name <- paste("class012","idx",i,sep="")


class012.list <- list()
class012.list <- list(c(class012.name))


for(i in 1:length(ls.class012)){
  class012.list[[i]] <- class012[[i]]
  class012.list[[i]] <- class012.list[[i]][-c(1:8),-13]
}


names(class012.list) <- c(class012.name)
class012.file <- class012.list


for(i in 1:length(ls.class012)){    
  write.xlsx(class012.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class012.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class012)){
  class012[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class012.xlsx",i)
  class012[[i]] %>% select(-구분)
  class012[[i]] <- class012[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class012[[i]] <- melt(data=class012[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class012[[i]]$value) <- "numeric"
  class012[[i]]$year <- substr(class012[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class012)
nk <- paste("class012","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class012)
prv <- paste("class012","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class012)
kyn <- paste("class012","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class012$구분 <- factor(class012$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class012)){
  nk.list[[j]] <- subset(class012[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class012)){
  prv.list[[j]] <- subset(class012[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class012)){   
  kyn.list[[j]] <- subset(class012[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class012_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class012_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class012_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=40)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2016) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2016년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",color="black",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2016) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2016년") +
    theme(plot.title = element_text(size=40,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=40, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=40)) +
    theme(title = element_text(face="bold",size=40)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class013 데이터 불러오기########

ls.class013 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class013_과학연구_codingdata",pattern="xlsx",full.names = TRUE)

class013 <- lapply(ls.class013, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class013 데이터 가공하기########

i <- 1:length(ls.class013)
class013.name <- paste("class013","idx",i,sep="")


class013.list <- list()
class013.list <- list(c(class013.name))


for(i in 1:length(ls.class013)){
  class013.list[[i]] <- class013[[i]]
  class013.list[[i]] <- class013.list[[i]][-c(1:8),-13]
}


names(class013.list) <- c(class013.name)
class013.file <- class013.list


for(i in 1:length(ls.class013)){    
  write.xlsx(class013.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class013.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class013)){
  class013[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class013.xlsx",i)
  class013[[i]] %>% select(-구분)
  class013[[i]] <- class013[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class013[[i]] <- melt(data=class013[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class013[[i]]$value) <- "numeric"
  class013[[i]]$year <- substr(class013[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class013)
nk <- paste("class013","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class013)
prv <- paste("class013","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class013)
kyn <- paste("class013","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class013$구분 <- factor(class013$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class013)){
  nk.list[[j]] <- subset(class013[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class013)){
  prv.list[[j]] <- subset(class013[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class013)){   
  kyn.list[[j]] <- subset(class013[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class013_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class013_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class013_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=15)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2016) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2016년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",color="black",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2016) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2016년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}


########rawdata 변경으로 그래프 변수명 변경

######class014 데이터 불러오기########

ls.class014 <- list.files(path="D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class014_양성평등_codingdata",pattern="xlsx",full.names = TRUE)

class014 <- lapply(ls.class014, function(data){
  read_excel(data,sheet=1,col_names = FALSE)
})



######class014 데이터 가공하기########

i <- 1:length(ls.class014)
class014.name <- paste("class014","idx",i,sep="")


class014.list <- list()
class014.list <- list(c(class014.name))


for(i in 1:length(ls.class014)){
  class014.list[[i]] <- class014[[i]]
  class014.list[[i]] <- class014.list[[i]][-c(1:8),-13]
}


names(class014.list) <- c(class014.name)
class014.file <- class014.list


for(i in 1:length(ls.class014)){    
  write.xlsx(class014.file, file = "D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class014.xlsx", sheetName=i,colNames=FALSE)
}



for(i in 1:length(ls.class014)){
  class014[[i]] <- read_excel("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/class014.xlsx",i)
  class014[[i]] %>% select(-구분)
  class014[[i]] <- class014[[i]] %>% mutate(구분=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  class014[[i]] <- melt(data=class014[[i]],
                        id.var="구분",
                        variable.name = "year",
                        value.name = "value")
  mode(class014[[i]]$value) <- "numeric"
  class014[[i]]$year <- substr(class014[[i]]$year,1,4)
}



##################그래프 그리기 저장

j <- 1:length(ls.class014)
nk <- paste("class014","idx",j,"_nk",sep="")
nk.list <- list()
nk.list <- list(c(nk))

j <- 1:length(ls.class014)
prv <- paste("class014","idx",j,"_prv",sep="")
prv.list <- list()
prv.list <- list(c(prv))

j <- 1:length(ls.class014)
kyn <- paste("class014","idx",j,"_kyn",sep="")
kyn.list <- list()
kyn.list <- list(c(kyn))





class014$구분 <- factor(class014$구분, levels=c("전국","서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주","창원","의창구","성산구","합포구","회원구","진해구","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))


for(j in 1:length(ls.class014)){
  nk.list[[j]] <- subset(class014[[j]], 구분 %in% c("전국","경남"))
}

for(j in 1:length(ls.class014)){
  prv.list[[j]] <- subset(class014[[j]], 구분 %in% c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
}

for(j in 1:length(ls.class014)){   
  kyn.list[[j]] <- subset(class014[[j]], 구분 %in% c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
}

nk.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class014_", sprintf("%03d",1:length(nk.list)),"_nk_g",".png")
prv.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class014_", sprintf("%03d",1:length(prv.list)),"_prv_g",".png")
kyn.file.nm <- paste0("D:/지표/통합_최종/경남100개지표/통합100_coding/codingdata/graph/","class014_", sprintf("%03d",1:length(kyn.list)),"_kyn_g",".png")



for(k in 1:length(nk.list)){
  
  nk.list[[k]]$구분 <- factor(nk.list[[k]]$구분, c("전국","경남"))
  prv.list[[k]]$구분 <- factor(prv.list[[k]]$구분, c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주"))
  kyn.list[[k]]$구분 <- factor(kyn.list[[k]]$구분, c("창원","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천"))
  
  
  
  nk.list_g <- nk.list[[k]]
  
  nk_g <- ggplot(data=nk.list[[k]], 
                 aes(x=year, y=value, group=factor(구분), shape=factor(구분))) +
    theme_bw() +
    geom_line(size=1, color="#0066ff") +
    ylim(0,max(nk.list[[k]]$value)) +
    geom_point(size=4, color="#0066ff") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="", y=" ") + 
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.text= element_text(face='bold',size=15)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(axis.line = element_line(size = 0.7))
  
  
  
  prv.list_g <- prv.list[[k]]
  prv.list_g <- prv.list_g %>% filter(year==2018) 
  
  prv_g <- ggplot(data=prv.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",color="black",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  kyn.list_g <- kyn.list[[k]]
  kyn.list_g <- kyn.list_g %>% filter(year==2018) 
  
  kyn_g <- ggplot(data=kyn.list_g, aes(x=구분, y=value, fill=구분)) +
    theme_bw() +
    geom_bar(stat="identity",fill="#0066ff") +
    scale_fill_discrete(limits=c("창원","진해","진주","통영","사천","김해","밀양","거제","양산","의령","함안","창녕","고성","남해","하동","산청","함양","거창","합천")) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x=" ", y=" ") + 
    labs(title = "2018년") +
    theme(plot.title = element_text(size=10,hjust = 1)) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "cm")) +
    theme(axis.text.x = element_text(face="bold",size=13, angle = 0, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(face="bold",size=13)) +
    theme(title = element_text(face="bold",size=15)) +
    theme(axis.line = element_line(size = 0.5))
  
  
  
  ggsave(file=nk.file.nm[k], plot=nk_g, width=20, height=12,units=c("cm"))
  ggsave(file=prv.file.nm[k], plot=prv_g, width=20, height=12,units=c("cm"))
  ggsave(file=kyn.file.nm[k], plot=kyn_g, width=20, height=12,units=c("cm"))
}
