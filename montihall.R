## montihall

final <- c()

for(i in 1:10000){ # 각 시행, 1만번 수행
  data <- sample(c(1,1,2)) # 염소 = 1, 차 = 2 를 랜덤으로 생성
  selected <- sample(c(1:3), 1) # 첫 선택 (index)
  unselected <- c(1:3)[-selected] # 선택되지 않은것 (index)
  
  deleted <- unselected[which(data[unselected] == 1)[1]] # 선택되지 않은것 중 하나 삭제
  
  lefted <- c(1:3)[ c(-selected, -deleted) ] # 삭제되지 않은 것 중 남은것 (index)
  
  #바꾸지 않았을 때, 바꿨을 때
  final <- rbind(final, c(data[selected] == 2, data[lefted] == 2)) 
  
  cat("선택을 바꾸지 않았을때" , data[selected] == 2,"선택을 바꾸었을때",  data[lefted] == 2,"\n")
  
  Sys.sleep(time = 0.0005)
}

colSums(final)

