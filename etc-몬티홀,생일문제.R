# 1.
# 시뮬레이션 횟수 설정
num_simulations <- 10000

# 승리 횟수 집계
win_count_switch <- 0
win_count_no_switch <- 0

# 시뮬레이션 반복 실행
for (i in 1:num_simulations) {
  doors <- c("car", "goat", "goat")
  # 문 배열을 무작위로 섞음
  shuffle_doors <- sample(doors)
  
  # 참가자의 초기 선택
  initial_choice <- sample(1:3, 1)
  
  # 호스트가 염소가 있는 문을 하나 열어 보임
  host_options <- which(shuffle_doors == "goat")
  host_show <- host_options[host_options != initial_choice]
  if (length(host_show) > 1) {
    host_show <- sample(host_show, 1)
  }
  
  # 참가자가 선택을 바꾸는 경우
  remaining_doors <- setdiff(1:3, c(initial_choice, host_show))
  final_choice_switch <- remaining_doors
  
  # 참가자가 선택을 바꾸지 않는 경우
  final_choice_no_switch <- initial_choice
  
  # 최종 선택이 자동차인 경우 승리 카운트 증가
  if (shuffle_doors[final_choice_switch] == "car") {
    win_count_switch <- win_count_switch + 1
  }
  if (shuffle_doors[final_choice_no_switch] == "car") {
    win_count_no_switch <- win_count_no_switch + 1
  }
}

# 승리 확률 계산
prob_switch <- win_count_switch / num_simulations
prob_no_switch <- win_count_no_switch / num_simulations

# 결과 출력
cat("Switching doors probability of winning:", prob_switch, "\n")
cat("Not switching doors probability of winning:", prob_no_switch, "\n")


# 2.
# 생일 문제 시뮬레이션 함수
simulate_birthday <- function(num_people, num_simulations) {
  same_birthday_count <- 0  # 같은 생일이 발생한 횟수
  
  for (i in 1:num_simulations) {
    # num_people 만큼의 무작위 생일 생성 (1일부터 365일 중 선택)
    birthdays <- sample(1:365, num_people, replace = TRUE)
    
    # 같은 생일이 있는지 확인
    if (length(unique(birthdays)) != num_people) {
      same_birthday_count <- same_birthday_count + 1
    }
  }
  
  # 같은 생일이 발생한 확률 계산
  probability <- same_birthday_count / num_simulations
}

# 23명이 있는 그룹에서 시뮬레이션 10,000번 실행
num_people <- 23
num_simulations <- 10000
result <- simulate_birthday(num_people, num_simulations)

# 결과 출력
cat("The probability of at least two people having the same birthday in a group of", num_people, "people is:", result, "\n")


# 결과 출력
cat("The probability of at least two people having the same birthday in a group of", num_people, "people is:", result, "\n")


# 결과 출력
cat("The probability of at least two people having the same birthday in a group of", num_people, "people is:", result, "\n")
