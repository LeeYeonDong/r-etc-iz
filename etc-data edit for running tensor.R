## data
df_ytb1 <- read_csv(file = "D:/대학원/논문/커뮤니케이션학과/유튜브.csv", col_names = TRUE, locale=locale('ko',encoding='utf-8'))

df_ytb1$댓글_ytb %>% head()
  
df_ytb1_댓글_doc <- c()

n <- 100

for (i in 1:n){
  cat(i, '번째 데이터 통합 중 입니다.\n') 
  df_ytb1_댓글_doc <- paste(df_ytb1_댓글_doc, df_ytb1$댓글_ytb[i], sep = " ")
}

df_ytb1_댓글_doc <- df_ytb1_댓글_doc %>% as.data.frame()
names(df_ytb1_댓글_doc) <- c("댓글_doc")

write.csv(df_ytb1_댓글_doc, "D:/대학원/논문/커뮤니케이션학과/df_ytb1_댓글_doc.csv", fileEncoding = "utf-8")


댓글_ytb_tag <- read_csv(file = "D:/대학원/논문/커뮤니케이션학과/댓글_ytb_tag.csv", col_names = TRUE, locale=locale('ko',encoding='utf-8'))

names(댓글_ytb_tag) <- "token-tag"
댓글_ytb_tag$id <- c(1:(dim(댓글_ytb_tag)[1]))

token <- c()
tag <- c()
id <- c()

for (i in 1:(dim(댓글_ytb_tag)[1])){
  for (j in 1:length((댓글_ytb_tag[i,] %>% str_split(pattern = "\\),"))[[1]])){
    
  token_tmp <- (((댓글_ytb_tag[i,] %>% 
                    str_split(pattern = "\\),"))[[1]][j] %>% 
                   str_split(pattern = "\\,"))[[1]][1] %>% 
                  str_match('([a-zA-Z]+)'))[,1]

  tag_tmp <- (((댓글_ytb_tag[i,] %>% 
                  str_split(pattern = "\\),"))[[1]][j] %>% 
                 str_split(pattern = "\\,"))[[1]][2] %>% 
                str_match('([a-zA-Z]+)'))[,1]
  
  id_tmp <- (댓글_ytb_tag[i,] %>% str_split(pattern = "\\),"))[[2]][1]
  
  token <- append(token,token_tmp)
  tag <- append(tag,tag_tmp)
  id <- append(id,id_tmp)
  
  cat(i, '번째 tokenizer 중 ', j, '번째 단어 vectorizing 완료.\n') 
}
}

댓글_ytb_tag_df <- tibble(id,token,tag) %>% 
                    na.omit() %>% 
                    filter(str_length(token) >= 3) %>% 
                    group_by(id) %>%
                    mutate(reply = paste0(token, collapse = " ")) %>% 
                    select(1,4,2,3)

# write.csv(댓글_ytb_tag_df, "D:/대학원/논문/커뮤니케이션학과/댓글_ytb_tag_df.csv", fileEncoding = "utf-8")
# 
# 
# ytb_sen_df <- read_csv(file = "D:/대학원/논문/커뮤니케이션학과/ytb_sen_df.csv", col_names = TRUE, locale=locale('ko',encoding='utf-8'))
# 
# ytb_sen_df
# 
# ytb_sen_df$sent_token <- gsub("[]]","",ytb_sen_df$sent_token)
# ytb_sen_df$sent_token <- gsub("[[]","",ytb_sen_df$sent_token)
# ytb_sen_df$sent_token <- gsub("\'","",ytb_sen_df$sent_token)
# 
# write.csv(ytb_sen_df, "D:/대학원/논문/커뮤니케이션학과/ytb_sen_df_re.csv", fileEncoding = "utf-8")
# 
# token_sen_df <- read_csv(file = "D:/대학원/논문/커뮤니케이션학과/token_sen_df.csv", col_names = TRUE, locale=locale('ko',encoding='utf-8'))
# 
# token_sen_df
# 
# token_sen_df$word_token <- gsub("[]]","",token_sen_df$word_token)
# token_sen_df$word_token <- gsub("[[]","",token_sen_df$word_token)
# token_sen_df$word_token <- gsub("\'","",token_sen_df$word_token)
# token_sen_df$word_token <- gsub(" ","",token_sen_df$word_token)
# 
# token_sen_list <- token_sen_df$word_token %>% 
#   str_split(pattern = "[,]") 
# 
# length(token_sen_list[[100]])
# 
# token_vec <- c()
# 
# for (i in 1:(dim(token_sen_df)[1])){
#   word_token
#   token_vec.tmp <- token_sen_list[[i]][j]
#   token_vec <- append(token_vec,token_vec.tmp)
# }
# 
# token_df <- as_tibble(token_vec)
# names(token_df) <- c("word_token")
# 
# write.csv(token_df, "D:/대학원/논문/커뮤니케이션학과/token_df.csv", fileEncoding = "utf-8")
# 
# 
# token_df

# 데이터 프레임에 있는 모든 벡터 합치기
reply <- 댓글_ytb_tag_df$reply %>% 
                unique() %>% 
                tolower()

reply_paste <- c()

for (i in 1:length(reply)){
  reply_paste <- paste(reply_paste, reply[i], sep = ". ")
  cat(i, '번째 document 합지는 중.\n')
}

write.table(reply_paste, "D:/대학원/논문/커뮤니케이션학과/reply_paste.txt", fileEncoding = "utf-8")


########## 문장별 - 소문자, 각각 불용어 제거, 단어길이 제한
reply <- 댓글_ytb_tag_df$reply %>% 
  unique() %>% 
  tolower() # 소문자

names(reply) <- c("reply")

reply_df$line_number <- c(1:dim(reply_df)[1])

reply_list <- reply %>% str_split(pattern = " ")
reply_list <- reply_list %>% head(1000)

line_number <- c()
word_token <- c()

for (i in 1:length(reply)){
  for (j in 1:length(reply_list[[i]])){
  line_number <- append(line_number,i)
  word_token <- append(word_token,reply_list[[i]][j])
  cat(i, '번째 document.\n')
  }
}

word_token_df <- tibble(line_number,word_token)

stopwords_df <- stopwords("en") %>% as_tibble()
names(stopwords_df) <- c("word_token")
stopwords_df$stopwords <- c("stopwords")

word_token_df <- word_token_df %>% 
                  left_join(stopwords_df, by = c("word_token"))

word_token_df$stopwords[is.na(word_token_df$stopwords)] <- "not_stopwords"

word_token_df <- word_token_df %>% 
                  filter(stopwords == "not_stopwords") %>% 
                  select(-stopwords) %>% # 불용어 제거
                  mutate(str_len = str_length(word_token)) %>% 
                  filter(str_len > 2) %>%  # 단어길이 제한 
                  select(-str_len)

paste(word_token_df$word_token[i])

word_token_df %>% 
 conut(word_token)
  
