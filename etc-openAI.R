library(openai)
library(tidyverse)

Sys.setenv(
  OPENAI_API_KEY = "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")

create_completion(
  model = "ada",
  prompt = "Generate a question and an answer"
)
# 이미지 그리기
create_image("10 people")

create_image("10 people sitting and gathering")

create_image("Ten Koreans are sitting and gathering for a reading meeting.")

create_image("Ten Koreans are sitting and gathering for a reading meeting at a cafe.")

create_image("Ten Koreans are sitting around a table for a reading meeting at a cafe. view from above.")

create_image("Ten Koreans are sitting around a table for a reading meeting at a cafe. view from above. With a feeling of oil painting.")



# 
library(chatgpt)

cat(comment_code("for (i in 1:10) {
                 print(i ** 2)
                 }"))

"how can i make the document-term matrix. show me simple example using data frame in R" %>% ask_chatgpt() %>% cat()

"how can i make the document-term matrix. show me simple example using data frame in python" %>% ask_chatgpt() %>% cat()

cat(document_code("square_numbers <- function(numbers) numbers ** 2"))

cat(explain_code("for (i in 1:10) {\n print(i ** 2)\n}"))

cat(find_issues_in_code("i <- 0\nwhile (i < 0) {\n i <- i - 1\n}"))

cat(optimize_code("i <- 10
                  while (i > 0) {
                  i <- i - 1\n print(i)
                  }"))

