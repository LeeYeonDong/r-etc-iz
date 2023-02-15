library(openai)


Sys.setenv(
  OPENAI_API_KEY = "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")

create_completion(
  model = "ada",
  prompt = "Generate a question and an answer"
)
# 이미지 그리기
create_image("A young, slender, and fashionable Korean woman in her 20s plays with her brown Pomeranian in a park on a sunny day. No female faces are shown. You can see the dog's face. A Pomeranian running from the floor looking up at a woman in her twenties. A woman in her 20s is on the left. Pomeranian is on the right")

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

