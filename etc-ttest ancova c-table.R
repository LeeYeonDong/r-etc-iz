library(tidyverse)
library(tidyr)
library(dplyr)
library(HH)


���� <- sample(70:100,100,replace=TRUE)
���� <- c(rep(c(0,1),50))

����df <- data.frame(����,����)

����df$���� <- ifelse(����df$���� >= 85,"85�̻�",
ifelse(����df$���� >=75, "75-85",
ifelse(����df$���� >=65, "65-75", "65�̸�")))

table(����df$����,����df$����)

head(mpg)


# c-table
table(mpg$cty,mpg$year)

cty_df <- table(mpg$cty,mpg$year) %>% as.data.frame()
names(cty_df) <- c("cty","year","value")
cty_df <- tibble(cty_df)
cty_df$cty <- cty_df$cty %>% as.integer()

cty_df$cty <- ifelse(cty_df$cty >= 25,"5",
ifelse(cty_df$cty >=20, "4",
ifelse(cty_df$cty >=15, "3", 
ifelse(cty_df$cty >=10, "2","1"))))
cty_df$cty <- cty_df$cty %>% as.integer()


# pt
t.test(value ~ year, data=cty_df, paired = TRUE, var.equal = FALSE)

# ancova
ancova(cty ~ year * value, data = cty_df)




# ���� c-table
���� <- c(2,2,2,2,2,2,1,1,1,1,2,2,2,1,1,2,1,2,1,1,1)
������ <- c(2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)

����df <- tibble(����,������)
����_cdf <- table(����df$����,����df$������) %>% as.data.frame()
����_cdf <- tibble(����_cdf)
names(����_cdf) <- c("����","����","value")
����_cdf$���� <- ����_cdf$���� %>% as.integer()

# pt
t.test(value ~ ����, data=����_cdf, paired = TRUE, var.equal = FALSE)

# ancova
ancova(���� ~ ���� * value, data = ����_cdf)