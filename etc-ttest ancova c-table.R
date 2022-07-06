library(tidyverse)
library(tidyr)
library(dplyr)
library(HH)


연령 <- sample(70:100,100,replace=TRUE)
참미 <- c(rep(c(0,1),50))

연령df <- data.frame(연령,참미)

연령df$연령 <- ifelse(연령df$연령 >= 85,"85이상",
ifelse(연령df$연령 >=75, "75-85",
ifelse(연령df$연령 >=65, "65-75", "65미만")))

table(연령df$연령,연령df$참미)

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




# 성별 c-table
참여 <- c(2,2,2,2,2,2,1,1,1,1,2,2,2,1,1,2,1,2,1,1,1)
미참여 <- c(2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)

성별df <- tibble(참여,미참여)
성별_cdf <- table(성별df$참여,성별df$미참여) %>% as.data.frame()
성별_cdf <- tibble(성별_cdf)
names(성별_cdf) <- c("참여","성별","value")
성별_cdf$참여 <- 성별_cdf$참여 %>% as.integer()

# pt
t.test(value ~ 참여, data=성별_cdf, paired = TRUE, var.equal = FALSE)

# ancova
ancova(참여 ~ 성별 * value, data = 성별_cdf)