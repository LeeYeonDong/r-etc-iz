# load data
library(readxl)
raw <- read_excel("D:/대학원/사교육영향/rawdata_04.xlsx", sheet = 1)

## PSM
#install.packages("moonBook")
library(moonBook)
#install.packages("MatchIt")
library(MatchIt)
library(tidyverse)
library(dbplyr)

# trimming var name
names(raw_1) <- names(raw) %>% str_remove("\\..*")

str(raw_1)
dim(raw_1)

# for psm select neccesary var
raw_1 <- raw_1 %>% select(id:A14)

raw_1 <- raw_1 %>% na.omit()
raw_1$A14 <- as.factor(raw_1$A14)

# run PSM
match_fit <- matchit(A14 ~ A01 + A04 + A06 + A07 + A08 + A09 + A10 + A11, method='nearest', data = raw_1)
matched_dat <- match.data(match_fit)
matched_dat$id

# extract psm data (matching for raw data)
psm_df <- data.frame()

for (i in 1:length(matched_dat$id)){
  df_tmp <- raw %>% filter(id == matched_dat$id[i])
  psm_df <- psm_df %>% rbind(df_tmp)
}

names(psm_df) <- names(raw)
  
library(openxlsx)
write.xlsx(psm_df, sheetName="sheet1", file="D:/대학원/사교육영향/psm_df.xlsx")
