library(tidyverse)

# we use read.csv2 here because the separator is ";"
unclean = read.csv2('CA_study.csv')

# check if there is any drama language other than English, Chinese and Korean
lancheck = any(unclean$A4 == 0, na.rm = TRUE)

# delete the columns that are irrelevant to data analysis
clean = unclean[, !names(unclean) %in% c("A4_4_TEXT", "B1_0_TEXT", "C2_0_TEXT", "C3_0_TEXT", "D1_0_TEXT", "D2_0_TEXT", "D3_0_TEXT", "E1_0_TEXT", "F1")]

# then remove the cases that are not supposed to be included for all data analysis. e.g. for question B1, any cases falls under non-binary or others should be removed
clean = clean[!(clean$B1 %in% c(0, 3)), ]

# 1.create separate tibbles for different hypothesis
# 2.remove cases when they contain at least one "not mentioned / other" answers
# 3.further clean / alter the data set to fit the data analysis

cols_1a = c("B1", "C1", "C2", "C3")
clean_1a = clean[cols_1a]

clean_1a = clean_1a[!apply(clean_1a[, cols_1a] == 0, 1, any), ] |> 
  mutate(c_scale = round((C1 + C2 + C3)/3, 2)) # create a scale for social economic status