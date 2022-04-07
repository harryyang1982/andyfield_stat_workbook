library(tidyverse)
library(discovr)
library(correlation)

# 6.5 이변량 상관

data(exam_anxiety)
exam_anxiety %>% 
  .[, c("exam_grade", "anxiety")] %>% 
  cor()

exam_anxiety %>% 
  .[, c("exam_grade", "revise")] %>% 
  correlation()

exam_anxiety %>% 
  correlation(digits = 3,
              ci_digits = 3)

exam_anxiety %>% 
  correlation(digits = 3,
              ci_digits = 3) %>% 
  summary()

exam_cor <- exam_anxiety %>% 
  correlation()

# 결정계수
(exam_cor$r)^2

## spearman

exam_anxiety %>% 
  correlation(method = "spearman",
              digits = 3, ci_digits = 3)

## kendall
exam_anxiety %>% 
  correlation(method = "kendall",
              digits = 3, ci_digits = 3)

