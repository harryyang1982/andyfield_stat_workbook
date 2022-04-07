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

## 표집분포가 정규분포가 아니거나, 구간/비율 측정이 아닐 경우 Pearson을 쓸 수 없음. 다른 상관계수 연산이나 부트스트래핑을 써야 함.

exam_anxiety %>% 
  correlation(method = "pearson",
              digits = 3, ci_digits = 3)

exam_anxiety %>% 
  select(exam_grade, anxiety, revise) %>% 
  cor()

exam_anxiety %>% 
  select(exam_grade, anxiety, revise) %>% 
  cor() %>% 
  . ^2

## Robust correlations

library(WRS2)

exam_anxiety %>% .[, c("exam_grade", "revise", "anxiety")] %>%   
  winall()

exam_anxiety %>% 
  correlation(method = "percentage")

## 6.5.5 스피어만 상관계수 (비모수)

data(biggest_liar)
biggest_liar

biggest_liar %>% 
  correlation(method = 'spearman',
              digits = 3, ci_digits = 3)

biggest_liar %>% 
  mutate(novice = as_factor(novice)) %>% 
  select(position, creativity) %>% 
  correlation(method = "spearman",
              digits = 3, ci_digits = 3)

## 6.5.6 켄달 상관계수 (비모수)
biggest_liar %>% 
  correlation(method = 'kendall',
              digits = 3, ci_digits = 3)

## 6.5.7 부트스트래핑
boot_tau <- function(liar_data, i) {cor(liar_data$position[i], liar_data$creativity[i],
                                        use = "complete.obs", method = "kendall")}

boot_r <- function(data, i) {
  cor(data[i, "exam_grade"], data[i, "revise"])
}

library(boot)
boot_kendall <- boot(biggest_liar, boot_tau, 2000)
boot_kendall

boot.ci(boot_kendall)

grade_revise_bs <- boot(exam_anxiety, boot_r, R= 2000)
boot.ci(grade_revise_bs)

boot_r <- function(data, var1, var2, i){
  cor(data[i, var1], data[i, var2])
}

grade_revise_bs <- boot::boot(exam_anxiety, boot_r, var1 = "exam_grade", var2 = "revise", R = 2000)
grade_revise_bs


# 6.5.8 이연 상관과 점이연 상관

data(package="discovr")
data("roaming_cats")
roaming_cats

roaming_cats %>% select(time, sex) %>% mutate(sex = ifelse(sex == "Male", 1, 0)) %>% 
  correlation()

table(roaming_cats %>% select(time, sex) %>% mutate(sex = ifelse(sex == "Male", 1, 0)) %>% .$sex) %>% prop.table()

roaming_cats %>% mutate(sex = ifelse(sex == "Male", 0, 1)) %>% 
  correlation(method = "biserial")

exam_anxiety %>% 
  correlation(partial = TRUE)
