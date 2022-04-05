library(tidyverse)

facebook <- c(22, 40, 53, 57, 93, 98, 103, 108, 116, 121, 252)

mean(facebook)

fb_df <- tibble(facebook)

# 제곱합, 분산, 표준편차
fb_df %>% 
  mutate(dev = facebook - mean(facebook),
         sq_dev = dev ^ 2) %>% 
  summarize(ss = sum(sq_dev),
            var = ss / (length(facebook) -1),
            sd = sqrt(var))

# 극단치 제거 후 제곱합, 분산, 표준편차
fb_df %>% 
  .[-length(facebook), ] %>% 
  mutate(dev = facebook - mean(facebook),
         sq_dev = dev ^ 2) %>% 
  summarize(ss = sum(sq_dev),
            var = ss / (length(facebook) -1),
            sd = sqrt(var))
