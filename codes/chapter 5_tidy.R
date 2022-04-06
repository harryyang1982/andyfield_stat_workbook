library(tidyverse)
# library(car)
# library(pastecs)
# library(psych)
# library(Rcmdr)
# 
# 5.5 정규성 가정
library(discovr)
library(moments)

data(download)

# hist_day1 <- download %>% 
#   ggplot(aes(day_1)) +
#   geom_density(color = 'black', fill = 'white') +
#   labs(x = "Hyginene Score on day 1", y = "Density")
# 
# # download %>% 
# #   ggplot(aes(day_1)) +
# #   geom_histogram(aes(y = ..density..)) +
# #   labs(x = "Hyginene Score on day 1", y = "Density")
# 
# hist_day1 +
#   stat_function(fun = dnorm, args = list(mean = mean(download$day_1, na.rm = TRUE),
#                                          sd = sd(download$day_1, na.rm = TRUE)), color = "black", size = 1)


download_tib <- discovr::download
unique(download_tib$gender)

table(download_tib$gender)

ggplot(download_tib, aes(day_1)) +
  geom_histogram(binwidth = 1, fill = "#56B4E9", color = "#336C8b", alpha = 0.2) + 
  labs(y = "Frequency", x = "Hygiene Scores (0-5)", title = "Hygiene Scores on day 1") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(download_tib, aes(x = "Day 1", y = day_1)) +
  geom_boxplot(fill = "#5c97bf", alpha = 0.7) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  labs(x = "Day of festival", y = "Hygiene scores (0-5)") +
  theme_minimal()

download_tib %>% 
  mutate(day_1 = recode(day_1, `20.02` = 2.02)) -> download_tib

download_tib %>% 
  pivot_longer(day_1:day_3, 
               names_to = "day", 
               values_to = "hygiene") -> download_tidy_tib

download_tidy_tib %>% 
  mutate(day = str_to_sentence(day) %>% str_replace(., "_", " ")) -> download_tidy_tib #str_to_sentence => 첫 문장 대문자로

download_tidy_tib

ggplot(download_tidy_tib, aes(x = day, y = hygiene, fill = gender)) +
  geom_boxplot(alpha = 0.7) + 
  scale_y_continuous(breaks = seq(0, 4, 1)) +
  labs(x = "Day of Festival", y = "Hygiene Scores (0-5)", fill = "Gender") +
  facet_wrap(~gender) +
  theme_minimal()

make_z <- function(x) {
  (x - mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE))
}

download_tib %>% 
  mutate(day_1_z = make_z(day_1),
         day_2_z = make_z(day_2),
         day_3_z = make_z(day_3)) -> download_tib

download_tib %>% 
  mutate(across(day_1:day_3, list(make_z))) -> download_tib

download_tib %>% 
  filter(abs(day_1_z) >= 1.96) %>% 
  arrange(day_1_z)

get_cum_percent <- function(var,  cut_off = 1.96){
  ecdf_var <- abs(var) %>% ecdf()
  100*(1 - ecdf_var(cut_off))
}

download_tib %>%
  dplyr::summarize(
    `z >= 1.96` = get_cum_percent(day_1_z),
    `z >= 2.58` = get_cum_percent(day_1_z,cut_off = 2.58),
    `z >= 3.29` = get_cum_percent(day_1_z,cut_off = 3.29)
  )
#

download_tidy_tib <- download_tidy_tib %>% 
  group_by(day) %>% 
  mutate(
    zhygiene = make_z(hygiene)
  )


download_tidy_tib %>% 
  group_by(day) %>%
  summarize(
    `z >= 1.96` = get_cum_percent(zhygiene),
    `z >= 2.58` = get_cum_percent(zhygiene,cut_off = 2.58),
    `z >= 3.29` = get_cum_percent(zhygiene,cut_off = 3.29)
  )


ggplot(download_tidy_tib, aes(sample = hygiene)) +
  stat_qq_band(fill = "#5c97bf", alpha = 0.3) +
  stat_qq_line(color = "#5c97bf") +
  stat_qq_point(alpha=0.2, size=1) +
  labs(x = "Theoretical quantiles", y = "Sample Quantiles") +
  facet_wrap(~day, ncol = 1) +
  theme_minimal()

download_tidy_tib %>%
  dplyr::filter(day == "Day 2") %>% 
  ggplot2::ggplot(., aes(sample = hygiene)) +
  qqplotr::stat_qq_band(fill = "#5c97bf", alpha = 0.3, detrend = TRUE) +
  qqplotr::stat_qq_line(colour = "#5c97bf", detrend = TRUE) +
  qqplotr::stat_qq_point(alpha = 0.2, size = 1, detrend = TRUE) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal()


# Skew and kurtosis
download_tidy_tib %>% 
  group_by(day) %>% 
  summarize(
    valid_cases = sum(!is.na(hygiene)),
    mean = mean_cl_normal(hygiene)$y,
    ci_lower = mean_cl_normal(hygiene)$ymin,
    ci_upper = mean_cl_normal(hygiene)$ymax,
    skew = skewness(hygiene, na.rm = TRUE),
    kurtosis = kurtosis(hygiene, na.rm = TRUE)
  ) 

download_tidy_tib %>% 
  group_by(gender, day) %>% 
  summarize(
    valid_cases = sum(!is.na(hygiene)),
    mean = mean_cl_normal(hygiene)$y,
    ci_lower = mean_cl_normal(hygiene)$ymin,
    ci_upper = mean_cl_normal(hygiene)$ymax,
    skew = skewness(hygiene, na.rm = TRUE),
    kurtosis = kurtosis(hygiene, na.rm = TRUE)
  ) 

ggplot(download_tidy_tib, aes(sample = hygiene)) +
  stat_qq_band(fill = "#5c97bf", alpha = 0.3) +
  stat_qq_line(color = "#5c97bf") +
  stat_qq_point(alpha=0.2, size=1) +
  labs(x = "Theoretical quantiles", y = "Sample Quantiles") +
  facet_grid(day~gender, scales = "free_x") +
  theme_minimal()
