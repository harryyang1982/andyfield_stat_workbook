library(tidyverse)
library(discovr)

data(package='discovr')

cloak <- invisibility_cloak
cloak_rm <- invisibility_rm

library(here)
library(moments)

cloak %>% 
  group_by(cloak) %>% 
  summarize(n = n(),
            mean = mean(mischief),
            sd = sd(mischief),
            ci_lower = mean_cl_normal(mischief)$ymin,
            ci_upper = mean_cl_normal(mischief)$ymax,
            iqr = IQR(mischief),
            skew = skewness(mischief),
            kurtosis = kurtosis(mischief))

cloak_lm <- lm(mischief ~ cloak, data=cloak)
summary(cloak_lm)

# t-test from summary data
t_from_means <- function(m1, m2, sd1, sd2, n1, n2){
  df <- n1 + n2 - 2
  poolvar <- (((n1-1) *sd1 ^ 2) + ((n2-1) * sd2 ^ 2)) / df
  t <- (m1-m2)/sqrt(poolvar*((1/n1)+(1/n2)))
  sig <- 2*(1-(pt(abs(t), df)))
  paste0("t(df = ", df, ") = ", round(t, 3), ", p = ", round(sig, 5))
}

t_from_means(m1 = 5, m2 = 3.75, sd1 = 1.651, sd2 = 1.913, n1 = 12, n2 = 12)

#coefficients estimate: 평균간 차이.
# 차이들의 표집분포 SE = (sqrt(var(x1) / n1, var(x2) / n2))
# t = (Xbar1 - Xbar2) / SE

# 독립표본 t-test

spider <- tibble(group = gl(2, 12, labels = c("Picture", "Real Spider")),
                 anxiety = c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45, 40, 50, 40, 35, 50, 55, 65, 55, 50, 35, 30, 50, 60, 39))
spider

ggplot(spider, aes(x = group,
                   y = anxiety)) +
  geom_boxplot()

library(pastecs)


spider %>% 
  group_by(group) %>% 
  summarize(n = n(),
            mean = mean(anxiety),
            sd = sd(anxiety),
            ci_lower = mean_cl_normal(anxiety)$ymin,
            ci_upper = mean_cl_normal(anxiety)$ymax,
            iqr = IQR(anxiety),
            skew = skewness(anxiety),
            kurtosis = kurtosis(anxiety))

ind_t_test <- t.test(anxiety ~ group, data = spider)
ind_t_test

ggplot(cloak, aes(x = cloak,
                  y = mischief)) +
  geom_violin() +
  stat_summary(fun.data = "mean_cl_normal") +
  labs(x = "Cloak group", y = "Acts of mischief") +
  theme_minimal()

cloak_mod <- t.test(mischief ~ cloak, data = cloak)
cloak_mod

# Robust comparision

library(WRS2)

yuen(mischief ~ cloak, data = cloak)
yuen(mischief ~ cloak, data = cloak, tr = 0.1)

yuenbt(mischief ~ cloak, data = cloak, nboot = 1000, side = TRUE)
yuenbt(mischief ~ cloak, data = cloak, nboot = 100000, side = TRUE)

pb2gen(mischief ~ cloak, data = cloak, nboot = 1000)

yuen(anxiety ~ group, data = spider)

yuenbt(anxiety ~ group, data =spider, nboot = 50000, side = TRUE)

pb2gen(anxiety ~ group, data = spider, nboot = 10000)

# Bayes Factors
cloak_bf <- BayesFactor::ttestBF(formula = mischief ~ cloak, data = cloak)
cloak_bf

BayesFactor::posterior(cloak_bf, iterations = 1000) %>% 
  summary()

1 / cloak_bf

# Effect sizes
library(effectsize)
t_to_r(t = cloak_mod$statistic, df_error = cloak_mod$parameter)

cohens_d(mischief ~ cloak, data = cloak)

hedges_g(mischief ~ cloak, data = cloak)

glass_delta(mischief ~ cloak, data = cloak)

r_from_t <- function(t, df, digits = 5) {
  r <- sqrt(t^2 / (t^2+df))
  r <- round(r, digits)
  return(paste0("r = ", r))
}

r_from_t(t = cloak_mod$statistic, df = cloak_mod$parameter)

r_from_t(t = cloak_mod$statistic, df = cloak_mod$parameter, digits = 2)

# Entering data for Related Means

cloak_rm %>% 
  pivot_wider(
    id_cols = id,
    values_from = mischief,
    names_from = cloak
  ) %>% 
  mutate(diff = Cloak - `No cloak`) %>% 
  ggplot(., aes(sample = diff)) +
  qqplotr::stat_qq_band(fill = '#5c97bf', alpha = 0.3) +
  qqplotr::stat_qq_line(color = '#5c97bf') +
  qqplotr::stat_qq_point() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal()
