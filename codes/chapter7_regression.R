library(tidyverse)
library(discovr)
library(broom)

data("album_sales")
data(df_beta)
data(pubs)

df_beta  %>% 
  lm(y ~ x, data = .) -> df_lm

df_beta %>% 
  filter(case != 30) %>% 
  lm(y ~ x, data = .) %>% 
  tidy() 

dfbeta(df_lm)

df_lm %>% tidy()

pubs %>% 
  lm(mortality ~ pubs, data = .) -> pub_lm

pub_inf <- pub_lm %>% 
  augment() %>% 
  rename(Residual = .resid,
         `Cook's Distance`= .cooksd,
         Leverage = .hat) %>% 
  mutate(District = 1:8,
         `DF beta (intercept)` = dfbeta(pub_lm)[, 1] %>% round(., 3),
         `DF beta (pubs)` = dfbeta(pub_lm)[, 2] %>% round(., 3)) %>% 
  select(District, Residual, `Cook's Distance`, Leverage, `DF beta (intercept)`, `DF beta (pubs)`) %>% 
  round(., 3)

pub_inf

# album

album_sales %>% 
  pivot_longer(adverts:image, "Variable", "value") %>% 
  group_by(Variable) %>% 
  summarize(mean = mean(value),
            sd = sd(value),
            cl_lower = mean_cl_normal(value)$ymin,
            cl_upper = mean_cl_normal(value)$ymax,
            min = min(value),
            max = max(value)) -> album_sum

album_sales %>% 
  ggplot(aes(x = adverts, y = sales)) +
  geom_point(color = "#5c97bf") +
  coord_cartesian(ylim = c(0, 400), xlim = c(0, 2500)) +
  scale_x_continuous(breaks = seq(0, 2500, 500)) +
  scale_y_continuous(breaks = seq(0, 400, 100)) +
  labs(x = "Advertising Budget (thousands)",
       y = "Album Sales (thousands)") +
  geom_smooth(method = "lm", color = "#d47500", fill = "#d47500", alpha = 0.2) +
  theme_minimal()


album_lm <- lm(sales ~ adverts, data = album_sales, na.action = na.exclude)


confint(album_lm)
album_lm %>% 
  confint()

album_lm %>% 
  glance()

summary(album_lm)$r.squared %>% 
  sqrt()

album_lm %>% 
  tidy(conf.int = TRUE)

album_lm %>% 
  tidy(conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))

library(pixiedust)

album_lm %>% 
  tidy(conf.int = TRUE) %>% 
  dust() %>% 
  sprinkle(col = 2:7, round = 3)

# 7.6 다중회귀: 기초 Fit a model with several predictor
## Bivariate Correlations and scatterplots

library(GGally)

ggscatmat(album_sales, columns = c("adverts", "airplay", "image", "sales")) +
  theme_minimal()

album_full_lm <- lm(sales ~ adverts + airplay + image, data = album_sales, na.action = na.exclude)

album_full_lm2 <- update(album_lm, .~. + airplay + image)

album_full_lm
album_full_lm2

glance(album_full_lm)

summary(album_full_lm)$r.squared %>% 
  sqrt()

### Compare Models
anova(album_lm, album_full_lm) %>% 
  tidy()

### Parameter estimates

summary(album_full_lm)
confint(album_full_lm)

tidy(album_full_lm, conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))

### Standardized betas
library(parameters)
model_parameters(album_full_lm, standardize = "refit", digits = 3)

### influence measures

album_full_rsd <- album_full_lm %>% 
  augment() %>% 
  rowid_to_column(var = "case_no")

album_full_inf <- influence.measures(album_full_lm)

album_full_inf <- album_full_inf$infmat %>% 
  as_tibble() %>% 
  rowid_to_column(var = "case_no")

album_full_inf

album_full_rsd <- album_full_rsd %>% 
  left_join(., album_full_inf, by = "case_no") %>% 
  select(-c(cook.d, hat))

album_full_rsd

### Variance Inflation Factor
### 10이 넘으면 걱정할 필요가 있다.
### 1이 넘으면 회귀모형 편향 가능

