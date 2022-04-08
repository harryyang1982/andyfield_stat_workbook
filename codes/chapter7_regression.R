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
