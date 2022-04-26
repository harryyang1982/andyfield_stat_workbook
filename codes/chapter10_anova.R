library(tidyverse)
library(discovr)

data(package="discovr")

puppy_tib <- puppies %>% 
  mutate(dose = as_factor(dose))

levels(puppy_tib$dose)

puppy_tib <- puppy_tib %>% 
  mutate(dummy1 = case_when(dose == "30 mins" ~ 1,
                            TRUE ~ 0),
         dummy2 = case_when(dose == "15 mins" ~ 1,
                            TRUE ~ 0))

puppy_lm <- lm(happiness ~ dummy1 + dummy2, data = puppy_tib)

library(broom)
glance(puppy_lm)

tidy(puppy_lm, conf.int = TRUE)

# Self - Test

puppy_tib <- puppy_tib %>% 
  mutate(contrast1 = case_when(dose == "No puppies" ~ -2/3,
                               TRUE ~ 1/3),
         contrast2 = case_when(dose == "No puppies" ~ 0,
                               dose == "15 mins" ~ -0.5,
                               dose == "30 mins" ~ 0.5))

puppy_tib

puppy_con_lm <- lm(happiness ~ contrast1 + contrast2, data = puppy_tib)
glance(puppy_con_lm)

tidy(puppy_con_lm, conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))


# Summary statistics and plot
## A violin plot

ggplot(puppy_tib, aes(dose, happiness)) +
  geom_violin() +
  stat_summary(fun.data = "mean_cl_boot") +
  labs(x = "Dose of puppies", y = "Happiness (0-10)") +
  scale_y_continuous(breaks = 1:7) +
  theme_minimal()

puppy_tib %>% 
  group_by(dose) %>% 
  summarize(
    valid_cases = sum(!is.na(happiness)),
    min = min(happiness, na.rm = TRUE),
    max = max(happiness, na.rm = TRUE),
    median = median(happiness, na.rm = TRUE),
    mean = mean(happiness, na.rm = TRUE),
    sd = sd(happiness, na.rm = TRUE),
    `95% CI lower` = mean_cl_normal(happiness)$ymin,
    `95% CI upper` = mean_cl_normal(happiness)$ymax
  ) %>% 
  mutate(across(where(is.numeric), ~round(., 2)))

# Set contrasts

contrasts(puppy_tib$dose) <- contr.treatment(3, base = 3)
contrasts(puppy_tib$dose)

puppy_vs_none <- c(-2/3, 1/3, 1/3)
short_vs_long <- c(0, -1/2, 1/2)

contrasts(puppy_tib$dose) <-  cbind(puppy_vs_none, short_vs_long)

# Fit the Model
# Assumptions met

puppy_lm <- lm(happiness ~ dose, data = puppy_tib, na.action = na.exclude)
anova(puppy_lm) %>% 
  parameters::parameters(., omega_squared = 'raw')

tidy(puppy_lm, conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))

plot(puppy_lm)

library(ggfortify)

autoplot(puppy_lm,
         which = c(1, 3, 2, 4),
         colour = '#5c97bf',
         smooth.colour = '#ef4836',
         alpha = 0.5,
         size = 1) +
  theme_minimal()

## Post hoc tests 사후검정

library(modelbased)
estimate_contrasts(puppy_lm)

### Bonferroni
estimate_contrasts(puppy_lm, adjust = "bonferroni")

## Trend analysis

contrasts(puppy_tib$dose) <- contr.poly(3)

puppy_trend <- lm(happiness ~ dose, data = puppy_tib, na.action = na.exclude)

tidy(puppy_trend, conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))

puppy_tib

## Robust model

contrasts(puppy_tib$dose) <- cbind(puppy_vs_none, short_vs_long)

## Robust F
oneway.test(happiness ~ dose, data = puppy_tib)

## Robust parameter estimates

puppy_rob <- robust::lmRob(happiness ~ dose, data = puppy_tib)
summary(puppy_rob)


parameters::model_parameters(puppy_lm, robust = TRUE, vcov.type = "HC4", digits = 3)

## WRS2 package
library(WRS2)

t1way(happiness ~ dose, data = puppy_tib, nboot = 1000)

lincon(happiness ~ dose, data = puppy_tib)

t1waybt(happiness ~ dose, data = puppy_tib, nboot = 1000)

mcppb20(happiness ~ dose, data = puppy_tib, nboot = 1000)

## Self-test
t1way(happiness ~ dose, data = puppy_tib, tr = 0.1, nboot = 1000)

lincon(happiness ~ dose, data = puppy_tib, tr = 0.1)

## Bayes Factors
puppy_bf <- BayesFactor::lmBF(happiness ~ dose, data = puppy_tib, rscaleFixed = "medium")
puppy_bf
