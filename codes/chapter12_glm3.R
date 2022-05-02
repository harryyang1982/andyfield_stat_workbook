library(tidyverse)
library(discovr)
library(knitr)

data("goggles")

goggles <- goggles %>% 
  dplyr::mutate(
    facetype = forcats::as_factor(facetype),
    alcohol = forcats::as_factor(alcohol))

ggplot(goggles, aes(x = alcohol, y = attractiveness, color = facetype, shape = facetype)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "pointrange", position = position_dodge(width = 0.2)) +
  coord_cartesian(ylim=c(0,8)) +
  scale_y_continuous(breaks = 0:8) +
  scale_color_manual(values = c("#E84646", "#65ADC2")) +
  labs(x = "Alcohol consumption", y = "Attractiveness (0-10)", color = "Type of face", shape = "Type of face") +
  theme_minimal()

# summary

goggles %>% 
  group_by(facetype, alcohol) %>% 
  summarize(
    mean = mean(attractiveness, na.rm = TRUE),
    `95% CI lower` = mean_cl_normal(attractiveness)$ymin,
    `95% CI upper` = mean_cl_normal(attractiveness)$ymax
  ) %>% 
  kable(caption = "Summary statistics for the beer goggles data", digits = 2)

# self-test

goggles %>% 
  filter(alcohol != "Low dose") %>% 
  lm(attractiveness ~ facetype*alcohol, data = .) %>% 
  broom::tidy() %>% 
  mutate(across(where(is.numeric), ~round(., 3)))

# fitting the model using afex::aov_4

library(afex)

goggles_afex <- aov_4(attractiveness ~ facetype*alcohol + (1|id), data = goggles)
goggles_afex


goggles_afex <- aov_4(attractiveness ~ facetype*alcohol + (1|id), anova_table = list(p_adjust_method = "bonferroni"), data = goggles)
goggles_afex

afex_plot(goggles_afex, "alcohol", "facetype", legend_title = "Face type") +
  labs(x = "Alcohol consumption", y = "Attractiveness rating (0-10)") +
  theme_minimal()

# fitting the model using lm()

unatt_vs_att <- c(-0.5, 0.5)
contrasts(goggles$facetype) <- unatt_vs_att

none_vs_alcohol <- c(-2/3, 1/3, 1/3)
low_vs_high <- c(0, -1/2, 1/2)
contrasts(goggles$alcohol) <- cbind(none_vs_alcohol, low_vs_high)

contrasts(goggles$alcohol)

goggles_lm <- lm(attractiveness ~ facetype * alcohol, data = goggles)
car::Anova(goggles_lm, type = 3)

# Interpreting effects
## Self test

ggplot(goggles, aes(x = facetype, y = attractiveness)) +
  geom_violin(color = "#316675", fill = "#65ADC2", alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_normal", color = "#316675") +
  scale_y_continuous(breaks = 0:8) + 
  labs(y = "Attractiveness (out of 10)", x = "Type of face") +
  theme_minimal()

emmeans::emmeans(goggles_afex, "facetype")

emmeans::emmeans(goggles_lm, "facetype")

## Self test
ggplot(goggles, aes(x = alcohol, y= attractiveness)) +
  geom_violin(colour = "#168E7F", fill = "#109B37", alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_normal", colour = "#168E7F") +
  scale_y_continuous(breaks = 0:8) +
  labs(y = "Attractiveness (out of 10)", x = "Dose of alcohol") +
  theme_minimal()

emmeans::emmeans(goggles_afex, "alcohol")
emmeans::emmeans(goggles_lm, "alcohol")

afex_plot(goggles_afex, "alcohol", "facetype", legend_type = "Face type") +
  labs(x = "Alcohol consumption", y = "Attractiveness rating (0-10)") +
  theme_minimal()

emmeans::emmeans(goggles_afex, c("alcohol", "facetype"))
library(emmeans)

emmeans(goggles_afex, c("alcohol", "facetype"))

# Contrasts

library(broom)
tidy(goggles_lm, conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))

# Simple effects

joint_tests(goggles_afex, "alcohol")

joint_tests(goggles_lm, "alcohol")

joint_tests(goggles_lm, "facetype")

library(ggfortify)

autoplot(goggles_lm,
         which = c(1, 3, 2, 4),
         color = "#5c97bf",
         smooth.color = "#ef4836",
         alpha = 0.5,
         size = 1) +
  theme_minimal()

# Robust models
## Self-test

goggles_rob <- robust::lmRob(attractiveness ~ facetype * alcohol, data = goggles)
summary(goggles_rob)

library(parameters)
model_parameters(goggles_lm, robust = TRUE, vcov.type = "HC4", digits = 3)
bootstrap_parameters(goggles_lm)

# Robust overall tests

WRS2::t2way(attractiveness ~ alcohol*facetype, goggles)
goggles_mcp2atm <- WRS2::mcp2atm(attractiveness ~ alcohol*facetype, goggles)
goggles_mcp2atm

goggles_mcp2atm$contrasts
WRS2::pbad2way(attractiveness ~ alcohol*facetype, goggles, nboot = 1000)

goggles_mcp2a <- WRS2::mcp2a(attractiveness ~ alcohol*facetype, goggles, nboot = 1000)
goggles_mcp2a

goggles_mcp2a$contrasts

# Bayes Factors
alcohol_bf <- BayesFactor::lmBF(formula = attractiveness ~ alcohol, data = goggles)

facetype_bf <-  BayesFactor::lmBF(formula = attractiveness ~ alcohol + facetype, data = goggles)

int_bf <- BayesFactor::lmBF(formula = attractiveness ~ alcohol + facetype + alcohol:facetype, data = goggles)

alcohol_bf
facetype_bf/alcohol_bf
int_bf/facetype_bf

# Effect sizes
effectsize::omega_squared(goggles_afex, ci = 0.95, partial = FALSE)
effectsize::omega_squared(goggles_afex, ci = 0.95, partial = TRUE)

car::Anova(goggles_lm, type = 3) %>% 
  effectsize::omega_squared(., ci = 0.95, partial = FALSE)

car::Anova(goggles_lm, type = 3) %>% 
  effectsize::omega_squared(., ci = 0.95, partial = TRUE)
