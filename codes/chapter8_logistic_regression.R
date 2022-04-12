library(tidyverse)
library(discovr)
library(broom)

data(eel)
eel

eel %>% 
  mutate(cured = fct_relevel(cured, c("Not Cured", "Cured")),
         intervention = fct_relevel(intervention, c("No Treatment", "Treatment")))

model1 <- glm(cured ~ intervention, data = eel, family = binomial())
model1 %>% 
  tidy()

model2 <- glm(cured ~ intervention + duration, data = eel, family = binomial())
model2 %>% 
  tidy()


bind_rows(model1 %>% tidy(), model2 %>% tidy())

summary(model1)
summary(model2)

parameters::parameters(model1)

anova(model1, model2) %>% 
  tidy()

model1 %>% 
  confint()

model1 %>% 
  glance()

model2 %>% 
  glance()

model2 %>% 
  confint()

model_chi <- model1$null.deviance - model1$deviance
model_chi

chidf <- model1$df.null - model1$df.residual
chidf

chisq.prob <- 1 - pchisq(model_chi, chidf)
chisq.prob

R2_hl <- model_chi / model1$null.deviance
R2_hl

R_cs <- 1 - exp((model1$deviance - model1$null.deviance) / 113)
R_cs

R_n <- R_cs / (1-(exp(-(model1$null.deviance/113))))
R_n

model1
parameters::model_parameters(model1,
                             digits=3)

exp(model1$coefficients)

model1 %>% 
  confint() %>% exp(.)

# model2 prediction
