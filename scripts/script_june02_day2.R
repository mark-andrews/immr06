library(tidyverse)
library(modelr)
library(lme4)

# random slopes & random intercepts & correlation between them
M_11 <- lmer(Reaction ~ Days + (Days|Subject),
            REML = FALSE,
            data = sleepstudy)

# random intercepts only model
M_12 <- lmer(Reaction ~ Days + (1|Subject),
             REML = FALSE,
             data = sleepstudy)

# random slopes only model
M_13 <- lmer(Reaction ~ Days + (0 + Days|Subject),
            REML = FALSE,
            data = sleepstudy)


# random slopes & random intercepts & NO correlation between them
M_14 <- lmer(Reaction ~ Days + (Days||Subject),
             REML = FALSE,
             data = sleepstudy)


# Visualize the predictions of the models ---------------------------------

add_predictions(sleepstudy, M_11) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('M_11: random slope & intercepts & corr')

add_predictions(sleepstudy, M_12) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('M_12: random intercepts only')
