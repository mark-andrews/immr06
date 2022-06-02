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

add_predictions(sleepstudy, M_11) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  ggtitle('M_11: random slope & intercepts & corr')

add_predictions(sleepstudy, M_12) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('M_12: random intercepts only')

add_predictions(sleepstudy, M_12) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  ggtitle('M_12: random intercepts only')



add_predictions(sleepstudy, M_13) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('M_13: random slopes only')

add_predictions(sleepstudy, M_13) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  ggtitle('M_13: random slopes only')


add_predictions(sleepstudy, M_14) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('M_14: random slopes & intercepts & no corr')

# Complete pooling model --------------------------------------------------

M_15 <- lm(Reaction ~ Days, data = sleepstudy)

# -------------------------------------------------------------------------


# No pooling model --------------------------------------------------------

# varying intercept & slope model, but NOT mixed effects aka multilevel
M_16 <- lm(Reaction ~ Days * Subject, data = sleepstudy)
M_16a <- lm(Reaction ~ Days + Subject + Days:Subject, data = sleepstudy)

# varying intercepts model but NOT a mixed effects aka multilevel
M_17 <- lm(Reaction ~ Days + Subject, data = sleepstudy)


# Visualize these models --------------------------------------------------

add_predictions(sleepstudy, M_16) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('M_16: varying slopes & intercepts; no multilevel model')

add_predictions(sleepstudy, M_15) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('M_15: complete pooling')


# Model comparison --------------------------------------------------------

# log likelihood of model M_11
logLik(M_11)

# log likelihood of model M_12
logLik(M_12)

# deviance of model M_11
-2 * logLik(M_11)
deviance(M_11)

# Wilks's theorem to do a null hypothesis test using chi ^ 2
anova(M_12, M_11) # compare model 11 to 12

anova(M_14, M_11) # test of corr between intercepts and slopes

anova(M_14, M_12) # test of random slopes



# Multilevel models with nested groups ------------------------------------

classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr06/main/data/classroom.csv")

ggplot(classroom_df,
       aes(x = ses, y = mathscore)
) + geom_point() +
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~schoolid)
       
