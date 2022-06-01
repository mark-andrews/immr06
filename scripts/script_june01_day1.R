library(tidyverse)
library(lme4)

# read in the rats data
rats_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr06/main/data/rats.csv")

rats_df <- mutate(rats_df, batch = factor(batch))

# get batch 42 data
rats_df_42 <- filter(rats_df, batch == '42')

# binomial model of probability of tumour in batch 42
M_1 <- glm(cbind(m, n-m) ~ 1, 
           data = rats_df_42,
           family = binomial())

# get the inferred probability of a tumour in batch 42
plogis(coef(M_1))

# 95% confidence interval on the estimate
plogis(confint.default(M_1))


# Binomial models for all the 71 batches ----------------------------------

M_2 <- glm(cbind(m, n -m) ~ 0 + batch,
           data = rats_df,
           family = binomial())

plogis(coef(M_2))
plogis(coef(M_2)) %>% round(3)

plogis(confint.default(M_2)) %>% round(3)


# Multilevel model aka mixed effects model of the rats data ---------------

M_3 <- glmer(cbind(m, n - m) ~ 1 + (1|batch),
             data = rats_df,
             family = binomial())

summary(M_3)

# mean of the probabilty distribution over the probability distributions
plogis(fixef(M_3))

# 95% interval of the distribution of effects in the population
plogis(c(fixef(M_3) - 2 * 0.66, fixef(M_3) + 2 * 0.66))

# estimated effects (as probabilities) in the 71 batches
plogis(coef(M_3)$batch[,1])
