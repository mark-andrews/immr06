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

# Random effects normal ---------------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr06/main/data/alcohol.csv")

# Normal model for year by year variability in per capita alcohol consumption in Russia
M_4 <- lm(alcohol ~ 1, data = alcohol_df %>% filter(country == 'Russia'))
coef(M_4)
sigma(M_4)

# multilevel normal model
M_5 <- lmer(alcohol ~ 1 + (1|country), data = alcohol_df)

summary(M_5)

confint(M_5)


# the estimates of the country level effects
coef(M_5)

round(coef(M_5)$country, 2) # rounded to 2 dp


library(lmerTest)

head(ranef(M_5)$country) # zeta in the notation

head(coef(M_5)$country)  # mu, which is also theta + zeta

head(fixef(M_5) + ranef(M_5)$country) # theta + zeta

# the epsilons
residuals(M_5)

# Intra class correlation -------------------------------------------------

sd_terms <- as.data.frame(VarCorr(M_5))[,5]
sd_terms[1]^2 / sum(sd_terms^2)

var_terms <- as.data.frame(VarCorr(M_5))[,4]
var_terms[1] / sum(var_terms)

# sample variance of outcome variable
var(alcohol_df$alcohol)
# estimate of population variance
sum(var_terms)



# Visualize sleep data ----------------------------------------------------

ggplot(sleepstudy,
       aes(x = Days, y = Reaction)
) + geom_point()

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point()

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + facet_wrap(~Subject)



# Multilevel linear model, aka linear mixed effects -----------------------

#equivalently: Reaction ~ 1 + Days + (1 + Days|Subject)
M_6 <- lmer(Reaction ~ Days + (Days|Subject), 
            data = sleepstudy)

summary(M_6)

coef(M_6)
ranef(M_6)
residuals(M_6)

# random intercept only model
M_7 <- lmer(Reaction ~ 1 + Days + (1|Subject), 
            data = sleepstudy)

summary(M_7)

# random slopes only model
M_8 <- lmer(Reaction ~ 1 + Days + (0 + Days|Subject), 
            data = sleepstudy)

summary(M_8)

# random slopes and random intercepts, but no correlation
M_9 <- lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject) , 
            data = sleepstudy)
summary(M_9)

M_10 <- lmer(Reaction ~ Days + (Days||Subject), 
            data = sleepstudy)
summary(M_10)


# Plot random effects -----------------------------------------------------

library(sjPlot)
plot_model(M_6, 're', sort.est = 'Days')

