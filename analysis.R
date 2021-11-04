#### Visualize Cleaned Data #####
library(tidyverse)
library(estimatr)
library(ggrepel)

load("ab_cleaned.rda")

results <- dat %>%
  group_by(treat) %>%
  do(
    lm_robust(outcome ~ 1, data = .) %>% tidy()
  )

fig <- ggplot(results,
              aes(x = as.factor(treat), y = estimate, color = as.factor(treat))) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  xlab("Experimental Condition") +
  ylab("Outcome") +
  ylim(-1,1) +
  theme_bw() +
  theme(legend.position = "none")

fig + 
  geom_jitter(data = dat, 
              aes(x = as.factor(treat), 
                  y = outcome, 
                  fill = as.factor(treat)), 
              alpha = 0.2)
  