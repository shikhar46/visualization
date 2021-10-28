rm(list = ls())

##### Section 7: Experiments ######
library(tidyverse)
library(ggrepel)
library(patchwork)
library(estimatr)

# Create mock data based on actual results from an experiment
library(fabricatr)

exp_control = fabricate(N = 1255,
                        goodbad_spread = draw_categorical(
                          N = N,
                          prob = c(0.4924303, 0.1952191, 0.1075697, 0.1466135, 0.05816733),
                          category_labels = c(0, 0.25, 0.5, 0.75, 1)
                        )
) %>%
  mutate(Z=0)

exp_treat = fabricate(N = 1255,
                      goodbad_spread = draw_categorical(
                        N = N,
                        prob = c(0.12191235, 0.20796813, 0.19601594, 0.33227092, 0.14183267),
                        category_labels = c(0, 0.25, 0.5, 0.75, 1)
                      )
) %>%
  mutate(Z=1)


exp_dat = bind_rows(exp_control, exp_treat) %>%
  mutate(Condition = case_when(Z == 0 ~ "Negative", Z == 1 ~ "Positive"))

exp_dat_means <- exp_dat %>%
  group_by(Condition) %>%
  do(lm_robust(goodbad_spread ~ 1, data = .) %>% tidy()) %>%
  rename(goodbad_spread = estimate)

# Visualizing this data 

fig1 <- ggplot(data = exp_dat,
               aes(x = Condition,
                   y = goodbad_spread,
                   color = Condition)) +
  #geom_point(position = "jitter",alpha = 0.2) +
  geom_count(alpha = 0.2) +
  geom_point(data = exp_dat_means, size = 1.5) +
  geom_errorbar(data = exp_dat_means,
                aes(ymin = conf.low, ymax = conf.high),
                width = 0.1) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  ylab("Would you say the U.S. is doing a good job\or a bad job of controlling the spread of COVID-19?") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Now obtaining standard errors using the bootstrap method

output = c()

for(i in 1:5000) {
  sample_data = sample_n(exp_dat, replace = TRUE, size = nrow(exp_dat))
  results = sample_data %>%
    group_by(Condition) %>%
    summarise(Avg_Boot = mean(goodbad_spread)) %>%
    mutate(iteration = i)
  output = bind_rows(output, results)
}


output_means <- output %>%
  group_by(Condition) %>%
  summarise(
    goodbad_spread = mean(Avg_Boot),
    SE = sd(Avg_Boot),
    conf.low = quantile(Avg_Boot, probs = 0.025),
    conf.high = quantile(Avg_Boot, probs = 0.975)
  )

fig2 <- ggplot(data = output,
               aes(x = Avg_Boot,
                   group = Condition)) +
  geom_histogram(alpha = 0.5, aes(fill = Condition)) +
  geom_vline(data = output_means,
             aes(xintercept = goodbad_spread),
             linetype = "dashed") +
  geom_vline(data = output_means, aes(xintercept = conf.low, color = Condition)) +
  geom_vline(data = output_means, aes(xintercept = conf.high, color = Condition)) +
  geom_label_repel(data = output_means, aes(
    x = goodbad_spread,
    y = 2500,
    label = round(goodbad_spread, 1)
  )) +
  theme_bw() +
  theme(legend.position = "none")

fig2


fig3 <- ggplot(data = exp_dat,
               aes(x = Condition,
                   y = goodbad_spread,
                   color = Condition)) +
  #geom_point(position = "jitter",alpha = 0.2) +
  geom_count(alpha = 0.2) +
  geom_point(data = output_means, size = 1.5) +
  geom_errorbar(data = output_means,
                aes(ymin = conf.low, ymax = conf.high),
                width = 0.1) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_bw() +
  ylab("Would you say the U.S. is doing a good job\or a bad job of controlling the spread of COVID-19?") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig =  (fig1 + fig3)

### EXERCISE

# Pair/group up
# Make two graphs that differ along a perception dimension
# Do not do axis tricks - we already know that these work
# Do not be subtle in your manipulation of perception
# Write *a* question that measures beliefs after seeing one of the two graphs
# The question should elicit a *quantitaively* measured response
# Avoid binary responses so that we can have a greater variation
# The question should not be double-barreled



