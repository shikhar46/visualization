rm(list = ls())

setwd("~/Dropbox/PLSC349_DataVisualization/Section_6")

library(tidyverse)
library(plotly)
library(lubridate)
library(ggrepel)
library(patchwork)

polls <- read_csv("president_polls.csv")

polls <- 
  polls %>% 
  filter(candidate_name %in% c("Donald Trump", "Joseph R. Biden Jr.")) %>%
  dplyr::select(candidate_name, end_date, pct, pollster) %>%
  mutate(end_date = as.Date(end_date, "%m/%d/%y"),
         label_text = paste0(pollster,": ",pct))

fig1 <-
ggplot(polls, aes(end_date, pct, color = candidate_name)) +
  geom_point(aes(text=label_text), alpha = 0.1, stroke = 0) +
  scale_color_manual(values = c("red", "blue")) +
  stat_smooth() +
  theme_minimal() +
  theme(legend.position = "none")
fig1

plotly1 <- ggplotly(fig1, tooltip = "text")
plotly1

# Idea of Bootstrapping

polls_oct10 <- polls %>%
  filter(end_date == "2020-10-12")

means_oct10 <- polls_oct10 %>%
  group_by(candidate_name) %>%
  summarise(avg.pct = mean(pct))

fig2 <- ggplot(polls_oct10,
               aes(x = candidate_name, 
                   y = pct, 
                   color = candidate_name)) +
  geom_jitter(position = position_dodge2(width = 0.1)) +
  geom_point(data = means_oct10, 
             color = "black",
             aes(x = candidate_name, y = avg.pct)) +
  geom_label_repel(data = means_oct10,
             aes(x = candidate_name, y = avg.pct, label = round(avg.pct,2))) +
  theme_minimal()+
  theme(legend.position = "none")
fig2

# Calculating Uncertainty Around Estimates
# Bootstrapping

polls_oct10_wider <- polls_oct10 %>%
  dplyr::select(candidate_name, pct,pollster) %>%
  group_by(candidate_name) %>%
  mutate(ID = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = ID,
    names_from = "candidate_name",
    values_from = pct
  ) %>%
  rename("Biden" = "Joseph R. Biden Jr.",
         "Trump" = "Donald Trump")

output = c()

for(i in 1:10000){
  boot_dat <- sample_n(polls_oct10_wider, 
                       replace = TRUE, 
                       size = nrow(polls_oct10_wider))
  results <- boot_dat %>%
    summarise(Biden = mean(Biden),
              Trump = mean(Trump)) %>%
    mutate(iteration = i)
  output = bind_rows(output, results)
}

output <- output %>%
  pivot_longer(
    cols = c("Biden","Trump"),
    names_to = "Candidate",
    values_to = "Voteshare"
  )

output_means <- output %>%
  group_by(Candidate) %>%
  summarise(Average = mean(Voteshare),
            SE = sd(Voteshare),
            conf.low = quantile(Voteshare, probs = 0.025),
            conf.high = quantile(Voteshare, probs = 0.975))

fig3 <- ggplot(data = output,
               aes(x = Voteshare,
                   group = Candidate)) +
  geom_histogram(alpha = 0.5, aes(fill = Candidate)) +
  geom_vline(data = output_means, aes(xintercept = Average), linetype = "dashed") +
  geom_vline(data = output_means, aes(xintercept = conf.low, color = Candidate)) + 
  geom_vline(data = output_means, aes(xintercept = conf.high, color = Candidate)) + 
  geom_label_repel(data = output_means, aes(x = Average, y= 1500, label = round(Average,1))) +
  scale_color_manual(values = c("blue","red")) + 
  scale_fill_manual(values = c("blue","red")) +
  xlim(c(40,55)) +
  ggthemes::theme_clean()+
  theme(legend.position = "none")
fig3

fig4 <- ggplot(data = output_means,
               aes(x = Average, y = Candidate, color = Candidate)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  scale_color_manual(values = c("blue","red")) + 
  xlim(c(40,55)) +
  ggthemes::theme_clean()+
  theme(legend.position = "none")

fig3/fig4


