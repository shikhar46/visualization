#### Data Cleaning Script ######

library(tidyverse)

dat <- read_csv("ab_test.csv") %>% 
  slice(-c(1:2)) %>%
  dplyr::select(Q146, Alex_DO)


unique(dat$Q146)

dat <- dat %>%
  mutate(
    outcome = case_when(
      Q146 == "Partisanship has a large causal effect on evaluations of Donald Trump" ~ 1,
      Q146 == "Evaluation of Donald Trump has a large causal effect on partisanship" ~ -1,
      Q146 == "Partisanship and evaluations of Donald Trump are not causally related" ~ 0
    ),
    treat = grepl("Q147", x = Alex_DO)*1
  ) %>%
  dplyr::select(outcome, treat)

save(file = "ab_cleaned.rda", dat)


