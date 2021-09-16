####### Section 2 #########

### Revision of Key Concepts ###

# From last week: group_by, pivot_wider, pivot_longer, left_join
# This week: filter, case_when, fct_reorder


### Introductory Steps (from last week) ###

rm(list = ls())

library(tidyverse)
library(ggrepel)
library(patchwork)

setwd("~/Dropbox/PLSC349_DataVisualization/Section_2")
#setwd("/Users/shikharsingh/Dropbox/Yale/Teaching/PLSC349_DataVisualization/Section_2")
# (Alternatively: Session >> Set Working Directory >> To Source File Location)

yale_dat <- read_csv("yale_race_gender.csv")

yale_dat <-
  yale_dat %>%
  separate(Year, c("Year", NA)) %>%
  rename(race = RaceEthn) %>%
  mutate(Year = as.numeric(Year),
         race = as.factor(race))

head(yale_dat)

### Reordering Factors ####

## Lets take last week's data set and focus on the race/ethnicity variable
## How many levels or unique values does this variable have?

with(yale_dat, fct_unique(race))
with(yale_dat, table(race,Gender))

## How does the racial composition of students vary over time?
# We might be interested in other summaries, for example
# the average in each school, or the minimum and maximum

d_race_overtime <-
  yale_dat %>%
  filter(race != "International") %>%
  mutate(
    race = case_when(
      race == "Black or African American" ~ "Black/Af.Am.",
      race == "Hispanic of Any Race" ~ "Hispanic",
      race == "White" ~ "White",
      TRUE ~ "Other")
    ) %>%
  group_by(race, Year) %>%
  summarise(
    total = sum(Headcount),
    maximum = max(Headcount),
    minimum = min(Headcount),
    average = mean(Headcount)
  )

head(d_race_overtime)

# Line graph

plot1 <-
  ggplot(d_race_overtime, aes(
    x = Year,
    y = log(total),
    color = race
  )) +
  geom_line() +
  ylim(c(5, 10)) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  xlab("Year") +
  ylab("log(Student Count)") +
  ggtitle("Yale Students by Race",
          subtitle = "(1984 to 2014)") +
  theme_bw() +
  theme(
    legend.position = c(0.8, 0.15),
    legend.text = element_text(size = 7),
    legend.spacing.y = unit(0.08, 'cm')
  )

plot1

# What if you want the legend to appear at the end of the line?
# Using geom_label_repel

plot2 <-
  ggplot(d_race_overtime, aes(
    x = Year,
    y = log(total),
    color = race
  )) +
  geom_line() +
  geom_label_repel(data = filter(d_race_overtime, Year == 2020),
                   aes(label = race),
                   nudge_y = 0.2) +
  ylim(c(5, 10)) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  xlab("Year") +
  ylab("log(Student Count)") +
  ggtitle("Yale Students by Race",
          subtitle = "(1984 to 2014)") +
  theme_bw() +
  theme(legend.position = "none")

plot2

## Creating Bar Charts, Reordering Factor Levels

d_race_1995 <- d_race_overtime %>%
  filter(Year == 1995) %>%
  ungroup()

plot3 <-
  ggplot(d_race_1995, aes(x = race, y = total, fill = race)) +
  geom_bar(stat = "identity") +
  xlab("Group") +
  ylab("Student Count") +
  ggtitle("Yale Students by Race (1995)") +
  theme_bw() +
  theme(legend.position = "bottom")

plot3

# What if we want it in descending order?

d_race_1995 <-
  d_race_1995 %>%
  mutate(race = fct_reorder(race, desc(total)))

plot4 <-
  ggplot(d_race_1995, aes(x = race, y = total, fill = race)) +
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("Student Count") +
  ggtitle("Yale Students by Race (1995)") +
  theme_bw() +
  theme(legend.position = "none") 

plot4

# How about manually setting factor levels?

d_race_1995 <-
  d_race_1995 %>%
  mutate(race = fct_relevel(race,
                            "Other",
                            "Black/Af.Am.",
                            "Hispanic",
                            "White"))

plot5 <-
  ggplot(d_race_1995,
         aes(y = total, x = race, fill = race)) +
  geom_bar(stat = "identity") +
  xlab("Race") +
  ylab("Student Count") +
  ggtitle("Yale Students by Race (1995)") +
  theme_bw()+
  theme(legend.position = "none")

plot5


### Long v. Wide Formats ###

# Using the Long Format

agnes_dat <- read_csv("agnes_cleaned.csv")

plot6 <- 
  ggplot(agnes_dat,
         aes(x = X, y = Y, group = line_id)) + # see after removing group
  geom_line() + 
  xlab(" ") +
  ylab(" ") +
  theme_minimal() 

plot6

# Making the same figure in wide format 

agnes_wide_format <-
  agnes_dat %>%
  group_by(line_id) %>%
  arrange((X)) %>%
  mutate(col_name = row_number()) %>%
  pivot_wider(names_from = "col_name",
              id_cols = line_id,
              values_from = c(X,Y))

plot7 <- 
  ggplot(agnes_wide_format,
         aes(
           x = X_1, 
           y = Y_1, 
           xend = X_2, 
           yend = Y_2)) +
  geom_segment() +
  theme_minimal() +
  xlab(" ") +
  ylab(" ")

plot7

# LIE FACTOR 

lie_dat <- tibble(
  country = c("Latvia","Australia","Scotland","Peru","South Africa", "India"),
  height = c(5.5,5.4,5.4,5.4,5.2, 5),
  measurement = c(325,270,270,270,190,85))

lie_plot1 <- 
  lie_dat %>%
  mutate(country = fct_reorder(country, desc(height))) %>%
  ggplot( aes(x = country, y = height)) + 
          geom_bar(stat = "identity") +
  ggtitle("Not Distorted") +
  theme_bw()

lie_plot2 <- 
  lie_dat %>%
  mutate(country = fct_reorder(country, desc(measurement))) %>%
  ggplot( aes(x = country, y = measurement)) + 
  geom_bar(stat = "identity") +
  ggtitle("Distorted") +
  theme_bw()

lie_plot1 + lie_plot2

# What is the lie factor?

#lie factor = Percent change in the graph \ Percent change in the data

percent_change_data <- (5.5 - 5.1) / 5.1
percent_change_graph <- (325 - 85) / 85

percent_change_graph/percent_change_data

