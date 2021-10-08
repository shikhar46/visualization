rm(list = ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)

setwd("~/Dropbox/PLSC349_DataVisualization/Section_5")

# Loading dataset
v_dem <- readRDS("V-Dem-CY-Core-v11.1.rds")

### Countries that were democracies in 1990 from 1900 onwards

country_names <- v_dem %>%
  filter(v2x_polyarchy >= 0.5 & year == 1990) %>%
  select(country_name) %>%
  as_vector()

#creating data frame for  democracies in 1990 from 1900

democracies_in_1990 <- v_dem %>%
  filter(country_name %in% country_names) %>%
  filter(year >= 1900)

# Data for Figure 1:

democracies_sum <- democracies_in_1990 %>%
  group_by(year) %>%
  summarize(
    `Clean elections` = mean(v2xel_frefair, na.rm = TRUE),
    `Liberal component` = mean(v2x_liberal, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(`Clean elections`, `Liberal component`),
    names_to = "index",
    values_to = "value"
  )

# Figure 1
plot_lib_elections_dem <-
  ggplot(democracies_sum,
         aes(x = year,
             y = value,
             color = index)) +
  geom_point()  +
  geom_line() +
  scale_y_continuous(breaks = seq(0 , 1, 0.02)) +
  scale_x_continuous(breaks = seq(1900 , 2025, 5)) +
  xlab("Year") +
  ylab("V-Dem Indices") +
  ggtitle("V-Dem, Democracies - yearly average.
          Liberal Component versus Clean Elections") +
  theme(
    legend.position = c(0.25, 0.75),
    axis.text.x = element_text(angle = 45),
    panel.grid.major = element_line(colour = "gray80")
  )
plot_lib_elections_dem

# Cleaner Version of Figure 1

plot_lib_elections_dem2 <-
  ggplot(democracies_sum,
         aes(x = year,
             y = value,
             color = index)) +
  geom_line()   +
  geom_label_repel(data = filter(democracies_sum, year == 1945),
                   aes(label = index)) +
  scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  xlab("Year") +
  ylab("V-Dem Indices") +
  ggtitle("V-Dem, Democracies - yearly average",
          subtitle = "Liberal Component versus Clean Elections") +
  theme_classic() +  #try other themes
  theme(legend.position = "none")
plot_lib_elections_dem2

# Going into the theme function

plot_lib_elections_dem3 <-
  ggplot(democracies_sum,
         aes(x = year,
             y = value,
             color = index)) +
  geom_line() +
  theme(
    panel.grid = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(), 
    axis.title = element_blank(),
    #axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    axis.text = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(
      size = 0.5,
      colour = "black",
      linetype = 1
    ),
    legend.position = "none"
  )

# TASK:
## Use the data set code we have have below and
## create a "high data ink" figure
## We have produced a "low data ink" version for reference

southasia <- v_dem %>%
  filter(year >= 1947) %>%
  filter(country_name %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka")) %>%
  select(year, country_name, v2xel_frefair) %>%
  group_by(year, country_name) %>%
  rename(`Clean elections` = v2xel_frefair) %>%
  mutate(country_name = as.factor(country_name))

sasia_low_data_ink <-
  southasia %>%
  ggplot(aes(x = year,
             y = `Clean elections`,
             color = country_name)) +
  geom_line() +
  geom_label_repel(data = filter(southasia, year == 2020),
                   aes(label = country_name)) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_color_manual(values = c("Orange", "Green", "Blue", "Red")) +
  ylab("Clean Elections Index") +
  labs(title = "South Asia: Clean Elections Index",
       subtitle = "Source: V-Dem") +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title =
                                                                   "")) +
  theme_classic() +
  theme(legend.position = "none")
sasia_low_data_ink

# Focusing on one country

india <- v_dem %>%
  filter(year >= 1947) %>%
  filter(country_name == "India")  %>%
  select(year, v2xel_frefair, v2x_liberal) %>%
  rename(`clean elections` = v2xel_frefair,
         `liberal component` = v2x_liberal) %>%
  pivot_longer(
    cols = c(`clean elections`, `liberal component`),
    names_to = "index",
    values_to = "value"
  )

plot_india <- india %>%
  ggplot(aes(
    x = year,
    y = value,
    linetype = index,
    color = index
  )) +
  geom_line() +
  geom_vline(xintercept = 2014,
             linetype = "longdash") +
  annotate(
    x = 2008 ,
    y = 1,
    label = "BJP Elected",
    color = "black",
    geom = "label"
  ) +
  scale_x_continuous(breaks = c(seq(1940, 2020, 20), 2014)) +
  xlab("Year") +
  ylab("V-Dem Indices") +
  labs(title = "India: Clean Elections Index\nand Liberal Component Index",
       subtitle = "Source: V-Dem") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.5, 0.4),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(
      size = 0.5,
      colour = "black",
      linetype = 1
    )
  )
plot_india