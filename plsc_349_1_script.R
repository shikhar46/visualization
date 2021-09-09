####### Section 1: The Women's Table #########

# Never save work space, always start an R session by clearing out any items in the global environment
rm(list=ls())
# Another way is to:
# R Studio >> Preferences >> General >> Unselect certain automatic preferences

# Load packages that we will need
library("tidyverse")

# Check your working directory
getwd()

# Set your working directory to the problem set folder 

# Option 1: Session >> Set Working Directory >> To Source File Location\
# Option 2: (if you know the file path)
setwd("/Users/shikharsingh/Dropbox/Yale/Teaching/PLSC349_DataVisualization/Section_1")

#setwd("/Users/padmanabhan/Desktop/yaleclasses/fall 2021/dataviz/sec1")

# Lets load a dataset on the race/ethnicity and gender of Yale students over time

yale_dat <- read_csv("yale_race_gender.csv")

# Examine the data set
# How many rows and columns does it have?
dim(yale_dat)
# What are the names of variables?
names(yale_dat)
# What type is each column?
glimpse(yale_dat)
# How many unique levels does gender or race have?
yale_dat %>% 
  group_by(Gender) %>% 
  summarise(n_rows = n(), 
            N = sum(Headcount))

yale_dat %>% 
  group_by(RaceEthn) %>% 
  summarise(n_rows = n(), 
            N = sum(Headcount))
# How is n_rows different from N?

# What happens when a column name has spaces

yale_dat <- 
  yale_dat %>%
  mutate(`Men or Women` = Gender)

yale_dat %>% 
  group_by(Men or Women) %>% 
  summarise(n_rows = n())
# doesn't work

yale_dat %>% 
  group_by(`Men or Women`) %>% 
  summarise(n_rows = n())
# does work

# How can you rename columns?

yale_dat <-
  yale_dat %>%
  rename(race = RaceEthn)

head(yale_dat)

# How can we transform the year column? .

yale_dat_1 <- 
  yale_dat %>% 
  separate(Year, c("Year","Remove"))

head(yale_dat_1)


yale_dat <- yale_dat %>% 
  separate(Year, c("Year", NA))

head(yale_dat)

# How can we re-code columns?

yale_dat <- yale_dat %>%
  mutate(Women = case_when(Gender == "Women" ~ 1,
                           Gender == "Men" ~ 0))

with(yale_dat, table(Gender, Women))
# Is this the number of men and women at Yale? What is this table telling us?

# Visualization

# Scatter plot or Time Series 

# Gender by year 

d_gender_overtime <-
  yale_dat %>%
  group_by(Gender, Year) %>%
  summarise(N = sum(Headcount))

head(d_gender_overtime)

# What if the data were in "wide" format

d_wide_format <-
  d_gender_overtime %>%
  pivot_wider(names_from = Gender,
              values_from = N)

head(d_wide_format)

# What if you wanted to merge some other data with this

d_international_overtime <-
  yale_dat %>%
  filter(race == "International") %>%
  group_by(race, Year) %>%
  summarise(N = sum(Headcount)) %>%
  pivot_wider(names_from = race,
              values_from = N)

d_merged <-
  left_join(
    d_gender_overtime,
    d_international_overtime,
    by = "Year"
  )

# How do we go back to "long" format?

d_long_format <-
  d_wide_format %>%
  pivot_longer(
    cols = c("Men","Women"),
    names_to = "Gender",
    values_to = "N"
  )

head(d_long_format)

# Scatter/Line plot

fig1 <-
  ggplot(d_gender_overtime, 
         aes(x = Year, 
             y = N, 
             color = Gender
             )
         ) +
  geom_point() +
  geom_line() +  # remove color=Gender and see how this changes
  ylim(c(2500, 7500)) + # demonstrate how changing this changes the visualization
  xlab("Year") +
  ylab("Student Count") +
  ggtitle("Yale Students by Gender",
          subtitle = "(1984 to 2014)") +
  theme_bw()

# Fixing the x-axis labels
fig2 <-
  fig1 + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# How about summary statistics, overlaying a regression line? 

fig3 <-
  fig2 + geom_smooth()

# Why did this not work? Because "Year" is a character not a numeric variable

d_gender_overtime <- 
  d_gender_overtime %>%
  mutate(
    Year = as.numeric(Year)
  )

fig4 <-
  ggplot(d_gender_overtime, aes(x = Year, y = N, color = Gender)) +
  geom_point() +
  geom_smooth(alpha = 0.1) + # explain alpha (0 = translucent, 1 = not translucent)
  ylim(c(2500, 7500)) + # demonstrate how changing this changes the visualization
  scale_x_continuous(breaks = c(1980,1985,1990,1995,2000,2005,2010,2015,2020)) + # imp when year is numeric
  xlab("Year") +
  ylab("Student Count") +
  ggtitle("Yale Students by Gender",
          subtitle = "(1984 to 2014)") +
  theme_bw()

ggsave(
  plot = fig4,
  filename = "name_pset1_fig1.png", #lets follow a naming protocol
  height = 4,
  width = 6,
  units = "in"
)

# Barplot

fig5 <-
  ggplot(d_gender_overtime, aes(Year, N, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("gray80", "steelblue1")) +
  xlab("Year") +
  ylab("Student Count") +
  ggtitle("Bar Chart of Yale Students by Gender") +
  theme_bw()

# Facet Wrapping

fig6 = ggplot(d_gender_overtime, aes(Year,N)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill = "gray60") +
  xlab("Year") + 
  ylab("Student Count") +
  ggtitle("Bar Chart of Yale Students by Gender") +
  theme_bw() +
  facet_wrap(~Gender)

# Some feedback about homework

# Homework is graded on a 0 to 2 scale:
# 0 = did not submit or complete, 
# 1 = partially completed with some engagement with the questions,
# 2 = completed homework and engagement with questions
# HW graded on effort not correctness of answer. There are many correct answers

# Folder should follow this naming format: name_pset_number

# All the plots referenced in the word document must be in the document

# Remember to set working directory. 
## A good way to do this is to save the R script in the same folder, 
## and R Studio will automatically make that the working directory.

# A good practice: comment/ explain your code, always useful for future

# What are office hours for. 
# An opportunity to clarify one-on-one any doubts or questions
# An opportunity to discuss how we can create a more inclusive, participatory environment in section



