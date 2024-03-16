### This script is for the PIER 2024 mentoring program's day 1 analysis workshop
### Author: Daniel Novoa and Dr. Jimmy Lee

### If you're having trouble with the syntax take a look at how I use chatGPT
###   to help understand code. This is an example of how using chatGPT can be
###   beneficial in your academic career. Make sure to always validate your results!
# https://chat.openai.com/share/87aeec9f-b352-43cb-9c6b-3e1b8cbee9f7

# Load packages
library(tibble)
library(dplyr)
library(ggplot2)

# Assign the data path as a string to the variable data_path
data_path = '~/Desktop/QTLrocks/PIER-2024/data.tsv'
# Read in tab separated value data
data = read.csv(data_path, sep = "\t")
# Filter the data, only keeping the data that contains "SPOT" in the column Cast
data <- data[data$Cast == "SPOT", ]

# Add categorical variables to new column Season
#   If 1,2,3 then Winter, else if 4,5,6 then Spring, etc.
data$Season = ifelse(data$Month %in% c("1", "2", "3"), "Winter", 
                     ifelse(data$Month %in% c("4", "5", "6"), "Spring",
                            ifelse(data$Month %in% c("7", "8", "9"), "Summer",
                                   ifelse(data$Month %in% c("10", "11", "12"), "Fall", "NA"))))

### Unique year data
# What is happening here? Hint: Look at line 19
dataALL = data
data2013 = data[data$Year == "2013", ]
data2014 = data[data$Year == "2014", ]
data2017 = data[data$Year == "2017", ]
data2018 = data[data$Year == "2018", ]


### Choosing what data we want to look at
observed_data = dataALL


### Depth vs. Temperature scatter plot 
ggplot(observed_data, aes(x = observed_data$Temperature..ITS.90..deg.C.,
                 y = observed_data$Depth..salt.water..m...lat...0.00)) +
  geom_point() +
  scale_y_reverse() +
  labs(x = 'Temperature (C)', y = 'Depth (m)')


### Depth vs. Temperature scatter plot by seasons
season_colors = c('Winter' = 'royalblue3', 'Spring' = 'chartreuse3',
                  'Summer' = 'firebrick', 'Fall' = 'goldenrod')

ggplot(observed_data, aes(x = observed_data$Temperature..ITS.90..deg.C., 
                 y = observed_data$Depth..salt.water..m...lat...0.00,
                 color = factor(observed_data$Season))) +
  geom_point() +
  scale_y_reverse() +
  labs(x = 'Temperature °C', y = 'Depth (m)', color = 'Seasons') +
  scale_color_manual(values = season_colors)


### Confidence interval by season
# Create subset of data
depth = observed_data$Depth..salt.water..m...lat...0.00
surface_data = subset(observed_data, depth <= 60)
# Calculate confidence intervals
surface_summary <- surface_data %>%
  group_by(Season) %>%
  summarise(
    mean = mean(Temperature..ITS.90..deg.C.),
    sd = sd(Temperature..ITS.90..deg.C.),
    n = n(),
    se = sd/sqrt(n),
    ci = 1.96*se)
# Define the order of seasons
season_order <- c('Winter', 'Spring', "Summer", "Fall")
# Reorder the "Season" variable
surface_summary$Season <- factor(surface_summary$Season, levels = season_order)
# Plot with reordered x-values
ggplot(surface_summary, aes(x = Season,
                            y = mean,
                            color = Season)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  labs(x = 'Season', y = 'Mean Temperature °C') +
  scale_color_manual(values = season_colors)


### Confidence interval by months
# Create subset of data
depth = observed_data$Depth..salt.water..m...lat...0.00
surface_data = subset(observed_data, depth <= 60)
# Calculate confidence intervals
surface_summary <- surface_data %>%
  group_by(Month) %>%
  summarise(
    mean = mean(Temperature..ITS.90..deg.C.),
    sd = sd(Temperature..ITS.90..deg.C.),
    n = n(),
    se = sd/sqrt(n),
    ci = 1.96*se)

month_labs = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
               'Oct', 'Nov', 'Dec')
# Plot with reordered x-values
ggplot(surface_summary, aes(x = Month, y = mean, color = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  labs(x = 'Month', y = 'Mean Temperature °C', color = '°C') +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), 
              se = TRUE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  scale_x_continuous(breaks = 1:12, labels = month_labs)


### Violin plot
surface_data = subset(observed_data, depth <= 60)
# Define the order of seasons
season_order <- c('Winter', 'Spring', "Summer", "Fall")
# Reorder the "Season" variable
surface_data$Season <- factor(surface_data$Season, levels = season_order)
# Plot
ggplot(surface_data, aes(x = surface_data$Season, y = surface_data$Temperature..ITS.90..deg.C., fill = Season)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", fill = "white") +
  labs(x = 'Seasons', y = 'Temperature °C') +
  scale_fill_manual(values = season_colors)


### Confidence interval by years
# Create subset of data
depth = observed_data$Depth..salt.water..m...lat...0.00
surface_data = subset(observed_data, depth <= 60)
# Calculate confidence intervals
surface_summary <- surface_data %>%
  group_by(Year) %>%
  summarise(
    mean = mean(Temperature..ITS.90..deg.C.),
    sd = sd(Temperature..ITS.90..deg.C.),
    n = n(),
    se = sd/sqrt(n),
    ci = 1.96*se)
# Plot with reordered x-values
ggplot(surface_summary, aes(x = Year, y = mean, color = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  labs(x = 'Year', y = 'Mean Temperature °C', color = '°C') +
  scale_color_gradient(low = "blue", high = "red")
