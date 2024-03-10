
# Load packages
library(tibble)
library(dplyr)
library(ggplot2)

# Read in tab separated data
data = read.csv('C:/Users/Nox/Desktop/PIER/2014.tsv', sep = "\t")

# Add categorical variables to new column Season
data$Season = ifelse(data$Month %in% c("1", "2", "3"), "Winter", 
                     ifelse(data$Month %in% c("4", "5", "6"), "Spring",
                            ifelse(data$Month %in% c("7", "8", "9"), "Summer",
                                   ifelse(data$Month %in% c("10", "11", "12"), "Fall", "NA"))))



### Depth vs. Temperature scatter plot 2014 data
ggplot(data, aes(x = data$Depth..salt.water..m...lat...0.00, y = data$Temperature..ITS.90..deg.C.)) +
  geom_point() +
  labs(x = 'Depth (m)', y = 'Temperature (C)')

### https://chat.openai.com/share/87aeec9f-b352-43cb-9c6b-3e1b8cbee9f7

# Depth vs. Temperature scatter plot 2014 data by seasons
season_colors = c('Winter' = 'royalblue3', 'Spring' = 'chartreuse3',
                  'Summer' = 'firebrick', 'Fall' = 'goldenrod')

ggplot(data, aes(x = data$Depth..salt.water..m...lat...0.00, 
                 y = data$Temperature..ITS.90..deg.C.,
                 color = factor(data$Season))) +
  geom_point() +
  labs(x = 'Depth (m)', y = 'Temperature °C', color = 'Seasons') +
  scale_color_manual(values = season_colors)










### Confidence interval by season
# Create subset of data
depth = data$Depth..salt.water..m...lat...0.00
surface_data = subset(data, depth <= 60)

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









### Confidence interval by month
# Create subset of data
depth = data$Depth..salt.water..m...lat...0.00
surface_data = subset(data, depth <= 60)

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
surface_data = subset(data, depth <= 60)
# Define the order of seasons
season_order <- c('Winter', 'Spring', "Summer", "Fall")

# Reorder the "Season" variable
surface_data$Season <- factor(surface_data$Season, levels = season_order)

ggplot(surface_data, aes(x = surface_data$Season, y = surface_data$Temperature..ITS.90..deg.C., fill = Season)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", fill = "white") +
  labs(x = 'Seasons', y = 'Temperature °C') +
  scale_fill_manual(values = season_colors)

