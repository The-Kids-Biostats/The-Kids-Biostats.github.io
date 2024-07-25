# Load the necessary libraries
library(tidyverse)
library(lme4)
library(parameters)
library(modelbased); library(merTools)
library(ggeffects); library(emmeans)

# Load the data from CSV file (you would replace 'synthetic_data.csv' with the path to the file you downloaded)
#data <- read.csv("/Users/mcooper/Documents/GitHub/another_between_group_plot/synthetic_data.csv")
data <- read.csv("posts/pre_post/synthetic_data.csv")

# Convert Time to a factor to ensure the order of the levels is maintained in the plot
data$Time <- factor(data$Time, levels = c('0 months', '9 months', '24 months'))

# Plot using ggplot2
ggplot(data, aes(x = Time, y = Metric, group = Group, color = Group)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  stat_summary(geom = "errorbar", aes(ymin = Metric - sd(Metric), ymax = Metric + sd(Metric)), width = 0.2,
                position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(title = 'Metric Over Time by Group',
       x = 'Time (months)',
       y = 'Metric (units)',
       color = 'Group') +
  scale_color_manual(values = c('Control' = 'black', 'Treatment' = 'red')) # Assuming you want to keep black and red colors

ggplot(data, aes(x = Time, y = Metric, group = Group, color = Group)) +
  stat_summary(geom = "line", position = position_dodge(width = 0.5)) +
  #geom_point(position = position_dodge(width = 0.5)) +
  stat_summary(geom = "errorbar", aes(ymin = Metric - sd(Metric), ymax = Metric + sd(Metric)), width = 0.2,
               position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(title = 'Metric Over Time by Group',
       x = 'Time (months)',
       y = 'Metric (units)',
       color = 'Group') +
  scale_color_manual(values = c('Control' = 'black', 'Treatment' = 'red')) # Assuming you want to keep black and red colors


OR

data$Time <- factor(data$Time, levels = c('0 months', '9 months', '24 months'))

# Calculate the means and standard deviations
summary <- data %>%
  group_by(Time, Group) %>%
  summarise(
    Mean = mean(Metric),
    SD = sd(Metric),
    lower = Mean - SD, # assuming error bars represent 1 SD from the mean
    upper = Mean + SD
  )

# Plot using ggplot2
ggplot(summary, aes(x = Time, y = Mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_minimal() +
  labs(title = 'Metric Over Time by Group',
       x = 'Time (months)',
       y = 'Metric (units)',
       color = 'Group') +
  scale_color_manual(values = c('Control' = 'black', 'Treatment' = 'red')) # Assuming you want to keep black and red colors


tdata <- data %>% 
  filter(Time != "0 months")

tdata %>% 
  group_by(Time, Group) %>% 
  summarise(mean = mean(Metric))

mod <- lm(Metric ~ Time*Group, data = tdata)
model_parameters(mod, digits = 3)


tdata$id <- c(sample(1:30),sample(1:30),sample(31:60),sample(31:60))
mod <- lmer(Metric ~ Time*Group + (1 | id), data = tdata)
model_parameters(mod, digits = 3)
