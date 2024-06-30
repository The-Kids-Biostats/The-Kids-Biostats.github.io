library(tidyverse)
library(simstudy)
library(data.table)
library(lme4)
library(jtools)

theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(family = "BarlowSemiCondensed-Bold"),
          axis.title = element_text(family = "BarlowSemiCondensed-Medium"),
          strip.text = element_text(family = "BarlowSemiCondensed-Bold",
                                    size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA))
}

# Initialize random seed for reproducibility
set.seed(2024)

# Generate Treatment group data
dtTreatPre <- data.table(id = 1:204, group = "Treatment", time = sample(0:28, 204, replace = TRUE), 
                         Out = rnorm(204, mean = 20, sd = 7.5), timepoint = "Pre")
dtTreatPost <- data.table(id = 1:204, group = "Treatment", time = sample(150:210, 204, replace = TRUE), 
                          Out = dtTreatPre$Out + rnorm(204, mean = 50, sd = 7.5), timepoint = "Post")

# Generate Control group data
dtControlPre <- data.table(id = 205:401, group = "Control", time = sample(0:28, 197, replace = TRUE), 
                           Out = pmax(rnorm(197, mean = 10, sd = 5), 0), timepoint = "Pre")
dtControlPost <- data.table(id = 205:401, group = "Control", time = sample(150:210, 197, replace = TRUE), 
                            Out = dtControlPre$Out + rnorm(197, mean = 20, sd = 5), timepoint = "Post")

# Combine data
dtCombined <- rbind(dtTreatPre, dtTreatPost, dtControlPre, dtControlPost)

# Get a visual of the data
dtCombined %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(timepoint, Out, group = group, colour = group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.25)) +
  geom_smooth(position = position_dodge(width = 0.25), 
              method = "lm", formula= y ~ x) +
  theme_clean() +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(y = "Outcome", x = "Timepoint",
       title = "aadfsaad", colour = "Group")

# Get a visual of the data
dtCombined %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(timepoint, Out)) +#, group = group, colour = group)) +
  # geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.25)) +
  # geom_smooth(position = position_dodge(width = 0.25), 
  #             method = "lm", formula= y ~ x) +
  # theme_clean() +
  # scale_colour_viridis_d(option = "plasma", end = 0.85) +
  # labs(y = "Outcome", x = "Timepoint",
  #      title = "aadfsaad", colour = "Group") +
  coord_flip()+
  geom_bracket(
    xmin = 1, xmax = 1, 
    y.position = 20, label = c("***"),
    tip.length = 2, coord.flip = T)
  


# View the numbers we're trying to calculate with our model
dtCombined %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  group_by(group, timepoint) %>% 
  summarise(`Group Means` = mean(Out),
            `Group SDs` = sd(Out))

# Run a basic ANCOVA model
dtCombined %>% tibble %>% 
  pivot_wider(id_cols = c(id, group), names_from = timepoint, values_from = c(Out, time)) %>% 
  lm(Out_Post ~ Out_Pre + group, data = .) %>% 
  export_summs(error_pos = "right", error_format = "[{conf.low}, {conf.high}]")

# Run a basic mixed effects model
dtCombined %>% tibble %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  lmer(Out ~ group*timepoint + (1 | id), data = .) %>% 
  export_summs(error_pos = "right", error_format = "[{conf.low}, {conf.high}]")

# A further visual of the data
# dtCombined <- rbind(dtTreatPre, dtTreatPost, dtControlPre, dtControlPost, fill = TRUE)
dtCombined <- dtCombined %>% tibble %>% 
  mutate(timepoint = as.numeric(factor(timepoint, levels = c("Pre", "Post"))) - 1,
         group = as.numeric(factor(group, levels = c("Control", "Treatment"))) -1) %>% 
  mutate(jittered_pos = jitter(timepoint, amount = 0.05),
         dodge_pos = group * 0.25 )#- 0.25/2)

dtCombined %>% 
  mutate(#timepoint = as.numeric(factor(timepoint, levels = c("Pre", "Post"))) - 1,
       group = factor(group, labels = c("Control", "Treatment"))) %>% 
ggplot(aes(x = dodge_pos + jittered_pos, y = Out, group = id, colour = group)) +
  geom_point(aes(x = dodge_pos + jittered_pos), 
             position = position_dodge(width = 0.25)) +
  geom_line(aes(x = dodge_pos + jittered_pos), alpha = 0.2) +
  scale_x_continuous(breaks = 0:1, labels = c("Pre", "Post")) +
  theme_clean() +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(y = "Outcome", x = "Timepoint",
       title = "aadfsaad", colour = "Group")# Adjust based on your actual levels and needs

# A further visual of the data

dtCombined <- rbind(dtTreatPre %>% 
                      mutate(Out = sort(Out)), dtTreatPost %>% 
                      mutate(Out = sort(Out)), dtControlPre %>% 
                      mutate(Out = sort(Out)), dtControlPost %>% 
                      mutate(Out = sort(Out)), fill = TRUE)
dtCombined <- dtCombined %>% tibble %>% 
  mutate(timepoint = as.numeric(factor(timepoint, levels = c("Pre", "Post"))) - 1,
         group = as.numeric(factor(group, levels = c("Control", "Treatment"))) -1) %>% 
  mutate(jittered_pos = jitter(timepoint, amount = 0.05),
         dodge_pos = group * 0.25 )#- 0.25/2)

dtCombined %>% 
  mutate(#timepoint = as.numeric(factor(timepoint, levels = c("Pre", "Post"))) - 1,
    group = factor(group, labels = c("Control", "Treatment"))) %>% 
  ggplot(aes(x = dodge_pos + jittered_pos, y = Out, group = id, colour = group)) +
  geom_point(aes(x = dodge_pos + jittered_pos), 
             position = position_dodge(width = 0.25)) +
  geom_line(aes(x = dodge_pos + jittered_pos), alpha = 0.2) +
  scale_x_continuous(breaks = 0:1, labels = c("Pre", "Post")) # Adjust based on your actual levels and needs

# Run a basic mixed effects model
dtCombined %>% tibble %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  lmer(Out ~ group*timepoint + (1 | id), data = .) -> mod
mod %>% 
  export_summs(error_pos = "right", error_format = "[{conf.low}, {conf.high}]")



