library(simstudy)
library(data.table)
library(lme4)
library(jtools)
library(ggbrace)
library(gtsummary)
library(tidyverse)

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
dat <- rbind(dtTreatPre, dtTreatPost, dtControlPre, dtControlPost)

# Table the data
dat %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  select(group, timepoint, Out) %>% 
  tbl_strata(strata = group,
             .tbl_fun =
               ~ .x %>%
               tbl_summary(by = timepoint, missing = "no",
                           statistic = all_continuous() ~ c("{mean} ({sd})  \n[{N_obs}]"),
                           digits = all_continuous() ~ c(2,1,0)) %>% 
               modify_header(label = "**Variable**",
                             all_stat_cols() ~ "*{level}*"),
             .header = "**{strata}**, N = {n}") %>% 
  as_gt() %>% 
  gt::fmt_markdown(columns = everything())

# Get a visual of the data
dat %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(timepoint, Out, group = group, colour = group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.25)) +
  geom_smooth(position = position_dodge(width = 0.25), 
              method = "lm", formula= y ~ x) +
  theme_clean() +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(y = "Outcome", x = "Timepoint",
       title = "aadfsaad", colour = "Group")

# Run a basic ANCOVA model
dat %>% 
  tibble %>% 
  pivot_wider(id_cols = c(id, group), names_from = timepoint, values_from = c(Out, time)) %>% 
  lm(Out_Post ~ Out_Pre + group, data = .) %>% 
  tbl_regression(intercept = T,
                 estimate_fun = function(x) style_number(x, digits = 2))

# Another visual
dat <- dat %>% 
  tibble %>% 
  mutate(timepoint = as.numeric(factor(timepoint, levels = c("Pre", "Post"))) - 1,
         group = as.numeric(factor(group, levels = c("Control", "Treatment"))) -1) %>% 
  mutate(jittered_pos = jitter(timepoint, amount = 0.05),
         dodge_pos = group * 0.25 )#- 0.25/2)

dat %>% 
  tibble %>% 
  mutate(timepoint = as.numeric(factor(timepoint, levels = c("Pre", "Post"))) - 1,
         group = as.numeric(factor(group, levels = c("Control", "Treatment"))) -1) %>% 
  mutate(jittered_pos = jitter(timepoint, amount = 0.05),
         dodge_pos = group * 0.25 ) %>% #- 0.25/2)
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

# what are we seeing
# With braces

dat_b <- dat %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  group_by(group, timepoint) %>% 
  summarise(Out_sd = sd(Out),
            Out = mean(Out)) %>% 
  filter(timepoint == "Post")

dat %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(timepoint, Out)) +
  geom_jitter(aes(colour = group, group = group), 
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.35)) +
  geom_smooth(aes(colour = group, group = group), 
              position = position_dodge(width = 0.45),
              method = "lm", formula= y ~ x) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(y = "Outcome", x = "Timepoint",
       title = "aadfsaad", colour = "Group") +
  stat_brace(data = dat_b, aes(group = timepoint), 
             rotate = 90, width = 0.15, outerstart = 2.2, bending = 1) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  geom_text(data = tibble(timepoint = c(2.4),
                          Out = c(50.8), # (71.2-30.4) / 2 + 30.4
                          label = c("A difference of 40?\nThe model said 30?")),
            aes(label = label), size = 3.5, hjust = 0) +
  coord_cartesian(xlim = c(1, 2.7)) 

# Run a basic mixed effects model
dat %>% tibble %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  lmer(Out ~ group*timepoint + (1 | id), data = .) %>% 
  tbl_regression(intercept = T,
                 estimate_fun = function(x) style_number(x, digits = 2))

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



