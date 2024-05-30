library(lme4); library(merTools)
library(gtsummary); library(jtools); library(parameters)
library(tidyverse); library(AER); library(patchwork)

theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(family = "BarlowSemiCondensed-Bold"),
          axis.title = element_text(family = "BarlowSemiCondensed-Medium"),
          strip.text = element_text(family = "BarlowSemiCondensed-Bold",
                                    size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA))
}

# motivated by https://statisticalhorizons.com/wp-content/uploads/MissingDataByML.pdf

data("PSID7682")
dat <- PSID7682

dat <- dat %>% 
  mutate(gender = case_when(gender == "male" ~ "Male",
                            T ~ "Female"),
         gender = factor(gender),
         gender = relevel(gender, "Male"),
         lwage = log(wage)) %>% 
  group_by(id) %>% 
  mutate(t = 1:7)

# Initial view of the data
head(dat)

dat %>% 
  group_by(gender, t) %>% 
  summarise(N = sum(!is.na(lwage)))

dat %>% 
  ungroup() %>% 
  select(gender, t) %>% 
  tbl_summary(by = gender,
              statistic = all_categorical() ~ "{n}")

# Graphically
dat %>% 
  ggplot(aes(t, lwage, colour = gender)) +
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9))

# LM - A basic model without taking person level data into account
mod1 <- lm(lwage ~ gender * t, data = dat)
tbl_regression(mod1, 
               estimate_fun = function(x) style_number(x, digits = 3))
export_summs(mod1, error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)
model_parameters(mod1, digits = 3)

# Plot the LM fit
newdat <- expand.grid(t = 1:7, gender = c("Male", "Female")) %>% 
  mutate(gender = factor(gender),
         gender = relevel(gender, "Male"),
         id = c(rep(1, 7), rep(2, 7)))

p_mod1 <- cbind(newdat, lwage = predict(mod1, newdat, interval = "confidence"))
p_mod1 <- cbind(newdat, lwage = predict(mod1, newdat, interval = "prediction"))

dat %>% 
  ggplot(aes(t, lwage)) +
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  geom_line(data = p_mod1, aes(y = lwage.fit), colour = "red", linewidth = 1) +
  scale_colour_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9))

p_mod1 %>% 
  ggplot(aes(t, lwage.fit, ymin = lwage.lwr, ymax = lwage.upr, fill = gender)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  facet_wrap(~ gender) +
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)")+
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p1
p1

# A basic model that takes into account the person level data
mod2 <- lmer(lwage ~ gender * t + (1 | id), data = dat)
tbl_regression(mod2, 
               estimate_fun = function(x) style_number(x, digits = 3))
export_summs(mod2, error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)
model_parameters(mod2, digits = 3)

export_summs(mod1, mod2,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)

summ(mod1, mod2,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)

# Plot the LMER fit
newdat <- expand.grid(t = 1:7, gender = c("Male", "Female")) %>% 
  mutate(gender = factor(gender),
         gender = relevel(gender, "Male"),
         id = c(rep(1, 7), rep(2, 7)))

p_mod2 <- cbind(newdat,
                predictInterval(mod2, newdat, which = "fixed", n.sims = 1000, level = 0.95))


dat %>% 
  ggplot(aes(t, lwage)) +
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  geom_line(data = p_mod2, colour = "blue", linewidth = 1) +
  #scale_fill_viridis_d(option = "plasma", end = 0.8) +
  theme_clean() 

# Plot both fits
newdat <- expand.grid(t = 1:7, gender = c("Male", "Female")) %>% 
  mutate(gender = factor(gender),
         gender = relevel(gender, "Male"),
         id = c(rep(1, 7), rep(2, 7)))

p_mod2 <- cbind(newdat,
                predictInterval(mod2, newdat, which = "fixed", n.sims = 1000, level = 0.95))

p_mod2 %>% 
  ggplot(aes(t, fit, ymin = lwr, ymax = upr, fill = gender)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  facet_wrap(~ gender) +
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p2

p1 / p2

#####
# NOW lets bring in the missing
#####

mdat <- dat %>% 
  group_by(id) %>% 
  mutate(plwage = c(0, lwage[1:6]),
         pt = 1 / (1+exp(-6.9 + plwage)),
         mlwage = case_when(pt > 0.5 ~ lwage,
                           pt < 0.5 ~ 0),
         is_zero_following = ifelse(mlwage == 0, 1, 0),
         # Use cummax to make this vector 1 from the first occurrence of 0 onwards
         is_zero_following = cummax(is_zero_following),
         # Replace wage with 0 where is_zero_following is 1
         mlwage = ifelse(is_zero_following == 1, NA, mlwage)) 

mdat %>% 
  group_by(t) %>% 
  summarise(N = sum(!is.na(mlwage)))

mdat %>% 
  ungroup() %>% 
  filter(!is.na(mlwage)) %>% 
  select(gender, t) %>% 
  tbl_summary(by = gender,
              statistic = all_categorical() ~ "{n}")
  
# Graphically
mdat %>% 
  ggplot(aes(t, mlwage, colour = gender)) +
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)")  +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) 

# Graphically
mdat %>% 
  ggplot(aes(t, mlwage, colour = gender)) +
  geom_point(data = mdat %>% filter(is_zero_following == 1),
             aes(y = lwage, group = id), alpha = 0.1, colour = "red") +
  geom_line(data = mdat %>% filter(is_zero_following == 1),
            aes(y = lwage, group = id), alpha = 0.15, colour = "red") + 
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)")  +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) 

mod3 <- lm(mlwage ~ gender * t, data = mdat)
tbl_regression(mod3, 
               estimate_fun = function(x) style_number(x, digits = 3))
export_summs(mod3, error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)
model_parameters(mod3, digits = 3)

mod4 <- lmer(mlwage ~ gender * t + (1 | id), data = mdat)
tbl_regression(mod4, 
               estimate_fun = function(x) style_number(x, digits = 3))
export_summs(mod4, error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)
model_parameters(mod4, digits = 3)

export_summs(mod3, mod4,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)

export_summs(mod2, mod4,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)
#####
# NOW lets graph it
#####

newdat <- expand.grid(t = 1:7, gender = c("Male", "Female"), id = 1) %>% 
  mutate(gender = factor(gender),
         gender = relevel(gender, "Male"))

p_mod3 <- cbind(newdat, 
                lwage = predict(mod3, newdat, interval = "prediction"))
p_mod3 <- cbind(newdat, lwage = predict(mod3, newdat, interval = "prediction"))

p_mod4 <- cbind(newdat,
                predictInterval(mod4, newdat, which = "fixed", n.sims = 1000, level = 0.95))

mdat %>% 
  ggplot(aes(t, mlwage, colour = gender)) +
  geom_ribbon(data = p_mod3, aes(t, lwage.fit, ymin = lwage.lwr, ymax = lwage.upr, fill = gender), alpha = 0.5) +
  geom_line(data = p_mod3, aes(t, lwage.fit), colour = "black", linewidth = 1) + 
  geom_point(data = mdat %>% filter(is_zero_following == 1),
             aes(y = lwage, group = id), alpha = 0.1, colour = "red") +
  geom_line(data = mdat %>% filter(is_zero_following == 1),
            aes(y = lwage, group = id), alpha = 0.15, colour = "red") + 
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)")  +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) 

p_mod3 %>% 
  ggplot(aes(t, lwage.fit, ymin = lwage.lwr, ymax = lwage.upr, fill = gender)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  facet_wrap(~ gender) +
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)")  +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p3
p3

p_mod4 %>% 
  ggplot(aes(t, fit, ymin = lwr, ymax = upr, fill = gender)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  facet_wrap(~ gender) +
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p4

p3 / p4

p_mod3 %>% 
  ggplot(aes(t, lwage.fit, ymin = lwage.lwr, ymax = lwage.upr, fill = gender)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)")   +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p3
p3

p_mod4 %>% 
  ggplot(aes(t, fit, ymin = lwr, ymax = upr, fill = gender)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)")  +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p4

p3 + p4

#####
# NOW BONUS8
#####

# Less aggressive missing data 

mdat <- dat %>% 
  group_by(id) %>% 
  mutate(plwage = c(0, lwage[1:6]),
         pt = 1 / (1+exp(-7.2 + plwage))) %>% 
  rowwise() %>% 
  mutate(pt = case_when(pt > 0.5 & runif(1) < 0.10 & t > 2 ~ 0,
                        T ~ pt)) %>% 
  group_by(id) %>% 
  mutate(mlwage = case_when(pt > 0.5 ~ lwage,
                            pt < 0.5 ~ 0),
         is_zero_following = ifelse(mlwage == 0, 1, 0),
         is_zero_following = cummax(is_zero_following),
         mlwage = ifelse(is_zero_following == 1, NA, mlwage)) 

mdat %>% 
  ungroup() %>% 
  filter(!is.na(mlwage)) %>% 
  select(gender, t) %>% 
  tbl_summary(by = gender,
              statistic = all_categorical() ~ "{n}")

# Graphically
mdat %>% 
  ggplot(aes(t, mlwage, colour = gender)) +
  geom_point(data = mdat %>% filter(is_zero_following == 1),
             aes(y = lwage, group = id), alpha = 0.1, colour = "red") +
  geom_line(data = mdat %>% filter(is_zero_following == 1),
            aes(y = lwage, group = id), alpha = 0.15, colour = "red") + 
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9))

modb1 <- lm(mlwage ~ gender * t, data = mdat)
modb2 <- lmer(mlwage ~ gender * t + (1 | id), data = mdat)

export_summs(mod2, modb1, modb2,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)


# More random missing data

mdat <- dat %>% 
  group_by(id) %>% 
  rowwise() %>% 
  mutate(pt = 1,
         pt = case_when(runif(1) < 0.10 & t > 2 ~ 0,
                        T ~ pt)) %>% 
  group_by(id) %>% 
  mutate(mlwage = case_when(pt > 0.5 ~ lwage,
                            pt < 0.5 ~ 0),
         is_zero_following = ifelse(mlwage == 0, 1, 0),
         is_zero_following = cummax(is_zero_following),
         mlwage = ifelse(is_zero_following == 1, NA, mlwage)) 

mdat %>% 
  ungroup() %>% 
  filter(!is.na(mlwage)) %>% 
  select(gender, t) %>% 
  tbl_summary(by = gender,
              statistic = all_categorical() ~ "{n}")

# Graphically
mdat %>% 
  ggplot(aes(t, mlwage, colour = gender)) +
  geom_point(data = mdat %>% filter(is_zero_following == 1),
             aes(y = lwage, group = id), alpha = 0.1, colour = "red") +
  geom_line(data = mdat %>% filter(is_zero_following == 1),
            aes(y = lwage, group = id), alpha = 0.15, colour = "red") + 
  geom_point(alpha = 0.15) +
  geom_line(aes(group = id), alpha = 0.15) + 
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) 

modb1 <- lm(mlwage ~ gender * t, data = mdat)
modb2 <- lmer(mlwage ~ gender * t + (1 | id), data = mdat)

export_summs(mod2, modb1, modb2,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "right", digits = 3)

