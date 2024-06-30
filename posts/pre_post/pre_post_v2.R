

dtCombined %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(timepoint, Out, group = group, colour = group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.25)) +
  geom_smooth(position = position_dodge(width = 0.25),
              method = "lm", formula= y ~ x) +
  theme_clean() +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(y = "Outcome", x = "Timepoint",
       title = "aadfsaad", colour = "Group") +
  stat_brace(data = dat_b, rotate = 90,
             width = 0.25, outerstart = 2.2)

dat_b <- dtCombined %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  group_by(group, timepoint) %>% 
  summarise(Out_sd = sd(Out),
            Out = mean(Out)) 
dat_b


dat_b <- dtCombined %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  group_by(group, timepoint) %>% 
  summarise(Out_sd = sd(Out),
            Out = mean(Out))

dtCombined %>% 
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Post"))) %>% 
  ggplot(aes(timepoint, Out)) +
  geom_jitter(aes(colour = group, group = group), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.35)) +
  geom_smooth(aes(colour = group, group = group), position = position_dodge(width = 0.45),
              method = "lm", formula= y ~ x) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(y = "Outcome", x = "Timepoint",
       title = "aadfsaad", colour = "Group") +
  stat_brace(data = dat_b, aes(group = timepoint), 
             rotate = 90, width = 0.15, outerstart = 2.3, bending = 1) +
  geom_text(data = tibble(timepoint = c(2.5, 2.5),
                          Out = c(20, 50),
                          label = c("blah blah\nmore blah", "moo")),
            aes(label = label), size = 3.5, hjust = 0) +
  coord_cartesian(xlim = c(1, 2.7))
  

