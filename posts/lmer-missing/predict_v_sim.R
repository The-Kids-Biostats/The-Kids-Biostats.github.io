newdat <- expand.grid(t = 1:7, gender = c("Male", "Female")) %>% 
  mutate(gender = factor(gender),
         gender = relevel(gender, "Male"),
         id = c(rep(1, 7), rep(2, 7)))

p_mod2 <- cbind(newdat,
                predictInterval(mod2, newdat, which = "fixed", n.sims = 1000, level = 0.95))

p_mod2 %>% 
  ggplot(aes(t, fit)) +
  geom_line(data = dat %>% rename(fit = lwage), aes(colour = gender, group = id), alpha = 0.10) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = gender), alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  facet_wrap(~ gender) +
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p1
p1

store <- simulate(mod2, seed=1, newdata=newdat, re.form=NA,
                  allow.new.levels=T, nsim = 500)
p_mod2 <- cbind(newdat,
                store %>% 
                  rowwise() %>% 
                  mutate(fit = mean(c_across(sim_1:sim_500)),
                         lwr = quantile(c_across(sim_1:sim_500), 0.025),
                         upr = quantile(c_across(sim_1:sim_500), 0.975)) %>% 
                  select(fit, lwr, upr))

p_mod2 %>% 
  ggplot(aes(t, fit)) +
  geom_line(data = dat %>% rename(fit = lwage), aes(colour = gender, group = id), alpha = 0.10) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = gender), alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) + 
  facet_wrap(~ gender) +
  scale_fill_viridis_d(option = "viridis", end = 0.4) +
  theme_clean() +
  theme(legend.position="none") +
  labs(x = "Time (years)", y = "Wage (logged)") +
  coord_cartesian(xlim = c(0,7),
                  ylim = c(4.5,9)) -> p2

p1 / p2
