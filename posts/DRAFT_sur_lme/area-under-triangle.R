library(simstudy)
library(lavaan)
library(tidyverse)
library(data.table)
library(lme4)

# Initialise random seed for reproducibility
set.seed(2024)

# Generate Treatment group data
dat_treat_pre <- data.table(id = 1:204, group = "treatment", time = sample(0:28, 204, replace = TRUE), 
                            out = rnorm(204, mean = 20, sd = 5), timepoint = "pre")
dat_treat_post <- data.table(id = 1:204, group = "treatment", time = sample(150:210, 204, replace = TRUE), 
                             out = dat_treat_pre$out + rnorm(204, mean = 50, sd = 5), timepoint = "post")

# Generate Control group data
dat_cont_pre <- data.table(id = 205:401, group = "control", time = sample(0:28, 197, replace = TRUE), 
                           out = pmax(rnorm(197, mean = 10, sd = 5), 0), timepoint = "pre")
dat_cont_post <- data.table(id = 205:401, group = "control", time = sample(150:210, 197, replace = TRUE), 
                            out = dat_cont_pre$out + rnorm(197, mean = 20, sd = 5), timepoint = "post")

# Combine data
dat <- rbind(dat_treat_pre, dat_treat_post, dat_cont_pre, dat_cont_post)

dat %>% 
  mutate(timepoint = factor(timepoint, levels = c("pre", "post"))) %>% 
  ggplot(aes(timepoint, out, group = group, colour = group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.25), alpha = 0.3) +
  geom_smooth(position = position_dodge(width = 0.25), 
              method = "lm", formula= y ~ x) +
  biometrics::theme_institute() +
  labs(y = "Outcome", x = "Timepoint",
       title = "Pre-post analysis", 
       subtitle = "Two-groups, jitterplot with line connecting observed means", colour = "Group")

# ------------------------------------------------------------------------------

summary(lmer(out ~ group*timepoint + (1|id), data = dat))

# ------------------------------------------------------------------------------

library(lavaan)
library(boot)

model <- '
  # regressions
  post ~ pre + group
'
dat_long <- dat %>%
  pivot_wider(id_cols = c("id", "group"), values_from = "out", names_from = "timepoint")

# Analysis without bootstrapping for plot. 
sur_output <- sem(model, data = dat_long, missing = "fiml")

fit <- parameterEstimates(sur_output)

std.coef <- data.frame(standardizedsolution(sur_output, type = "std.nox", level = 0.9)) %>%
  filter(rhs == "group") %>% 
  select(lhs, est.std, ci.lower, ci.upper) %>%
  mutate(ci = ci.upper - est.std)

d2 <- std.coef[1,2]
ci2 <- std.coef[1,5]

models$SEM$adjusted$d <- data.frame(
  time = c(0,1),
  d = c(0,d2),
  ci = c(0, ci2)
)


# Stage 10. Area Under Curve Analysis for SEM

#### Boostrapping Function

boot_area_SEM <- function(dat, indices, lavaan_model) {
  
  # Resample Outcomes
  resample <- dat[indices,]
  
  # Define Models for SEM (lavaan format)
  model <- lavaan_model
  fit <- sem(model, data = resample, missing = "fiml")
  
  # Cohen's D (?) - std.nox method from lavaan
  std.coef <- data.frame(standardizedsolution(fit, type = "std.nox")) %>%
    filter(rhs == "group") %>% 
    select(lhs, est.std, ci.lower, ci.upper) %>%
    mutate(ci = ci.upper - est.std)
  
  d2 <- std.coef[1,2]
  
  # Area calculation
  area <- d2/2
  
  area
}

# Bootstrap

lavaan_model <- '
  # regressions
  post ~ pre + group
'

bt <- boot(dat_long, boot_area_SEM, R = 1000, lavaan_model = lavaan_model)

models$bootstrap_sem$estimate <- bt
models$bootstrap_sem$ci <- boot.ci(bt, type = "norm", conf = 0.99999999)



