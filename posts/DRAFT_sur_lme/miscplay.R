library(simstudy)
library(lavaan)
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Define data structure
def <- defData(varname = "x1", formula = 0, variance = 1)
def <- defData(def, varname = "x2", formula = 0, variance = 1)
def <- defData(def, varname = "y1", formula = "1 + 2*x1 - 1*x2", variance = 1)
def <- defData(def, varname = "y2", formula = "0.5*x1 + 1*x2", variance = 1)

# Generate dataset
data <- genData(100, def)

# Introduce correlation in residuals (for SUR)
data <- data %>%
  mutate(y1 = y1 + 0.5 * y2)

# Fit separate OLS models
ols1 <- lm(y1 ~ x1 + x2, data = data, )
ols2 <- lm(y2 ~ x1 + x2, data = data)

# Combine OLS coefficients
ols_coefficients <- tibble(
  Equation = c("y1", "y2"),
  Intercept = c(coef(ols1)[1], coef(ols2)[1]),
  x1 = c(coef(ols1)[2], coef(ols2)[2]),
  x2 = c(coef(ols1)[3], coef(ols2)[3])
)

# Define the SUR model in lavaan syntax
sur_model <- '
  y1 ~ x1 + x2
  y2 ~ x1 + x2
  y1 ~~ y2
'

# Fit the model
fit <- sem(sur_model, data = data, fixed.x = FALSE)

# Extract SUR coefficients
sur_coefficients <- parameterEstimates(fit) %>%
  filter(op == "~") %>%
  select(lhs, rhs, est) %>%
  spread(rhs, est) %>%
  rename(Equation = lhs)