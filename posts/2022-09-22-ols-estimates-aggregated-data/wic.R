# Source: Moineddin, R., Urquia, M.L., 2014. Regression Analysis of Aggregate Continuous Data. Epidemiology 25, 929–930. https://doi.org/10.1097/EDE.0000000000000172
# Load library for the recipe. parsnip, workflow and hardhat packages, along with the rest of tidymodels
library(dplyr)
library(modeldata)

# Read data
data <- read.table("C:/Users/pjast/OneDrive/Dev/Quarto/blog/posts/2022-09-22-ols-estimates-aggregated-data/wic.txt", header = TRUE)

# Transform data
db1 <- data %>% 
  rename_all(tolower) %>% 
  mutate(wtgaink = wtgain * 0.453592, 
         wic = as.factor(wic), 
         mracethn = as.factor(mracethn), mracethn = relevel(mracethn, ref = 3), 
         latecare = as.factor(latecare)
         ) %>%
  select(wtgaink, wic, mracethn, latecare)
  
# Model 1
model1 <- lm(formula = wtgaink ~ wic, data = db1)
summary(model1)
# Model 2
model2 <- lm(formula = wtgaink ~ wic + mracethn + latecare, data = db1)
summary(model2)
# Model 3
model3 <- lm(formula = wtgaink ~ wic + mracethn * latecare, data = db1)
summary(model3)



# Fit regression model to individual data
fit1 <- lm(formula = wtgaink ~ wic + mracethn + latecare, data = db1)
summary(fit1)

# Aggregate data
db2 <- 
  db1 %>%
  group_by(across(c(wic, mracethn, latecare))) %>% 
  summarise(N = length(wtgaink), meany = mean(wtgaink), stdy = sd(wtgaink)) %>% 
  ungroup()

# Table aggregate data set

db2

# Fit regression model to aggregated data
fit2 <- lm(formula = meany ~ wic + mracethn + latecare, data = db2, weights = N)
summary(fit2)

# Calculate sigma squared
fit2_sigma2 <- sum(length(db1$wtgaink) * (db1$wtgaink - predict.lm(fit2, db1))^2)/(nrow(db1)*sum(db2$N)-fit2$rank)
fit2_sigma2
# Calculate the covariance matrix
WX <- sqrt(length(db1$wtgaink)) * model.matrix(~ 1 + wic + mracethn + latecare, db1)
fit2_covmatrix <- fit2_sigma2 * chol2inv(qr(WX)$qr)
fit2_covmatrix
# Calculate the standard errors for the regression coefficients
fit2_stderr <- sqrt(diag(fit2_covmatrix))
fit2_stderr
