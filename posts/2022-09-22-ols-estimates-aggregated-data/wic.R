# Source: Moineddin, R., Urquia, M.L., 2014. Regression Analysis of Aggregate Continuous Data. Epidemiology 25, 929–930. https://doi.org/10.1097/EDE.0000000000000172
# Load library for the recipe. parsnip, workflow and hardhat packages, along with the rest of tidymodels
library(dplyr)
library(modeldata)

# Read data
#data <- read.table("C:/Users/PietStam/Downloads/wic.txt", header = TRUE)
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

# Calculate sigma squared
fit1_sigma2 <- sum(weighted.residuals(fit1)^2)/df.residual(fit1)
fit1_sigma2
# Calculate the covariance matrix
fit1_covmatrix <- fit1_sigma2 * chol2inv(qr(fit1)$qr)
fit1_covmatrix
# Calculate the standard errors for the regression coefficients
fit1_stderr <- sqrt(diag(fit1_covmatrix))
fit1_stderr

#find sse (explained sum of squares)
sse <- sum((fitted(fit1) - mean(fit1$model$wtgaink))^2)
sse

#find ssr (redidual sum of squares)
ssr <- sum((fit1$model$wtgaink - fitted(fit1))^2)
ssr

#find sst (total sum of squares)
sst <- sse + ssr
sst

#find squared standard error of the regression
ssr/(length(fitted(fit1))-fit1$rank)

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

##### ALTERNATIVE PROCEDURE E-APPENDIX OF PAPER IN EPIDEMIOLOGY #####

#find sse (explained sum of squares) - weighted version
sse <- sum(db2$N * (fitted(fit2) - mean(db2$meany))^2)
sse

#find ssr (residual sum of squares) - weighted version
ssr <- sum(db2$N * (db2$meany - fitted(fit2))^2)
ssr

#find sst (total sum of squares)
sst <- sse + ssr
sst

#find squared standard error of the regression
ssr/(length(fitted(fit2))-fit2$rank)

# Calculating total sum of squares (= pooled variance)
db3 <- db2 %>% 
  mutate(n_1 = N-1, S_n_1 = stdy^2*n_1) %>% 
  summarise(n_k = sum(n_1), 
            S2_k = sum(S_n_1), 
            p_v = S2_k/n_k, # pooled-variance
            errorms = sum(db2$N * (fitted(fit2) - meany)^2) / (length(fitted(fit2)) - fit2$rank), # residual sum of squares / degrees of freedom (incl. weights)
            factor = sqrt(p_v/errorms), # correction factor < 1 (= fixed ratio of standard errors of parameter estimates for individual data vs aggregated data)
            StdErr = sqrt(diag(vcov(fit2))), # standard errors of parameter estimates using aggregated data
            standard_error = StdErr*factor, # derivation of standard errors of parameter estimates for individual data from those using aggregated data
            
            estimate = coefficients(fit2),
            z=estimate/standard_error,
            p = 2*(1 - pnorm(abs(z))),
            pvalue = format_p(p, stars = TRUE),
            CI_low = estimate - 1.96*standard_error,
            CI_high = estimate + 1.96*standard_error
  ) %>% 
  select(estimate, standard_error, CI_low, CI_high, z, pvalue)

library(insight)
format_table(db3, digits = 3, ci_digits = 3)
