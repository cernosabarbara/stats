library(foreign)
library(psych)
library(car)
library(lattice)

# Chosen dataset for analysis
data("USArrests")
head(USArrests)
us_crime_data <- USArrests
str(us_crime_data)
psych::describe(us_crime_data)

####### 1. Analysis and model fitting ########################################

########## Correlation between variables
cor(us_crime_data)
#Murder and Assault: There’s a strong positive correlation, meaning as assault rates increase, murder rates tend to increase as well.
#Murder and UrbanPop: Weak correlation - urban population size has little to no relationship with murder rates.
#Murder and Rape: Moderate correlation - indicating some connection between murder and rape rates.


########## Linear regression models:
# For Murder 
fit1 <- lm(Murder ~ ., data = us_crime_data)
summary(fit1)
# This model shows that Assault has a significant positive relationship with Murder while UrbanPop has a small negative relationship.
# This model explains 67.21% of the variance in Murder (R^2). F-statistic and the p-value indicate that this model is statistically significant.

target <- "Murder"
predictors <- c("Assault", "UrbanPop", "Rape")
data <- na.omit(us_crime_data[c(target, predictors)])
n <- nrow(data)

# Total sum of squares (SStotal) for Murder
SS_total_murder <- sum((data$Murder - mean(data$Murder))^2)

# Linear regression model for Murder
fit_lm <- lm(Murder ~ ., data = data)
summary(fit_lm)
# This model shows that Assault has a significant positive relationship with Murder while UrbanPop has a small negative relationship.
# This model explains 67.21% of the variance in Murder (R^2). F-statistic and the p-value indicate that this model is statistically significant.
lm_pred <- predict(fit_lm, newdata = data)
R2_lm_orig <- 1 - sum((lm_pred - data$Murder)^2) / SS_total_murder

################ 2. Validations ##########################################
# 10-fold cross-validation
k <- 10
set.seed(2010)
part <- rep(1:k, length.out = n)
part <- sample(part)

lm_pred_cv <- rep(NA, n)

for (i in 1:k) {
  fit_lm_cv <- lm(Murder ~ ., data = data[part != i, ])
  lm_pred_cv[part == i] <- predict(fit_lm_cv, newdata = data[part == i, ])
}

# R^2 for cross-validation
R2_lm_cv <- 1 - sum((lm_pred_cv - data$Murder)^2) / SS_total_murder
R2_lm_orig
R2_lm_cv
#Low R2 - the model may not generalize perfectly, but it’s still doing a 
# good job of predicting Murder with 60.19% of the variation explained on new data. 

# Jackknife resampling
lm_pred_jack <- rep(NA, n)

for (i in 1:n) {
  fit_lm_jack <- lm(Murder ~ ., data = data[-i, ])
  lm_pred_jack[i] <- predict(fit_lm_jack, newdata = data[i, ])
}

# R^2 for jackknife resampling
R2_lm_jack <- 1 - sum((lm_pred_jack - data$Murder)^2) / SS_total_murder
R2_lm_orig
R2_lm_jack

# 100 repetitions of 10-fold cross-validation
m <- 100
R2_lm_cv_vec <- rep(NA, m)

for (j in 1:m) {
  part <- sample(part)
  
  lm_pred_cv <- rep(NA, n)
  for (i in 1:k) {
    fit_lm_cv <- lm(Murder ~ ., data = data[part != i, ])
    lm_pred_cv[part == i] <- predict(fit_lm_cv, newdata = data[part == i, ])
  }
  
  R2_lm_cv_vec[j] <- 1 - sum((lm_pred_cv - data$Murder)^2) / SS_total_murder
}

R2_lm_cv_mean <- mean(R2_lm_cv_vec)
hist(R2_lm_cv_vec)
R2_lm_orig
R2_lm_cv_mean

# Bootstrap resampling
m <- 1000
diffErrSsBoot_lm <- numeric(m)

set.seed(2010)
for (i in 1:m) {
  boot_ids <- sample(1:n, size = n, replace = TRUE)
  fit_lm_boot <- lm(Murder ~ ., data = data[boot_ids, ])
  errSsBoot_lm <- sum(fit_lm_boot$resid^2)
  predOrg_lm <- predict(fit_lm_boot, newdata = data)
  errSsOrg_lm <- sum((predOrg_lm - data$Murder)^2)
  diffErrSsBoot_lm[i] <- errSsOrg_lm - errSsBoot_lm
}

mean(diffErrSsBoot_lm)

# Calculated R^2 from bootstrap
R2_lm_boot <- 1 - (sum(fit_lm$resid^2) + mean(diffErrSsBoot_lm)) / SS_total_murder
R2_lm_orig
R2_lm_boot

# Comparing R2 values
R2s <- c(R2_lm_orig = R2_lm_orig, R2_lm_cv = R2_lm_cv, 
         R2_lm_cv_mean = R2_lm_cv_mean, R2_lm_jack = R2_lm_jack, 
         R2_lm_boot = R2_lm_boot)
R2s

# Interpretation:
# The original R² was 0.67, showing that Linear Regression explains 67.2% of the variance in Murder rates. The 10-fold CV R² was 0.620, lower than the original, suggesting the model doesn’t generalize perfectly but still explains about 62% of the variance on new data. Jackknife R² was 0.605, close to 10-fold CV, indicating consistent performance. The 100-repetition CV mean R² was 0.602, and bootstrap R² was 0.613. All these R² values are fairly close to each other, indicating that the model generalizes well across different validations. The small variations suggest that the model is quite stable and works similarly in different testing conditions.


############# 2. Predictions ################################################
crime_pred_murder_lm <- predict(fit_lm, newdata = data)
crime_predictions <- data.frame(
  Murder_Predicted_LM = crime_pred_murder_lm
)
head(crime_predictions)
head(data)
# Comparing predictions to actual Murder rates for the first few states, Alabama’s predicted rate was 10.79 vs. actual 13.2 (underpredicted by 2.41), while Alaska’s was 13.85 vs. 10.0 (overpredicted by 3.85). These differences vary, with errors ranging from -2.41 to +4.40, but the R² values capture the overall fit across all states.








