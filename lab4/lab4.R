library(dplyr)
library(ggplot2)

zeta <- read.csv("lab4/zeta.csv")

# zeta <- zeta[zeta$sex == 'F', ]
# zeta <- zeta[!duplicated(zeta$meanhouseholdincome),]
zeta <- zeta %>%
  filter(sex == "F") %>%
  distinct(meanhouseholdincome, .keep_all = TRUE)

# remove sex and zcta
# zeta_clear <- zeta[, -c(2, 3)]
zeta <- zeta %>%
  select(-zcta, -sex)

# remove outliers
# zeta_clear <- zeta_clear[zeta_clear$meaneducation > 8 & zeta_clear$meaneducation < 18,]
# zeta_clear <- zeta_clear[zeta_clear$meanhouseholdincome > 10000 & zeta_clear$meanhouseholdincome < 200000,]
# zeta_clear <- zeta_clear[zeta_clear$meanemployment > 0 & zeta_clear$meanemployment < 3,]
# zeta_clear <- zeta_clear[zeta_clear$meanage > 20 & zeta_clear$meanage < 60,]

# %>% - pipe operator
zeta <- zeta %>%
  filter(
    meaneducation > 8 & meaneducation < 18,
    meanhouseholdincome > 10000 & meanhouseholdincome < 200000,
    meanemployment > 0 & meanemployment < 3,
    meanage > 20 & meanage < 60
  )

zeta$log_income <- log10(zeta$meanhouseholdincome)

zeta <- zeta %>%
  rename(
    age = meanage,
    education = meaneducation,
    employment = meanemployment
  )

ggplot(zeta, aes(x = age, y = log_income)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Age vs Log Income", x = "Age", y = "Log Income")

# t-value is the coefficient divided by the standard error
# A higher absolute t-value suggests the variable is more likely to have a significant effect on the dependent variable

# r-squared = 1 - (residual sum of squares / total sum of squares)
# Model explains some portion of the variability. A higher value indicates a better fit.

# f-static = (regression sum of squares / residual sum of squares) * (n - k - 1) / k
# n - number of data points, k - number of independent variables
# is a measure of the overall significance of the regression model
# The p-value associated with the F-statistic helps confirm if the F-statistic is significant
model_age <- lm(log_income ~ age, data = zeta)
summary(model_age)
model_age


ggplot(zeta, aes(x = education, y = log_income)) +
  geom_point() +
  geom_smooth(method = "lm", col = "green") +
  labs(title = "Education vs. Log Income", x = "Education", y = "Log Income")

model_education <- lm(log_income ~ education, data = zeta)
summary(model_education)

model_multiple <- lm(log_income ~ age + education + employment, data = zeta)
summary(model_multiple)

coef_education <- summary(model_multiple)$coefficients["education", "Estimate"]
percentage_change <- (10^coef_education - 1) * 100
print(paste("Percentage change in income per unit of education:", percentage_change, "%"))


zeta$predicted <- predict(model_multiple, newdata = zeta)

ggplot(zeta, aes(x = log_income, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "green") +
  labs(title = "Predicted vs Actual Log Income", x = "Actual Log Income", y = "Predicted Log Income")



