if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("margins", quietly = TRUE)) install.packages("margins")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

library(readxl)
library(lmtest)
library(car)
library(dplyr)     
library(ggplot2)
library(tidyr)
library(psych)       
library(gridExtra)  
library(margins)
library(caret)


# Read the dataset from the Excel file
data <- read_xlsx("nls80.xlsx")

# Check the column names
print("Column names:")
print(names(data))


# Define a vector with the names of the continuous variables
cont_vars <- c("wage", "hours", "iq", "kww", "educ", "exper", "tenure", "age", "meduc", "feduc", "lwage")

# Force conversion of the variables to numeric using as.numeric
data <- data %>% 
  mutate(across(all_of(cont_vars), ~ as.numeric(as.character(.))))

# Confirm the conversion by checking the data types of each variable
print("Data types after conversion:")
print(sapply(data[cont_vars], class))

# If any variable is still not numeric, inspect its values:
sapply(data[cont_vars], unique)

# View the structure and summary of the data after conversion
str(data[, cont_vars])
summary(data[, cont_vars])

# Check for missing values in each variable
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values)

# Remove duplicate rows if any exist
data <- data %>% distinct()

# -------------------------------------------------------------------
# Q1.1. Descriptive Statistics
# -------------------------------------------------------------------

# Basic descriptive statistics using summary()
descriptive_stats <- summary(data[, cont_vars])
print("Basic Summary Statistics:")
print(descriptive_stats)

# Detailed descriptive statistics using the psych package
detailed_stats <- describe(data[, cont_vars])
print("Detailed Descriptive Statistics:")
print(detailed_stats)

# -------------------------------------------------------------------
# Q1.2. Visualizations
# -------------------------------------------------------------------

# Faceted Histograms
data_long <- data %>% 
  select(all_of(cont_vars)) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(data_long, aes(x = value)) +
  geom_histogram(fill = "salmon2", color = "black", bins = 11) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histograms of Continuous Variables", x = "Value", y = "Count") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray90", color = NA) 
  )


# -------------------------------------------------------------------
# Q1.3. Multiple Linear Regression Model for Wages
# -------------------------------------------------------------------

# Fit the multiple linear regression model
model <- lm(lwage ~ hours + tenure + age + educ + 
              exper + kww + feduc + iq + meduc, data = data)

# Display the summary of the model to see coefficients, p-values, and overall fit
summary(model)

# Fit the multiple linear regression model with 5 most relevant predictors
model1 <- lm(lwage ~ hours + tenure + educ + 
               exper + iq, data = data)

# Display the summary of the model to see coefficients, p-values, and overall fit
summary(model1)


# -------------------------------------------------------------------
# Q1.4. Test for heteroskedasticity and multicollinearity
# -------------------------------------------------------------------

# Test for heteroskedasticity
bptest(model1)

# Test for multicollinearity
vif(model1)


# -------------------------------------------------------------------
# Q2.1. Logit and probit, binary variable
# -------------------------------------------------------------------

# Create university studies variable
data$uni <- ifelse(data$educ > 15, 1, 0)
data$uni <- as.numeric(data$uni)
str(data)

# Logit model
logit_model <- glm(uni ~ urban + iq + sibs + 
                     married + black + south, 
                   family = binomial(link = "logit"), data = data)
summary(logit_model)

# Probit model
probit_model <- glm(uni ~ urban + iq + sibs + 
                      married + black + south, 
                    family = binomial(link = "probit"), data = data)
summary(probit_model)

# -------------------------------------------------------------------
# Q2.2. Logit and probit, marginal effects
# -------------------------------------------------------------------

# Marginal effects for logit model
margins_logit <- margins(logit_model)
summary(margins_logit)

# Marginal effects for probit model
margins_probit <- margins(probit_model)
summary(margins_probit)


# -------------------------------------------------------------------
# Q2.2. Logit and probit, model performance
# -------------------------------------------------------------------

# Logit predictions (threshold = 0.5)
logit_pred <- ifelse(predict(logit_model, type="response") >= 0.5, 1, 0)

# Confusion matrix and summary (logit)
(logit_cm <- confusionMatrix(factor(logit_pred),
                             factor(data$uni)))

# Probit predictions (threshold = 0.5)
probit_pred <- ifelse(predict(probit_model, type="response") >= 0.5, 1, 0)

# Confusion matrix and summary (probit)
(probit_cm <- confusionMatrix(factor(probit_pred),
                              factor(data$uni)))


# Plot estimated probabilities
data$pred_logit <- predict(logit_model, type = "response")
data$pred_probit <- predict(probit_model, type = "response")

p1 <- ggplot(data, aes(x = iq, y = pred_logit)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Logit Model",
       x = "IQ", y = "Probability of Postgrad") +
  theme_minimal()

p2 <- ggplot(data, aes(x = iq, y = pred_probit)) +
  geom_point(alpha = 0.3, color = "salmon") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Probit Model",
       x = "IQ", y = "Probability of Postgrad") +
  theme_minimal()


grid.arrange(p1, p2, ncol = 2)


# -------------------------------------------------------------------
# Q3.1. Simulation dataset
# -------------------------------------------------------------------
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer")

library(AER)
library(stargazer)


# Set seed for reproducibility
set.seed(123)

df <- lm(lwage ~ educ + exper, data = data)
df


# Define the coefficients for the wage equation
beta0 <- 5.5       # intercept for lwage equation
beta1 <- 0.08     # coefficient for 'exper'
beta2 <- 0.2     # coefficient for 'educ'


# Number of observations and simulations
n <- 1000
n_sims <- 1000

sim_data <- function(n) {

  # Generate the exogenous variable 'exper' 
  exper <- runif(n, 0, 20)

  # Generate the endogenous variable 'educ'
  v <- rnorm(n, 0, 5)
  educ <- 10 + 0.2 * v + 0.1 * exper

  # Generate error term correlated with education
  u <- rnorm(n, 0, 1) + 0.2 * exper + 0.2 * educ
  
  # Generate the dependent variable 'lwage' (natural logarithm of wage)
  lwage <- beta0 + beta1 * exper + beta2 * educ + u

  # Combine the generated variables into a data frame
  data.frame(exper, educ, lwage)
}


# -------------------------------------------------------------------
# Q3.2. Estimate an OLS regression
# -------------------------------------------------------------------


# Function to estimate the OLS model and return coefficients
estimate_models <- function(data) {
  ols <- lm(lwage ~ educ + exper, data = data)
  return(coef(ols))
}

# Perform Monte Carlo simulation
results <- replicate(n_sims, {
  data <- sim_data(n)
  estimate_models(data)
})


# Calculate means and variances
means_ols <- rowMeans(results)
variances_ols <- apply(results, 1, var)


true_values <- c(beta0, beta2, beta1)
coef_names <- c("Intercept", "Education", "Experience")


# Build the results table
results_table <- data.frame(
  Estimator   = "OLS",
  Coefficient = coef_names,
  True_Value  = true_values,
  Estimate    = means_ols,
  Bias        = means_ols - true_values,
  Variance    = variances_ols
)

# Print the results table
print(results_table, digits = 4)


# Visualize distributions
par(mfrow = c(1, 3))  

for (i in 1:3) {
  plot_data <- data.frame(OLS = results[i, ])
  boxplot(plot_data,
          main = coef_names[i],
          ylab = "Coefficient Estimate")
  abline(h = true_values[i], col = "indianred1", lwd = 2)
}



# -------------------------------------------------------------------
# Q3.3. 2SLS estimation
# -------------------------------------------------------------------


# Define the coefficients for the wage equation
beta0 <- 5.5
beta1 <- 0.08
beta2 <- 0.2


# Extend data generation to produce a valid instrument (z).
sim_data_iv <- function(n) {
  
  # Generate the exogenous variable 'exper'
  exper <- runif(n, 0, 20)
  
  # Generate a random component v for use in 'educ'
  v <- rnorm(n, 0, 5)
  educ <- 10 + 0.2 * v + 0.1 * exper
  
  # Create an instrument 'z' correlated with 'educ' but not with the lwage error
  z <- 2 + 0.5 * educ + rnorm(n, 0, 1)
  
  # Generate error term 'u' correlated with 'exper' and 'educ'
  u <- rnorm(n, 0, 1) + 0.2 * exper + 0.2 * educ
  
  # Generate lwage
  lwage <- beta0 + beta1 * exper + beta2 * educ + u
  
  # Return all variables in a data frame (including z for 2SLS)
  data.frame(exper, educ, lwage, z)
}



# Define a function that estimates a 2SLS model via ivreg
estimate_2SLS <- function(data) {
  model_2sls <- ivreg(lwage ~ educ + exper | z + exper, data = data)
  
  coef(model_2sls)
}


# Monte Carlo simulation comparing OLS vs. 2SLS

n_sims <- 1000
n <- 1000

# Storage for 2SLS
results_2sls <- replicate(n_sims, {
  data_iv <- sim_data_iv(n)
  estimate_2SLS(data_iv)
})


results_ols <- replicate(n_sims, {
  data_ols <- sim_data_iv(n)     
  ols <- lm(lwage ~ educ + exper, data = data_ols)
  coef(ols)
})

# Calculate the mean of estimates across simulations
means_2sls <- rowMeans(results_2sls)
variances_2sls <- apply(results_2sls, 1, var)

means_ols <- rowMeans(results_ols)
variances_ols <- apply(results_ols, 1, var)

# True values: Intercept, Education, Experience
beta0 <- 5.5
beta1 <- 0.08
beta2 <- 0.2
true_values <- c(beta0, beta2, beta1)
coef_names <- c("Intercept", "Education", "Experience")

# Build results table for 2SLS
results_table_2sls <- data.frame(
  Estimator   = "2SLS",
  Coefficient = coef_names,
  True_Value  = true_values,
  Estimate    = means_2sls,
  Bias        = means_2sls - true_values,
  Variance    = variances_2sls
)

# Build results table for OLS
results_table_ols <- data.frame(
  Estimator   = "OLS",
  Coefficient = coef_names,
  True_Value  = true_values,
  Estimate    = means_ols,
  Bias        = means_ols - true_values,
  Variance    = variances_ols
)

# Combine them to see side-by-side
results_comparison <- rbind(results_table_ols, results_table_2sls)
print(results_comparison, digits = 4)

# Visualize the distribution of estimates for OLS vs 2SLS
par(mfrow = c(1, 3)) 

for (i in 1:3) {
  box_data <- data.frame(
    OLS  = results_ols[i, ],
    S2LS = results_2sls[i, ]
  )
  boxplot(box_data,
          main = coef_names[i],
          ylab = "Coefficient Estimate")
  abline(h = true_values[i], col = "indianred1", lwd = 2)
}
