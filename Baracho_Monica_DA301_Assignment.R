# ============================================================
# Baracho_Monica_DA301_Assignment.R
# Module 5 & 6: EDA + Regression Analysis for Turtle Games

# ============================================================

# ------------------------------------------------------------
# 1. Load Required Libraries
# ------------------------------------------------------------
# --------------------------- 0.  Libraries -------------------
pkg <- c("tidyverse", "plotly", "stargazer", "car",
         "caret", "pROC", "broom", "gt", "modelsummary")
installed <- rownames(installed.packages())
to_install <- setdiff(pkg, installed)
if (length(to_install)) install.packages(to_install, quiet = TRUE)
lapply(pkg, library, character.only = TRUE)

set.seed(123)     

# ------------------------------------------------------------
# 2. Load and Inspect the Data
# ------------------------------------------------------------

# Place turtle_reviews_clean.csv in the same folder as this script before running it
data <- read_csv("~/Desktop/LSE_DA301_Module3/Baracho_Monica_DA301_FINAL/turtle_reviews_clean.csv")


# View structure of dataset
glimpse(data)

# Check column names
names(data)

# Summary Data
summary(data)
str(data)


# Check for missing values
colSums(is.na(data))

# ------------------------------------------------------------
# 3. Static Visualisations with ggplot2
# ------------------------------------------------------------

# Histogram of Loyalty points
ggplot(data, aes(x = loyalty_points)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Loyalty Points", x = "Loyalty Points", y = "Frequency") +
  theme_minimal()

# Boxplot of loyalty points (general spread)
ggplot(data, aes(y = loyalty_points)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Loyalty Points", y = "Loyalty Points") +
  theme_minimal()

# Boxplot loyalty points by gender 
ggplot(data, aes(x = gender, y = loyalty_points)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Loyalty Points by Gender", x = "Gender", y = "Loyalty Points") +
  theme_minimal()
#Boxplot loyalty points by education 
ggplot(data, aes(x = education, y = loyalty_points)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Loyalty Points by Education Level", x = "Education Level", y = "Loyalty Points") +
  theme_minimal()

# 
ggplot(data, aes(x = remuneration, y = loyalty_points)) +
  annotate("rect", xmin = 50, xmax = 70, ymin = 0, ymax = Inf, fill = "orange", alpha = 0.1) +
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Income vs Loyalty Points",
       subtitle = "Customers earning £50k–£70k show stronger loyalty engagement",
       x = "Annual Income (k£)", y = "Loyalty Points") +
  theme_minimal(base_size = 14)
#-------------------------------------------------------------
 #3B. Interactive Visualisations with Plotly
# ------------------------------------------------------------

# Interactive Boxplot with plot_ly
interactive_box <- plot_ly(data,
                           x = ~education,
                           y = ~loyalty_points,
                           type = "box",
                           color = ~education,
                           colors = "Set2") %>%
  layout(title = "Interactive Boxplot: Loyalty Points by Education Level",
         xaxis = list(title = "Education Level"),
         yaxis = list(title = "Loyalty Points"))
interactive_box

# Store your plot object first
interactive_box <- plot_ly(data,
                           x = ~education,
                           y = ~loyalty_points,
                           type = "box",
                           color = ~education,
                           colors = "Set2")

# Now explicitly call plotly::layout to avoid the masking issue
interactive_box <- plotly::layout(interactive_box,
                                  title = "Interactive Boxplot: Loyalty Points by Education Level",
                                  xaxis = list(title = "Education Level"),
                                  yaxis = list(title = "Loyalty Points"))

# Display the plot
interactive_box


# Interactive Scatterplot using ggplotly
scatterplot_static <- ggplot(data, aes(x = remuneration, y = loyalty_points, color = gender)) +
  geom_point(alpha = 0.6) +
  labs(title = "Remuneration vs Loyalty Points (Interactive)",
       x = "Remuneration (k£)", y = "Loyalty Points") +
  theme_minimal()
ggplotly(scatterplot_static)

# Interactive Histogram using ggplotly
histogram_static <- ggplot(data, aes(x = loyalty_points)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Interactive Histogram: Loyalty Points",
       x = "Loyalty Points", y = "Count") +
  theme_minimal()
ggplotly(histogram_static)

# ------------------------------------------------------------
# 4. Regression Analysis
# ------------------------------------------------------------
model <- lm(loyalty_points ~ remuneration + age + spending_score, data = data)
summary(model)
stargazer(model, type = "text")
vif(model)

ggplot(data, aes(x = predict(model), y = loyalty_points)) +
  geom_point(alpha = 0.6, color = "dodgerblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Predicted vs Actual Loyalty Points",
       x = "Predicted Loyalty Points", y = "Actual Loyalty Points") +
  theme_minimal()

ggplot(data, aes(x = predict(model), y = residuals(model))) +
  geom_point(alpha = 0.5, color = "grey40") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Predicted Loyalty Points", y = "Residuals") +
  theme_minimal()

# Histogram of Residuals 
ggplot(data.frame(resid = residuals(model)), aes(x = resid)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Cook's distance 
qqnorm(residuals(model))
qqline(residuals(model), col = "red")
plot(model, which = 4)

new_data <- data.frame(remuneration = 55, age = 35, spending_score = 60)
predicted_points <- predict(model, newdata = new_data)
print(paste("Predicted loyalty points for new customer:", round(predicted_points)))

# Predict Loyalty Points for New Customers 
ggplot(data, aes(x = remuneration, y = loyalty_points)) +
  geom_point(alpha = 0.4, color = "grey70") +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +
  geom_point(data = new_data, aes(x = remuneration, y = predicted_points),
             color = "darkgreen", size = 4) +
  geom_text(data = new_data, aes(x = remuneration, y = predicted_points,
                                 label = paste("Prediction:\\n", round(predicted_points))),
            vjust = -1.2, color = "darkgreen", fontface = "bold") +
  labs(title = "Predicted Loyalty Points for New Customer",
       subtitle = "35 y/o, £55k income, Spending Score = 60",
       x = "Annual Income (k£)", y = "Loyalty Points") +
  theme_minimal(base_size = 14)
  
# ------------------------------------------
# Train/Test Split and Model Evaluation in R
# ------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% testing
split <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[split, ]
test_data <- data[-split, ]

# Fit multiple linear regression on training set
model <- lm(loyalty_points ~ remuneration + spending_score + age, data = train_data)

# Predict loyalty points on test set
predicted <- predict(model, newdata = test_data)

# Evaluate model performance
actual <- test_data$loyalty_points
mse <- mean((predicted - actual)^2)
r2 <- 1 - sum((predicted - actual)^2) / sum((actual - mean(actual))^2)

# Output evaluation metrics
print(paste("Test MSE:", round(mse, 2)))
print(paste("Test R-squared:", round(r2, 3)))
"""

# Save to file
r_code_path = Path("/mnt/data/Baracho_Monica_R_Train_Test_Split.R")
r_code_path.write_text(r_train_test_code.strip())

r_code_path.name


# --------------------------------------------
# Log-Transformed Loyalty Points Regression
# --------------------------------------------

# Remove zero or negative values before taking log
log_data <- subset(data, loyalty_points > 0)

# Log-transform the target variable
log_data$log_loyalty_points <- log(log_data$loyalty_points)

# Fit the log-linear model
log_model <- lm(log_loyalty_points ~ remuneration + age + spending_score, data = log_data)

# Summary of the model
summary(log_model)

# VIF check
vif(log_model)

# Plot predicted vs actual (exponentiated)
predicted_log <- exp(predict(log_model))
actual_log <- log_data$loyalty_points

plot(predicted_log, actual_log,
     xlab = "Predicted Loyalty Points",
     ylab = "Actual Loyalty Points",
     main = "Predicted vs Actual (Log-Model)",
     col = "steelblue", pch = 20)
abline(0, 1, col = "darkred", lwd = 2)

#  Histogram of residuals
hist(residuals(log_model), col = "skyblue", main = "Residuals (Log Model)", xlab = "Residuals")
"""

# Save the script
log_model_path = Path("/mnt/data/Baracho_Monica_Log_Regression_Model.R")
log_model_path.write_text(log_model_code.strip())

log_model_path.name







# ------------------------------------------------------------
# 5. Logistic Regression (Classification)
# ------------------------------------------------------------
data$high_value <- ifelse(data$loyalty_points >= 2000 & data$remuneration >= 50, 1, 0)
model_data <- data %>% select(high_value, remuneration, spending_score, age)
logit_model <- glm(high_value ~ remuneration + spending_score + age,
                   data = model_data,
                   family = binomial)
summary(logit_model)
exp(coef(logit_model))
predicted_probs <- predict(logit_model, type = "response")
predicted_class <- ifelse(predicted_probs >= 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class), as.factor(model_data$high_value))
roc_obj <- roc(model_data$high_value, predicted_probs)
plot(roc_obj, col = "blue")
auc(roc_obj)
table(model_data$high_value)
prop.table(table(model_data$high_value))
predicted_class <- ifelse(predicted_probs > 0.3, 1, 0)
confusionMatrix(as.factor(predicted_class), as.factor(model_data$high_value))

# Tidy the logistic model
tidy_logit <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE)

# Rename and format
logit_table <- tidy_logit %>%
  mutate(term = recode(term,
                       "(Intercept)" = "Intercept",
                       "remuneration" = "Remuneration (£k)",
                       "spending_score" = "Spending Score",
                       "age" = "Age")) %>%
  mutate(across(c(estimate, conf.low, conf.high, p.value), round, digits = 3)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(
    `Variable` = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `P-value` = p.value
  )

# Create beautiful table
logit_table %>%
  gt() %>%
  tab_header(
    title = "Logistic Regression Results",
    subtitle = "Odds Ratios for Predicting High-Value Customers"
  )



# Step 1: Fit the logistic regression model
logit_model <- glm(high_value ~ remuneration + spending_score + age,
                   data = model_data, family = "binomial")

# Step 2: Get predicted probabilities
predicted_probs <- predict(logit_model, type = "response")

# Step 3: Choose a threshold (e.g., 0.5 or optimized later)
threshold <- 0.5
predicted_class <- ifelse(predicted_probs >= threshold, 1, 0)

# Step 4: Confusion Matrix
conf_matrix <- confusionMatrix(as.factor(predicted_class),
                               as.factor(model_data$high_value),
                               positive = "1")  # Positive class is "1" (high-value)
print(conf_matrix)

# Step 5: ROC Curve and AUC
roc_obj <- roc(model_data$high_value, predicted_probs)

# Plot ROC
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve for Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal line
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")

# Step 6: Find Best Threshold (optional)
coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE)


# Recreate model_data with gender included
model_data <- data %>%
  select(high_value, remuneration, spending_score, age, gender)

# Fit logistic model including gender
logit_gender <- glm(high_value ~ remuneration + spending_score + age + gender,
                    data = model_data,
                    family = binomial)

# View summary
summary(logit_gender)

# Odds Ratios
exp(coef(logit_gender))

# 95% Confidence Intervals
exp(confint(logit_gender))

# Logistic model with gender
logit_gender <- glm(high_value ~ remuneration + spending_score + age + gender,
                    data = model_data,
                    family = binomial)
summary(logit_gender)

# Fit logistic regression model
logit_gender <- glm(high_value ~ remuneration + spending_score + age + gender,
                    data = model_data,
                    family = binomial)

# Tidy model with exponentiated coefficients and confidence intervals
logit_table <- tidy(logit_gender, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(term = recode(term,
                       "(Intercept)" = "Intercept",
                       "remuneration" = "Remuneration (£k)",
                       "spending_score" = "Spending Score",
                       "age" = "Age",
                       "genderMale" = "Gender (Male)")) %>%
  mutate(across(c(estimate, conf.low, conf.high), round, 3),
         p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(
    `Variable` = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `P-value` = p.value
  )

# Display using gt
logit_table %>%
  gt() %>%
  tab_header(
    title = "Logistic Regression Results",
    subtitle = "Predicting High-Value Customers"
  )


# Load package
library(modelsummary)

# Build all models
linear_model <- lm(loyalty_points ~ remuneration + spending_score + age, data = data)
log_model <- lm(log(loyalty_points) ~ remuneration + spending_score + age, data = data)
logit_model <- glm(high_value ~ remuneration + spending_score + age, data = data, family = "binomial")

# Load the modelsummary package
library(modelsummary)

# Re-fit the models if not already in your environment
linear_model <- lm(loyalty_points ~ remuneration + spending_score + age, data = data)
log_model <- lm(log(loyalty_points) ~ remuneration + spending_score + age, data = data)
logit_model <- glm(high_value ~ remuneration + spending_score + age, data = data, family = "binomial")

# Generate a clean comparison table
modelsummary(
  list(
    "Linear Model" = linear_model,
    "Log-Linear Model" = log_model,
    "Logistic Model" = logit_model
  ),
  exponentiate = c(FALSE, FALSE, TRUE),  # Only exponentiate the logistic model
  statistic = "std.error",               # Show standard errors instead of t/z values
  stars = TRUE,                          # Add significance stars
  output = "gt",                         # Use gt for a nice display
  title = "Model Comparison: Predicting Loyalty Points and High-Value Customers"
)

# Save to file
r_script_path = Path("/mnt/data/Baracho_Monica_DA301_Assignmen.R")
r_script_path.write_text(r_script)

r_script_path.name
