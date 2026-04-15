# ============================================================
# IT3011 - Theory and Practices in Statistical Modelling
# Section 3: Predictive Analytics (Full Version)
# Topic: Project Understanding vs Quality of Work
# ============================================================

# ---- Load Library ------------------------------------------
library(ggplot2)

# ---- Load Data ---------------------------------------------
df <- read.csv("Project understanding vs Quality work.csv", check.names = FALSE)

# ---- Rename Columns ----------------------------------------
colnames(df) <- c(
  "Timestamp",
  "Role",
  "Field",
  "Experience",
  "Q4",  # Project goals were clearly explained before the work began
  "Q5",  # I clearly understood the objectives of the project
  "Q6",  # I understood how my assigned tasks contributed to the overall project outcome
  "Q7",  # The expected quality standards for the project were clearly communicated
  "Q8",  # When I had doubts about project goals, I received sufficient clarification
  "Q9",  # Any changes to project goals were communicated effectively
  "Q10", # My work met the required quality standards
  "Q11", # I delivered tasks with minimal errors or defects
  "Q12", # I rarely had to redo tasks due to misunderstandings
  "Q13", # I often produced work that did not meet project expectations (REVERSE)
  "Q14", # I completed my assigned tasks within the required deadlines
  "Q15"  # My contribution had a positive impact on the overall success of the project
)

# ---- Reverse Score Q13 -------------------------------------
df$Q13_R <- 6 - df$Q13

# ---- Create Composite Scores -------------------------------
df$Understanding_Score <- (df$Q4 + df$Q5 + df$Q6 + df$Q7 + df$Q8 + df$Q9) / 6
df$Quality_Score       <- (df$Q10 + df$Q11 + df$Q12 + df$Q13_R + df$Q14 + df$Q15) / 6


# ============================================================
# PART A — SIMPLE LINEAR REGRESSION
# ============================================================

cat("============================================================\n")
cat("PART A — SIMPLE LINEAR REGRESSION\n")
cat("============================================================\n\n")

cat("Model: Quality Score = a + b x Understanding Score\n\n")

# --- A1: Build the Model ------------------------------------
simple_model   <- lm(Quality_Score ~ Understanding_Score, data = df)
simple_summary <- summary(simple_model)

# --- A2: Print Regression Equation --------------------------
a <- round(simple_model$coefficients[1], 4)
b <- round(simple_model$coefficients[2], 4)

cat("--- Regression Equation ---\n")
cat("Quality Score =", a, "+", b, "x Understanding Score\n\n")

# --- A3: Print Full Summary ---------------------------------
cat("--- Full Model Summary ---\n")
print(simple_summary)

# --- A4: Key Statistics -------------------------------------
r_squared     <- round(simple_summary$r.squared, 4)
adj_r_squared <- round(simple_summary$adj.r.squared, 4)
f_stat        <- round(simple_summary$fstatistic[1], 4)
p_value_model <- round(pf(simple_summary$fstatistic[1],
                          simple_summary$fstatistic[2],
                          simple_summary$fstatistic[3],
                          lower.tail = FALSE), 6)
rse           <- round(simple_summary$sigma, 4)

cat("\n--- Key Model Statistics ---\n")
cat("R-squared          =", r_squared, "\n")
cat("Adjusted R-squared =", adj_r_squared, "\n")
cat("F-statistic        =", f_stat, "\n")
cat("Model p-value      =", p_value_model, "\n")
cat("Residual Std Error =", rse, "\n")

cat("\n--- Interpretation ---\n")
cat("R-squared =", r_squared, "means Understanding Score explains",
    round(r_squared * 100, 1), "% of the variation in Quality Score.\n")
if (p_value_model < 0.05) {
  cat("The model is STATISTICALLY SIGNIFICANT (p < 0.05)\n")
  cat("Conclusion: Understanding Score is a significant predictor of Quality Score.\n")
} else {
  cat("The model is NOT statistically significant (p > 0.05)\n")
}
cat("\nSlope (b) =", b, "\n")
cat("For every 1 unit increase in Understanding Score,\n")
cat("Quality Score increases by", b, "units on average.\n")

# --- A5: Scatter Plot with Regression Line ------------------
ggplot(df, aes(x = Understanding_Score, y = Quality_Score)) +
  geom_point(color = "#1976D2", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  annotate("text", x = 1.5, y = 4.9,
           label = paste0("Y = ", a, " + ", b, " X"),
           color = "darkred", size = 4) +
  annotate("text", x = 1.5, y = 4.7,
           label = paste0("R2 = ", r_squared),
           color = "darkred", size = 4) +
  labs(title = "Simple Linear Regression: Understanding vs Quality Score",
       x = "Project Understanding Score (1 to 5)",
       y = "Work Quality Score (1 to 5)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("A1_simple_regression.png", width = 7, height = 5)


# ============================================================
# PART B — RESIDUAL ANALYSIS (Simple Model)
# ============================================================

cat("\n============================================================\n")
cat("PART B — RESIDUAL ANALYSIS\n")
cat("============================================================\n\n")

df$fitted_simple   <- simple_model$fitted.values
df$residual_simple <- simple_model$residuals

# --- B1: Shapiro-Wilk Test on Residuals ---------------------
shapiro_resid <- shapiro.test(df$residual_simple)
cat("--- Shapiro-Wilk Test on Residuals ---\n")
cat("W =", round(shapiro_resid$statistic, 4), "\n")
cat("p-value =", round(shapiro_resid$p.value, 4), "\n")
if (shapiro_resid$p.value > 0.05) {
  cat("Result: Residuals are NORMALLY distributed (p > 0.05)\n")
  cat("Conclusion: Regression normality assumption is satisfied.\n")
} else {
  cat("Result: Residuals are NOT normally distributed (p < 0.05)\n")
  cat("Conclusion: Normality assumption is slightly violated.\n")
  cat("            This is common with Likert scale survey data.\n")
}

# --- B2: Residuals vs Fitted Plot ---------------------------
ggplot(df, aes(x = fitted_simple, y = residual_simple)) +
  geom_point(color = "#1976D2", alpha = 0.6, size = 2.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Residual Plot — Simple Linear Regression",
       subtitle = "Residuals should be randomly scattered around 0",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("B1_residual_plot_simple.png", width = 7, height = 5)

# --- B3: Q-Q Plot of Residuals ------------------------------
png("B2_qq_residuals_simple.png", width = 700, height = 500)
qqnorm(df$residual_simple,
       main = "Q-Q Plot of Residuals (Simple Regression)",
       pch  = 16,
       col  = "#1976D2")
qqline(df$residual_simple, col = "red", lwd = 2)
dev.off()

# --- B4: Homoscedasticity Check -----------------------------
cat("\n--- Homoscedasticity Check (Constant Variance) ---\n")
lower_half <- df$residual_simple[df$fitted_simple < median(df$fitted_simple)]
upper_half <- df$residual_simple[df$fitted_simple >= median(df$fitted_simple)]
var_lower  <- round(var(lower_half), 4)
var_upper  <- round(var(upper_half), 4)
ratio      <- round(max(var_lower, var_upper) / min(var_lower, var_upper), 2)

cat("Variance of residuals (lower fitted values) =", var_lower, "\n")
cat("Variance of residuals (upper fitted values) =", var_upper, "\n")
cat("Variance ratio =", ratio, "\n")
if (ratio < 2) {
  cat("Result: Variances are similar -> Homoscedasticity is satisfied.\n")
} else {
  cat("Result: Variances differ -> Some heteroscedasticity may exist.\n")
}


# ============================================================
# PART C — TRAIN / TEST SPLIT (Overfitting Check)
# ============================================================

cat("\n============================================================\n")
cat("PART C — TRAIN / TEST SPLIT (Overfitting Check)\n")
cat("============================================================\n\n")

cat("Splitting data: 80% Training (build model), 20% Testing (evaluate model)\n\n")

# --- C1: Split Data -----------------------------------------
set.seed(42)
n          <- nrow(df)
train_size <- round(0.8 * n)
train_idx  <- sample(1:n, train_size, replace = FALSE)

train_df <- df[train_idx, ]
test_df  <- df[-train_idx, ]

cat("Training set size:", nrow(train_df), "rows\n")
cat("Testing set size :", nrow(test_df), "rows\n\n")

# --- C2: Build Model on Training Data -----------------------
train_model   <- lm(Quality_Score ~ Understanding_Score, data = train_df)
train_summary <- summary(train_model)
train_r2      <- round(train_summary$r.squared, 4)
cat("Training Model R-squared =", train_r2, "\n\n")

# --- C3: Predict on Test Data -------------------------------
test_predictions <- predict(train_model, newdata = test_df)
test_actual      <- test_df$Quality_Score

# --- C4: Calculate Test Performance Metrics -----------------
rmse     <- round(sqrt(mean((test_actual - test_predictions)^2)), 4)
mae      <- round(mean(abs(test_actual - test_predictions)), 4)
ss_res   <- sum((test_actual - test_predictions)^2)
ss_total <- sum((test_actual - mean(test_actual))^2)
test_r2  <- round(1 - ss_res / ss_total, 4)

cat("--- Test Set Performance ---\n")
cat("Test R-squared =", test_r2, "\n")
cat("RMSE (Root Mean Squared Error) =", rmse, "\n")
cat("MAE  (Mean Absolute Error)     =", mae, "\n")

# --- C5: Overfitting Check ----------------------------------
cat("\n--- Overfitting Check ---\n")
cat("Training R-squared =", train_r2, "\n")
cat("Testing  R-squared =", test_r2, "\n")
diff <- round(abs(train_r2 - test_r2), 4)
cat("Difference         =", diff, "\n")

if (diff < 0.1) {
  cat("Result: Difference is small (< 0.10)\n")
  cat("Conclusion: NO significant overfitting detected.\n")
  cat("            The model generalizes well to unseen data.\n")
} else if (diff < 0.2) {
  cat("Result: Moderate difference (0.10 to 0.20)\n")
  cat("Conclusion: Slight overfitting may exist.\n")
} else {
  cat("Result: Large difference (> 0.20)\n")
  cat("Conclusion: Significant overfitting detected.\n")
}

# --- C6: Actual vs Predicted Plot ---------------------------
test_plot_df <- data.frame(
  Actual    = test_actual,
  Predicted = test_predictions
)

ggplot(test_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#1976D2", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 2.2, y = 4.8,
           label = paste0("Test R2 = ", test_r2),
           color = "darkred", size = 4) +
  annotate("text", x = 2.2, y = 4.6,
           label = paste0("RMSE = ", rmse),
           color = "darkred", size = 4) +
  labs(title = "Actual vs Predicted Quality Score — Test Set",
       subtitle = "Points close to red line = good predictions",
       x = "Actual Quality Score",
       y = "Predicted Quality Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("C1_actual_vs_predicted.png", width = 7, height = 5)


# ============================================================
# PART D — MULTIPLE LINEAR REGRESSION
# ============================================================

cat("\n============================================================\n")
cat("PART D — MULTIPLE LINEAR REGRESSION\n")
cat("============================================================\n\n")

cat("Model: Quality Score = a + b1xQ4 + b2xQ5 + b3xQ6 + b4xQ7 + b5xQ8 + b6xQ9\n\n")

# --- D1: Build the Model ------------------------------------
multi_model   <- lm(Quality_Score ~ Q4 + Q5 + Q6 + Q7 + Q8 + Q9, data = df)
multi_summary <- summary(multi_model)

cat("--- Full Model Summary ---\n")
print(multi_summary)

# --- D2: Key Statistics -------------------------------------
r_squared_multi     <- round(multi_summary$r.squared, 4)
adj_r_squared_multi <- round(multi_summary$adj.r.squared, 4)
f_stat_multi        <- round(multi_summary$fstatistic[1], 4)
p_value_multi       <- round(pf(multi_summary$fstatistic[1],
                                multi_summary$fstatistic[2],
                                multi_summary$fstatistic[3],
                                lower.tail = FALSE), 6)
rse_multi           <- round(multi_summary$sigma, 4)

cat("\n--- Key Model Statistics ---\n")
cat("R-squared          =", r_squared_multi, "\n")
cat("Adjusted R-squared =", adj_r_squared_multi, "\n")
cat("F-statistic        =", f_stat_multi, "\n")
cat("Model p-value      =", p_value_multi, "\n")
cat("Residual Std Error =", rse_multi, "\n")

cat("\n--- Interpretation ---\n")
cat("R-squared =", r_squared_multi, "means Q4 to Q9 together explain",
    round(r_squared_multi * 100, 1), "% of the variation in Quality Score.\n")
if (p_value_multi < 0.05) {
  cat("The model is STATISTICALLY SIGNIFICANT (p < 0.05)\n")
} else {
  cat("The model is NOT statistically significant (p > 0.05)\n")
}

# --- D3: Significant Predictors -----------------------------
cat("\n--- Significant Predictors (p < 0.05) ---\n")
coefficients_table <- as.data.frame(multi_summary$coefficients)
coefficients_table <- round(coefficients_table, 4)
colnames(coefficients_table) <- c("Estimate", "Std_Error", "t_value", "p_value")

predictors        <- c("Q4", "Q5", "Q6", "Q7", "Q8", "Q9")
found_significant <- FALSE

for (pred in predictors) {
  p     <- coefficients_table[pred, "p_value"]
  b_val <- coefficients_table[pred, "Estimate"]
  if (p < 0.05) {
    cat(pred, "-> Estimate =", b_val, ", p-value =", p, "** SIGNIFICANT **\n")
    found_significant <- TRUE
  } else {
    cat(pred, "-> Estimate =", b_val, ", p-value =", p, "(not significant)\n")
  }
}

# --- D4: Model Comparison -----------------------------------
cat("\n--- Model Comparison: Simple vs Multiple ---\n")
cat("Simple Regression R-squared   =", r_squared, "\n")
cat("Multiple Regression R-squared =", r_squared_multi, "\n")
improvement <- round((r_squared_multi - r_squared) * 100, 1)
cat("Improvement in R-squared      =", improvement, "%\n")
if (r_squared_multi > r_squared) {
  cat("Multiple regression explains MORE variation than simple regression.\n")
} else {
  cat("Simple regression performs similarly to multiple regression.\n")
}

# --- D5: Coefficient Plot -----------------------------------
coef_df <- data.frame(
  Predictor   = predictors,
  Estimate    = as.numeric(coefficients_table[predictors, "Estimate"]),
  p_value     = as.numeric(coefficients_table[predictors, "p_value"])
)
coef_df$Significant <- ifelse(coef_df$p_value < 0.05, "Significant", "Not Significant")

ggplot(coef_df, aes(x = Predictor, y = Estimate, fill = Significant)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_text(aes(label = round(Estimate, 3)),
            vjust = ifelse(coef_df$Estimate >= 0, -0.5, 1.5), size = 3.5) +
  scale_fill_manual(values = c("Significant"     = "#388E3C",
                               "Not Significant" = "#B0BEC5")) +
  labs(title = "Multiple Regression Coefficients (Q4 to Q9)",
       x    = "Understanding Question",
       y    = "Coefficient Estimate",
       fill = "") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "top")
ggsave("D1_coefficient_plot.png", width = 7, height = 5)


# ============================================================
# PART E — MULTICOLLINEARITY CHECK (VIF)
# ============================================================

cat("\n============================================================\n")
cat("PART E — MULTICOLLINEARITY CHECK (VIF)\n")
cat("============================================================\n\n")

cat("VIF (Variance Inflation Factor) checks if predictors are\n")
cat("too correlated with each other.\n")
cat("Rule: VIF < 5 = acceptable\n")
cat("      VIF 5 to 10 = concerning\n")
cat("      VIF > 10 = severe multicollinearity\n\n")

# --- E1: Calculate VIF manually -----------------------------
vif_values <- c()

for (pred in predictors) {
  other_preds <- predictors[predictors != pred]
  formula_str <- paste(pred, "~", paste(other_preds, collapse = " + "))
  vif_model   <- lm(as.formula(formula_str), data = df)
  r2_vif      <- summary(vif_model)$r.squared
  vif_val     <- round(1 / (1 - r2_vif), 3)
  vif_values  <- c(vif_values, vif_val)
}

vif_table <- data.frame(
  Predictor = predictors,
  VIF       = vif_values
)

cat("--- VIF Results ---\n")
print(vif_table)

cat("\n--- VIF Interpretation ---\n")
for (i in 1:nrow(vif_table)) {
  pred <- vif_table$Predictor[i]
  vif  <- vif_table$VIF[i]
  if (vif < 5) {
    cat(pred, "-> VIF =", vif, "-> Acceptable (no multicollinearity)\n")
  } else if (vif < 10) {
    cat(pred, "-> VIF =", vif, "-> Concerning (moderate multicollinearity)\n")
  } else {
    cat(pred, "-> VIF =", vif, "-> Severe multicollinearity!\n")
  }
}

# --- E2: Correlation Matrix of Predictors -------------------
cat("\n--- Correlation Matrix of Q4 to Q9 ---\n")
corr_matrix <- round(cor(df[, c("Q4","Q5","Q6","Q7","Q8","Q9")]), 3)
print(corr_matrix)

# --- E3: Correlation Heatmap --------------------------------
corr_df <- data.frame(
  Var1  = character(),
  Var2  = character(),
  Value = numeric()
)

for (i in 1:6) {
  for (j in 1:6) {
    corr_df <- rbind(corr_df, data.frame(
      Var1  = predictors[i],
      Var2  = predictors[j],
      Value = corr_matrix[i, j]
    ))
  }
}

ggplot(corr_df, aes(x = Var1, y = Var2, fill = Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Value, 2)), size = 3.5) +
  scale_fill_gradient2(low      = "#d32f2f",
                       mid      = "white",
                       high     = "#388E3C",
                       midpoint = 0,
                       limits   = c(-1, 1)) +
  labs(title = "Correlation Heatmap of Understanding Questions (Q4 to Q9)",
       x    = "",
       y    = "",
       fill = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("E1_correlation_heatmap.png", width = 7, height = 6)

# ============================================================
cat("\n\n=== Section 3 Complete! ===\n")
cat("Plots saved:\n")
cat("  A1_simple_regression.png\n")
cat("  B1_residual_plot_simple.png\n")
cat("  B2_qq_residuals_simple.png\n")
cat("  C1_actual_vs_predicted.png\n")
cat("  D1_coefficient_plot.png\n")
cat("  E1_correlation_heatmap.png\n")
cat("\nAll 3 sections are now complete!\n")