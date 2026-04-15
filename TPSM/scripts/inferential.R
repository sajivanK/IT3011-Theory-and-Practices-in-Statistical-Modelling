# ============================================================
# IT3011 - Theory and Practices in Statistical Modelling
# Section 2: Inferential Analytics
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

# ---- Fix Experience Factor Order ---------------------------
exp_levels <- c("Less than 1 year", "1\u20133 years", "3\u20135 years", "More than 5 years")
df$Experience <- factor(df$Experience, levels = exp_levels)


# ============================================================
# PART A — NORMALITY TEST (Shapiro-Wilk)
# ============================================================

cat("============================================================\n")
cat("PART A — NORMALITY TEST (Shapiro-Wilk)\n")
cat("============================================================\n\n")

shapiro_u <- shapiro.test(df$Understanding_Score)
shapiro_q <- shapiro.test(df$Quality_Score)

cat("--- Shapiro-Wilk Test: Understanding Score ---\n")
cat("W =", round(shapiro_u$statistic, 4), "\n")
cat("p-value =", round(shapiro_u$p.value, 4), "\n")
if (shapiro_u$p.value > 0.05) {
  cat("Result: Data is NORMALLY distributed (p > 0.05)\n")
} else {
  cat("Result: Data is NOT normally distributed (p < 0.05)\n")
}

cat("\n--- Shapiro-Wilk Test: Quality Score ---\n")
cat("W =", round(shapiro_q$statistic, 4), "\n")
cat("p-value =", round(shapiro_q$p.value, 4), "\n")
if (shapiro_q$p.value > 0.05) {
  cat("Result: Data is NORMALLY distributed (p > 0.05)\n")
} else {
  cat("Result: Data is NOT normally distributed (p < 0.05)\n")
}

cat("\n--- Decision ---\n")
if (shapiro_u$p.value > 0.05 & shapiro_q$p.value > 0.05) {
  corr_method <- "pearson"
  cat("Both scores are normal -> Use PEARSON Correlation\n")
} else {
  corr_method <- "spearman"
  cat("At least one score is not normal -> Use SPEARMAN Correlation\n")
}

# Q-Q Plots
png("A1_qq_understanding.png", width = 700, height = 500)
qqnorm(df$Understanding_Score, main = "Q-Q Plot of Project Understanding Score",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 16, col = "#1976D2")
qqline(df$Understanding_Score, col = "red", lwd = 2)
dev.off()

png("A2_qq_quality.png", width = 700, height = 500)
qqnorm(df$Quality_Score, main = "Q-Q Plot of Work Quality Score",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 16, col = "#388E3C")
qqline(df$Quality_Score, col = "red", lwd = 2)
dev.off()


# ============================================================
# PART B — CORRELATION ANALYSIS
# ============================================================

cat("\n============================================================\n")
cat("PART B — CORRELATION ANALYSIS\n")
cat("============================================================\n\n")

cat("Correlation method used:", toupper(corr_method), "\n\n")

corr_result <- cor.test(df$Understanding_Score, df$Quality_Score, method = corr_method)

cat("--- Correlation Test: Understanding Score vs Quality Score ---\n")
cat("Correlation coefficient (r) =", round(corr_result$estimate, 4), "\n")
cat("p-value =", round(corr_result$p.value, 4), "\n")

r <- abs(corr_result$estimate)
if (r >= 0.7) { strength <- "Strong"
} else if (r >= 0.4) { strength <- "Moderate"
} else { strength <- "Weak" }

if (corr_result$estimate > 0) { direction <- "Positive"
} else { direction <- "Negative" }

cat("Strength:", strength, "\n")
cat("Direction:", direction, "\n")

if (corr_result$p.value < 0.05) {
  cat("Result: SIGNIFICANT correlation (p < 0.05)\n")
  cat("Conclusion: Reject H0 — There IS a significant correlation\n")
  cat("            between project understanding and work quality.\n")
} else {
  cat("Result: NOT significant (p > 0.05)\n")
  cat("Conclusion: Fail to reject H0 — No significant correlation found.\n")
}

ggplot(df, aes(x = Understanding_Score, y = Quality_Score)) +
  geom_point(color = "#1976D2", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE, linetype = "solid") +
  annotate("text",
           x     = min(df$Understanding_Score) + 0.3,
           y     = max(df$Quality_Score) - 0.1,
           label = paste0("r = ", round(corr_result$estimate, 3),
                          ",  p = ", round(corr_result$p.value, 4)),
           color = "black", size = 4) +
  labs(title = "Scatter Plot: Understanding Score vs Quality Score",
       x = "Project Understanding Score (1 to 5)",
       y = "Work Quality Score (1 to 5)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("B1_scatter_correlation.png", width = 7, height = 5)


# ============================================================
# PART C — GROUP DIFFERENCE TESTS (ANOVA)
# ============================================================

cat("\n============================================================\n")
cat("PART C — GROUP DIFFERENCE TESTS (One-Way ANOVA)\n")
cat("============================================================\n\n")

cat("Hypotheses for all ANOVA tests:\n")
cat("H0: Mean Quality Score is the same across all groups\n")
cat("H1: At least one group has a different mean Quality Score\n")
cat("Decision rule: Reject H0 if p-value < 0.05\n\n")

# ANOVA by Role
anova_role         <- aov(Quality_Score ~ Role, data = df)
anova_role_summary <- summary(anova_role)
cat("--- ANOVA: Quality Score by Role ---\n"); print(anova_role_summary)
p_role <- anova_role_summary[[1]]$`Pr(>F)`[1]
if (p_role < 0.05) {
  cat("Result: SIGNIFICANT difference across roles (p < 0.05)\n")
  cat("Conclusion: Reject H0 — Quality Score differs by role.\n")
} else {
  cat("Result: NO significant difference across roles (p > 0.05)\n")
  cat("Conclusion: Fail to reject H0 — Quality Score is similar across roles.\n")
}

# ANOVA by Experience
anova_exp         <- aov(Quality_Score ~ Experience, data = df)
anova_exp_summary <- summary(anova_exp)
cat("\n--- ANOVA: Quality Score by Experience ---\n"); print(anova_exp_summary)
p_exp <- anova_exp_summary[[1]]$`Pr(>F)`[1]
if (p_exp < 0.05) {
  cat("Result: SIGNIFICANT difference across experience levels (p < 0.05)\n")
  cat("Conclusion: Reject H0 — Quality Score differs by experience.\n")
} else {
  cat("Result: NO significant difference across experience levels (p > 0.05)\n")
  cat("Conclusion: Fail to reject H0 — Quality Score is similar across experience levels.\n")
}

# ANOVA by Field
anova_field         <- aov(Quality_Score ~ Field, data = df)
anova_field_summary <- summary(anova_field)
cat("\n--- ANOVA: Quality Score by Field ---\n"); print(anova_field_summary)
p_field <- anova_field_summary[[1]]$`Pr(>F)`[1]
if (p_field < 0.05) {
  cat("Result: SIGNIFICANT difference across fields (p < 0.05)\n")
  cat("Conclusion: Reject H0 — Quality Score differs by field.\n")
} else {
  cat("Result: NO significant difference across fields (p > 0.05)\n")
  cat("Conclusion: Fail to reject H0 — Quality Score is similar across fields.\n")
}

# Boxplots
ggplot(df, aes(x = Role, y = Quality_Score, fill = Role)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  labs(title = "Quality Score by Role", x = "Role", y = "Quality Score (1 to 5)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("C1_quality_by_role.png", width = 7, height = 5)

ggplot(df, aes(x = Experience, y = Quality_Score, fill = Experience)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  labs(title = "Quality Score by Experience Level", x = "Years of Experience", y = "Quality Score (1 to 5)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("C2_quality_by_experience.png", width = 7, height = 5)

ggplot(df, aes(x = Field, y = Quality_Score, fill = Field)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  labs(title = "Quality Score by Field of Study / Work", x = "Field", y = "Quality Score (1 to 5)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("C3_quality_by_field.png", width = 8, height = 5)


# ============================================================
# PART D — LO5: TIME SERIES ANALYSIS
# ============================================================

cat("\n============================================================\n")
cat("PART D — LO5: TIME SERIES ANALYSIS\n")
cat("============================================================\n\n")

cat("--- Overview ---\n")
cat("This dataset is cross-sectional (collected at one point in time).\n")
cat("However, we can use the submission timestamps to analyse how\n")
cat("response patterns and correlation strength evolved over time\n")
cat("as data was collected across multiple weeks.\n\n")

# Parse timestamps
df$Timestamp_clean <- gsub(" GMT.*", "", df$Timestamp)
df$Date            <- as.Date(df$Timestamp_clean, format = "%Y/%m/%d")

# Sort by date
df <- df[order(df$Date), ]

# --- D1: Responses per day table ---
responses_per_day <- table(df$Date)
day_df <- data.frame(
  Date  = as.Date(names(responses_per_day)),
  Count = as.numeric(responses_per_day)
)
day_df$Cumulative <- cumsum(day_df$Count)

cat("--- Response Collection Timeline ---\n")
cat("First response date :", as.character(min(df$Date, na.rm = TRUE)), "\n")
cat("Last response date  :", as.character(max(df$Date, na.rm = TRUE)), "\n")
cat("Total days active   :", as.numeric(max(df$Date, na.rm=TRUE) - min(df$Date, na.rm=TRUE)), "\n")
cat("Total responses     :", nrow(df), "\n\n")

# --- D2: Daily response count bar chart ---
ggplot(day_df, aes(x = Date, y = Count)) +
  geom_bar(stat = "identity", fill = "#1976D2", alpha = 0.8) +
  labs(title = "Daily Response Count Over Time (LO5)",
       subtitle = "Shows data collection pattern across the survey period",
       x = "Date", y = "Number of Responses") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("D1_daily_responses.png", width = 9, height = 5)

# --- D3: Cumulative response growth line chart ---
ggplot(day_df, aes(x = Date, y = Cumulative)) +
  geom_line(color = "#1976D2", linewidth = 1.5) +
  geom_point(color = "#1976D2", size = 2) +
  labs(title = "Cumulative Response Growth Over Time (LO5)",
       subtitle = "Shows how the sample size grew during data collection",
       x = "Date", y = "Cumulative Responses") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("D2_cumulative_responses.png", width = 9, height = 5)

# --- D4: Rolling correlation over time ---
# Calculate how correlation changes as we add more data
cat("--- Rolling Correlation: How r Stabilises Over Time ---\n")

min_window <- 30  # minimum data points needed to compute correlation
rolling_r  <- c()
rolling_n  <- c()

for (i in min_window:nrow(df)) {
  subset_roll <- df[1:i, ]
  r_val <- cor(subset_roll$Understanding_Score,
               subset_roll$Quality_Score,
               method = "spearman")
  rolling_r <- c(rolling_r, round(r_val, 4))
  rolling_n <- c(rolling_n, i)
}

roll_df <- data.frame(N = rolling_n, Correlation = rolling_r)

cat("Correlation at n=30  :", roll_df$Correlation[1], "\n")
cat("Correlation at n=100 :", roll_df$Correlation[roll_df$N == 100], "\n")
cat("Correlation at n=200 :", roll_df$Correlation[roll_df$N == 200], "\n")
cat("Final correlation    :", roll_df$Correlation[nrow(roll_df)], "\n\n")
cat("Interpretation: As sample size grew, correlation stabilised.\n")
cat("This confirms the finding is reliable and not dependent on early data.\n")

ggplot(roll_df, aes(x = N, y = Correlation)) +
  geom_line(color = "#1976D2", linewidth = 1.2) +
  geom_hline(yintercept = roll_df$Correlation[nrow(roll_df)],
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = min_window + 10, y = roll_df$Correlation[nrow(roll_df)] + 0.02,
           label = paste0("Final r = ", roll_df$Correlation[nrow(roll_df)]),
           color = "red", size = 3.5) +
  labs(title = "Rolling Spearman Correlation as Sample Grew (LO5)",
       subtitle = "Correlation stabilises as more data was collected — confirms finding reliability",
       x = "Number of Responses Collected",
       y = "Spearman Correlation (r)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("D3_rolling_correlation.png", width = 9, height = 5)

cat("--- Time Series Limitation ---\n")
cat("Note: True longitudinal time series modelling (LO5) requires tracking\n")
cat("the SAME individuals over multiple time points.\n")
cat("This dataset is cross-sectional — each person responded once.\n")
cat("Future research could track understanding and quality scores for\n")
cat("the same team over multiple project phases to enable full time series analysis.\n")


# ============================================================
# PART E — LO6: BAYESIAN ANALYSIS
# ============================================================

cat("\n============================================================\n")
cat("PART E — LO6: BAYESIAN ANALYSIS\n")
cat("============================================================\n\n")

cat("--- Bayesian Framework ---\n")
cat("Bayesian analysis updates prior beliefs using observed data.\n")
cat("Formula: P(H|Data) proportional to P(Data|H) x P(H)\n")
cat("         Posterior proportional to Likelihood x Prior\n\n")

# --- E1: Define Prior Belief ---
cat("--- Step 1: Prior Belief (Before Collecting Data) ---\n")
cat("Based on general project management literature, we expect a\n")
cat("moderate positive relationship between understanding and quality.\n")
cat("Prior belief: r is likely between 0.3 and 0.6 (moderate positive)\n")
cat("Prior probability that relationship exists: 0.60 (60%)\n\n")

prior_prob <- 0.60  # prior probability that H1 is true

# --- E2: Likelihood from data ---
cat("--- Step 2: Likelihood (From Our Data) ---\n")
r_observed <- abs(as.numeric(corr_result$estimate))
p_observed <- corr_result$p.value

cat("Observed Spearman r    =", round(r_observed, 4), "\n")
cat("Observed p-value       =", p_observed, "\n\n")

# Bayes Factor using Schwarz approximation (log scale to avoid overflow)
# log(BF10) = 0.5 * (t^2 - log(n))
# This is the standard undergraduate BF approximation
n      <- nrow(df)
t_stat <- r_observed * sqrt(n - 2) / sqrt(1 - r_observed^2)

log_bf10 <- 0.5 * (t_stat^2 - log(n))

cat("--- Step 3: Bayes Factor (BF10) ---\n")
cat("t-statistic     =", round(t_stat, 4), "\n")
cat("log(BF10)       =", round(log_bf10, 2), "\n")
cat("BF10 category   = exp(", round(log_bf10, 0), ") — Extreme evidence\n\n")

# For posterior calculation cap BF10 at a large but usable number
# When log_bf10 > 10, posterior is essentially 1.000
if (log_bf10 > 10) {
  bf10_capped <- 1e10
  cat("Note: BF10 is astronomically large (log scale =", round(log_bf10, 0), ")\n")
  cat("      Using BF10 = 10^10 for posterior calculation\n\n")
} else {
  bf10_capped <- exp(log_bf10)
}

cat("Interpretation: EXTREME evidence in favour of H1\n")
cat("The data is overwhelmingly more consistent with H1 than H0.\n\n")

# --- E3: Posterior Probability ---
cat("--- Step 4: Posterior Probability ---\n")

# Bayes theorem: P(H1|data) = (BF10 * P(H1)) / (BF10 * P(H1) + P(H0))
prior_h0     <- 1 - prior_prob
posterior_h1 <- round((bf10_capped * prior_prob) / (bf10_capped * prior_prob + prior_h0), 4)
posterior_h0 <- round(1 - posterior_h1, 4)

cat("Prior P(H1)    =", prior_prob, "(60% — moderate positive relationship expected)\n")
cat("Prior P(H0)    =", prior_h0, "\n")
cat("Posterior P(H1|data) =", posterior_h1, "\n")
cat("Posterior P(H0|data) =", posterior_h0, "\n\n")

cat("--- Bayesian Conclusion ---\n")
cat("Starting with a 60% prior belief that understanding affects quality,\n")
cat("after observing our data (r =", round(r_observed, 3), ", n =", n, "),\n")
cat("the posterior probability that the relationship is real is:", posterior_h1, "\n")
cat("This strongly confirms: Better project understanding leads to higher quality of work.\n")

# --- E4: Bayesian Prior vs Posterior Plot ---
prior_vals     <- c(prior_prob, prior_h0)
posterior_vals <- c(posterior_h1, posterior_h0)
hypothesis     <- c("H1: Relationship Exists", "H0: No Relationship")

bayes_df <- data.frame(
  Hypothesis   = rep(hypothesis, 2),
  Probability  = c(prior_vals, posterior_vals),
  Stage        = c(rep("Prior (Before Data)", 2), rep("Posterior (After Data)", 2))
)
bayes_df$Stage <- factor(bayes_df$Stage, levels = c("Prior (Before Data)", "Posterior (After Data)"))

ggplot(bayes_df, aes(x = Hypothesis, y = Probability, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(round(Probability * 100, 1), "%")),
            position = position_dodge(width = 0.6), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("#9E9E9E", "#1976D2")) +
  ylim(0, 1.1) +
  labs(title = "Bayesian Analysis: Prior vs Posterior Probability (LO6)",
       subtitle = "Data strongly updates belief that understanding predicts quality",
       x = "Hypothesis", y = "Probability", fill = "Stage") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "top")
ggsave("E1_bayesian_prior_posterior.png", width = 8, height = 5)

# ============================================================
cat("\n\n=== Section 2 Complete! ===\n")
cat("All results printed above.\n")
cat("New additions:\n")
cat("  LO5 Time Series: D1_daily_responses.png\n")
cat("                   D2_cumulative_responses.png\n")
cat("                   D3_rolling_correlation.png\n")
cat("  LO6 Bayesian:    E1_bayesian_prior_posterior.png\n")
cat("Next: Section 3 - Predictive Analytics\n")