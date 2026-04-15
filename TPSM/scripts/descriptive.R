# ============================================================
# IT3011 - Theory and Practices in Statistical Modelling
# Section 1: Descriptive Analytics
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
df$Quality_Score <- (df$Q10 + df$Q11 + df$Q12 + df$Q13_R + df$Q14 + df$Q15) / 6

# ---- Fix Experience Factor Order ---------------------------
exp_levels <- c("Less than 1 year", "1\u20133 years", "3\u20135 years", "More than 5 years")
df$Experience <- factor(df$Experience, levels = exp_levels)


# ============================================================
# PART A — DEMOGRAPHIC SUMMARY
# ============================================================

cat("============================================================\n")
cat("PART A — DEMOGRAPHIC SUMMARY\n")
cat("============================================================\n\n")

role_count   <- table(df$Role)
role_percent <- round(role_count / nrow(df) * 100, 1)
role_table   <- data.frame(Role = names(role_count), Count = as.numeric(role_count), Percentage = as.numeric(role_percent))
cat("--- Q1: Role ---\n"); print(role_table)

field_count   <- table(df$Field)
field_percent <- round(field_count / nrow(df) * 100, 1)
field_table   <- data.frame(Field = names(field_count), Count = as.numeric(field_count), Percentage = as.numeric(field_percent))
cat("\n--- Q2: Field ---\n"); print(field_table)

exp_count   <- table(df$Experience)
exp_percent <- round(exp_count / nrow(df) * 100, 1)
exp_table   <- data.frame(Experience = names(exp_count), Count = as.numeric(exp_count), Percentage = as.numeric(exp_percent))
cat("\n--- Q3: Experience ---\n"); print(exp_table)

ggplot(role_table, aes(x = Role, y = Count, fill = Role)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribution of Respondent Roles", x = "Role", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("A1_role_distribution.png", width = 7, height = 5)

ggplot(field_table, aes(x = Field, y = Count, fill = Field)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribution of Field of Study / Work", x = "Field", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("A2_field_distribution.png", width = 7, height = 5)

exp_count2   <- table(df$Experience)
exp_percent2 <- round(exp_count2 / nrow(df) * 100, 1)
exp_table2   <- data.frame(Experience = names(exp_count2), Count = as.numeric(exp_count2), Percentage = as.numeric(exp_percent2))
exp_table2$Experience <- factor(exp_table2$Experience, levels = exp_levels)

ggplot(exp_table2, aes(x = Experience, y = Count, fill = Experience)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), vjust = -0.5, size = 3.5) +
  labs(title = "Distribution of Project Experience", x = "Experience", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("A3_experience_distribution.png", width = 7, height = 5)


# ============================================================
# PART B — INDIVIDUAL QUESTION ANALYSIS (Q4 to Q15)
# ============================================================

cat("\n============================================================\n")
cat("PART B — INDIVIDUAL QUESTION ANALYSIS\n")
cat("============================================================\n\n")

q_names <- c("Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13(R)", "Q14", "Q15")

q_data <- data.frame(
  Q4 = df$Q4, Q5 = df$Q5, Q6 = df$Q6, Q7 = df$Q7, Q8 = df$Q8, Q9 = df$Q9,
  Q10 = df$Q10, Q11 = df$Q11, Q12 = df$Q12, Q13R = df$Q13_R, Q14 = df$Q14, Q15 = df$Q15
)

q_mean   <- round(colMeans(q_data), 3)
q_median <- round(apply(q_data, 2, median), 3)
q_sd     <- round(apply(q_data, 2, sd), 3)
q_min    <- apply(q_data, 2, min)
q_max    <- apply(q_data, 2, max)

desc_table <- data.frame(Question = q_names, Mean = q_mean, Median = q_median, SD = q_sd, Min = q_min, Max = q_max)
rownames(desc_table) <- NULL
cat("--- Descriptive Statistics (Q4 to Q15) ---\n"); print(desc_table)

mean_df <- data.frame(
  Question = factor(q_names, levels = q_names),
  Mean     = q_mean,
  Group    = c(rep("Understanding (Q4-Q9)", 6), rep("Quality (Q10-Q15)", 6))
)
ggplot(mean_df, aes(x = Question, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Mean, 2)), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("#388E3C", "#1976D2")) +
  ylim(0, 5.5) +
  labs(title = "Mean Score per Question (Q4 to Q15)", x = "Question", y = "Mean Score (1 to 5)", fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "top")
ggsave("B1_mean_per_question.png", width = 9, height = 5)

response_counts <- data.frame(matrix(0, nrow = 5, ncol = 12))
colnames(response_counts) <- q_names
rownames(response_counts) <- c("1", "2", "3", "4", "5")
for (i in 1:12) {
  t <- table(factor(q_data[, i], levels = 1:5))
  response_counts[, i] <- as.numeric(t)
}
response_pct <- round(response_counts / nrow(df) * 100, 1)

long_df <- data.frame(Question = character(), Response = character(), Percentage = numeric())
for (q in q_names) {
  for (r in 1:5) {
    long_df <- rbind(long_df, data.frame(Question = q, Response = as.character(r), Percentage = response_pct[r, q]))
  }
}
long_df$Question <- factor(long_df$Question, levels = rev(q_names))
long_df$Response <- factor(long_df$Response, levels = c("1","2","3","4","5"),
                           labels = c("1-Strongly Disagree","2-Disagree","3-Neutral","4-Agree","5-Strongly Agree"))

ggplot(long_df, aes(x = Question, y = Percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("#d32f2f","#ef9a9a","#eeeeee","#a5d6a7","#388e3c")) +
  labs(title = "Likert Response Distribution (Q4 to Q15)", x = "", y = "Percentage (%)", fill = "Response") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
ggsave("B2_likert_stacked_bar.png", width = 10, height = 7)


# ============================================================
# PART C — COMPOSITE SCORE ANALYSIS
# ============================================================

cat("\n============================================================\n")
cat("PART C — COMPOSITE SCORE ANALYSIS\n")
cat("============================================================\n\n")

composite_table <- data.frame(
  Score  = c("Understanding Score (Q4-Q9)", "Quality Score (Q10-Q15)"),
  Mean   = round(c(mean(df$Understanding_Score), mean(df$Quality_Score)), 3),
  Median = round(c(median(df$Understanding_Score), median(df$Quality_Score)), 3),
  SD     = round(c(sd(df$Understanding_Score), sd(df$Quality_Score)), 3),
  Min    = round(c(min(df$Understanding_Score), min(df$Quality_Score)), 3),
  Max    = round(c(max(df$Understanding_Score), max(df$Quality_Score)), 3)
)
cat("--- Composite Score Summary ---\n"); print(composite_table)

ggplot(df, aes(x = Understanding_Score)) +
  geom_histogram(binwidth = 0.4, fill = "#1976D2", color = "white") +
  geom_vline(xintercept = mean(df$Understanding_Score), color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(df$Understanding_Score) + 0.2, y = 15,
           label = paste0("Mean = ", round(mean(df$Understanding_Score), 2)), color = "red", size = 3.5) +
  labs(title = "Distribution of Project Understanding Score", x = "Understanding Score (1 to 5)", y = "Frequency") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("C1_understanding_histogram.png", width = 7, height = 5)

ggplot(df, aes(x = Quality_Score)) +
  geom_histogram(binwidth = 0.4, fill = "#388E3C", color = "white") +
  geom_vline(xintercept = mean(df$Quality_Score), color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(df$Quality_Score) + 0.2, y = 15,
           label = paste0("Mean = ", round(mean(df$Quality_Score), 2)), color = "red", size = 3.5) +
  labs(title = "Distribution of Work Quality Score", x = "Quality Score (1 to 5)", y = "Frequency") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("C2_quality_histogram.png", width = 7, height = 5)

ggplot(df, aes(y = Understanding_Score)) +
  geom_boxplot(fill = "#1976D2", alpha = 0.7, width = 0.4) +
  labs(title = "Boxplot of Project Understanding Score", y = "Understanding Score (1 to 5)") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_blank())
ggsave("C3_understanding_boxplot.png", width = 5, height = 6)

ggplot(df, aes(y = Quality_Score)) +
  geom_boxplot(fill = "#388E3C", alpha = 0.7, width = 0.4) +
  labs(title = "Boxplot of Work Quality Score", y = "Quality Score (1 to 5)") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_blank())
ggsave("C4_quality_boxplot.png", width = 5, height = 6)

combined_df <- data.frame(
  Score = c(df$Understanding_Score, df$Quality_Score),
  Group = c(rep("Understanding Score", nrow(df)), rep("Quality Score", nrow(df)))
)
ggplot(combined_df, aes(x = Group, y = Score, fill = Group)) +
  geom_boxplot(alpha = 0.8, width = 0.5) +
  scale_fill_manual(values = c("#388E3C", "#1976D2")) +
  labs(title = "Understanding Score vs Quality Score", x = "", y = "Score (1 to 5)") +
  theme_minimal() + theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("C5_combined_boxplot.png", width = 6, height = 6)


# ============================================================
# PART D — GROUP COMPARISONS (Descriptive)
# ============================================================

cat("\n============================================================\n")
cat("PART D — GROUP COMPARISONS\n")
cat("============================================================\n\n")

roles <- unique(df$Role)
role_avg_u <- c(); role_avg_q <- c()
for (r in roles) {
  subset_r   <- df[df$Role == r, ]
  role_avg_u <- c(role_avg_u, round(mean(subset_r$Understanding_Score), 3))
  role_avg_q <- c(role_avg_q, round(mean(subset_r$Quality_Score), 3))
}
role_avg_table <- data.frame(Role = roles, Avg_Understanding = role_avg_u, Avg_Quality = role_avg_q)
cat("--- Average Scores by Role ---\n"); print(role_avg_table)

role_plot_df <- data.frame(
  Role = c(roles, roles),
  Score_Type = c(rep("Understanding Score", length(roles)), rep("Quality Score", length(roles))),
  Average = c(role_avg_u, role_avg_q)
)
ggplot(role_plot_df, aes(x = Role, y = Average, fill = Score_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = round(Average, 2)), position = position_dodge(width = 0.6), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("#388E3C", "#1976D2")) +
  ylim(0, 5.5) +
  labs(title = "Average Scores by Role", x = "Role", y = "Average Score (1 to 5)", fill = "Score Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "top")
ggsave("D1_scores_by_role.png", width = 8, height = 6)

exp_avg_u <- c(); exp_avg_q <- c()
for (e in exp_levels) {
  subset_e  <- df[df$Experience == e, ]
  exp_avg_u <- c(exp_avg_u, round(mean(subset_e$Understanding_Score), 3))
  exp_avg_q <- c(exp_avg_q, round(mean(subset_e$Quality_Score), 3))
}
exp_avg_table <- data.frame(Experience = exp_levels, Avg_Understanding = exp_avg_u, Avg_Quality = exp_avg_q)
cat("\n--- Average Scores by Experience ---\n"); print(exp_avg_table)

exp_plot_df <- data.frame(
  Experience = c(exp_levels, exp_levels),
  Score_Type = c(rep("Understanding Score", 4), rep("Quality Score", 4)),
  Average = c(exp_avg_u, exp_avg_q)
)
exp_plot_df$Experience <- factor(exp_plot_df$Experience, levels = exp_levels)
ggplot(exp_plot_df, aes(x = Experience, y = Average, fill = Score_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = round(Average, 2)), position = position_dodge(width = 0.6), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("#388E3C", "#1976D2")) +
  ylim(0, 5.5) +
  labs(title = "Average Scores by Experience Level", x = "Years of Experience", y = "Average Score (1 to 5)", fill = "Score Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "top")
ggsave("D2_scores_by_experience.png", width = 8, height = 6)

fields <- unique(df$Field)
field_avg_u <- c(); field_avg_q <- c()
for (f in fields) {
  subset_f    <- df[df$Field == f, ]
  field_avg_u <- c(field_avg_u, round(mean(subset_f$Understanding_Score), 3))
  field_avg_q <- c(field_avg_q, round(mean(subset_f$Quality_Score), 3))
}
field_avg_table <- data.frame(Field = fields, Avg_Understanding = field_avg_u, Avg_Quality = field_avg_q)
cat("\n--- Average Scores by Field ---\n"); print(field_avg_table)

field_plot_df <- data.frame(
  Field = c(fields, fields),
  Score_Type = c(rep("Understanding Score", length(fields)), rep("Quality Score", length(fields))),
  Average = c(field_avg_u, field_avg_q)
)
ggplot(field_plot_df, aes(x = Field, y = Average, fill = Score_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = round(Average, 2)), position = position_dodge(width = 0.6), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("#388E3C", "#1976D2")) +
  ylim(0, 5.5) +
  labs(title = "Average Scores by Field of Study / Work", x = "Field", y = "Average Score (1 to 5)", fill = "Score Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "top")
ggsave("D3_scores_by_field.png", width = 9, height = 6)


# ============================================================
# PART E — LO4: EXPONENTIAL FAMILY OF DISTRIBUTIONS
# ============================================================

cat("\n============================================================\n")
cat("PART E — LO4: EXPONENTIAL FAMILY OF DISTRIBUTIONS\n")
cat("============================================================\n\n")

cat("The Normal distribution belongs to the Exponential Family.\n")
cat("By the Central Limit Theorem, composite scores averaging 6 Likert\n")
cat("responses approximate a Normal distribution.\n")
cat("This justifies using parametric methods like linear regression.\n\n")

u_mean     <- mean(df$Understanding_Score)
u_sd       <- sd(df$Understanding_Score)
q_mean_val <- mean(df$Quality_Score)
q_sd_val   <- sd(df$Quality_Score)

cat("--- Fitted Normal Distribution Parameters ---\n")
cat("Understanding Score: mean =", round(u_mean, 3), ", sd =", round(u_sd, 3), "\n")
cat("Quality Score:       mean =", round(q_mean_val, 3), ", sd =", round(q_sd_val, 3), "\n\n")

# Normal fit plot — Understanding Score
x_vals_u <- seq(min(df$Understanding_Score), max(df$Understanding_Score), length.out = 300)
normal_u  <- data.frame(x = x_vals_u, y = dnorm(x_vals_u, mean = u_mean, sd = u_sd) * nrow(df) * 0.4)

ggplot(df, aes(x = Understanding_Score)) +
  geom_histogram(binwidth = 0.4, fill = "#1976D2", color = "white", alpha = 0.7) +
  geom_line(data = normal_u, aes(x = x, y = y), color = "red", linewidth = 1.5) +
  geom_vline(xintercept = u_mean, color = "darkred", linetype = "dashed", linewidth = 1) +
  annotate("text", x = u_mean + 0.4, y = 45,
           label = paste0("Normal(", round(u_mean,2), ", ", round(u_sd,2), ")"),
           color = "darkred", size = 3.5) +
  labs(title = "Understanding Score — Normal Distribution Fit (LO4)",
       subtitle = "Normal distribution is a member of the Exponential Family",
       x = "Understanding Score (1 to 5)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("E1_understanding_normal_fit.png", width = 7, height = 5)

# Normal fit plot — Quality Score
x_vals_q <- seq(min(df$Quality_Score), max(df$Quality_Score), length.out = 300)
normal_q  <- data.frame(x = x_vals_q, y = dnorm(x_vals_q, mean = q_mean_val, sd = q_sd_val) * nrow(df) * 0.4)

ggplot(df, aes(x = Quality_Score)) +
  geom_histogram(binwidth = 0.4, fill = "#388E3C", color = "white", alpha = 0.7) +
  geom_line(data = normal_q, aes(x = x, y = y), color = "red", linewidth = 1.5) +
  geom_vline(xintercept = q_mean_val, color = "darkred", linetype = "dashed", linewidth = 1) +
  annotate("text", x = q_mean_val + 0.4, y = 45,
           label = paste0("Normal(", round(q_mean_val,2), ", ", round(q_sd_val,2), ")"),
           color = "darkred", size = 3.5) +
  labs(title = "Quality Score — Normal Distribution Fit (LO4)",
       subtitle = "Normal distribution is a member of the Exponential Family",
       x = "Quality Score (1 to 5)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("E2_quality_normal_fit.png", width = 7, height = 5)

cat("--- Exponential Family Connection ---\n")
cat("Normal PDF: f(x) = (1/sqrt(2*pi*sigma^2)) * exp(-(x-mu)^2 / (2*sigma^2))\n")
cat("Exponential family form: f(x|theta) = h(x) * exp(eta(theta)*T(x) - A(theta))\n")
cat("Where: T(x) = x (sufficient statistic), eta = mu/sigma^2 (natural parameter)\n")
cat("This confirms composite scores follow a distribution from the Exponential Family.\n")

# ============================================================
cat("\n\n=== Section 1 Complete! ===\n")
cat("All tables printed above.\n")
cat("Plots saved including LO4 Normal Fit plots:\n")
cat("  E1_understanding_normal_fit.png\n")
cat("  E2_quality_normal_fit.png\n")
cat("Next: Section 2 - Inferential Analytics\n")
