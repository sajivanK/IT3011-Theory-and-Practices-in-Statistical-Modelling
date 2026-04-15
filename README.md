# IT3011 — Theory and Practices in Statistical Modelling

## Does Better Project Understanding Lead to Higher Quality of Work?

> **Group Assignment** | SLIIT | Module: IT3011

---

## Project Overview

This project investigates the analytical statement:

**"Better project understanding leads to higher quality of work"**

We collected **315 primary survey responses** from engineering professionals and students across multiple disciplines and validated the statement using three layers of statistical analysis — Descriptive, Inferential, and Predictive Analytics.

---

## Key Results

| Metric | Value |
|---|---|
| Total Respondents | 315 |
| Spearman Correlation (r) | 0.790 |
| Simple Regression R² | 69.7% |
| Multiple Regression R² | 78.4% |
| p-value (all tests) | ≈ 0 |
| Bayesian Posterior P(H₁\|data) | 1.000 |
| Overfitting (Train-Test diff) | 0.076 ✅ |

---

## Repository Structure

```
IT3011-Statistical-Modelling/
│
├── data/
│   └── Project_understanding_vs_Quality_work.csv   # Primary survey data (315 responses)
│
├── scripts/
│   ├── descriptive.R       # Section 1: Descriptive Analytics
│   ├── inferential.R       # Section 2: Inferential Analytics
│   └── predictive.R        # Section 3: Predictive Analytics
│
├── plots/
│   ├── A1_role_distribution.png
│   ├── A2_field_distribution.png
│   ├── A3_experience_distribution.png
│   ├── B1_mean_per_question.png
│   ├── B2_likert_stacked_bar.png
│   ├── C1_understanding_histogram.png
│   ├── C2_quality_histogram.png
│   ├── C3_understanding_boxplot.png
│   ├── C4_quality_boxplot.png
│   ├── C5_combined_boxplot.png
│   ├── D1_scores_by_role.png
│   ├── D2_scores_by_experience.png
│   ├── D3_scores_by_field.png
│   ├── E1_understanding_normal_fit.png
│   ├── E2_quality_normal_fit.png
│   ├── A1_qq_understanding.png
│   ├── A2_qq_quality.png
│   ├── B1_scatter_correlation.png
│   ├── C1_quality_by_role.png
│   ├── C2_quality_by_experience.png
│   ├── C3_quality_by_field.png
│   ├── D1_daily_responses.png
│   ├── D2_cumulative_responses.png
│   ├── D3_rolling_correlation.png
│   ├── E1_bayesian_prior_posterior.png
│   ├── A1_simple_regression.png
│   ├── B1_residual_plot_simple.png
│   ├── B2_qq_residuals_simple.png
│   ├── C1_actual_vs_predicted.png
│   ├── D1_coefficient_plot.png
│   └── E1_correlation_heatmap.png
│
└── README.md
```

---

## Survey Design

The survey used a **5-point Likert scale** (1 = Strongly Disagree, 5 = Strongly Agree).

| Questions | Construct Measured |
|---|---|
| Q4 – Q9 | Project Understanding |
| Q10 – Q15 | Work Quality |

> **Note:** Q13 was negatively worded and was reverse-scored using `Q13_R = 6 - Q13` before analysis.

**Composite Scores:**
```
Understanding Score = (Q4 + Q5 + Q6 + Q7 + Q8 + Q9) / 6
Quality Score       = (Q10 + Q11 + Q12 + Q13_R + Q14 + Q15) / 6
```

---

## Section 1 — Descriptive Analytics (`descriptive.R`)

Covers **LO1** and **LO4**

### What was done

**Part A — Demographics**
- Frequency tables and bar charts for Role, Field of Study, and Experience level
- 315 respondents: 38.1% Professional Engineers, 27% Engineering Students, 25.1% Interns

**Part B — Individual Question Analysis**
- Descriptive statistics (Mean, Median, SD, Min, Max) for all 12 questions
- Likert response stacked bar chart showing distribution of 1–5 ratings
- Mean score bar chart grouped by Understanding vs Quality questions

**Part C — Composite Score Analysis**
- Summary statistics for both composite scores
- Histograms with mean line overlay
- Individual and combined boxplots

**Part D — Group Comparisons**
- Average Understanding and Quality scores by Role, Experience, and Field

**Part E — LO4: Exponential Family**
- Fitted Normal distribution curve over both composite score histograms
- Demonstrates that composite scores approximate a Normal distribution (member of Exponential Family)
- Justifies use of parametric regression methods

### Key Descriptive Findings

| Score | Mean | Median | SD |
|---|---|---|---|
| Understanding Score | 3.454 | 3.500 | 0.997 |
| Quality Score | 3.603 | 3.667 | 0.861 |

---

## Section 2 — Inferential Analytics (`inferential.R`)

Covers **LO2**, **LO5**, and **LO6**

### What was done

**Part A — Normality Test (Shapiro-Wilk)**
- Tested both composite scores for normality
- Both failed (p < 0.05) — expected with Likert integer data
- Decision: automatically switched to Spearman correlation

**Part B — Correlation Analysis (Spearman)**

| | Value |
|---|---|
| Method | Spearman (non-parametric) |
| Correlation coefficient r | **0.790** |
| p-value | **≈ 0** |
| Strength | Strong positive |
| Decision | Reject H₀ — significant correlation confirmed |

**Part C — One-Way ANOVA (Group Differences)**

| Group | F-value | p-value | Result |
|---|---|---|---|
| Role | 3.583 | 0.014 | ✅ Significant |
| Experience | 5.949 | 0.001 | ✅ Highly Significant |
| Field | 1.892 | 0.112 | ❌ Not Significant |

**Part D — LO5: Time Series Analysis**
- Parsed Google Forms timestamps to analyse data collection patterns
- Tracked daily and cumulative response growth over 38 days
- Rolling Spearman correlation as sample grew: started at 0.354 (n=30), stabilised at 0.790 (n=315)
- Confirms finding is stable and not dependent on early responses

**Part E — LO6: Bayesian Analysis**
- Prior probability P(H₁) = 0.60 (based on literature)
- Bayes Factor: log(BF10) = 257.26 — Extreme evidence
- Posterior probability P(H₁|data) = **1.000**
- Strongly confirms the analytical statement

---

## Section 3 — Predictive Analytics (`predictive.R`)

Covers **LO3**

### What was done

**Part A — Simple Linear Regression**

```
Quality Score = 1.113 + 0.721 × Understanding Score
```

| Metric | Value |
|---|---|
| R² | 0.6968 (69.7%) |
| Adjusted R² | 0.6959 |
| F-statistic | 719.4 |
| p-value | ≈ 0 |
| Slope interpretation | +1 unit understanding → +0.721 quality |

**Part B — Residual Analysis**

| Check | Result | Status |
|---|---|---|
| Shapiro-Wilk on residuals | p = 0.064 | ✅ Normal |
| Homoscedasticity ratio | 1.22 | ✅ Satisfied |

**Part C — Overfitting Check (80/20 Train-Test Split)**

| Metric | Value |
|---|---|
| Training R² | 0.7087 |
| Test R² | 0.6323 |
| Difference | 0.0764 |
| Verdict | ✅ No significant overfitting |

**Part D — Multiple Linear Regression**

```
Quality Score = 0.707 + (-0.112)×Q4 + 0.236×Q5 + 0.375×Q6 + (-0.108)×Q7 + 0.234×Q8 + 0.168×Q9
```

| Metric | Value |
|---|---|
| R² | 0.7836 (78.4%) |
| Adjusted R² | 0.7794 |
| Significant predictors | All 6 (Q4–Q9) |
| Strongest predictor | **Q6** (coefficient = 0.375) |

> Q6 asks: *"I understood how my assigned tasks contributed to the overall project outcome."*  
> This is the most powerful individual predictor of work quality.

**Part E — Multicollinearity Check (VIF)**

| Predictor | VIF | Status |
|---|---|---|
| Q4 | 5.068 | 🟡 Moderate |
| Q5 | 5.201 | 🟡 Moderate |
| Q6 | 3.936 | ✅ Acceptable |
| Q7 | 6.209 | 🟡 Moderate |
| Q8 | 2.703 | ✅ Acceptable |
| Q9 | 4.132 | ✅ Acceptable |

> Moderate multicollinearity in Q4, Q5, Q7 is expected — these questions all measure aspects of the same construct (project understanding). All VIF values are below 10. Overall model validity is maintained.

---

## Learning Outcomes Coverage

| LO | Description | Where Covered |
|---|---|---|
| LO1 | Discrete and Continuous Probability Distributions | descriptive.R — Parts B, C |
| LO2 | Hypothesis Testing for Decision Making | inferential.R — Parts A, B, C |
| LO3 | Regression Analysis and Interpretation | predictive.R — Parts A, B, C, D |
| LO4 | Exponential Family of Distributions | descriptive.R — Part E |
| LO5 | Time Series Modelling | inferential.R — Part D |
| LO6 | Bayesian Methods | inferential.R — Part E |

---

## How to Run

### Requirements

- R (version 4.0 or higher)
- RStudio (recommended)
- Package: `ggplot2`

### Installation

```r
install.packages("ggplot2")
```

### Steps

1. Clone or download this repository
2. Open RStudio
3. Go to **Session → Set Working Directory → To Source File Location**
4. Place the CSV file in the same folder as the R scripts
5. Run the scripts in order:

```r
source("scripts/descriptive.R")   # Section 1
source("scripts/inferential.R")   # Section 2
source("scripts/predictive.R")    # Section 3
```

6. All plots will be saved as PNG files in your working directory

---

## Limitations

- **Multicollinearity:** Q4, Q5, Q7 show moderate VIF (5–6) due to questions measuring related aspects of the same construct
- **Self-reported bias:** Both understanding and quality were rated by respondents themselves, which may inflate scores
- **Cross-sectional design:** Data collected at one point in time — longitudinal tracking of the same individuals would enable stronger time series analysis
- **Sample scope:** Respondents are primarily from engineering fields — generalisability to other industries requires caution

---

## Conclusion

The analytical statement is **strongly confirmed**:

> *"Better project understanding leads to higher quality of work"*

The relationship is significant (r = 0.790, p ≈ 0), strong (R² = 78.4%), robust (no overfitting), and universal across engineering roles and fields. The most critical factor is **Q6** — whether team members understand how their tasks contribute to the overall project outcome.

---

## Module Information

| Field | Detail |
|---|---|
| Module | IT3011 — Theory and Practices in Statistical Modelling |
| Assignment | Group Assignment |
| Institution | SLIIT |
| Data | Primary — Google Forms Survey |
| Sample Size | 315 respondents |
| Collection Period | 23 February 2026 to 2 April 2026 |
